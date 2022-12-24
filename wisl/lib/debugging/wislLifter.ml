open WSemantics
open WSyntax
open Gil_syntax
module L = Logging
module DL = Debugger_log
module Exec_map = Debugger.Utils.Exec_map
module Unify_map = Debugger.Utils.Unify_map
open Syntaxes.Option
module ExtList = Utils.ExtList
module Annot = WParserAndCompiler.Annot
open Annot
open WBranchCase
open Debugger.Lifter

type rid = L.Report_id.t [@@deriving yojson, show]

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (Verification : Engine.Verifier.S with type annot = Annot.t) =
struct
  open Exec_map

  type memory_error = WislSMemory.err_t
  type tl_ast = WParserAndCompiler.tl_ast
  type memory = WislSMemory.t
  type annot = Annot.t

  module CmdReport = Verification.SAInterpreter.Logging.ConfigReport
  module Gil_lifter = Gil.Lifter

  type cmd_report = CmdReport.t [@@deriving yojson]
  type branch_case = WBranchCase.t [@@deriving yojson]
  type branch_data = rid * BranchCase.t option [@@deriving yojson]
  type exec_data = cmd_report executed_cmd_data [@@deriving yojson]

  let annot_to_wisl_stmt annot wisl_ast =
    let origin_id = annot.origin_id in
    let wprog = WProg.get_by_id wisl_ast origin_id in
    match wprog with
    | `WStmt wstmt ->
        DL.log (fun m -> m "WISL STMT: %a" WStmt.pp wstmt);
        Some wstmt.snode
    | _ -> None

  let get_origin_node_str wisl_ast origin_id =
    let node = WProg.get_by_id wisl_ast origin_id in
    match node with
    | `Return we -> Some (Fmt.str "return %a" WExpr.pp we)
    | `WExpr we -> Some (Fmt.str "Evaluating: %a" WExpr.pp we)
    | `WLCmd lcmd -> Some (Fmt.str "%a" WLCmd.pp lcmd)
    | `WStmt stmt -> Some (Fmt.str "%a" WStmt.pp_head stmt)
    | `WLExpr le -> Some (Fmt.str "LEXpr: %a" WLExpr.pp le)
    | `WFun f -> Some (Fmt.str "WFun: %s" f.name)
    | `None -> None
    | _ -> failwith "get_origin_node_str: Unknown Kind of Node"

  let get_fun_call_name exec_data =
    let cmd = CmdReport.(exec_data.cmd_report.cmd) in
    match cmd with
    | Cmd.Call (_, name_expr, _, _, _) -> (
        match name_expr with
        | Expr.Lit (Literal.String name) -> Some name
        | _ ->
            failwith "get_fun_call_name: function name wasn't a literal expr!")
    | _ -> None

  type map = (branch_case, cmd_data, branch_data) Exec_map.t

  and cmd_data = {
    ids : rid list;
    display : string;
    unifys : unification list;
    errors : string list;
    submap : map submap;
    gil_branch_path : BranchCase.path;
    branch_path : branch_case list;
    parent : parent; [@to_yojson fun _ -> `Null]
  }

  and parent = (map * (branch_data * branch_case) option) option
  [@@deriving yojson]

  module PartialCmds = struct
    type partial_data = {
      display : string;
      ids : rid ExtList.t;
      errors : string ExtList.t;
      mutable submap : map submap;
      mutable inner_path : branch_data list;
      unifys : unification ExtList.t;
      unexplored_paths : branch_data list Stack.t;
      out_paths : (branch_case * branch_data list) ExtList.t;
      mutable unknown_outs_count : int;
    }
    [@@deriving to_yojson]

    let make_partial_data display =
      {
        display;
        ids = ExtList.make ();
        errors = ExtList.make ();
        unifys = ExtList.make ();
        submap = NoSubmap;
        inner_path = [];
        unexplored_paths = Stack.create ();
        out_paths = ExtList.make ();
        unknown_outs_count = 0;
      }

    type t = (int, partial_data) Hashtbl.t [@@deriving to_yojson]

    let update_partial_data end_kind exec_data d =
      let { id; unifys; errors; cmd_report; _ } = exec_data in
      let annot = CmdReport.(cmd_report.annot) in
      d.ids |> ExtList.append id;
      unifys |> List.iter (fun unify -> d.unifys |> ExtList.append unify);
      errors |> List.iter (fun error -> d.errors |> ExtList.append error);
      (match exec_data.kind with
      | Branch cases ->
          cases
          |> List.iter (fun (case, _) ->
                 let path = (id, Some case) :: d.inner_path in
                 match end_kind with
                 | NotEnd -> d.unexplored_paths |> Stack.push path
                 | EndNormal ->
                     let count = d.unknown_outs_count in
                     let case = Gil count in
                     d.unknown_outs_count <- count + 1;
                     d.out_paths |> ExtList.append (case, path)
                 | EndWithBranch _ ->
                     failwith "EndWithBranch on branching cmd not supported!")
      | Normal -> (
          let path = (id, None) :: d.inner_path in
          match end_kind with
          | NotEnd -> ()
          | EndNormal ->
              let count = d.unknown_outs_count in
              let case = Gil count in
              d.unknown_outs_count <- count + 1;
              d.out_paths |> ExtList.append (case, path)
          | EndWithBranch case -> d.out_paths |> ExtList.append (case, path))
      | Final -> ());
      match (d.submap, annot.nest_kind) with
      | _, NoNest -> ()
      | NoSubmap, Proc p -> d.submap <- Proc p
      | _, _ ->
          DL.failwith
            (fun () ->
              [
                ("annot", Annot.to_yojson annot);
                ("exec_data", exec_data_to_yojson exec_data);
                ("partial_data", partial_data_to_yojson d);
              ])
            "WislLifter.update_partial_data: multiple submaps in WISL \
             statement!"

    type partial_cmd_result =
      | Finished of {
          ids : rid list;
          display : string;
          unifys : unification list;
          errors : string list;
          cmd_kind : (branch_case, branch_data) cmd_kind;
          submap : map submap;
        }
      | StepAgain of (rid option * BranchCase.t option)

    let make_finished_partial
        is_final
        { ids; display; unifys; errors; out_paths; submap; _ } =
      let ids = ids |> ExtList.to_list in
      let unifys = unifys |> ExtList.to_list in
      let errors = errors |> ExtList.to_list in
      let cmd_kind =
        match out_paths |> ExtList.to_list with
        | [] | [ (_, [ (_, None) ]) ] -> if is_final then Final else Normal
        | paths ->
            let cases =
              paths
              |> List.map (fun (case, path) ->
                     let branch_data = List.hd path in
                     (case, branch_data))
            in
            Branch cases
      in
      Finished { ids; display; unifys; errors; cmd_kind; submap }

    let update annot (d : partial_data) (exec_data : exec_data) :
        partial_cmd_result =
      let failwith s =
        DL.failwith
          (fun () ->
            [
              ("annot", Annot.to_yojson annot);
              ("exec_data", exec_data_to_yojson exec_data);
              ("partial_data", partial_data_to_yojson d);
            ])
          ("WislLifter.PartialCmds.update: " ^ s)
      in
      let end_kind =
        match annot.stmt_kind with
        | Multi b -> b
        | _ -> failwith "tried to update partial with non-Multi cmd!"
      in
      d |> update_partial_data end_kind exec_data;
      let is_final, result =
        match (exec_data.kind, end_kind) with
        | Final, _ -> (true, None)
        | _, (EndNormal | EndWithBranch _) | Branch _, _ -> (false, None)
        | Normal, _ -> (false, Some (StepAgain (None, None)))
      in
      match result with
      | Some r -> r
      | None -> (
          match d.unexplored_paths |> Stack.pop_opt with
          | Some path ->
              let id, gil_case = List.hd path in
              d.inner_path <- path;
              StepAgain (Some id, gil_case)
          | None -> make_finished_partial is_final d)

    let create annot tl_ast =
      match annot.stmt_kind with
      | Multi NotEnd ->
          let* origin_id = annot.origin_id in
          let+ display = get_origin_node_str tl_ast (Some origin_id) in
          make_partial_data display
      | _ -> None

    let handle exec_data tl_ast partial_cmds =
      let annot =
        let cmd_report = exec_data.cmd_report in
        CmdReport.(cmd_report.annot)
      in
      let* origin_id = annot.origin_id in
      let+ partial_data =
        match Hashtbl.find_opt partial_cmds origin_id with
        | None ->
            let+ pd = create annot tl_ast in
            Hashtbl.add partial_cmds origin_id pd;
            pd
        | pd -> pd
      in
      let result = update annot partial_data exec_data in
      (match result with
      | Finished _ -> Hashtbl.remove partial_cmds origin_id
      | _ -> ());
      result
  end

  type t = {
    proc_name : string;
    gil_state : Gil_lifter.t; [@to_yojson Gil_lifter.dump]
    tl_ast : tl_ast; [@to_yojson fun _ -> `Null]
    partial_cmds : PartialCmds.t;
    mutable map : map;
    id_map : (rid, map) Hashtbl.t; [@to_yojson fun _ -> `Null]
    mutable before_partial : (rid * BranchCase.t option) option;
    mutable is_loop_func : bool;
  }
  [@@deriving to_yojson]

  let path_of_map = function
    | Nothing -> []
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        data.branch_path

  let gil_path_of_map = function
    | Nothing -> []
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        data.gil_branch_path

  let id_of_map_opt ?(last = false) = function
    | Nothing -> None
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        if last then Some (List.hd (List.rev data.ids))
        else Some (List.hd data.ids)

  let id_of_map ?(last = false) map =
    match id_of_map_opt ~last map with
    | None -> failwith "id_of_map: Nothing"
    | Some id -> id

  let new_cmd
      id_map
      kind
      ids
      display
      unifys
      errors
      gil_branch_path
      ?(submap = NoSubmap)
      ~(parent : parent)
      () : map =
    let branch_path =
      match parent with
      | None -> []
      | Some (parent_map, case) -> (
          let parent_path = path_of_map parent_map in
          match case with
          | None -> parent_path
          | Some (_, case) -> case :: parent_path)
    in
    let data =
      {
        ids;
        display;
        unifys;
        errors;
        submap;
        gil_branch_path;
        branch_path;
        parent;
      }
    in
    let cmd =
      match kind with
      | Normal -> Cmd { data; next = Nothing }
      | Branch cases -> (
          match cases with
          | [ (Gil _, (_, None)) ] -> Cmd { data; next = Nothing }
          | _ ->
              let nexts = Hashtbl.create (List.length cases) in
              cases
              |> List.iter (fun (case, bdata) ->
                     Hashtbl.add nexts case (bdata, Nothing));
              BranchCmd { data; nexts })
      | Final -> FinalCmd { data }
    in
    ids |> List.iter (fun id -> Hashtbl.replace id_map id cmd);
    cmd

  let dump = to_yojson

  let convert_kind id = function
    | Normal -> Normal
    | Final -> Final
    | Branch cases -> (
        match cases with
        | (BranchCase.GuardedGoto _, ()) :: _ ->
            let cases =
              cases
              |> List.map (fun (case, _) ->
                     match case with
                     | BranchCase.GuardedGoto b -> (IfElse b, (id, Some case))
                     | _ -> failwith "convert_kind: inconsistent branch cases!")
            in
            Branch cases
        | (BranchCase.LCmd _, ()) :: _ ->
            let cases =
              cases
              |> List.map (fun (case, _) ->
                     match case with
                     | BranchCase.LCmd lcmd -> (LCmd lcmd, (id, Some case))
                     | _ -> failwith "convert_kind: inconsistent branch cases!")
            in
            Branch cases
        | _ -> failwith "convert_kind: unsupported branch case!")

  let rec insert_new_cmd
      (new_cmd : parent:parent -> unit -> map)
      (new_id : rid)
      (id : rid)
      (gil_case : BranchCase.t option)
      state =
    let failwith s = failwith ("WislLifter.insert_new_cmd: " ^ s) in
    let { id_map; gil_state; _ } = state in
    match Hashtbl.find_opt id_map id with
    | None -> (
        DL.log (fun m ->
            m "couldn't find id %a; attempting with previous step from GIL."
              pp_rid id);
        match gil_state |> Gil_lifter.previous_step id with
        | None -> failwith "couldn't step back any farther!"
        | Some (prev_id, _) ->
            insert_new_cmd new_cmd new_id prev_id gil_case state)
    | Some map -> (
        match map with
        | Cmd c ->
            let parent = Some (map, None) in
            c.next <- new_cmd ~parent ()
        | BranchCmd { nexts; _ } ->
            let case, next, bdata =
              match gil_case with
              | None ->
                  let rec aux new_id =
                    let result =
                      nexts
                      |> Hashtbl.find_map (fun case (bdata, next) ->
                             match bdata with
                             | case_id, None when case_id = new_id ->
                                 Some (case, next, bdata)
                             | _ -> None)
                    in
                    match result with
                    | Some r -> r
                    | None -> (
                        let prev =
                          gil_state |> Gil_lifter.previous_step new_id
                        in
                        match prev with
                        | Some (new_id, _) ->
                            DL.log (fun m ->
                                m
                                  "Inserting without gil case; attempting to \
                                   look back for link");
                            aux new_id
                        | _ ->
                            failwith
                              "HORROR - tried to insert without branch case!")
                  in
                  aux new_id
              | Some gil_case ->
                  Hashtbl.find_map
                    (fun case (bdata, next) ->
                      let gil_case' = bdata |> snd |> Option.get in
                      if gil_case <> gil_case' then None
                      else Some (case, next, bdata))
                    nexts
                  |> Option.get
            in
            if next <> Nothing then
              failwith "HORROR - tried to insert to non-Nothing!";
            let parent = Some (map, Some (bdata, case)) in
            Hashtbl.replace nexts case (bdata, new_cmd ~parent ())
        | _ -> failwith "HORROR - tried to insert to FinalCmd or Nothing!")

  let prepare_basic_cmd ?display ?(final = false) tl_ast id_map exec_data =
    let { cmd_report; _ } = exec_data in
    let annot = CmdReport.(cmd_report.annot) in
    let { origin_id; nest_kind; _ } = annot in
    let display =
      match display with
      | Some d -> d
      | None ->
          get_origin_node_str tl_ast origin_id
          |> Option.value ~default:"Unknown command!"
    in
    let { id; unifys; errors; branch_path = gil_branch_path; kind; _ } =
      exec_data
    in
    let submap =
      match nest_kind with
      | NoNest -> NoSubmap
      | Proc p -> Proc p
    in
    let kind = if final then Final else convert_kind id kind in
    new_cmd id_map kind [ id ] display unifys errors gil_branch_path ~submap

  let handle_loop_prefix exec_data =
    let annot = CmdReport.(exec_data.cmd_report.annot) in
    match annot.stmt_kind with
    | LoopPrefix ->
        Some
          (match exec_data.cmd_report.cmd with
          | Cmd.GuardedGoto _ ->
              ExecNext (None, Some (BranchCase.GuardedGoto true))
          | _ -> ExecNext (None, None))
    | _ -> None

  let init_or_handle prev_id branch_case exec_data state =
    let Debugger.Lifter.{ id; _ } = exec_data in
    DL.log (fun m ->
        m
          ~json:
            [
              ("state", dump state); ("exec_data", exec_data_to_yojson exec_data);
            ]
          "HANDLING %a (prev %a)" L.Report_id.pp id (pp_option L.Report_id.pp)
          prev_id);
    let { tl_ast; partial_cmds; id_map; proc_name; is_loop_func; _ } = state in
    match handle_loop_prefix exec_data with
    | Some result ->
        state.is_loop_func <- true;
        result
    | None -> (
        match PartialCmds.handle exec_data tl_ast partial_cmds with
        | Some (StepAgain result) ->
            if Option.is_none state.before_partial then
              prev_id
              |> Option.iter (fun prev_id ->
                     state.before_partial <- Some (prev_id, branch_case));
            ExecNext result
        | None ->
            let display, final =
              if is_loop_func && get_fun_call_name exec_data = Some proc_name
              then (Some "<end of loop>", true)
              else (None, false)
            in
            let new_cmd =
              prepare_basic_cmd ?display ~final tl_ast id_map exec_data
            in
            (match (state.map, prev_id) with
            | Nothing, _ -> state.map <- new_cmd ~parent:None ()
            | _, Some prev_id ->
                insert_new_cmd new_cmd id prev_id branch_case state
            | _, _ ->
                failwith
                  "HORROR - tried to insert to non-Nothing map without \
                   previous id!");
            Stop
        | Some (Finished { ids; display; unifys; errors; cmd_kind; submap }) ->
            let gil_branch_path =
              Gil_lifter.path_of_id (List.hd ids) state.gil_state
            in
            let new_cmd =
              new_cmd id_map cmd_kind ids display unifys errors gil_branch_path
                ~submap
            in
            let prev_id, branch_case =
              match state.before_partial with
              | Some (prev_id, branch_case) ->
                  state.before_partial <- None;
                  (Some prev_id, branch_case)
              | None -> (prev_id, branch_case)
            in
            (match (state.map, prev_id) with
            | Nothing, _ -> state.map <- new_cmd ~parent:None ()
            | _, Some prev_id ->
                insert_new_cmd new_cmd id prev_id branch_case state
            | _, _ ->
                failwith
                  "HORROR - tried to insert to non-Nothing map without \
                   previous id!");
            Stop)

  let init proc_name tl_ast exec_data =
    let gil_state = Gil.get_state () in
    let+ tl_ast in
    let partial_cmds = Hashtbl.create 0 in
    let id_map = Hashtbl.create 0 in
    let before_partial = None in
    let state =
      {
        proc_name;
        gil_state;
        tl_ast;
        partial_cmds;
        map = Nothing;
        id_map;
        before_partial;
        is_loop_func = false;
      }
    in
    let result = init_or_handle None None exec_data state in
    (state, result)

  let init_exn proc_name tl_ast exec_data =
    match init proc_name tl_ast exec_data with
    | None -> failwith "init: wislLifter needs a tl_ast!"
    | Some x -> x

  let handle_cmd prev_id branch_case (exec_data : exec_data) state =
    init_or_handle (Some prev_id) branch_case exec_data state

  let get_gil_map _ = failwith "get_gil_map: not implemented!"

  let package_case (bd : branch_data) (case : branch_case) :
      Packaged.branch_case =
    let json = branch_case_to_yojson case in
    let kind, display =
      match case with
      | IfElse b -> ("IfElse", ("If/Else", Fmt.str "%B" b))
      | LCmd x -> ("LCmd", ("Logical command", Fmt.str "%d" x))
      | Gil _ -> (
          let id, gil_case = bd in
          match gil_case with
          | Some gil_case ->
              let kind_display, display =
                Packaged.(package_gil_case gil_case).display
              in
              let kind = "GIL" in
              let kind_display = Fmt.str "(GIL) %s" kind_display in
              let display = Fmt.str "(%a) %s" pp_rid id display in
              (kind, (kind_display, display))
          | None -> ("GIL", ("(GIL) Unknown", Fmt.str "(%a) Unknown" pp_rid id))
          )
    in
    { kind; display; json }

  let package_data package { ids; display; unifys; errors; submap; _ } =
    let submap =
      match submap with
      | NoSubmap -> NoSubmap
      | Proc p -> Proc p
      | Submap map -> Submap (package map)
    in
    Packaged.{ ids; display; unifys; errors; submap }

  let package = Packaged.package package_data package_case
  let get_lifted_map_exn { map; _ } = package map
  let get_lifted_map state = Some (get_lifted_map_exn state)

  let get_unifys_at_id id { id_map; _ } =
    let map = Hashtbl.find id_map id in
    match map with
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> data.unifys
    | _ -> failwith "get_unifys_at_id: HORROR - tried to get unifys at non-cmd!"

  let get_root_id { map; _ } =
    match map with
    | Nothing -> None
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        Some (List.hd data.ids)

  let path_of_id id { id_map; _ } =
    let map = Hashtbl.find id_map id in
    gil_path_of_map map

  let existing_next_steps id { gil_state; id_map; _ } =
    Gil_lifter.existing_next_steps id gil_state
    |> List.filter (fun (id, _) -> Hashtbl.mem id_map id)

  let next_gil_step id case state =
    let failwith s =
      DL.failwith
        (fun () ->
          [
            ("state", dump state);
            ("id", rid_to_yojson id);
            ("case", opt_to_yojson Packaged.branch_case_to_yojson case);
          ])
        ("next_gil_step: " ^ s)
    in
    match (Hashtbl.find state.id_map id, case) with
    | Nothing, _ -> failwith "HORROR - cmd at id is Nothing!"
    | FinalCmd _, _ -> failwith "can't get next at final cmd!"
    | Cmd _, Some _ -> failwith "got branch case at non-branch cmd!"
    | BranchCmd _, None -> failwith "expected branch case at branch cmd!"
    | Cmd { data; _ }, None ->
        let id = List.hd (List.rev data.ids) in
        (id, None)
    | BranchCmd { nexts; _ }, Some case -> (
        let case =
          Packaged.(case.json) |> branch_case_of_yojson |> Result.get_ok
        in
        match Hashtbl.find_opt nexts case with
        | None -> failwith "branch case not found!"
        | Some ((id, case), _) -> (id, case))

  let previous_step id { id_map; _ } =
    match Hashtbl.find id_map id with
    | Nothing -> None
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        let+ parent, case = data.parent in
        let id = id_of_map parent in
        let case =
          case |> Option.map (fun (bdata, case) -> package_case bdata case)
        in
        (id, case)

  let select_next_path case id { gil_state; _ } =
    Gil_lifter.select_next_path case id gil_state

  let find_unfinished_path ?at_id state =
    let { map; id_map; _ } = state in
    let rec aux = function
      | Nothing ->
          DL.failwith
            (fun () ->
              [
                ("state", dump state);
                ("at_id", opt_to_yojson rid_to_yojson at_id);
              ])
            "find_unfinished_path: started at Nothing"
      | Cmd { data = { ids; _ }; next = Nothing } ->
          let id = List.hd (List.rev ids) in
          Some (id, None)
      | Cmd { next; _ } -> aux next
      | BranchCmd { nexts; _ } -> (
          match
            Hashtbl.find_map
              (fun _ ((id, gil_case), next) ->
                if next = Nothing then Some (id, gil_case) else None)
              nexts
          with
          | None -> Hashtbl.find_map (fun _ (_, next) -> aux next) nexts
          | result -> result)
      | FinalCmd _ -> None
    in
    let map =
      match at_id with
      | None -> map
      | Some id -> Hashtbl.find id_map id
    in
    aux map

  let get_wisl_stmt gil_cmd wisl_ast =
    let* annot =
      match gil_cmd with
      | Some (_, annot) -> Some annot
      | _ -> None
    in
    annot_to_wisl_stmt annot wisl_ast

  let get_cell_var_from_cmd gil_cmd wisl_ast =
    let open Syntaxes.Option in
    match wisl_ast with
    | Some ast -> (
        let* stmt = get_wisl_stmt gil_cmd ast in
        match stmt with
        | WStmt.Lookup (_, e) | WStmt.Update (e, _) -> Some (WExpr.str e)
        | _ -> None)
    | None -> (
        let open WislLActions in
        match gil_cmd with
        | Some (Cmd.LAction (_, name, [ _; Expr.BinOp (PVar var, _, _) ]), _)
          when name = str_ac GetCell -> Some var
        | _ -> None)

  let free_error_to_string msg_prefix prev_annot gil_cmd wisl_ast =
    let open Syntaxes.Option in
    let var =
      match wisl_ast with
      | Some ast -> (
          let* stmt = get_wisl_stmt gil_cmd ast in
          match stmt with
          (* TODO: Catch all the cases that use after free can happen to get the
                      variable names *)
          | WStmt.Dispose e | WStmt.Lookup (_, e) | WStmt.Update (e, _) ->
              Some (WExpr.str e)
          | _ -> None)
      | None -> (
          let open WislLActions in
          let* cmd, _ = gil_cmd in
          match cmd with
          | Cmd.LAction (_, name, [ Expr.BinOp (PVar var, _, _) ])
            when name = str_ac Dispose -> Some var
          | Cmd.LAction (_, name, [ _; Expr.BinOp (PVar var, _, _) ])
            when name = str_ac GetCell -> Some var
          | _ -> None)
    in
    let var = Option.value ~default:"" var in
    let msg_prefix = msg_prefix var in
    match prev_annot with
    | None -> Fmt.str "%s in specification" msg_prefix
    | Some annot -> (
        let origin_loc = Annot.get_origin_loc annot in
        match origin_loc with
        | None -> Fmt.str "%s at unknown location" msg_prefix
        | Some origin_loc ->
            let origin_loc =
              Debugger.Utils.location_to_display_location origin_loc
            in
            Fmt.str "%s at %a" msg_prefix Location.pp origin_loc)

  let get_previously_freed_annot loc =
    let annot = Logging.Log_queryer.get_previously_freed_annot loc in
    match annot with
    | None -> None
    | Some annot ->
        annot |> Yojson.Safe.from_string |> Annot.of_yojson |> Result.to_option

  let get_missing_resource_var wstmt =
    match wstmt with
    | Some stmt -> (
        match stmt with
        | WStmt.Lookup (_, e) | Update (e, _) -> Some (WExpr.str e)
        | _ -> None)
    | None -> None

  let get_missing_resource_msg missing_resource_error_info gil_cmd wisl_ast =
    let core_pred, loc, offset = missing_resource_error_info in
    let default_err_msg =
      let prefix =
        Fmt.str "Missing %s at location='%s'"
          (WislLActions.str_ga core_pred)
          loc
      in
      match offset with
      | None -> prefix
      | Some offset -> Fmt.str "%s, offset='%a'" prefix Expr.pp offset
    in
    match wisl_ast with
    | None -> default_err_msg
    | Some wisl_ast -> (
        match core_pred with
        | WislLActions.Cell -> (
            let wstmt = get_wisl_stmt gil_cmd wisl_ast in
            let var = get_missing_resource_var wstmt in
            match var with
            | Some var ->
                Fmt.str "Try adding %s -> #new_var to the specification" var
            | None -> default_err_msg)
        | _ -> default_err_msg)

  let memory_error_to_exception_info info : Debugger.Utils.exception_info =
    let id = Fmt.to_to_string WislSMemory.pp_err info.error in
    let description =
      match info.error with
      | WislSHeap.MissingResource missing_resource_error_info ->
          Some
            (get_missing_resource_msg missing_resource_error_info info.command
               info.tl_ast)
      | DoubleFree loc ->
          let prev_annot = get_previously_freed_annot loc in
          let msg_prefix var = Fmt.str "%s already freed" var in
          Some
            (free_error_to_string msg_prefix prev_annot info.command info.tl_ast)
      | UseAfterFree loc ->
          let prev_annot = get_previously_freed_annot loc in
          let msg_prefix var = Fmt.str "%s freed" var in
          Some
            (free_error_to_string msg_prefix prev_annot info.command info.tl_ast)
      | OutOfBounds (bound, _, _) ->
          let var = get_cell_var_from_cmd info.command info.tl_ast in
          Some
            (Fmt.str "%a is not in bounds %a" (Fmt.option Fmt.string) var
               (Fmt.option ~none:(Fmt.any "none") Fmt.int)
               bound)
      | _ -> None
    in
    { id; description }

  let add_variables = WislSMemory.add_debugger_variables
end
