open WSemantics
open WSyntax
open Gil_syntax
module L = Logging
module DL = Debugger_log
module ExecMap = Debugger.Utils.ExecMap
module UnifyMap = Debugger.Utils.UnifyMap
open Syntaxes.Option
module ExtList = Utils.ExtList
open Debugger.Lifter

type rid = L.ReportId.t [@@deriving yojson]

let cmd_to_yojson = Cmd.to_yojson (fun x -> `Int x)

module Make
    (Gil : Gillian.Debugger.Lifter.GilFallbackLifter.GilLifterWithState)
    (Verification : Engine.Verifier.S) =
struct
  open ExecMap

  type memory_error = WislSMemory.err_t
  type tl_ast = WParserAndCompiler.tl_ast
  type memory = WislSMemory.t

  module CmdReport = Verification.SAInterpreter.Logging.ConfigReport
  module GilLifter = Gil.Lifter

  type cmd_report = CmdReport.t [@@deriving yojson]
  type branch_case = IfElse of bool | LCmd of int [@@deriving yojson]
  type branch_data = rid * BranchCase.t [@@deriving yojson]

  let annot_to_wisl_stmt annot wisl_ast =
    let origin_id = Annot.get_origin_id annot in
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

  type map = (branch_case, cmd_data, branch_data) ExecMap.t

  and cmd_data = {
    ids : rid list;
    display : string;
    unifys : unifys;
    errors : string list;
    submap : map submap;
    gil_branch_path : BranchCase.path;
    branch_path : branch_case list;
    parent : parent; [@to_yojson fun _ -> `Null]
  }

  and parent = (map * branch_case option) option [@@deriving yojson]

  module PartialCmds = struct
    type partial_data = {
      display : string;
      ids : rid ExtList.t;
      errors : string ExtList.t;
      unifys :
        (rid * Engine.Unifier.unify_kind * UnifyMap.unify_result) ExtList.t;
    }

    let make_partial_data display =
      {
        display;
        ids = ExtList.make ();
        errors = ExtList.make ();
        unifys = ExtList.make ();
      }

    type partial =
      | VarAssign of { d : partial_data; target_var : string }
      | Dispose of { d : partial_data }
      | Lookup of { d : partial_data; target_var : string }
      | Update of { d : partial_data; mutable get_cmd_found : bool }
      | FunCall of { d : partial_data; target_var : string }
      | While of { d : partial_data; submap : map submap }
      | Return of { d : partial_data }

    type t = (int, partial) Hashtbl.t

    let get_partial_data = function
      | VarAssign { d; _ }
      | Dispose { d }
      | Lookup { d; _ }
      | Update { d; _ }
      | FunCall { d; _ }
      | While { d; _ }
      | Return { d } -> d

    let update_partial_data { id; unifys; errors; _ } d =
      d.ids |> ExtList.append id;
      unifys |> List.iter (fun unify -> d.unifys |> ExtList.append unify);
      errors |> List.iter (fun error -> d.errors |> ExtList.append error)

    type partial_cmd_result =
      | Finished of {
          ids : rid list;
          display : string;
          unifys : unifys;
          errors : string list;
          cmd_kind : (branch_case, branch_data) cmd_kind;
          submap : map submap;
          fun_call_name : string option;
        }
      | StepAgain of (rid option * BranchCase.t option)
      | NoPartial

    let create annot tl_ast stmt exec_data =
      let* origin_id = Annot.get_origin_id annot in
      let* display = get_origin_node_str tl_ast (Some origin_id) in
      let { cmd_report; _ } = exec_data in
      let gil_cmd = CmdReport.(cmd_report.cmd) in
      let failwith s =
        DL.failwith
          (fun () -> [ ("gil_cmd", cmd_to_yojson gil_cmd) ])
          ("PartialCmds.create: " ^ s)
      in
      match exec_data.kind with
      | Final -> None
      | _ -> (
          let d = make_partial_data display in
          d |> update_partial_data exec_data;
          if annot |> Annot.is_return then
            Some (Return { d }, StepAgain (None, None))
          else
            let* stmt in
            match stmt with
            | WStmt.VarAssign (v, _) -> (
                match gil_cmd with
                | Cmd.Assignment (v', _) when v = v' -> None
                | _ ->
                    let partial = VarAssign { d; target_var = v } in
                    Some (partial, StepAgain (None, None)))
            | WStmt.Dispose _ -> (
                match gil_cmd with
                | Cmd.GuardedGoto _ ->
                    let partial = Dispose { d } in
                    Some
                      ( partial,
                        StepAgain (None, Some (BranchCase.GuardedGoto true)) )
                | _ -> failwith "first cmd of Dispose wasn't GuardedGoto!")
            | WStmt.Lookup (x, _) ->
                let partial = Lookup { d; target_var = x } in
                Some (partial, StepAgain (None, None))
            | WStmt.Update _ -> (
                match gil_cmd with
                | Cmd.LAction (_, action, _)
                  when WislLActions.(ac_from_str action = SetCell) ->
                    failwith "first cmd of Update was SetCell!"
                | Cmd.LAction (_, action, _)
                  when WislLActions.(ac_from_str action = GetCell) ->
                    let partial = Update { d; get_cmd_found = true } in
                    Some (partial, StepAgain (None, None))
                | _ ->
                    let partial = Update { d; get_cmd_found = false } in
                    Some (partial, StepAgain (None, None)))
            | WStmt.FunCall (x, _, _, _) -> (
                match gil_cmd with
                | Cmd.Call (x', _, _, _, _) when x = x' -> None
                | _ ->
                    let partial = FunCall { d; target_var = x } in
                    Some (partial, StepAgain (None, None)))
            | WStmt.While _ -> (
                match gil_cmd with
                | Cmd.Call _ ->
                    let submap =
                      match annot |> Annot.get_expansion_kind with
                      | Annot.NoExpansion ->
                          failwith "Call for While has no expansion!"
                      | Annot.Function p -> ExecMap.Proc p
                    in
                    let partial = While { d; submap } in
                    Some (partial, StepAgain (None, None))
                | _ -> failwith "first cmd of While wasn't Call!")
            | _ -> None)

    let update partial exec_data =
      let { cmd_report; _ } = exec_data in
      let gil_cmd = CmdReport.(cmd_report.cmd) in
      partial |> get_partial_data |> update_partial_data exec_data;
      let annot = CmdReport.(cmd_report.annot) in
      let failwith s =
        DL.failwith
          (fun () -> [ ("gil_cmd", cmd_to_yojson gil_cmd) ])
          ("PartialCmds.update: " ^ s)
      in
      let finished ?(submap = NoSubmap) ?fun_call_name cmd_kind =
        let { display; ids; unifys; errors } = get_partial_data partial in
        let ids = ids |> ExtList.to_list in
        let unifys = unifys |> ExtList.to_list in
        let errors = errors |> ExtList.to_list in
        Finished
          { ids; display; unifys; errors; cmd_kind; submap; fun_call_name }
      in
      match exec_data.kind with
      | Final -> finished Final
      | _ -> (
          match partial with
          | VarAssign { target_var; _ } -> (
              match gil_cmd with
              | Cmd.Assignment (v, _) when v = target_var -> finished Normal
              | _ -> StepAgain (None, None))
          | Dispose { d } -> (
              match gil_cmd with
              | Cmd.LAction (_, action, _)
                when WislLActions.(ac_from_str action = Dispose) ->
                  if d.ids |> ExtList.length <> 2 then
                    failwith "expected 2 ids when finishing Dispose!";
                  finished Normal
              | _ -> failwith "expected Dispose LAction for Dispose!")
          | Lookup { target_var; _ } -> (
              match gil_cmd with
              | Cmd.Assignment (x, _) when x = target_var -> finished Normal
              | _ -> StepAgain (None, None))
          | Update ({ get_cmd_found; _ } as u) -> (
              match gil_cmd with
              | Cmd.LAction (_, action, _)
                when WislLActions.(ac_from_str action = SetCell) ->
                  if get_cmd_found then finished Normal
                  else failwith "encountered SetCell before GetCell in Update!"
              | Cmd.LAction (_, action, _)
                when WislLActions.(ac_from_str action = GetCell) ->
                  if get_cmd_found then
                    failwith "encountered GetCell twice in Update!"
                  else u.get_cmd_found <- true;
                  StepAgain (None, None)
              | _ ->
                  if get_cmd_found then
                    failwith
                      "SetCell should immediately follow GetCell in Update!"
                  else StepAgain (None, None))
          | FunCall { target_var; _ } -> (
              match gil_cmd with
              | Cmd.Call (x, _, _, _, _) when x = target_var -> finished Normal
              | _ -> StepAgain (None, None))
          | While { submap; _ } -> (
              match gil_cmd with
              | Cmd.Assignment _ ->
                  if annot |> Annot.is_end_of_cmd then finished ~submap Normal
                  else StepAgain (None, None)
              | _ -> failwith "expected Call for While!")
          | Return _ -> StepAgain (None, None))

    let handle exec_data tl_ast partial_cmds =
      let annot =
        let cmd_report = exec_data.cmd_report in
        CmdReport.(cmd_report.annot)
      in
      (let* origin_id = Annot.get_origin_id annot in
       let stmt = annot_to_wisl_stmt annot tl_ast in
       match Hashtbl.find_opt partial_cmds origin_id with
       | Some partial ->
           let result = update partial exec_data in
           (match result with
           | Finished _ -> Hashtbl.remove partial_cmds origin_id
           | _ -> ());
           Some result
       | None ->
           let+ partial, result = create annot tl_ast stmt exec_data in
           Hashtbl.add partial_cmds origin_id partial;
           result)
      |> Option.value ~default:NoPartial
  end

  type t = {
    proc_name : string;
    gil_state : GilLifter.t; [@to_yojson GilLifter.dump]
    tl_ast : tl_ast; [@to_yojson fun _ -> `Null]
    partial_cmds : PartialCmds.t; [@to_yojson fun _ -> `Null]
    mutable map : map;
    id_map : (rid, map) Hashtbl.t; [@to_yojson fun _ -> `Null]
    mutable before_partial : (rid * BranchCase.t option) option;
    is_loop_func : bool;
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
          | Some case -> case :: parent_path)
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
      | Branch cases ->
          let nexts = Hashtbl.create (List.length cases) in
          cases
          |> List.iter (fun (case, bdata) ->
                 Hashtbl.add nexts case (bdata, Nothing));
          BranchCmd { data; nexts }
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
                     | BranchCase.GuardedGoto b -> (IfElse b, (id, case))
                     | _ -> failwith "convert_kind: inconsistent branch cases!")
            in
            Branch cases
        | (BranchCase.LCmd _, ()) :: _ ->
            let cases =
              cases
              |> List.map (fun (case, _) ->
                     match case with
                     | BranchCase.LCmd lcmd -> (LCmd lcmd, (id, case))
                     | _ -> failwith "convert_kind: inconsistent branch cases!")
            in
            Branch cases
        | _ -> failwith "convert_kind: unsupported branch case!")

  let insert_new_cmd
      (new_cmd : parent:parent -> unit -> map)
      (id : rid)
      (gil_case : BranchCase.t option)
      id_map =
    let map = Hashtbl.find id_map id in
    match map with
    | Cmd c ->
        let parent = Some (map, None) in
        c.next <- new_cmd ~parent ()
    | BranchCmd { nexts; _ } -> (
        match gil_case with
        | None ->
            failwith
              "insert_new_cmd: HORROR - need branch case to insert to branch \
               cmd!"
        | Some gil_case ->
            let case, bdata =
              Hashtbl.find_map
                (fun case (bdata, next) ->
                  if snd bdata <> gil_case then None
                  else
                    match next with
                    | Nothing -> Some (case, bdata)
                    | _ ->
                        failwith
                          "insert_new_cmd: HORROR - tried to insert into \
                           non-Nothing!")
                nexts
              |> Option.get
            in
            let parent = Some (map, Some case) in
            Hashtbl.replace nexts case (bdata, new_cmd ~parent ()))
    | _ ->
        failwith
          "insert_new_cmd: HORROR - tried to insert to FinalCmd or Nothing!"

  let prepare_basic_cmd ?display ?(final = false) tl_ast id_map exec_data =
    let { cmd_report; _ } = exec_data in
    let annot = CmdReport.(cmd_report.annot) in
    let origin_id = Annot.get_origin_id annot in
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
      match Annot.get_expansion_kind annot with
      | NoExpansion -> NoSubmap
      | Function p -> Proc p
    in
    let kind = if final then Final else convert_kind id kind in
    new_cmd id_map kind [ id ] display unifys errors gil_branch_path ~submap

  let handle_loop_prefix exec_data =
    let annot = CmdReport.(exec_data.cmd_report.annot) in
    if annot |> Annot.is_loop_prefix then
      Some
        (match exec_data.cmd_report.cmd with
        | Cmd.GuardedGoto _ ->
            ExecNext (None, Some (BranchCase.GuardedGoto true))
        | _ -> ExecNext (None, None))
    else None

  let init_opt proc_name tl_ast exec_data =
    let gil_state = Gil.get_state () in
    let+ tl_ast in
    CmdReport.(
      let annot = exec_data.cmd_report.annot in
      DL.log (fun m ->
          m "LIFT INIT - is loop prefix: %b" (annot |> Annot.is_loop_prefix)));
    let partial_cmds = Hashtbl.create 0 in
    let id_map = Hashtbl.create 0 in
    let before_partial = None in
    let map, result, is_loop_func =
      match handle_loop_prefix exec_data with
      | Some result -> (Nothing, result, true)
      | None -> (
          match PartialCmds.handle exec_data tl_ast partial_cmds with
          | StepAgain result -> (Nothing, ExecNext result, false)
          | NoPartial ->
              let new_cmd = prepare_basic_cmd tl_ast id_map exec_data in
              (new_cmd ~parent:None (), Stop, false)
          | Finished _ -> failwith "init: HORROR - Finished PartialCmd on init!"
          )
    in
    let state =
      {
        proc_name;
        gil_state;
        tl_ast;
        partial_cmds;
        map;
        id_map;
        before_partial;
        is_loop_func;
      }
    in
    (state, result)

  let init proc_name tl_ast exec_data =
    match init_opt proc_name tl_ast exec_data with
    | None -> failwith "init: wislLifter needs a tl_ast!"
    | Some x -> x

  let handle_cmd
      prev_id
      branch_case
      (exec_data : cmd_report executed_cmd_data)
      state =
    DL.log (fun m ->
        m "HANDLING %a (prev %a)" L.ReportId.pp exec_data.id L.ReportId.pp
          prev_id);
    let { tl_ast; partial_cmds; id_map; proc_name; is_loop_func; _ } = state in
    match handle_loop_prefix exec_data with
    | Some result -> result
    | None -> (
        match PartialCmds.handle exec_data tl_ast partial_cmds with
        | StepAgain result ->
            if Option.is_none state.before_partial then
              state.before_partial <- Some (prev_id, branch_case);
            ExecNext result
        | NoPartial ->
            let display, final =
              if is_loop_func && get_fun_call_name exec_data = Some proc_name
              then (Some "<end of loop>", true)
              else (None, false)
            in
            let new_cmd =
              prepare_basic_cmd ?display ~final tl_ast id_map exec_data
            in
            (match state.map with
            | Nothing -> state.map <- new_cmd ~parent:None ()
            | _ -> insert_new_cmd new_cmd prev_id branch_case id_map);
            Stop
        | Finished
            { ids; display; unifys; errors; cmd_kind; submap; fun_call_name } ->
            let display, cmd_kind =
              if is_loop_func && fun_call_name = Some proc_name then
                ("<end of loop>", Final)
              else (display, cmd_kind)
            in
            let gil_branch_path =
              GilLifter.path_of_id (List.hd ids) state.gil_state
            in
            let new_cmd =
              new_cmd id_map cmd_kind ids display unifys errors gil_branch_path
                ~submap
            in
            let prev_id, branch_case = state.before_partial |> Option.get in
            insert_new_cmd new_cmd prev_id branch_case id_map;
            state.before_partial <- None;
            Stop)

  let get_gil_map _ = failwith "get_gil_map: not implemented!"

  let package_case _ (case : branch_case) : Packaged.branch_case =
    let json = branch_case_to_yojson case in
    let kind, display =
      match case with
      | IfElse b -> ("IfElse", ("If/Else", Fmt.str "%B" b))
      | LCmd x -> ("LCmd", ("Logical command", Fmt.str "%d" x))
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
  let get_lifted_map { map; _ } = package map
  let get_lifted_map_opt state = Some (get_lifted_map state)

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
    GilLifter.existing_next_steps id gil_state
    |> List.filter (fun (id, _) -> Hashtbl.mem id_map id)

  let next_step_specific id case state =
    let failwith s =
      DL.failwith
        (fun () ->
          [
            ("state", dump state);
            ("id", rid_to_yojson id);
            ("case", opt_to_yojson Packaged.branch_case_to_yojson case);
          ])
        ("next_step_specific: " ^ s)
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
        | Some ((id, case), _) -> (id, Some case))

  let previous_step id { id_map; _ } =
    match Hashtbl.find id_map id with
    | Nothing -> None
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        let+ parent, case = data.parent in
        let id = id_of_map parent in
        let case = case |> Option.map (package_case ()) in
        (id, case)

  let select_next_path case id { gil_state; _ } =
    GilLifter.select_next_path case id gil_state

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
                if next = Nothing then Some (id, Some gil_case) else None)
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
    let annot = Logging.LogQueryer.get_previously_freed_annot loc in
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
