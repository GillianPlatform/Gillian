open WSemantics
open WSyntax
open Gil_syntax
module L = Logging
module DL = Debugger_log
module Exec_map = Debugger.Utils.Exec_map
open Syntaxes.Option
open Syntaxes.Result_of_option
open Utils
module Annot = WParserAndCompiler.Annot
module Gil_branch_case = Gil_syntax.Branch_case
module Branch_case = WBranchCase
open Annot
open Branch_case
open Debugger.Lifter

type rid = L.Report_id.t [@@deriving yojson, show]

let rec int_to_letters = function
  | 0 -> ""
  | i ->
      let i = i - 1 in
      let remainder = i mod 26 in
      let char = Char.chr (65 + remainder) |> Char.escaped in
      char ^ int_to_letters (i / 26)

let ( let++ ) f o = Result.map o f
let ( let** ) o f = Result.bind o f

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
  type branch_data = rid * Gil_branch_case.t option [@@deriving yojson]
  type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
  type stack_direction = In | Out of rid [@@deriving yojson]

  let annot_to_wisl_stmt annot wisl_ast =
    let origin_id = annot.origin_id in
    let wprog = WProg.get_by_id wisl_ast origin_id in
    match wprog with
    | `WStmt wstmt -> Some wstmt.snode
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

  type map = (Branch_case.t, cmd_data, branch_data) Exec_map.t

  and cmd_data = {
    id : rid;
    all_ids : rid list;
    display : string;
    matches : matching list;
    errors : string list;
    mutable submap : map submap;
    (* branch_path : Branch_case.t list; *)
    prev : (rid * Branch_case.t option) option;
    callers : rid list;
    func_return_label : (string * int) option;
  }
  [@@deriving yojson]

  type step_args = rid option * Gil_branch_case.t option * Gil_branch_case.path
  type _ Effect.t += Step : step_args -> exec_data Effect.t

  module Partial_cmds = struct
    type prev = rid * Branch_case.t option * rid list [@@deriving yojson]

    type partial_data = {
      prev : prev option;
      all_ids : (rid * (Branch_case.kind option * Branch_case.case)) Ext_list.t;
      unexplored_paths : (rid * Gil_branch_case.t option) Stack.t;
      ends : (Branch_case.case * branch_data) Ext_list.t;
      mutable id : rid option;
      mutable display : string option;
      mutable stack_info : (rid list * stack_direction option) option;
      mutable nest_kind : nest_kind option;
      mutable is_loop_end : bool;
      matches : matching Ext_list.t;
      errors : string Ext_list.t;
    }
    [@@deriving to_yojson]

    let init () = Hashtbl.create 0

    let init_partial ~prev =
      {
        prev;
        all_ids = Ext_list.make ();
        unexplored_paths = Stack.create ();
        ends = Ext_list.make ();
        id = None;
        display = None;
        stack_info = None;
        nest_kind = None;
        is_loop_end = false;
        matches = Ext_list.make ();
        errors = Ext_list.make ();
      }

    type t = (rid, partial_data) Hashtbl.t [@@deriving to_yojson]

    type finished = {
      prev : (rid * Branch_case.t option) option;
      id : rid;
      all_ids : rid list;
      display : string;
      matches : matching list;
      errors : string list;
      kind : (Branch_case.t, branch_data) cmd_kind;
      submap : map submap;
      callers : rid list;
      stack_direction : stack_direction option;
    }
    [@@deriving yojson]

    type partial_result =
      | Finished of finished
      | StepAgain of (rid * Gil_branch_case.t option)

    let step_again ~id ?branch_case () = Ok (StepAgain (id, branch_case))

    let ends_to_cases ~nest_kind (ends : (Branch_case.case * branch_data) list)
        =
      let- () =
        match (nest_kind, ends) with
        | Some (FunCall _), [ (Unknown, bdata) ] ->
            Some (Ok [ (FuncExitPlaceholder, bdata) ])
        | Some (FunCall _), _ ->
            Some (Error "Unexpected branching in cmd with FunCall nest!")
        | _ -> None
      in
      let counts = Hashtbl.create 0 in
      let () =
        ends
        |> List.iter (fun (case_kind, _) ->
               let total, _ =
                 Hashtbl.find_opt counts case_kind
                 |> Option.value ~default:(0, 0)
               in
               Hashtbl.replace counts case_kind (total + 1, 0))
      in
      ends
      |> List.map (fun (kind, branch_data) ->
             let total, count = Hashtbl.find counts kind in
             let ix =
               match (kind, total) with
               | IfElse _, 1 -> -1
               | _ -> count
             in
             let () = Hashtbl.replace counts kind (total, count + 1) in
             (Case (kind, ix), branch_data))
      |> Result.ok

    let is_return exec_data =
      let annot = CmdReport.(exec_data.cmd_report.annot) in
      match annot.stmt_kind with
      | Return _ -> true
      | _ -> false

    let is_loop_end ~is_loop_func ~proc_name exec_data =
      is_loop_func && get_fun_call_name exec_data = Some proc_name

    let finish ~exec_data partial =
      let ({
             prev;
             all_ids;
             id;
             display;
             stack_info;
             ends;
             nest_kind;
             matches;
             errors;
             is_loop_end;
             _;
           }
            : partial_data) =
        partial
      in
      let prev =
        let+ id, branch, _ = prev in
        (id, branch)
      in
      let** id =
        id |> Option.to_result ~none:"Trying to finish partial with no id!"
      in
      let** display =
        display
        |> Option.to_result ~none:"Trying to finish partial with no display!"
      in
      let** callers, stack_direction =
        stack_info
        |> Option.to_result ~none:"Trying to finish partial with no stack info!"
      in
      let all_ids = all_ids |> Ext_list.to_list |> List.map fst in
      let matches = matches |> Ext_list.to_list in
      let errors = errors |> Ext_list.to_list in
      let ends = Ext_list.to_list ends in
      let submap =
        match nest_kind with
        | Some (LoopBody p) -> Proc p
        | _ -> NoSubmap
      in
      let++ kind =
        let++ cases = ends_to_cases ~nest_kind ends in
        match cases with
        | _ when is_return exec_data -> Final
        | _ when is_loop_end -> Final
        | [] -> Final
        | [ (Case (Unknown, _), _) ] -> Normal
        | _ -> Branch cases
      in
      Finished
        {
          prev;
          all_ids;
          id;
          display;
          callers;
          stack_direction;
          matches;
          errors;
          submap;
          kind;
        }

    module Update = struct
      let get_is_end ({ stmt_kind; _ } : Annot.t) =
        match stmt_kind with
        | Normal b | Return b -> Ok b
        | Hidden -> Ok false
        | LoopPrefix as k ->
            Fmt.error "%a cmd should have been skipped!" Annot.pp_stmt_kind k

      let resolve_case
          ?gil_case
          (kind : Branch_case.kind option)
          (prev_case : Branch_case.case) =
        match (kind, prev_case) with
        | None, prev_case -> Ok prev_case
        | Some prev_kind, Unknown -> (
            match prev_kind with
            | IfElseKind | WhileLoopKind -> (
                match gil_case with
                | Some (Gil_branch_case.GuardedGoto b) -> Ok (IfElse b)
                | _ -> Error "IfElseKind expects a GuardedGoto gil case"))
        | Some _, _ ->
            Error "HORROR - branch kind is set with pre-existing case!"

      let update_paths ~exec_data ~branch_case ~branch_kind partial =
        let ({ id; kind; cmd_report; _ } : exec_data) = exec_data in
        let annot = CmdReport.(cmd_report.annot) in
        let { ends; unexplored_paths; _ } = partial in
        let** is_end = get_is_end annot in
        match (is_end, kind) with
        | _, Final -> Ok ()
        | false, Normal ->
            Stack.push (id, None) unexplored_paths;
            Ok ()
        | false, Branch cases ->
            cases
            |> List.iter (fun (gil_case, ()) ->
                   Stack.push (id, Some gil_case) unexplored_paths);
            Ok ()
        | true, Normal ->
            Ext_list.add (branch_case, (id, None)) ends;
            Ok ()
        | true, Branch cases ->
            cases
            |> List_utils.iter_results (fun (gil_case, ()) ->
                   let++ case =
                     resolve_case ~gil_case branch_kind branch_case
                   in
                   Ext_list.add (case, (id, Some gil_case)) ends)

      let get_stack_info ~(partial : partial_data) (exec_data : exec_data) =
        match partial.prev with
        | None -> Ok ([], None)
        | Some (prev_id, _, prev_callers) -> (
            let depth_change =
              let cs = exec_data.cmd_report.callstack in
              assert ((List.hd cs).pid = exec_data.cmd_report.proc_name);
              let prev_depth = List.length prev_callers in
              List.length cs - prev_depth - 1
            in
            match depth_change with
            | 0 -> Ok (prev_callers, None)
            | 1 -> Ok (prev_id :: prev_callers, Some In)
            | -1 -> (
                match prev_callers with
                | [] ->
                    Error "HORROR - stepping out when prev_callers is empty!"
                | hd :: tl -> Ok (tl, Some (Out hd)))
            | _ ->
                Error
                  "WislLifter.compute_callers: HORROR - too great a stack \
                   depth change!")

      let update_canonical_cmd_info
          ~id
          ~tl_ast
          ~annot
          ~is_loop_func
          ~proc_name
          ~exec_data
          (partial : partial_data) =
        match (partial.display, annot.stmt_kind, annot.origin_id) with
        | None, (Normal _ | Return _), Some origin_id ->
            let** display =
              match get_origin_node_str tl_ast (Some origin_id) with
              | Some display -> Ok display
              | None ->
                  if is_loop_end ~is_loop_func ~proc_name exec_data then
                    let () = partial.is_loop_end <- true in
                    Ok "<end of loop>"
                  else Error "Couldn't get display!"
            in
            let** stack_info = get_stack_info ~partial exec_data in
            partial.id <- Some id;
            partial.display <- Some display;
            partial.stack_info <- Some stack_info;
            Ok ()
        | _ -> Ok ()

      let insert_id_and_case
          ~prev_id
          ~(exec_data : exec_data)
          ~id
          ({ all_ids; _ } : partial_data) =
        let annot, gil_case =
          let { cmd_report; _ } = exec_data in
          CmdReport.(cmd_report.annot, cmd_report.branch_case)
        in
        let prev_kind_case =
          let* prev_id = prev_id in
          Ext_list.assoc_opt prev_id all_ids
        in
        let kind = annot.branch_kind in
        let++ case =
          match prev_kind_case with
          | None -> Ok Unknown
          | Some (prev_kind, prev_case) ->
              resolve_case ?gil_case prev_kind prev_case
        in
        Ext_list.add (id, (kind, case)) all_ids;
        (kind, case)

      (** Returns whether this function would be called compositionally *)
      let is_fcall_using_spec fn (prog : (annot, int) Prog.t) =
        let open Gillian.Utils in
        (match !Config.current_exec_mode with
        | Exec_mode.Verification | Exec_mode.BiAbduction -> true
        | Exec_mode.Concrete | Exec_mode.Symbolic -> false)
        &&
        match Hashtbl.find_opt prog.procs fn with
        | Some proc -> Option.is_some proc.proc_spec
        | None -> false

      let update_submap ~prog ~(annot : Annot.t) partial =
        match (partial.nest_kind, annot.nest_kind) with
        | None, Some (FunCall fn) when not (is_fcall_using_spec fn prog) ->
            partial.nest_kind <- Some (FunCall fn);
            Ok ()
        | None, nest ->
            partial.nest_kind <- nest;
            Ok ()
        | Some _, (None | Some (FunCall _)) -> Ok ()
        | Some _, Some _ -> Error "HORROR - multiple submaps!"

      let f ~tl_ast ~prog ~prev_id ~is_loop_func ~proc_name exec_data partial =
        let { id; cmd_report; errors; matches; _ } = exec_data in
        let annot = CmdReport.(cmd_report.annot) in
        let** branch_kind, branch_case =
          insert_id_and_case ~prev_id ~exec_data ~id partial
        in
        let** () = update_paths ~exec_data ~branch_case ~branch_kind partial in
        let** () =
          update_canonical_cmd_info ~id ~tl_ast ~annot ~exec_data ~is_loop_func
            ~proc_name partial
        in
        let** () = update_submap ~prog ~annot partial in
        Ext_list.add_all errors partial.errors;
        Ext_list.add_all matches partial.matches;

        (* Finish or continue *)
        match Stack.pop_opt partial.unexplored_paths with
        | None -> finish ~exec_data partial
        | Some (id, branch_case) -> step_again ~id ?branch_case ()
    end

    let update = Update.f

    let find_or_init ~partials ~get_prev prev_id =
      let partial =
        let* prev_id = prev_id in
        Hashtbl.find_opt partials prev_id
      in
      match partial with
      | Some p -> Ok p
      | None ->
          let++ prev = get_prev () in
          init_partial ~prev

    let failwith ~exec_data ?partial ~partials msg =
      DL.failwith
        (fun () ->
          [
            ("exec_data", exec_data_to_yojson exec_data);
            ("partial_data", opt_to_yojson partial_data_to_yojson partial);
            ("partials_state", to_yojson partials);
          ])
        ("WislLifter.PartialCmds.handle: " ^ msg)

    let handle
        ~(partials : t)
        ~tl_ast
        ~prog
        ~get_prev
        ~is_loop_func
        ~proc_name
        ~prev_id
        exec_data =
      let partial =
        find_or_init ~partials ~get_prev prev_id
        |> Result_utils.or_else (fun e -> failwith ~exec_data ~partials e)
      in
      Hashtbl.replace partials exec_data.id partial;
      let result =
        update ~tl_ast ~prog ~prev_id ~is_loop_func ~proc_name exec_data partial
        |> Result_utils.or_else (fun e ->
               failwith ~exec_data ~partial ~partials e)
      in
      let () =
        match result with
        | Finished _ ->
            partial.all_ids
            |> Ext_list.iter (fun (id, _) -> Hashtbl.remove_all partials id)
        | _ -> ()
      in
      result
  end

  type t = {
    proc_name : string;
    gil_state : Gil_lifter.t; [@to_yojson Gil_lifter.dump]
    tl_ast : tl_ast; [@to_yojson fun _ -> `Null]
    partial_cmds : Partial_cmds.t;
    mutable map : map;
    id_map : (rid, map) Hashtbl.t; [@to_yojson fun _ -> `Null]
    mutable is_loop_func : bool;
    prog : (annot, int) Prog.t; [@to_yojson fun _ -> `Null]
    func_return_map : (rid, string * int ref) Hashtbl.t;
    mutable func_return_count : int;
  }
  [@@deriving to_yojson]

  let dump = to_yojson

  module Insert_new_cmd = struct
    let new_function_return_label caller_id state =
      state.func_return_count <- state.func_return_count + 1;
      let label = int_to_letters state.func_return_count in
      let count = ref 0 in
      Hashtbl.add state.func_return_map caller_id (label, count);
      (label, count)

    let update_caller_branches ~caller_id ~cont_id (label, ix) state =
      match Hashtbl.find_opt state.id_map caller_id with
      | None ->
          Fmt.error "update_caller_branches - caller %a not found" pp_rid
            caller_id
      | Some (BranchCmd { nexts; _ }) ->
          Hashtbl.remove nexts FuncExitPlaceholder;
          let case = Case (FuncExit label, ix) in
          let bdata = (cont_id, None) in
          Hashtbl.add nexts case (bdata, Nothing);
          Ok ()
      | Some _ ->
          Fmt.error "update_caller_branches - caller %a does not branch" pp_rid
            caller_id

    let resolve_func_branches ~state finished_partial =
      let Partial_cmds.{ all_ids; kind; callers; _ } = finished_partial in
      match (kind, callers) with
      | Final, caller_id :: _ ->
          let () = DL.log (fun m -> m "A") in
          let label, count =
            match Hashtbl.find_opt state.func_return_map caller_id with
            | Some (label, count) -> (label, count)
            | None -> new_function_return_label caller_id state
          in
          incr count;
          let label = (label, !count) in
          let cont_id = all_ids |> List.rev |> List.hd in
          let** () = update_caller_branches ~caller_id ~cont_id label state in
          Ok (Some label)
      | _ ->
          let () = DL.log (fun m -> m "B") in
          Ok None

    let make_new_cmd ~func_return_label finished_partial : map =
      let Partial_cmds.
            {
              all_ids;
              id;
              display;
              matches;
              errors;
              submap;
              prev;
              callers;
              kind;
              _;
            } =
        finished_partial
      in
      let data =
        {
          all_ids;
          id;
          display;
          matches;
          errors;
          submap;
          prev;
          callers;
          func_return_label;
        }
      in
      match kind with
      | Final -> FinalCmd { data }
      | Normal -> Cmd { data; next = Nothing }
      | Branch ends ->
          let nexts = Hashtbl.create 0 in
          List.iter
            (fun (case, branch_data) ->
              Hashtbl.add nexts case (branch_data, Nothing))
            ends;
          BranchCmd { data; nexts }

    let insert_as_next ~state ~prev_id ?case new_cmd =
      match (Hashtbl.find state.id_map prev_id, case) with
      | Nothing, _ -> Error "trying to insert next of Nothing!"
      | FinalCmd _, _ -> Error "trying to insert next of FinalCmd!"
      | Cmd _, Some _ -> Error "tying to insert to non-branch cmd with branch!"
      | BranchCmd _, None ->
          Error "trying to insert to branch cmd with no branch!"
      | Cmd c, None ->
          c.next <- new_cmd;
          Ok ()
      | BranchCmd { nexts; _ }, Some case -> (
          match Hashtbl.find nexts case with
          | branch_data, Nothing ->
              Hashtbl.replace nexts case (branch_data, new_cmd);
              Ok ()
          | _ -> Error "duplicate insertion!")

    let insert_as_submap ~state ~parent_id new_cmd =
      let** parent_data =
        match Hashtbl.find state.id_map parent_id with
        | Nothing -> Error "trying to insert submap of Nothing!"
        | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> Ok data
      in
      match parent_data.submap with
      | Proc _ | Submap _ -> Error "duplicate submaps!"
      | NoSubmap ->
          parent_data.submap <- Submap new_cmd;
          Ok ()

    let insert_cmd ~state ~prev ~stack_direction new_cmd =
      match (stack_direction, state.map, prev) with
      | Some _, Nothing, _ -> Error "stepping in our out with empty map!"
      | _, Nothing, Some _ -> Error "inserting to empty map with prev!"
      | None, Nothing, None ->
          state.map <- new_cmd;
          Ok new_cmd
      | _, _, None -> Error "inserting to non-empty map with no prev!"
      | Some In, _, Some (parent_id, Some FuncExitPlaceholder)
      | Some In, _, Some (parent_id, None) ->
          let new_cmd = new_cmd |> map_data (fun d -> { d with prev = None }) in
          let++ () = insert_as_submap ~state ~parent_id new_cmd in
          new_cmd
      | Some In, _, Some (_, Some case) ->
          Fmt.error "stepping in with branch case (%a)!" Branch_case.pp case
      | None, _, Some (prev_id, case) ->
          let++ () = insert_as_next ~state ~prev_id ?case new_cmd in
          new_cmd
      | Some (Out prev_id), _, Some (inner_prev_id, _) ->
          let** case =
            let func_return_label =
              match Hashtbl.find state.id_map inner_prev_id with
              | Nothing -> None
              | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
                  data.func_return_label
            in
            match func_return_label with
            | Some (label, ix) -> Ok (Case (FuncExit label, ix))
            | None -> Error "stepping out without function return label!"
          in
          let new_cmd =
            new_cmd
            |> map_data (fun d -> { d with prev = Some (prev_id, Some case) })
          in
          let++ () = insert_as_next ~state ~prev_id ~case new_cmd in
          new_cmd

    let f ~state finished_partial =
      let r =
        let { id_map; _ } = state in
        let Partial_cmds.{ all_ids; prev; stack_direction; _ } =
          finished_partial
        in
        let** func_return_label =
          resolve_func_branches ~state finished_partial
        in
        let new_cmd = make_new_cmd ~func_return_label finished_partial in
        let** new_cmd = insert_cmd ~state ~prev ~stack_direction new_cmd in
        all_ids |> List.iter (fun id -> Hashtbl.replace id_map id new_cmd);
        Ok new_cmd
      in
      r
      |> Result_utils.or_else (fun e ->
             DL.failwith
               (fun () ->
                 [
                   ("state", dump state);
                   ( "finished_partial",
                     Partial_cmds.finished_to_yojson finished_partial );
                 ])
               ("WislLifter.insert_new_cmd: " ^ e))
  end

  let insert_new_cmd = Insert_new_cmd.f

  module Init_or_handle = struct
    (** Loop body functions have some boilerplate we want to ignore.
        This would normally be [Hidden], but we want to only consider
        the true case of the function *)
    let handle_loop_prefix exec_data =
      let { cmd_report; id; _ } = exec_data in
      let annot = CmdReport.(cmd_report.annot) in
      match annot.stmt_kind with
      | LoopPrefix ->
          (match exec_data.cmd_report.cmd with
          | Cmd.GuardedGoto _ -> (id, Some (Gil_branch_case.GuardedGoto true))
          | _ -> (id, None))
          |> Option.some
      | _ -> None

    let get_prev ~state ~gil_case ~prev_id () =
      let { map; id_map; _ } = state in
      let=* prev_id = Ok prev_id in
      let=* map =
        match Hashtbl.find_opt id_map prev_id with
        | None -> (
            match map with
            | Nothing -> Ok None
            | _ -> Error "couldn't find map at prev_id!")
        | map -> Ok map
      in
      match map with
      | Nothing -> Error "got Nothing map!"
      | FinalCmd { data } | Cmd { data; _ } ->
          Ok (Some (data.id, None, data.callers))
      | BranchCmd { data; nexts } -> (
          let case =
            Hashtbl.find_map
              (fun case ((id, gil_case'), _) ->
                if id = prev_id && gil_case' = gil_case then Some case else None)
              nexts
          in
          match case with
          | Some case -> Ok (Some (data.id, Some case, data.callers))
          | None -> Error "couldn't find prev in branches!")

    let f ~state ?prev_id ?gil_case (exec_data : exec_data) :
        (rid * Gil_branch_case.t option, map) Either.t =
      let- () =
        let+ id, case = handle_loop_prefix exec_data in
        state.is_loop_func <- true;
        Either.Left (id, case)
      in
      let gil_case =
        Option_utils.coalesce gil_case exec_data.cmd_report.branch_case
      in
      let { tl_ast; partial_cmds = partials; is_loop_func; proc_name; prog; _ }
          =
        state
      in
      match
        let get_prev = get_prev ~state ~gil_case ~prev_id in
        Partial_cmds.handle ~partials ~tl_ast ~prog ~get_prev ~is_loop_func
          ~proc_name ~prev_id exec_data
      with
      | Finished finished ->
          DL.log (fun m ->
              m
                ~json:
                  [
                    ("state", to_yojson state);
                    ("finished", Partial_cmds.finished_to_yojson finished);
                  ]
                "Finishing WISL command");
          let cmd = insert_new_cmd ~state finished in
          Either.Right cmd
      | StepAgain (id, case) -> Either.Left (id, case)
  end

  let init_or_handle = Init_or_handle.f
  let get_gil_map _ = failwith "get_gil_map: not implemented!"

  let package_case case =
    let json = Branch_case.to_yojson case in
    let display = Branch_case.display case in
    (display, json)

  let package_data package { id; all_ids; display; matches; errors; submap; _ }
      =
    let submap =
      match submap with
      | NoSubmap -> NoSubmap
      | Proc p -> Proc p
      | Submap map -> Submap (package map)
    in
    Packaged.{ id; all_ids; display; matches; errors; submap }

  let package =
    let package_case
        ~(bd : branch_data)
        ~(all_cases : (Branch_case.t * branch_data) list)
        case =
      ignore bd;
      ignore all_cases;
      package_case case
    in
    Packaged.package package_data package_case

  let get_lifted_map_exn { map; _ } = package map
  let get_lifted_map state = Some (get_lifted_map_exn state)

  let get_matches_at_id id { id_map; _ } =
    let map = Hashtbl.find id_map id in
    match map with
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        data.matches
    | _ ->
        failwith "get_matches_at_id: HORROR - tried to get matches at non-cmd!"

  let get_root_id { map; _ } =
    match map with
    | Nothing -> None
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        Some data.id

  let path_of_id id { gil_state; _ } = Gil_lifter.path_of_id id gil_state

  let previous_step id { id_map; _ } =
    let+ id, case =
      match Hashtbl.find id_map id with
      | Nothing -> None
      | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> data.prev
    in
    let case = case |> Option.map package_case in
    (id, case)

  let get_wisl_stmt gil_cmd wisl_ast =
    let* annot =
      match gil_cmd with
      | Some (_, annot) -> Some annot
      | _ -> None
    in
    annot_to_wisl_stmt annot wisl_ast

  let get_cell_var_from_cmd gil_cmd wisl_ast =
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

  let select_case nexts =
    let result =
      Hashtbl.fold
        (fun _ (id_case, map) acc ->
          match (map, acc) with
          | _, Some (Either.Right _) -> acc
          | Nothing, _ -> Some (Either.Right id_case)
          | map, None -> Some (Either.Left map)
          | _ -> acc)
        nexts None
    in
    Option.get result

  (* If a FinalCmd is in a function call, get the caller ID
     and the relevant branch case for stepping forward,
     while checking that it actually exists. *)
  let get_next_from_end state { callers; func_return_label; _ } =
    let* caller_id = List_utils.hd_opt callers in
    let* label, ix = func_return_label in
    let case = Case (FuncExit label, ix) in
    let* _ =
      match Hashtbl.find state.id_map caller_id with
      | BranchCmd { nexts; _ } -> Hashtbl.find_opt nexts case
      | _ -> None
    in
    Some (caller_id, Some case)

  let rec find_next state id case =
    let map = Hashtbl.find state.id_map id in
    match (map, case) with
    | Nothing, _ -> failwith "HORROR - map is Nothing!"
    | (FinalCmd _ | Cmd _), Some _ ->
        failwith "HORROR - tried to step case for non-branch cmd"
    | Cmd { next = Nothing; data = { all_ids; _ } }, None ->
        let id = List.hd (List.rev all_ids) in
        let case =
          match Gil_lifter.cases_at_id id state.gil_state with
          | [] -> None
          | [ case ] -> Some case
          | _ ->
              Fmt.failwith
                "find_next: id %a has multiple cases - not sure where to step!"
                L.Report_id.pp id
        in
        Either.Right (id, case)
    | Cmd { next; _ }, None -> Either.Left next
    | BranchCmd { nexts; _ }, None -> select_case nexts
    | BranchCmd { nexts; _ }, Some case -> (
        match Hashtbl.find_opt nexts case with
        | None -> failwith "case not found"
        | Some ((id, case), Nothing) -> Either.Right (id, case)
        | Some (_, next) -> Either.Left next)
    | FinalCmd { data }, None -> (
        match get_next_from_end state data with
        | Some (id, case) -> find_next state id case
        | None -> Either.left map)

  let request_next state id case =
    let rec aux id case =
      let path = path_of_id id state in
      let exec_data = Effect.perform (Step (Some id, case, path)) in
      match init_or_handle ~state ~prev_id:id ?gil_case:case exec_data with
      | Either.Left (id, case) -> aux id case
      | Either.Right map -> map
    in
    aux id case

  let step state id case =
    let () =
      DL.log (fun m ->
          m "Stepping %a %a" pp_rid id (pp_option Branch_case.pp) case)
    in
    match find_next state id case with
    | Either.Left next -> next
    | Either.Right (id, case) -> request_next state id case

  let step_all state id case =
    let cmd = Hashtbl.find state.id_map id in
    let stack_depth = List.length (get_cmd_data_exn cmd).callers in
    let rec aux ends = function
      | [] -> List.rev ends
      | (id, case) :: rest ->
          let ends, nexts =
            match step state id case with
            | Nothing -> failwith "Stepped to Nothing!"
            | Cmd { data; _ } -> (ends, (data.id, None) :: rest)
            | BranchCmd { data; nexts } ->
                let new_nexts =
                  Hashtbl.to_seq_keys nexts
                  |> Seq.map (fun case -> (data.id, Some case))
                  |> List.of_seq
                in
                (ends, new_nexts @ rest)
            | FinalCmd { data } as end_ ->
                let stack_depth' = List.length data.callers in
                if stack_depth' < stack_depth then
                  failwith "Stack depth too small!"
                else if stack_depth' > stack_depth then
                  let next = get_next_from_end state data |> Option.get in
                  (ends, next :: rest)
                else (end_ :: ends, rest)
          in
          aux ends nexts
    in
    match (case, cmd) with
    | _, Nothing -> failwith "Stepping from Nothing!"
    | _, FinalCmd _ -> [ cmd ]
    | None, BranchCmd { nexts; _ } ->
        let first_steps =
          Hashtbl.to_seq_keys nexts
          |> Seq.map (fun case -> (id, Some case))
          |> List.of_seq
        in
        aux [] first_steps
    | _, _ -> aux [] [ (id, case) ]

  let step_branch state id case =
    let case =
      Option.map
        (fun (_, json) -> json |> Branch_case.of_yojson |> Result.get_ok)
        case
    in
    let next = step state id case in
    let id = (get_cmd_data_exn next).id in
    (id, Debugger_utils.Step)

  let step_over state id =
    let map = Hashtbl.find state.id_map id in
    let () =
      let () =
        match map with
        | BranchCmd { nexts; _ } ->
            if Hashtbl.mem nexts FuncExitPlaceholder then
              step state id (Some FuncExitPlaceholder) |> ignore
        | _ -> ()
      in
      let> submap =
        match (get_cmd_data_exn map).submap with
        | NoSubmap | Proc _ | Submap Nothing -> None
        | Submap m -> Some m
      in
      let _ = step_all state (get_cmd_data_exn submap).id None in
      ()
    in
    let data = get_cmd_data_exn (step state id None) in
    (data.id, Debugger_utils.Step)

  let step_in state id =
    let cmd = Hashtbl.find state.id_map id in
    match cmd with
    | Nothing -> failwith "Stepping in from Nothing!"
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        (* Only BranchCmds should have submaps *)
        let- () =
          match data.submap with
          | NoSubmap | Proc _ | Submap Nothing -> None
          | Submap m -> Some ((get_cmd_data_exn m).id, Debugger_utils.Step)
        in
        step_branch state id None

  let step_back state id =
    let cmd = Hashtbl.find state.id_map id in
    let data = get_cmd_data_exn cmd in
    let id =
      match data.prev with
      | Some (id, _) -> id
      | None -> id
    in
    (id, Debugger_utils.Step)

  let continue state id =
    let rec aux ends = function
      | [] -> ends
      | (id, case) :: rest ->
          let ends' = step_all state id case in
          let ends', nexts =
            ends'
            |> List.partition_map (fun map ->
                   let data = get_cmd_data_exn map in
                   match get_next_from_end state data with
                   | Some (id, case) -> Either.Right (id, case)
                   | None -> Either.Left map)
          in
          aux (ends @ ends') (nexts @ rest)
    in
    let ends = aux [] [ (id, None) ] in
    let id = (get_cmd_data_exn (List.hd ends)).id in
    (id, Debugger_utils.Step)

  let step_out state id =
    let cmd = Hashtbl.find state.id_map id in
    match (get_cmd_data_exn cmd).callers with
    | [] -> continue state id
    | caller_id :: _ -> step_over state caller_id

  let is_breakpoint _ = true (* TODO *)

  let continue_back state id =
    let rec aux cmd =
      let { id; _ } = get_cmd_data_exn cmd in
      if is_breakpoint cmd then (id, Debugger_utils.Breakpoint)
      else
        match previous_step id state with
        | None -> (
            match (get_cmd_data_exn cmd).callers with
            | [] -> (id, Debugger_utils.Step)
            | caller_id :: _ -> aux (Hashtbl.find state.id_map caller_id))
        | Some (id, _) -> aux (Hashtbl.find state.id_map id)
    in
    aux (Hashtbl.find state.id_map id)

  let init ~proc_name ~all_procs:_ tl_ast prog =
    let gil_state = Gil.get_state () in
    let+ tl_ast = tl_ast in
    let partial_cmds = Partial_cmds.init () in
    let id_map = Hashtbl.create 0 in
    let state =
      {
        proc_name;
        gil_state;
        tl_ast;
        partial_cmds;
        map = Nothing;
        id_map;
        is_loop_func = false;
        prog;
        func_return_map = Hashtbl.create 0;
        func_return_count = 0;
      }
    in
    let finish_init () =
      let rec aux id_case =
        let id, case, path =
          match id_case with
          | Some (id, case) ->
              let path = path_of_id id state in
              (Some id, case, path)
          | None -> (None, None, [])
        in
        let exec_data = Effect.perform (Step (id, case, path)) in
        match init_or_handle ~state ?prev_id:id ?gil_case:case exec_data with
        | Either.Left (id, case) -> aux (Some (id, case))
        | Either.Right map -> map
      in
      let map = aux None in
      let id = (get_cmd_data_exn map).id in
      (id, Debugger_utils.Step)
    in
    (state, finish_init)

  let init_exn ~proc_name ~all_procs tl_ast prog =
    match init ~proc_name ~all_procs tl_ast prog with
    | None -> failwith "init: wislLifter needs a tl_ast!"
    | Some x -> x
end
