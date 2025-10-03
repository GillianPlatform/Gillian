open Gillian
open WSemantics
open WSyntax
open Gil_syntax
module L = Logging
module DL = Debugger_log
open Debugger.Utils
open Syntaxes.Option
open Syntaxes.Result_of_option
open Utils
module Annot = WParserAndCompiler.Annot
module Gil_branch_case = Gil_syntax.Branch_case
module Branch_case = WBranchCase
open Annot
open Branch_case
open Debugger.Lifter

type id = L.Report_id.t [@@deriving yojson, show]

let rec int_to_letters ?(acc = "") = function
  | 0 -> acc
  | i ->
      let i = i - 1 in
      let remainder = i mod 26 in
      let char = Char.chr (65 + remainder) |> Char.escaped in
      let acc = acc ^ char in
      int_to_letters ~acc (i / 26)

let ( let++ ) f o = Result.map o f
let ( let** ) o f = Result.bind o f

module Make
    (Gil :
      Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state
        with type Lifter.memory = WislSMemory.t)
    (Verification : Engine.Verifier.S with type annot = Annot.t) =
struct
  open Exec_map

  type memory_error = WislSMemory.err_t
  type tl_ast = WParserAndCompiler.tl_ast
  type memory = WislSMemory.t
  type annot = Annot.t
  type init_data = WParserAndCompiler.init_data
  type pc_err = WParserAndCompiler.err

  module CmdReport = Verification.SAInterpreter.Logging.ConfigReport
  module Gil_lifter = Gil.Lifter

  type cmd_report = CmdReport.t [@@deriving yojson]
  type branch_data = id * Gil_branch_case.t option [@@deriving yojson]
  type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
  type stack_direction = In | Out of id [@@deriving yojson]

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
    | Cmd.Call ({ fun_name; _ }, _) -> (
        match fun_name with
        | Expr.Lit (Literal.String name) -> Some name
        | _ ->
            failwith "get_fun_call_name: function name wasn't a literal expr!")
    | _ -> None

  type cmd_data = {
    id : id;
    all_ids : id list;
    display : string;
    matches : Match_map.matching list;
    errors : string list;
    submap : id submap;
    loc : string * int;
    prev : (id * Branch_case.t option) option;
    callers : id list;
    func_return_label : (string * int) option;
  }
  [@@deriving yojson]

  type node = (id, Branch_case.t, cmd_data, branch_data) Exec_map.node
  [@@deriving yojson]

  type map = (id, Branch_case.t, cmd_data, branch_data) Exec_map.map
  [@@deriving yojson]

  type step_args = id option * Gil_branch_case.t option * Gil_branch_case.path
  type _ Effect.t += Step : step_args -> exec_data Effect.t

  module Partial_cmds = struct
    type prev = id * Branch_case.t option * id list [@@deriving yojson]

    type canonical_cmd_data = {
      id : id;
      display : string;
      stack_info : id list * stack_direction option;
      is_loop_end : bool;
      loc : string * int;
    }
    [@@deriving to_yojson]

    type partial_data = {
      prev : prev option;
      all_ids : (id * (Branch_case.kind option * Branch_case.case)) Ext_list.t;
      unexplored_paths : (id * Gil_branch_case.t option) Stack.t;
      ends : (Branch_case.case * branch_data) Ext_list.t;
      matches : Match_map.matching Ext_list.t;
      errors : string Ext_list.t;
      mutable canonical_data : canonical_cmd_data option;
      mutable nest_kind : nest_kind option;
      mutable has_return : bool;
    }
    [@@deriving to_yojson]

    let init () = Hashtbl.create 0

    let init_partial ~prev =
      {
        prev;
        all_ids = Ext_list.make ();
        unexplored_paths = Stack.create ();
        ends = Ext_list.make ();
        matches = Ext_list.make ();
        errors = Ext_list.make ();
        canonical_data = None;
        nest_kind = None;
        has_return = false;
      }

    type t = (id, partial_data) Hashtbl.t [@@deriving to_yojson]

    type finished = {
      prev : (id * Branch_case.t option) option;
      id : id;
      all_ids : id list;
      display : string;
      matches : Match_map.matching list;
      errors : string list;
      next_kind : (Branch_case.t, branch_data) next_kind;
      submap : id submap;
      callers : id list;
      stack_direction : stack_direction option;
      loc : string * int;
      has_return : bool;
    }
    [@@deriving yojson]

    type partial_result =
      | Finished of finished
      | StepAgain of (id * Gil_branch_case.t option)

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
      let ({ prev; all_ids; ends; nest_kind; matches; errors; has_return; _ }
            : partial_data) =
        partial
      in
      let prev =
        let+ id, branch, _ = prev in
        (id, branch)
      in
      let** { id; display; stack_info; loc; is_loop_end } =
        partial.canonical_data
        |> Option.to_result
             ~none:"Trying to finish partial with no canonical data!"
      in
      let callers, stack_direction = stack_info in
      let all_ids = all_ids |> Ext_list.to_list |> List.map fst in
      let matches = matches |> Ext_list.to_list in
      let errors = errors |> Ext_list.to_list in
      let ends = Ext_list.to_list ends in
      let submap =
        match nest_kind with
        | Some (LoopBody p) -> Proc p
        | _ -> NoSubmap
      in
      let++ next_kind =
        let++ cases = ends_to_cases ~nest_kind ends in
        match cases with
        | _ when is_return exec_data -> Zero
        | _ when is_loop_end -> Zero
        | [] -> Zero
        | [ (Case (Unknown, _), _) ] ->
            One (Option.get (List_utils.last all_ids), None)
        | _ -> Many cases
      in
      Finished
        {
          prev;
          all_ids;
          id;
          display;
          callers;
          stack_direction;
          loc;
          matches;
          errors;
          submap;
          next_kind;
          has_return;
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
        let ({ id; next_kind; cmd_report; _ } : exec_data) = exec_data in
        let annot = CmdReport.(cmd_report.annot) in
        let { ends; unexplored_paths; _ } = partial in
        let** is_end = get_is_end annot in
        match (annot.stmt_kind, is_end, next_kind) with
        | _, _, Zero -> Ok ()
        | Return true, _, _ ->
            let () = partial.has_return <- true in
            Ok ()
        | _, false, One () ->
            Stack.push (id, None) unexplored_paths;
            Ok ()
        | _, false, Many cases ->
            cases
            |> List.iter (fun (gil_case, ()) ->
                   Stack.push (id, Some gil_case) unexplored_paths);
            Ok ()
        | _, true, One () ->
            Ext_list.add (branch_case, (id, None)) ends;
            Ok ()
        | _, true, Many cases ->
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
        match (partial.canonical_data, annot.stmt_kind, annot.origin_id) with
        | None, (Normal _ | Return _), Some origin_id ->
            let** display, is_loop_end =
              match get_origin_node_str tl_ast (Some origin_id) with
              | Some display -> Ok (display, false)
              | None ->
                  if is_loop_end ~is_loop_func ~proc_name exec_data then
                    Ok ("<end of loop>", true)
                  else Error "Couldn't get display!"
            in
            let** stack_info = get_stack_info ~partial exec_data in
            let loc =
              annot.origin_loc |> Option.get
              |> Debugger_utils.location_to_display_location
            in
            let loc = (loc.loc_source, loc.loc_start.pos_line) in
            partial.canonical_data <-
              Some { id; display; stack_info; is_loop_end; loc };
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
        | None, Some (FunCall fn) ->
            let () =
              if not (is_fcall_using_spec fn prog) then
                partial.nest_kind <- Some (FunCall fn)
            in
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

    let handle
        ~(partials : t)
        ~tl_ast
        ~prog
        ~get_prev
        ~is_loop_func
        ~proc_name
        ~prev_id
        (exec_data : exec_data) =
      let** partial = find_or_init ~partials ~get_prev prev_id in
      Hashtbl.replace partials exec_data.id partial;
      let** result =
        update ~tl_ast ~prog ~prev_id ~is_loop_func ~proc_name exec_data partial
      in
      let () =
        match result with
        | Finished _ ->
            partial.all_ids
            |> Ext_list.iter (fun (id, _) -> Hashtbl.remove_all partials id)
        | _ -> ()
      in
      Ok result
  end

  type t = {
    proc_name : string;
    gil_state : Gil_lifter.t; [@to_yojson Gil_lifter.dump]
    tl_ast : tl_ast; [@to_yojson fun _ -> `Null]
    partial_cmds : Partial_cmds.t;
    map : map;
    mutable is_loop_func : bool;
    prog : (annot, int) Prog.t; [@to_yojson fun _ -> `Null]
    func_return_map : (id, string * int ref) Hashtbl.t;
    mutable func_return_count : int;
  }
  [@@deriving to_yojson]

  let dump = to_yojson

  let package_case case =
    let json = Branch_case.to_yojson case in
    let display = Branch_case.display case in
    (json, display)

  let package_data { id; all_ids; display; matches; errors; submap; _ } =
    Packaged.{ id; all_ids; display; matches; errors; submap }

  let package_node { data : cmd_data; next } =
    let data = package_data data in
    let next =
      match next with
      | None -> None
      | Some (Single (next, _)) -> Some (Single (next, ""))
      | Some (Branch nexts) ->
          let nexts =
            nexts
            |> List.map (fun (case, (next, _)) ->
                   let case, bdata = package_case case in
                   (case, (next, bdata)))
          in
          Some (Branch nexts)
    in
    { data; next }

  module Insert_new_cmd = struct
    let failwith ~state ~finished_partial msg =
      DL.failwith
        (fun () ->
          [
            ("state", to_yojson state);
            ( "finished_partial",
              Partial_cmds.finished_to_yojson finished_partial );
          ])
        ("WislLifter.insert_new_cmd: " ^ msg)

    let new_function_return_label caller_id state =
      state.func_return_count <- state.func_return_count + 1;
      let label = int_to_letters state.func_return_count in
      let count = ref 0 in
      Hashtbl.add state.func_return_map caller_id (label, count);
      (label, count)

    let update_caller_branches ~caller_id ~cont_id (label, ix) state =
      let result =
        map_node_extra state.map caller_id (fun node ->
            let new_next =
              match node.next with
              | Some (Branch nexts) ->
                  let nexts = List.remove_assoc FuncExitPlaceholder nexts in
                  let case = Case (FuncExit label, ix) in
                  let bdata = (cont_id, None) in
                  let nexts = nexts @ [ (case, (None, bdata)) ] in
                  Ok (Some (Branch nexts))
              | None | Some (Single _) ->
                  Fmt.error "update_caller_branches - caller %a does not branch"
                    pp_id caller_id
            in
            match new_next with
            | Ok next ->
                let node = { node with next } in
                (node, Ok node)
            | Error e -> (node, Error e))
      in
      match result with
      | Some r ->
          let++ new_node = r in
          let () =
            Effect.perform
              (Node_updated (caller_id, Some (package_node new_node)))
          in
          ()
      | None ->
          Fmt.error "update_caller_branches - caller %a not found" pp_id
            caller_id

    let resolve_func_branches ~state finished_partial =
      let Partial_cmds.{ all_ids; next_kind; callers; has_return; _ } =
        finished_partial
      in
      match (next_kind, has_return, callers) with
      | Zero, true, caller_id :: _ ->
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
      | _ -> Ok None

    let make_new_cmd ~func_return_label finished_partial =
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
              next_kind;
              loc;
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
          loc;
        }
      in
      let next =
        match next_kind with
        | Zero -> None
        | One bdata -> Some (Single (None, bdata))
        | Many ends ->
            let nexts =
              List.map (fun (case, bdata) -> (case, (None, bdata))) ends
            in
            Some (Branch nexts)
      in
      { data; next }

    let with_prev prev { data; next } =
      let data = { data with prev } in
      { data; next }

    let insert_as_next ~state ~prev_id ?case new_id =
      let++ new_prev =
        map_node_extra_exn state.map prev_id (fun prev ->
            let new_next =
              let** next =
                match prev.next with
                | Some next -> Ok next
                | None -> Error "trying to insert next of final cmd!"
              in
              match (next, case) with
              | Single _, Some _ ->
                  Error "trying to insert to non-branch cmd with branch"
              | Branch _, None ->
                  Error "trying to insert to branch cmd with no branch"
              | Single (Some _, _), _ -> Error "duplicate insertion"
              | Single (None, bdata), None ->
                  Ok (Some (Single (Some new_id, bdata)))
              | Branch nexts, Some case -> (
                  match List.assoc_opt case nexts with
                  | None -> Error "case not found"
                  | Some (Some _, _) -> Error "duplicate insertion"
                  | Some (None, bdata) ->
                      let nexts =
                        List_utils.assoc_replace case (Some new_id, bdata) nexts
                      in
                      Ok (Some (Branch nexts)))
            in
            match new_next with
            | Ok next ->
                let prev = { prev with next } in
                (prev, Ok prev)
            | Error e ->
                (prev, Fmt.error "insert_as_next (%a) - %s" pp_id prev_id e))
      in
      let () =
        Effect.perform (Node_updated (prev_id, Some (package_node new_prev)))
      in
      ()

    let insert_as_submap ~state ~parent_id new_id =
      let++ parent =
        map_node_extra_exn state.map parent_id (fun parent ->
            match parent.data.submap with
            | Proc _ | Submap _ -> (parent, Error "duplicate submaps!")
            | NoSubmap ->
                let data = { parent.data with submap = Submap new_id } in
                let parent = { parent with data } in
                (parent, Ok parent))
      in
      let () =
        Effect.perform (Node_updated (parent_id, Some (package_node parent)))
      in
      ()

    let insert_to_empty_map ~state ~prev ~stack_direction new_cmd =
      let- () =
        match state.map.root with
        | Some _ -> Some None
        | None -> None
      in
      let r =
        match (stack_direction, prev) with
        | Some _, _ -> Error "stepping in our out with empty map!"
        | _, Some _ -> Error "inserting to empty map with prev!"
        | None, None ->
            let new_cmd = new_cmd |> with_prev None in
            let () = state.map.root <- Some new_cmd.data.id in
            Ok new_cmd
      in
      Some r

    let insert_cmd ~state ~prev ~stack_direction new_cmd =
      let- () = insert_to_empty_map ~state ~prev ~stack_direction new_cmd in
      match (stack_direction, prev) with
      | _, None -> Error "inserting to non-empty map with no prev!"
      | Some In, Some (parent_id, Some FuncExitPlaceholder)
      | Some In, Some (parent_id, None) ->
          let new_cmd = new_cmd |> with_prev None in
          let++ () = insert_as_submap ~state ~parent_id new_cmd.data.id in
          new_cmd
      | Some In, Some (_, Some case) ->
          Fmt.error "stepping in with branch case (%a)!" Branch_case.pp case
      | None, Some (prev_id, case) ->
          let++ () = insert_as_next ~state ~prev_id ?case new_cmd.data.id in
          new_cmd
      | Some (Out prev_id), Some (inner_prev_id, _) ->
          let** case =
            let func_return_label =
              (get_exn state.map inner_prev_id).data.func_return_label
            in
            match func_return_label with
            | Some (label, ix) -> Ok (Case (FuncExit label, ix))
            | None -> Error "stepping out without function return label!"
          in
          let new_cmd = new_cmd |> with_prev (Some (prev_id, Some case)) in
          let++ () = insert_as_next ~state ~prev_id ~case new_cmd.data.id in
          new_cmd

    let f ~state finished_partial =
      let r =
        let Partial_cmds.{ id; all_ids; prev; stack_direction; _ } =
          finished_partial
        in
        let** func_return_label =
          resolve_func_branches ~state finished_partial
        in
        let new_cmd = make_new_cmd ~func_return_label finished_partial in
        let** new_cmd = insert_cmd ~state ~prev ~stack_direction new_cmd in
        let () = insert state.map ~id ~all_ids new_cmd in
        let () =
          Effect.perform (Node_updated (id, Some (package_node new_cmd)))
        in
        Ok new_cmd
      in
      Result_utils.or_else (failwith ~state ~finished_partial) r
  end

  let insert_new_cmd = Insert_new_cmd.f

  module Init_or_handle = struct
    (** Loop body functions have some boilerplate we want to ignore. This would
        normally be [Hidden], but we want to only consider the true case of the
        function *)
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
      let { map; _ } = state in
      let=* prev_id = Ok prev_id in
      let=* prev =
        match get map prev_id with
        | None -> (
            match map.root with
            | None -> Ok None
            | _ -> Error "couldn't find map at prev_id!")
        | map -> Ok map
      in
      let { id; callers; _ } = prev.data in
      match prev.next with
      | None | Some (Single _) -> Ok (Some (id, None, callers))
      | Some (Branch nexts) -> (
          let case =
            List.find_map
              (fun (case, (_, (prev_id', gil_case'))) ->
                if prev_id' = prev_id && gil_case' = gil_case then Some case
                else None)
              nexts
          in
          match case with
          | Some case -> Ok (Some (id, Some case, callers))
          | None -> Error "couldn't find prev in branches!")

    let f ~state ?prev_id ?gil_case (exec_data : exec_data) =
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
      | Ok (Finished finished) ->
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
      | Ok (StepAgain (id, case)) -> Either.Left (id, case)
      | Error msg ->
          DL.failwith
            (fun () ->
              let prev_id =
                match prev_id with
                | Some id -> L.Report_id.to_yojson id
                | None -> `Null
              in
              [
                ("state", to_yojson state);
                ("exec_data", exec_data_to_yojson exec_data);
                ("prev_id", prev_id);
              ])
            ("Error while handling command: " ^ msg)
  end

  let init_or_handle = Init_or_handle.f
  let get_matches_at_id id { map; _ } = (get_exn map id).data.matches
  let path_of_id id { gil_state; _ } = Gil_lifter.path_of_id id gil_state

  let previous_step id { map; _ } =
    let+ id, case = (get_exn map id).data.prev in
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

  let select_case nexts =
    let result =
      List.fold_left
        (fun acc (_, (next, id_case)) ->
          match (next, acc) with
          | _, Some (Either.Right _) -> acc
          | None, _ -> Some (Either.Right id_case)
          | Some id, None -> Some (Either.Left id)
          | _ -> acc)
        None nexts
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
      match (get_exn state.map caller_id).next with
      | Some (Branch nexts) -> List.assoc_opt case nexts
      | _ -> None
    in
    Some (caller_id, Some case)

  let rec find_next state id case =
    let node = get_exn state.map id in
    match (node.next, case, node.data.submap) with
    | (None | Some (Single _)), Some _, _ ->
        failwith "HORROR - tried to step case for non-branch cmd"
    | ( Some (Branch [ (FuncExitPlaceholder, _) ]),
        Some FuncExitPlaceholder,
        Submap submap_id ) -> Either.left submap_id
    | Some (Single (None, _)), None, _ ->
        let id = List.hd (List.rev node.data.all_ids) in
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
    | Some (Single (Some next, _)), None, _ -> Either.Left next
    | Some (Branch nexts), None, _ -> select_case nexts
    | Some (Branch nexts), Some case, _ -> (
        match List.assoc_opt case nexts with
        | None -> failwith "case not found"
        | Some (None, id_case) -> Either.Right id_case
        | Some (Some next, _) -> Either.Left next)
    | None, None, _ -> (
        match get_next_from_end state node.data with
        | Some (id, case) -> find_next state id case
        | None -> Either.left id)

  let rec request_next state id case =
    let path = path_of_id id state in
    let exec_data = Effect.perform (Step (Some id, case, path)) in
    match init_or_handle ~state ~prev_id:id ?gil_case:case exec_data with
    | Either.Left (id, case) -> request_next state id case
    | Either.Right map -> map.data.id

  let step state id case =
    let () =
      DL.log (fun m ->
          m "Stepping %a %a" pp_id id (pp_option Branch_case.pp) case)
    in
    match find_next state id case with
    | Either.Left next -> next
    | Either.Right (id, case) -> request_next state id case

  let is_breakpoint ~start ~current =
    let file, line = current.data.loc in
    let- () =
      let file', line' = start.data.loc in
      if file = file' && line = line' then Some false else None
    in
    Effect.perform (IsBreakpoint (file, [ line ]))

  let step_all ~start state id case =
    let cmd = get_exn state.map id in
    let stack_depth = List.length cmd.data.callers in
    let rec aux ends = function
      | [] -> List.rev ends
      | (id, case) :: rest ->
          let next_id = step state id case in
          let node = get_exn state.map next_id in
          let ends, nexts =
            let- () =
              if is_breakpoint ~start ~current:node then
                Some (node :: ends, rest)
              else None
            in
            match node.next with
            | Some (Single _) -> (ends, (next_id, None) :: rest)
            | Some (Branch nexts) ->
                let new_nexts =
                  nexts |> List.map (fun (case, _) -> (next_id, Some case))
                in
                (ends, new_nexts @ rest)
            | None ->
                let stack_depth' = List.length node.data.callers in
                if stack_depth' < stack_depth then
                  failwith "Stack depth too small!"
                else if stack_depth' > stack_depth then
                  match get_next_from_end state node.data with
                  | Some next -> (ends, next :: rest)
                  | None -> (node :: ends, rest)
                else (node :: ends, rest)
          in
          aux ends nexts
    in
    match (case, cmd.next) with
    | _, None -> [ cmd ]
    | None, Some (Branch nexts) ->
        let first_steps =
          nexts |> List.map (fun (case, _) -> (id, Some case))
        in
        aux [] first_steps
    | _, _ -> aux [] [ (id, case) ]

  let step_branch state id case =
    let case =
      let+ json = case in
      json |> Branch_case.of_yojson |> Result.get_ok
    in
    let cmd = get_exn state.map id in
    (* Bodge: step in if on func exit placeholder *)
    let- () =
      match (case, cmd.data.submap) with
      | Some FuncExitPlaceholder, Submap submap_id ->
          Some (submap_id, Debugger_utils.Step)
      | _ -> None
    in
    let next_id = step state id case in
    (next_id, Debugger_utils.Step)

  let step_in state id =
    let cmd = get_exn state.map id in
    (* Only BranchCmds should have submaps *)
    let- () =
      match cmd.data.submap with
      | NoSubmap | Proc _ -> None
      | Submap submap_id -> Some (submap_id, Debugger_utils.Step)
    in
    step_branch state id None

  let step_over state id =
    let node = get_exn state.map id in
    let () =
      let () =
        match (node.next, node.data.submap) with
        | Some (Branch nexts), (NoSubmap | Proc _) ->
            if List.mem_assoc FuncExitPlaceholder nexts then
              step state id (Some FuncExitPlaceholder) |> ignore
        | _ -> ()
      in
      let node = get_exn state.map id in
      let> submap_id =
        match node.data.submap with
        | NoSubmap | Proc _ -> None
        | Submap m -> Some m
      in
      let _ = step_all ~start:node state submap_id None in
      ()
    in
    let node = get_exn state.map id in
    (* Failsafe in case of error paths in submap *)
    match node.next with
    | Some (Branch [ (FuncExitPlaceholder, _) ]) -> (id, Debugger_utils.Step)
    | _ -> step_branch state id None

  let step_back state id =
    let cmd = get_exn state.map id in
    let id =
      match cmd.data.prev with
      | Some (id, _) -> id
      | None -> id
    in
    (id, Debugger_utils.Step)

  let continue state id =
    let start = get_exn state.map id in
    let rec aux ends = function
      | [] -> ends
      | (id, case) :: rest ->
          let new_ends, nexts =
            let new_ends = step_all ~start state id case in
            new_ends
            |> List.partition_map (fun { data; _ } ->
                   match get_next_from_end state data with
                   | Some (id, case) -> Either.Right (id, case)
                   | None -> Either.Left data.id)
          in
          aux (ends @ new_ends) (nexts @ rest)
    in
    let ends = aux [] [ (id, None) ] in
    let id = List.hd ends in
    (id, Debugger_utils.Step)

  let step_out state id =
    match (get_exn state.map id).data.callers with
    | [] -> continue state id
    | caller_id :: _ -> step_over state caller_id

  let continue_back state id =
    let start = get_exn state.map id in
    let rec aux node =
      let { id; callers; _ } = node.data in
      if is_breakpoint ~start ~current:node then (id, Debugger_utils.Breakpoint)
      else
        match previous_step id state with
        | None -> (
            match callers with
            | [] -> (id, Debugger_utils.Step)
            | caller_id :: _ -> aux (get_exn state.map caller_id))
        | Some (id, _) -> aux (get_exn state.map id)
    in
    aux start

  let init ~proc_name ~all_procs:_ tl_ast prog =
    let gil_state = Gil.get_state () in
    let+ tl_ast = tl_ast in
    let partial_cmds = Partial_cmds.init () in
    let state =
      {
        proc_name;
        gil_state;
        tl_ast;
        partial_cmds;
        map = Exec_map.make ();
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
        | Either.Right map -> map.data.id
      in
      let id = aux None in
      (id, Debugger_utils.Step)
    in
    (state, finish_init)

  let init_exn ~proc_name ~all_procs tl_ast prog =
    match init ~proc_name ~all_procs tl_ast prog with
    | None -> failwith "init: wislLifter needs a tl_ast!"
    | Some x -> x

  let parse_and_compile_files ~entrypoint files =
    WParserAndCompiler.parse_and_compile_files files
    |> Result.map (fun r -> (r, entrypoint))

  let pp_comma_sep pp = Fmt.list ~sep:(Fmt.any ", ") pp

  module Expr_pp = struct
    let rec pp_lit ft =
      let open Literal in
      function
      | Int i -> Fmt.pf ft "%a" Z.pp_print i
      | LList ll -> Fmt.pf ft "[%a]" (pp_comma_sep pp_lit) ll
      | l -> Fmt.pf ft "%a" Literal.pp l

    let pp_binop ft =
      let open BinOp in
      function
      | ILessThanEqual -> Fmt.string ft "<="
      | ILessThan -> Fmt.string ft "<"
      | IPlus -> Fmt.string ft "+"
      | IMinus -> Fmt.string ft "-"
      | ITimes -> Fmt.string ft "*"
      | IDiv -> Fmt.string ft "/"
      | IMod -> Fmt.string ft "%"
      | StrLess -> Fmt.string ft "<"
      | op -> Fmt.pf ft "%s" (BinOp.str op)

    let rec pp_expr' ~outermost ft =
      ignore outermost;
      let open Expr in
      let pp = pp_expr' ~outermost:false in
      let pp_o = pp_expr' ~outermost:true in
      function
      | Lit l -> Fmt.pf ft "%a" pp_lit l
      | UnOp (LstLen, e) -> Fmt.pf ft "len(%a)" pp_o e
      | BinOp (e1, LstNth, e2) -> Fmt.pf ft "%a[%a]" pp e1 pp_o e2
      | BinOp (e1, op, e2) when outermost ->
          Fmt.pf ft "%a %a %a" pp e1 pp_binop op pp e2
      | BinOp (e1, op, e2) -> Fmt.pf ft "(%a %a %a)" pp e1 pp_binop op pp e2
      | EList ll -> Fmt.pf ft "[%a]" (pp_comma_sep pp_o) ll
      | ESet ll -> Fmt.pf ft "{%a}" (pp_comma_sep pp_o) ll
      | NOp (LstCat, ll) ->
          Fmt.pf ft "(%a)" (Fmt.list ~sep:(Fmt.any " %@ ") pp) ll
      | e -> Fmt.pf ft "%a" (pp_custom ~pp) e

    let f = Fmt.hbox (pp_expr' ~outermost:true)
  end

  let pp_expr = Expr_pp.f

  let pp_asrt ft =
    let open Asrt in
    function
    | Emp -> Fmt.pf ft "emp"
    | Pred (name, params) ->
        Fmt.pf ft "%s(%a)" name (pp_comma_sep pp_expr) params
    | Types tls ->
        let pp_tl ft (e, t) = Fmt.pf ft "%a : %s" pp_expr e (Type.str t) in
        Fmt.pf ft "%a" (pp_comma_sep pp_tl) tls
    | Pure e -> Fmt.pf ft "%a" pp_expr e
    | CorePred (a, ins, outs) -> (
        match (WislLActions.ga_from_str a, ins, outs) with
        | Some Cell, [ loc; offset ], [ value ] ->
            Fmt.pf ft "[%a, %a] -> %a" pp_expr loc pp_expr offset pp_expr value
        | Some Bound, [ loc ], [ bound ] ->
            Fmt.pf ft "%a has bound %a" pp_expr loc pp_expr bound
        | Some Freed, [ loc ], [] -> Fmt.pf ft "%a is freed" pp_expr loc
        | _ ->
            Fmt.pf ft "%s(%a, %a)" a (pp_comma_sep pp_expr) ins
              (pp_comma_sep pp_expr) outs)
    | Wand { lhs = lname, largs; rhs = rname, rargs } ->
        Fmt.pf ft "%s(%a) -* %s(%a)" lname (pp_comma_sep pp_expr) largs rname
          (pp_comma_sep pp_expr) rargs

  module Lift_variables = struct
    open Engine

    module Scopes = struct
      open Variable

      let pvars = { id = 1; name = "Program store" }
      let heap = { id = 2; name = "Heap" }
      let preds = { id = 3; name = "Predicates" }
      let pfs = { id = 4; name = "Pure formulae" }
      let types = { id = 5; name = "Types" }
      let axioms = { id = 6; name = "Axioms" }
      let all = [ pvars; heap; pfs; types; preds; axioms ]
    end

    let get_store_vars store =
      List.filter_map
        (fun (var, (value : Gil_syntax.Expr.t)) ->
          if Str.string_match (Str.regexp "gvar") var 0 then None
          else
            let match_offset lst loc =
              match lst with
              | [ Expr.Lit (Int offset) ] ->
                  Fmt.str "-> (%s, %d)" loc (Z.to_int offset)
              | [ offset ] -> Fmt.str "-> (%s, %a)" loc pp_expr offset
              | _ -> Fmt.to_to_string pp_expr value
            in
            let value =
              match value with
              | Expr.EList (Lit (Loc loc) :: rest)
              | Expr.EList (LVar loc :: rest) -> match_offset rest loc
              | _ -> Fmt.to_to_string pp_expr value
            in
            Some ({ name = var; value; type_ = None; var_ref = 0 } : Variable.t))
        store
      |> List.sort Stdlib.compare

    let add_heap_vars new_var_ref variables memory =
      let vstr = Fmt.to_to_string pp_expr in
      let compare_offsets (v, _) (w, _) =
        try
          let open Expr.Infix in
          let difference = v - w in
          match difference with
          | Expr.Lit (Int f) ->
              if Z.lt f Z.zero then -1 else if Z.gt f Z.zero then 1 else 0
          | _ -> 0
        with _ ->
          (* Do not sort the offsets if an exception has occurred *)
          0
      in
      let cell_vars l : Variable.t list =
        List.sort compare_offsets l
        |> List.map (fun (offset, value) : Variable.t ->
               (* Display offset as a number to match the printing of WISL pointers *)
               let offset_str =
                 match offset with
                 | Expr.Lit (Int o) -> Z.to_string o
                 | Expr.BinOp (Lit (Int i), IPlus, o) ->
                     (* Flip around e.g. (1 + #loc_5) to (#loc_5 + 1) *)
                     vstr Expr.(BinOp (o, IPlus, Lit (Int i)))
                 | other -> vstr other
               in
               Variable.create_leaf offset_str (vstr value) ())
      in
      memory |> WislSHeap.to_seq
      |> Seq.map (fun (loc, blocks) ->
             match blocks with
             | None -> Variable.create_leaf loc "freed" ()
             | Some (data, bound) ->
                 let bound =
                   match bound with
                   | None -> "none"
                   | Some bound -> string_of_int bound
                 in
                 let bound = Variable.create_leaf "bound" bound () in
                 let cells_id = new_var_ref () in
                 let () =
                   Hashtbl.replace variables cells_id
                     (cell_vars (SFVL.to_list data))
                 in
                 let cells = Variable.create_node "cells" cells_id () in
                 let loc_id = new_var_ref () in
                 let () = Hashtbl.replace variables loc_id [ bound; cells ] in
                 Variable.create_node loc loc_id ~value:"allocated" ())
      |> List.of_seq

    let pr_list_axiom =
      let open Expr in
      function
      | BinOp (Lit (Int i), ILessThanEqual, (UnOp (LstLen, _) as l))
        when i = Z.zero -> Some (Fmt.str "%a >= 0" pp_expr l)
      | _ -> None

    let get_pf_vars pfs =
      let pfs_vars, axiom_vars =
        pfs |> PFS.to_list
        |> List.partition_map (fun formula ->
               let var = Variable.make ~name:"" in
               match pr_list_axiom formula with
               | None ->
                   let value = Fmt.to_to_string pp_expr formula in
                   Left (var ~value ())
               | Some value -> Right (var ~value ()))
      in
      let pfs_vars = List.sort Stdlib.compare pfs_vars in
      let axiom_vars = List.sort Stdlib.compare axiom_vars in
      (pfs_vars, axiom_vars)

    let get_pred_vars preds =
      let open Variable in
      preds |> Preds.to_list
      |> List.map (fun (name, params) ->
             let value = Fmt.str "%s(%a)" name (pp_comma_sep pp_expr) params in
             { name = ""; value; type_ = None; var_ref = 0 })
      |> List.sort (fun v w -> Stdlib.compare v.value w.value)

    let is_ptr_val =
      let open Expr in
      function
      | EList [ (Lit (Loc _) | ALoc _); _ ] -> true
      | EList [ (LVar v | PVar v); _ ] ->
          Names.(is_aloc_name v || is_lloc_name v)
      | _ -> false

    let is_ptr_in_store ~store name =
      match List.assoc_opt name store with
      | Some v -> is_ptr_val v
      | None -> false

    let is_ptr_in_pfs ~pfs name =
      let open Expr in
      pfs
      |> List.exists @@ fun pf ->
         match pf with
         | BinOp (LVar name', Equal, v) -> name = name' && is_ptr_val v
         | _ -> false

    let is_ptr ~store ~pfs name typ =
      typ = Type.ListType
      && (is_ptr_in_pfs ~pfs name || is_ptr_in_store ~store name)

    let get_type_env_vars ~store ~pfs (types : Type_env.t) : Variable.t list =
      types |> Type_env.to_list
      |> List.map (fun (name, value) ->
             let value =
               if is_ptr ~store ~pfs name value then "Pointer"
               else if value = Type.ObjectType then "Block identifier"
               else Type.str value
             in
             Variable.{ name; value; type_ = None; var_ref = 0 })
      |> List.sort Variable.(fun v w -> Stdlib.compare v.name w.name)

    let f _ { store; memory; pfs; types; preds } _ =
      let variables = Hashtbl.create 0 in
      (* Scopes and var refs share IDs; they can't clash *)
      let new_var_ref =
        let count = ref (List.length Scopes.all) in
        fun () ->
          let () = count := !count + 1 in
          !count
      in
      let var_groups = [] in

      (* Program variables *)
      let var_groups =
        let pvars = get_store_vars store in
        (Scopes.pvars, pvars) :: var_groups
      in

      (* Heap *)
      let var_groups =
        let heap_vars = add_heap_vars new_var_ref variables memory in
        (Scopes.heap, heap_vars) :: var_groups
      in

      (* Predicates *)
      let var_groups =
        match preds with
        | Some preds ->
            let pred_vars = get_pred_vars preds in
            (Scopes.preds, pred_vars) :: var_groups
        | None -> var_groups
      in

      (* Pure formulae *)
      let var_groups, axioms =
        match pfs with
        | Some pfs ->
            let pfs_vars, axiom_vars = get_pf_vars pfs in
            ( (Scopes.pfs, pfs_vars) :: var_groups,
              Some (Scopes.axioms, axiom_vars) )
        | None -> (var_groups, None)
      in

      (* Type environment *)
      let var_groups =
        match types with
        | Some types ->
            let pfs = Option.fold ~some:PFS.to_list ~none:[] pfs in
            let type_vars = get_type_env_vars ~store ~pfs types in
            (Scopes.types, type_vars) :: var_groups
        | None -> var_groups
      in

      (* Axioms *)
      let var_groups =
        match axioms with
        | Some g -> g :: var_groups
        | None -> var_groups
      in

      let scopes =
        var_groups
        |> List.rev_map @@ fun (scope, vars) ->
           let () = Hashtbl.replace variables Variable.(scope.id) vars in
           scope
      in
      (scopes, variables)
  end

  let get_variables = Lift_variables.f
  let pp_expr _ = pp_expr
  let pp_asrt _ = pp_asrt
end
