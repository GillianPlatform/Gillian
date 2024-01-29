open Gillian.Gil_syntax
open Gillian.Debugger.Lifter
open Gillian.Debugger.Utils
open Gillian.Utils
open Prelude
open Syntaxes.Option
open Syntaxes.Result_of_option
module Program = Goto_lib.Program
module Branch_case = Kcommons.Branch_case
module Gil_branch_case = Gillian.Gil_syntax.Branch_case
module DL = Gillian.Debugger.Logging
module Exec_map = Gillian.Debugger.Utils.Exec_map
module Annot = Kanillian_compiler.K_annot
open Annot
open Branch_case

type rid = Gillian.Logging.Report_id.t [@@deriving yojson, show]

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
    (SMemory : Gillian.Symbolic.Memory_S)
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (Verification : Engine.Verifier.S
                      with type annot = Kanillian_compiler.K_annot.t) =
struct
  open Exec_map
  module CmdReport = Verification.SAInterpreter.Logging.ConfigReport

  type memory_error = SMemory.err_t
  type tl_ast = Program.t
  type memory = SMemory.t
  type cmd_report = CmdReport.t [@@deriving yojson]
  type annot = Annot.t

  module Gil_lifter = Gil.Lifter

  type branch_data = rid * Gil_branch_case.t option [@@deriving yojson]
  type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
  type stack_direction = In | Out of rid [@@deriving yojson]

  type map = (Branch_case.t, cmd_data, branch_data) Exec_map.t

  and cmd_data = {
    id : rid;
    all_ids : rid list;
    display : string;
    unifys : unification list;
    errors : string list;
    mutable submap : map submap;
    prev : (rid * Branch_case.t option) option;
    callers : rid list;
    func_return_label : (string * int) option;
  }
  [@@deriving yojson]

  module Partial_cmds = struct
    let ( let++ ) f o = Result.map o f
    let ( let** ) o f = Result.bind o f

    type canonical_cmd_data = {
      id : rid;
      display : string;
      callers : rid list;
      stack_direction : stack_direction option;
      nest_kind : nest_kind option;
    }
    [@@deriving to_yojson]

    type partial_data = {
      prev : (rid * Branch_case.t option * rid list) option;
          (** Where to put the finished cmd in the map. *)
      all_ids : (rid * (Branch_case.kind option * Branch_case.case)) Ext_list.t;
          (** All the GIL cmd IDs that build into this one (and the relevant branch case info). *)
      unexplored_paths : (rid * Gil_branch_case.t option) Stack.t;
          (** All the paths that haven't been explored yet; a stack means depth-first exploration. *)
      ends : (Branch_case.case * (rid * Gil_branch_case.t option)) Ext_list.t;
          (** All the end points; there may be multiple if the cmd branches. *)
      (* TODO: rename to matches *)
      unifys : unification Ext_list.t;
          (** Unifications contained in this command *)
      errors : string Ext_list.t;  (** Errors occurring during this command *)
      mutable canonical_data : canonical_cmd_data option;
      mutable is_unevaluated_funcall : bool;
    }
    [@@deriving to_yojson]

    type t = (rid, partial_data) Hashtbl.t [@@deriving to_yojson]

    type finished = {
      prev : (rid * Branch_case.t option) option;
      id : rid;
      all_ids : rid list;
      display : string;
      unifys : Exec_map.unification list;
      errors : string list;
      kind : (Branch_case.t, branch_data) cmd_kind;
      callers : rid list;
      stack_direction : stack_direction option;
    }
    [@@deriving yojson]

    type partial_result =
      | Finished of finished
      | StepAgain of (rid option * Gil_branch_case.t option)
    [@@deriving yojson]

    let step_again ?id ?branch_case () = StepAgain (id, branch_case)

    let ends_to_cases
        ~is_unevaluated_funcall
        ~nest_kind
        (ends : (Branch_case.case * branch_data) list) =
      let open Annot in
      let open Branch_case in
      let- () =
        match (nest_kind, ends) with
        | Some (Fun_call _), [ (Unknown, bdata) ] ->
            if is_unevaluated_funcall then None
            else Some (Ok [ (Func_exit_placeholder, bdata) ])
        | Some (Fun_call _), _ ->
            Some (Error "Unexpected branching in cmd with Fun_call nest")
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
               | (If_else _ | While_loop _ | For_loop _), 1 -> -1
               | _ -> count
             in
             let () = Hashtbl.replace counts kind (total, count + 1) in
             (Case (kind, ix), branch_data))
      |> Result.ok

    let finish partial =
      let ({ prev; canonical_data; all_ids; ends; is_unevaluated_funcall; _ }
            : partial_data) =
        partial
      in
      let** { id; display; callers; stack_direction; nest_kind } =
        Result_utils.of_option
          ~none:"Trying to finish partial with no canonical data!"
          canonical_data
      in
      let prev =
        let+ id, branch, _ = prev in
        (id, branch)
      in
      let all_ids = all_ids |> Ext_list.to_list |> List.map fst in
      let ends = Ext_list.to_list ends in
      let++ kind =
        let++ cases = ends_to_cases ~is_unevaluated_funcall ~nest_kind ends in
        match cases with
        | [] -> Final
        | [ (Case (Unknown, _), _) ] -> Normal
        | _ -> Branch cases
      in
      Finished
        {
          prev;
          id;
          all_ids;
          display;
          unifys = [];
          errors = [];
          kind;
          callers;
          stack_direction;
        }

    let init () = Hashtbl.create 0

    let init_partial ~prev =
      {
        prev;
        all_ids = Ext_list.make ();
        unexplored_paths = Stack.create ();
        ends = Ext_list.make ();
        unifys = Ext_list.make ();
        errors = Ext_list.make ();
        canonical_data = None;
        is_unevaluated_funcall = false;
      }

    module Update = struct
      let get_is_end (annot : Annot.t) =
        match annot.cmd_kind with
        | Normal b -> Ok b
        | Internal | Hidden -> Ok false
        | Harness as k ->
            Fmt.error "%a cmd should have been skipped!" Annot.pp_cmd_kind k
        | Return -> Ok true
        | Unknown -> Error "HORROR - unknown cmd kind"

      let resolve_case
          ?gil_case
          (kind : Branch_case.kind option)
          (prev_case : Branch_case.case) =
        match (kind, prev_case) with
        | None, prev_case -> Ok prev_case
        | Some prev_kind, Unknown -> (
            match prev_kind with
            | If_else_kind | For_loop_kind | While_loop_kind -> (
                match gil_case with
                | Some (Gil_branch_case.GuardedGoto b) ->
                    Ok (bool_kind_to_case prev_kind b)
                | _ -> Error "If_else_kind expects a GuardedGoto gil case"))
        | Some _, _ ->
            Error "HORROR - branch kind is set with pre-existing case!"

      let update_paths
          ~is_end
          ~exec_data
          ~branch_case
          ~annot
          ~branch_kind
          partial =
        let ({ id; kind; _ } : exec_data) = exec_data in
        let { ends; unexplored_paths; _ } = partial in
        match (annot.cmd_kind, kind, is_end) with
        | Return, _, _ | _, Final, _ -> Ok ()
        | _, Normal, false ->
            Stack.push (id, None) unexplored_paths;
            Ok ()
        | _, Branch cases, false ->
            cases
            |> List.iter (fun (gil_case, ()) ->
                   Stack.push (id, Some gil_case) unexplored_paths);
            Ok ()
        | _, Normal, true ->
            Ext_list.add (branch_case, (id, None)) ends;
            Ok ()
        | _, Branch cases, true ->
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
                Fmt.error "HORROR - too great a stack depth change! (%d)"
                  depth_change)

      let update_unevaluated_funcall
          ~(prog : Program.t)
          ~(exec_data : exec_data)
          ~annot
          (partial : partial_data) =
        let cmd_report = exec_data.cmd_report in
        let> () =
          match (partial.is_unevaluated_funcall, annot.cmd_kind) with
          | true, _ | _, (Harness | Unknown | Hidden | Internal) -> None
          | _ -> Some ()
        in
        let> pid =
          match
            (partial.is_unevaluated_funcall, CmdReport.(cmd_report.cmd))
          with
          | false, Call (_, Lit (String pid), _, _, _)
          | false, ECall (_, (Lit (String pid) | PVar pid), _, _) -> Some pid
          | _ -> None
        in
        let () =
          if
            Hashset.mem Program.(prog.unevaluated_funcs) pid
            || List.mem pid Constants.Internal_functions.names
          then partial.is_unevaluated_funcall <- true
        in
        ()

      let update_return_cmd_info
          ~id
          ~callers
          ~stack_direction
          (partial : partial_data) =
        partial.canonical_data <-
          Some
            {
              id;
              display = "<end of func>";
              callers;
              stack_direction;
              nest_kind = None;
            };
        Ok ()

      let update_canonical_data
          ~id
          ~(annot : Annot.t)
          ~exec_data
          (partial : partial_data) =
        let- () =
          match annot.cmd_kind with
          | Return ->
              let result =
                let** callers, stack_direction =
                  get_stack_info ~partial exec_data
                in
                update_return_cmd_info ~id ~callers ~stack_direction partial
              in
              Some result
          | _ -> None
        in
        match (annot.cmd_kind, partial.canonical_data, annot.display) with
        | (Harness | Unknown), _, _ ->
            Fmt.error "HORROR - trying to get display of %a" Annot.pp_cmd_kind
              annot.cmd_kind
        | (Internal | Hidden), _, _ -> Ok ()
        | _, None, Some display ->
            let** callers, stack_direction =
              get_stack_info ~partial exec_data
            in
            let Annot.{ nest_kind; _ } = annot in
            partial.canonical_data <-
              Some { id; display; callers; stack_direction; nest_kind };
            Ok ()
        | _ -> Ok ()

      let insert_id_and_case
          ~prev_id
          ~(exec_data : exec_data)
          ~id
          ({ all_ids; _ } : partial_data) =
        let open Branch_case in
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

      let f ~prog ~prev_id exec_data partial =
        let { id; cmd_report; unifys; errors; _ } = exec_data in
        let annot = CmdReport.(cmd_report.annot) in
        let** is_end = get_is_end annot in
        let** branch_kind, branch_case =
          insert_id_and_case ~prev_id ~exec_data ~id partial
        in
        let () = Ext_list.add_all unifys partial.unifys in
        let () = Ext_list.add_all errors partial.errors in
        let** () =
          update_paths ~is_end ~exec_data ~branch_case ~annot ~branch_kind
            partial
        in
        let** () = update_canonical_data ~id ~annot ~exec_data partial in
        let () = update_unevaluated_funcall ~prog ~exec_data ~annot partial in

        (* Finish or continue *)
        match Stack.pop_opt partial.unexplored_paths with
        | None -> finish partial
        | Some (id, branch_case) -> step_again ~id ?branch_case () |> Result.ok
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
        ("Kani_c_lifter.PartialCmds.handle: " ^ msg)

    let handle ~prog ~(partials : t) ~get_prev ~prev_id exec_data =
      DL.log (fun m ->
          let report = exec_data.cmd_report in
          let cmd = CmdReport.(report.cmd) in
          m "HANDLING %a" Gil_syntax.Cmd.pp_indexed cmd);
      let partial =
        find_or_init ~partials ~get_prev prev_id
        |> Result_utils.or_else (fun e -> failwith ~exec_data ~partials e)
      in
      Hashtbl.replace partials exec_data.id partial;
      let result =
        update ~prog ~prev_id exec_data partial
        |> Result_utils.or_else (fun e ->
               failwith ~exec_data ~partial ~partials e)
      in
      let () =
        match result with
        | Finished { display; _ } ->
            DL.log (fun m -> m "FINISHED %s" display);
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
    prog : (annot, int) Prog.t; [@to_yojson fun _ -> `Null]
    partial_cmds : Partial_cmds.t;
    mutable map : map;
    id_map : (rid, map) Hashtbl.t;
    func_return_map : (rid, string * int ref) Hashtbl.t;
    mutable func_return_count : int;
  }
  [@@deriving to_yojson]

  module Insert_new_cmd = struct
    let failwith ~state ~finished_partial msg =
      DL.failwith
        (fun () ->
          [
            ("state", to_yojson state);
            ( "finished_partial",
              Partial_cmds.finished_to_yojson finished_partial );
          ])
        ("Kani_c_lifter.insert_new_cmd: " ^ msg)

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
          Hashtbl.remove nexts Func_exit_placeholder;
          let case = Case (Func_exit label, ix) in
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
            { all_ids; id; display; unifys; errors; prev; kind; callers; _ } =
        finished_partial
      in
      let data =
        {
          all_ids;
          id;
          display;
          unifys;
          errors;
          submap = NoSubmap;
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
          Ok ()
      | _, _, None -> Error "inserting to non-empty map with no prev!"
      | Some In, _, Some (parent_id, Some Func_exit_placeholder)
      | Some In, _, Some (parent_id, None) ->
          insert_as_submap ~state ~parent_id new_cmd
      | Some In, _, Some (_, Some _) -> Error "stepping in with branch case!"
      | None, _, Some (prev_id, case) ->
          insert_as_next ~state ~prev_id ?case new_cmd
      | Some (Out prev_id), _, Some (inner_prev_id, _) ->
          let** case =
            let func_return_label =
              match Hashtbl.find state.id_map inner_prev_id with
              | Nothing -> None
              | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
                  data.func_return_label
            in
            match func_return_label with
            | Some (label, ix) -> Ok (Case (Func_exit label, ix))
            | None -> Error "stepping out without function return label!"
          in
          insert_as_next ~state ~prev_id ~case new_cmd

    let get_return_label_from_prev ~state prev_id =
      match Hashtbl.find state.id_map prev_id with
      | Nothing -> None
      | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
          data.func_return_label

    let f ~state finished_partial =
      (let { id_map; _ } = state in
       let Partial_cmds.{ all_ids; prev; stack_direction; _ } =
         finished_partial
       in
       let** new_cmd =
         let++ func_return_label =
           resolve_func_branches ~state finished_partial
         in
         make_new_cmd ~func_return_label finished_partial
       in
       let** () = insert_cmd ~state ~prev ~stack_direction new_cmd in
       all_ids |> List.iter (fun id -> Hashtbl.replace id_map id new_cmd);
       Ok ())
      |> Result_utils.or_else (failwith ~state ~finished_partial)
  end

  let insert_new_cmd = Insert_new_cmd.f

  module Init_or_handle = struct
    let get_prev ~state ~gil_case ~prev_id () =
      let { map; id_map; _ } = state in
      let=* prev_id = Ok prev_id in
      let=* map =
        match Hashtbl.find_opt id_map prev_id with
        | None -> (
            match map with
            | Nothing ->
                Ok None
                (* It's okay to not have a prev if we're still in the harness *)
            | _ -> Error "couldn't find map at prev_id!")
        | map -> Ok map
      in
      match map with
      | Nothing -> Error "got Nothing map!"
      | FinalCmd { data; _ } | Cmd { data; _ } ->
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

    let f ~state ?prev_id ?gil_case (exec_data : exec_data) =
      let annot = CmdReport.(exec_data.cmd_report.annot) in
      let { partial_cmds = partials; tl_ast = prog; _ } = state in
      match annot.cmd_kind with
      | Unknown ->
          let json () =
            [
              ("state", to_yojson state);
              ("gil_case", opt_to_yojson Gil_branch_case.to_yojson gil_case);
              ("exec_data", exec_data_to_yojson exec_data);
            ]
          in
          DL.failwith json "Kani_c_lifter: Encountered unknown cmd kind"
      | Harness -> ExecNext (None, None)
      | Normal _ | Internal | Return | Hidden -> (
          let get_prev = get_prev ~state ~gil_case ~prev_id in
          let partial_result =
            Partial_cmds.handle ~prog ~get_prev ~partials ~prev_id exec_data
          in
          match partial_result with
          | Finished finished ->
              insert_new_cmd ~state finished;
              Stop (Some finished.id)
          | StepAgain (id, case) -> ExecNext (id, case))
  end

  let init_or_handle = Init_or_handle.f

  let init ~proc_name ~all_procs:_ tl_ast prog exec_data =
    let gil_state = Gil.get_state () in
    let+ tl_ast = tl_ast in
    let partial_cmds = Partial_cmds.init () in
    let id_map = Hashtbl.create 0 in
    let state =
      {
        proc_name;
        gil_state;
        tl_ast;
        prog;
        partial_cmds;
        map = Nothing;
        id_map;
        func_return_map = Hashtbl.create 0;
        func_return_count = 0;
      }
    in
    let result = init_or_handle ~state exec_data in
    (state, result)

  let init_exn ~proc_name ~all_procs tl_ast prog exec_data =
    match init ~proc_name ~all_procs tl_ast prog exec_data with
    | None -> failwith "init: kani_c_lifter needs a tl_ast!"
    | Some x -> x

  let handle_cmd prev_id gil_case exec_data state =
    init_or_handle ~state ~prev_id ?gil_case exec_data

  let dump = to_yojson
  let get_gil_map _ = failwith "get_gil_map: not implemented!"

  let package_case case =
    let json = Branch_case.to_yojson case in
    let display = Branch_case.display case in
    (display, json)

  let package_data package { id; all_ids; display; unifys; errors; submap; _ } =
    let submap =
      match submap with
      | NoSubmap -> NoSubmap
      | Proc p -> Proc p
      | Submap m -> Submap (package m)
    in
    Packaged.{ id; all_ids; display; unifys; errors; submap }

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
  let get_unifys_at_id _ _ = []

  let get_root_id { map; _ } =
    match map with
    | Nothing -> None
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        Some data.id

  let path_of_id id { gil_state; _ } = Gil_lifter.path_of_id id gil_state

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
        let id = List.hd (List.rev data.all_ids) in
        (id, None)
    | BranchCmd { nexts; _ }, Some case -> (
        let case = case |> snd |> Branch_case.of_yojson |> Result.get_ok in
        match Hashtbl.find_opt nexts case with
        | None -> failwith "branch case not found!"
        | Some ((id, case), _) -> (id, case))

  let previous_step id { id_map; _ } =
    let+ id, case =
      match Hashtbl.find id_map id with
      | Nothing -> None
      | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> data.prev
    in
    let case = case |> Option.map package_case in
    (id, case)

  let select_next_path case id { gil_state; _ } =
    Gil_lifter.select_next_path case id gil_state

  let find_unfinished_path ?at_id state =
    let { map; id_map; _ } = state in
    let failwith m =
      DL.failwith
        (fun () ->
          [
            ("state", dump state); ("at_id", opt_to_yojson rid_to_yojson at_id);
          ])
        ("find_unfinished_path: " ^ m)
    in
    let rec aux = function
      | Nothing -> failwith "started at Nothing"
      | Cmd { data = { all_ids; _ }; next = Nothing } ->
          let id = List.hd (List.rev all_ids) in
          Some (id, None)
      | Cmd { next; _ } -> aux next
      | BranchCmd { nexts; _ } -> (
          let unfinished =
            Hashtbl.find_map
              (fun _ (gil_step, next) ->
                if next = Nothing then Some gil_step else None)
              nexts
          in
          match unfinished with
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

  let memory_error_to_exception_info
      (_ : (memory_error, annot, tl_ast) memory_error_info) : exception_info =
    { id = "unknown"; description = Some "Error lifting not supported yet!" }

  let add_variables = Memory_model.MonadicSMemory.Lift.add_variables
end
