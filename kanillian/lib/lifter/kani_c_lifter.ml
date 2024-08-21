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

type id = Gillian.Logging.Report_id.t [@@deriving yojson, show]

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

  type branch_data = id * Gil_branch_case.t option [@@deriving yojson]
  type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
  type stack_direction = In | Out of id [@@deriving yojson]
  type step_args = id option * Gil_branch_case.t option * Gil_branch_case.path
  type _ Effect.t += Step : step_args -> exec_data Effect.t

  type cmd_data = {
    id : id;
    all_ids : id list;
    display : string;
    matches : matching list;
    errors : string list;
    mutable submap : id submap;
    prev : (id * Branch_case.t option) option;
    callers : id list;
    func_return_label : (string * int) option;
    loc : (string * int) option;
  }
  [@@deriving yojson]

  type map = (id, Branch_case.t, cmd_data, branch_data) Exec_map.map
  [@@deriving yojson]

  module Partial_cmds = struct
    let ( let++ ) f o = Result.map o f
    let ( let** ) o f = Result.bind o f

    type canonical_cmd_data = {
      id : id;
      display : string;
      callers : id list;
      stack_direction : stack_direction option;
      nest_kind : nest_kind option;
      loc : (string * int) option;
    }
    [@@deriving to_yojson]

    type partial_end = Branch_case.case * (id * Gil_branch_case.t option)
    [@@deriving to_yojson]

    type partial_data = {
      prev : (id * Branch_case.t option * id list) option;
          (** Where to put the finished cmd in the map. *)
      all_ids : (id * (Branch_case.kind option * Branch_case.case)) Ext_list.t;
          (** All the GIL cmd IDs that build into this one (and the relevant branch case info). *)
      unexplored_paths : (id * Gil_branch_case.t option) Stack.t;
          (** All the paths that haven't been explored yet; a stack means depth-first exploration. *)
      ends : partial_end Ext_list.t;
          (** All the end points; there may be multiple if the cmd branches. *)
      matches : matching Ext_list.t;
          (** Unifications contained in this command *)
      errors : string Ext_list.t;  (** Errors occurring during this command *)
      mutable canonical_data : canonical_cmd_data option;
      mutable is_unevaluated_funcall : bool;
    }
    [@@deriving to_yojson]

    type t = (id, partial_data) Hashtbl.t [@@deriving to_yojson]

    type finished = {
      prev : (id * Branch_case.t option) option;
      id : id;
      all_ids : id list;
      display : string;
      matches : Exec_map.matching list;
      errors : string list;
      next_kind : (Branch_case.t, branch_data) next_kind;
      callers : id list;
      stack_direction : stack_direction option;
      loc : (string * int) option;
    }
    [@@deriving yojson]

    type partial_result =
      | Finished of finished
      | StepAgain of (id * Gil_branch_case.t option)
    [@@deriving yojson]

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
      let ({
             prev;
             canonical_data;
             all_ids;
             ends;
             is_unevaluated_funcall;
             matches;
             errors;
             _;
           }
            : partial_data) =
        partial
      in
      let** { id; display; callers; stack_direction; nest_kind; loc } =
        Result_utils.of_option
          ~none:"Trying to finish partial with no canonical data!"
          canonical_data
      in
      let prev =
        let+ id, branch, _ = prev in
        (id, branch)
      in
      let all_ids = all_ids |> Ext_list.to_list |> List.map fst in
      let matches = Ext_list.to_list matches in
      let errors = Ext_list.to_list errors in
      let ends = Ext_list.to_list ends in
      let++ next_kind =
        let++ cases = ends_to_cases ~is_unevaluated_funcall ~nest_kind ends in
        match cases with
        | [] -> Zero
        | [ (Case (Unknown, _), _) ] ->
            One (Option.get (List_utils.last all_ids), None)
        | _ -> Many cases
      in
      Finished
        {
          prev;
          id;
          all_ids;
          display;
          matches;
          errors;
          next_kind;
          callers;
          stack_direction;
          loc;
        }

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
        let ({ id; next_kind; _ } : exec_data) = exec_data in
        let { ends; unexplored_paths; _ } = partial in
        match (annot.cmd_kind, next_kind, is_end) with
        | Return, _, _ | _, Zero, _ -> Ok ()
        | _, One (), false ->
            Stack.push (id, None) unexplored_paths;
            Ok ()
        | _, Many cases, false ->
            cases
            |> List.iter (fun (gil_case, ()) ->
                   Stack.push (id, Some gil_case) unexplored_paths);
            Ok ()
        | _, One (), true ->
            Ext_list.add (branch_case, (id, None)) ends;
            Ok ()
        | _, Many cases, true ->
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
              loc = None;
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
            let loc =
              let+ loc =
                annot.origin_loc
                |> Option.map Debugger_utils.location_to_display_location
              in
              (loc.loc_source, loc.loc_start.pos_line)
            in
            partial.canonical_data <-
              Some { id; display; callers; stack_direction; nest_kind; loc };
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
        let { id; cmd_report; matches; errors; _ } = exec_data in
        let annot = CmdReport.(cmd_report.annot) in
        let** is_end = get_is_end annot in
        let** branch_kind, branch_case =
          insert_id_and_case ~prev_id ~exec_data ~id partial
        in
        let () = Ext_list.add_all matches partial.matches in
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
        | Some (id, branch_case) -> StepAgain (id, branch_case) |> Result.ok
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
    map : map;
    func_return_map : (id, string * int ref) Hashtbl.t;
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
      let result =
        map_node_extra state.map caller_id (fun node ->
            let new_next =
              match node.next with
              | Some (Branch nexts) ->
                  let nexts = List.remove_assoc Func_exit_placeholder nexts in
                  let case = Case (Func_exit label, ix) in
                  let bdata = (cont_id, None) in
                  let nexts = nexts @ [ (case, (None, bdata)) ] in
                  Ok (Some (Branch nexts))
              | None | Some (Single _) ->
                  Fmt.error "update_caller_branches - caller %a does not branch"
                    pp_id caller_id
            in
            match new_next with
            | Ok next -> ({ node with next }, Ok ())
            | Error e -> (node, Error e))
      in
      match result with
      | Some r -> r
      | None ->
          Fmt.error "update_caller_branches - caller %a not found" pp_id
            caller_id

    let resolve_func_branches ~state finished_partial =
      let Partial_cmds.{ all_ids; next_kind; callers; _ } = finished_partial in
      match (next_kind, callers) with
      | Zero, caller_id :: _ ->
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
              prev;
              next_kind;
              callers;
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
          submap = NoSubmap;
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
          | Ok next -> ({ prev with next }, Ok ())
          | Error e ->
              (prev, Fmt.error "insert_as_next (%a) - %s" pp_id prev_id e))

    let insert_as_submap ~state ~parent_id new_id =
      map_node_extra_exn state.map parent_id (fun parent ->
          match parent.data.submap with
          | Proc _ | Submap _ -> (parent, Error "duplicate submaps!")
          | NoSubmap ->
              let data = { parent.data with submap = Submap new_id } in
              ({ parent with data }, Ok ()))

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
      | Some In, Some (parent_id, Some Func_exit_placeholder)
      | Some In, Some (parent_id, None) ->
          let new_cmd = new_cmd |> with_prev None in
          let++ () = insert_as_submap ~state ~parent_id new_cmd.data.id in
          new_cmd
      | Some In, Some (_, Some _) -> Error "stepping in with branch case!"
      | None, Some (prev_id, case) ->
          let++ () = insert_as_next ~state ~prev_id ?case new_cmd.data.id in
          new_cmd
      | Some (Out prev_id), Some (inner_prev_id, _) ->
          let** case =
            let func_return_label =
              (get_exn state.map inner_prev_id).data.func_return_label
            in
            match func_return_label with
            | Some (label, ix) -> Ok (Case (Func_exit label, ix))
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
        Ok new_cmd
      in
      Result_utils.or_else (failwith ~state ~finished_partial) r
  end

  let insert_new_cmd = Insert_new_cmd.f

  module Init_or_handle = struct
    let get_prev ~state ~gil_case ~prev_id () =
      let { map; _ } = state in
      let=* prev_id = Ok prev_id in
      let=* prev =
        match get map prev_id with
        | None -> (
            match map.root with
            | None ->
                (* It's okay to not have a prev if we're still in the harness *)
                Ok None
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
      | Harness -> Either.Left (exec_data.id, None)
      | Normal _ | Internal | Return | Hidden -> (
          let get_prev = get_prev ~state ~gil_case ~prev_id in
          let partial_result =
            Partial_cmds.handle ~prog ~get_prev ~partials ~prev_id exec_data
          in
          match partial_result with
          | Finished finished ->
              let cmd = insert_new_cmd ~state finished in
              Either.Right cmd
          | StepAgain (id, case) -> Either.Left (id, case))
  end

  let init_or_handle = Init_or_handle.f

  let handle_cmd prev_id gil_case exec_data state =
    init_or_handle ~state ~prev_id ?gil_case exec_data

  let dump = to_yojson
  let get_gil_map _ = failwith "get_gil_map: not implemented!"

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

  let package = Packaged.package Fun.id package_node
  let get_lifted_map_exn { map; _ } = package map
  let get_lifted_map state = Some (get_lifted_map_exn state)
  let get_matches_at_id id { map; _ } = (get_exn map id).data.matches
  let path_of_id id { gil_state; _ } = Gil_lifter.path_of_id id gil_state

  let previous_step id { map; _ } =
    let+ id, case = (get_exn map id).data.prev in
    let case = case |> Option.map package_case in
    (id, case)

  let memory_error_to_exception_info
      (_ : (memory_error, annot, tl_ast) memory_error_info) : exception_info =
    { id = "unknown"; description = Some "Error lifting not supported yet!" }

  let add_variables = Memory_model.MonadicSMemory.Lift.add_variables

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
    let case = Case (Func_exit label, ix) in
    let* _ =
      match (get_exn state.map caller_id).next with
      | Some (Branch nexts) -> List.assoc_opt case nexts
      | _ -> None
    in
    Some (caller_id, Some case)

  let rec find_next state id case =
    let node = get_exn state.map id in
    match (node.next, case) with
    | (None | Some (Single _)), Some _ ->
        failwith "HORROR - tried to step case for non-branch cmd"
    | Some (Single (None, _)), None ->
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
    | Some (Single (Some next, _)), None -> Either.Left next
    | Some (Branch nexts), None -> select_case nexts
    | Some (Branch nexts), Some case -> (
        match List.assoc_opt case nexts with
        | None -> failwith "case not found"
        | Some (None, id_case) -> Either.Right id_case
        | Some (Some next, _) -> Either.Left next)
    | None, None -> (
        match get_next_from_end state node.data with
        | Some (id, case) -> find_next state id case
        | None -> Either.left id)

  let request_next state id case =
    let rec aux id case =
      let path = path_of_id id state in
      let exec_data = Effect.perform (Step (Some id, case, path)) in
      match init_or_handle ~state ~prev_id:id ?gil_case:case exec_data with
      | Either.Left (id, case) -> aux id case
      | Either.Right map -> map.data.id
    in
    aux id case

  let step state id case =
    let () =
      DL.log (fun m ->
          m "Stepping %a %a" pp_id id (pp_option Branch_case.pp) case)
    in
    match find_next state id case with
    | Either.Left next -> next
    | Either.Right (id, case) -> request_next state id case

  let is_breakpoint ~start ~current =
    let b =
      let+ file, line = current.data.loc in
      let- () =
        let* file', line' = start.data.loc in
        if file = file' && line = line' then Some false else None
      in
      Effect.perform (IsBreakpoint (file, [ line ]))
    in
    Option.value b ~default:false

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
                  let next = get_next_from_end state node.data |> Option.get in
                  (ends, next :: rest)
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
    let next_id = step state id case in
    (next_id, Debugger_utils.Step)

  let step_over state id =
    let node = get_exn state.map id in
    let () =
      let () =
        match node.next with
        | Some (Branch nexts) ->
            if List.mem_assoc Func_exit_placeholder nexts then
              step state id (Some Func_exit_placeholder) |> ignore
        | _ -> ()
      in
      let> submap_id =
        match node.data.submap with
        | NoSubmap | Proc _ -> None
        | Submap m -> Some m
      in
      let _ = step_all ~start:node state submap_id None in
      ()
    in
    let stop_id = step state id None in
    (stop_id, Debugger_utils.Step)

  let step_in state id =
    let cmd = get_exn state.map id in
    (* Only BranchCmds should have submaps *)
    let- () =
      match cmd.data.submap with
      | NoSubmap | Proc _ -> None
      | Submap submap_id -> Some (submap_id, Debugger_utils.Step)
    in
    step_branch state id None

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
          let ends' = step_all ~start state id case in
          let ends', nexts =
            ends'
            |> List.partition_map (fun { data; _ } ->
                   match get_next_from_end state data with
                   | Some (id, case) -> Either.Right (id, case)
                   | None -> Either.Left data.id)
          in
          aux (ends @ ends') (nexts @ rest)
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
        prog;
        partial_cmds;
        map = Exec_map.make ();
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
end
