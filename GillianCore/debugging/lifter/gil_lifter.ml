module L = Logging
module DL = Debugger_log
open Lifter_intf
open Syntaxes.Option
include Gil_lifter_intf

type id = L.Report_id.t [@@deriving yojson]

module Make : Make =
functor
  (SMemory : SMemory.S)
  (PC : ParserAndCompiler.S)
  (Verifier : Verifier.S with type annot = PC.Annot.t)
  ->
  struct
    open Exec_map
    module Annot = PC.Annot

    type annot = PC.Annot.t
    type branch_case = Branch_case.t [@@deriving yojson]
    type branch_path = Branch_case.path [@@deriving yojson]

    type cmd_data = {
      id : id;
      display : string;
      matches : matching list;
      errors : string list;
      submap : id submap;
      branch_path : branch_path;
      parent : (id * branch_case option) option;
      loc : (string * int) option;
    }
    [@@deriving to_yojson]

    type map = (id, branch_case, cmd_data, unit) Exec_map.map
    [@@deriving to_yojson]

    type t = { mutable map : map; root_proc : string; all_procs : string list }
    [@@deriving to_yojson]

    type memory = SMemory.t
    type tl_ast = PC.tl_ast
    type memory_error = SMemory.err_t

    type cmd_report = Verifier.SAInterpreter.Logging.ConfigReport.t
    [@@deriving yojson]

    type exec_data = cmd_report executed_cmd_data [@@deriving yojson]

    type _ Effect.t +=
      | Step :
          (Logging.Report_id.t option * Branch_case.t option * Branch_case.path)
          -> cmd_report executed_cmd_data Effect.t

    let dump = to_yojson

    let get_proc_name ({ cmd_report; _ } : exec_data) =
      let open Verifier.SAInterpreter.Logging.ConfigReport in
      let cs = cmd_report.callstack in
      let head = List.hd cs in
      head.pid

    let new_cmd
        ?(submap = NoSubmap)
        ~parent
        map
        {
          next_kind;
          id;
          matches;
          errors;
          cmd_report : cmd_report;
          branch_path;
          _;
        }
        () =
      let display = Fmt.to_to_string Cmd.pp_indexed cmd_report.cmd in
      let loc =
        let annot = cmd_report.annot in
        let+ loc =
          Annot.get_origin_loc annot
          |> Option.map Debugger_utils.location_to_display_location
        in
        (loc.loc_source, loc.loc_start.pos_line)
      in
      let data =
        { id; display; matches; errors; submap; branch_path; parent; loc }
      in
      let next =
        match next_kind with
        | One () -> Some (Single (None, ()))
        | Many cases ->
            let nexts = List.map (fun (case, ()) -> (case, (None, ()))) cases in
            Some (Branch nexts)
        | Zero -> None
      in
      let node = { data; next } in
      Hashtbl.replace map.entries id (Node node);
      node

    let should_skip_cmd (exec_data : cmd_report executed_cmd_data) state : bool
        =
      let proc_name = get_proc_name exec_data in
      not (List.mem proc_name state.all_procs)

    let consume_cmd prev_id branch_case exec_data state =
      let { map; _ } = state in
      let new_cmd = new_cmd map exec_data in
      let branch_case =
        Option_utils.coalesce branch_case exec_data.cmd_report.branch_case
      in
      let failwith s =
        DL.failwith
          (fun () ->
            [
              ("state", dump state);
              ("exec_data", exec_data_to_yojson exec_data);
              ("prev_id", id_to_yojson prev_id);
              ("branch_case", opt_to_yojson branch_case_to_yojson branch_case);
            ])
          ("Gil_lifter.handle_cmd: " ^ s)
      in
      let prev =
        match get map prev_id with
        | Some prev -> prev
        | None ->
            failwith
              (Fmt.str "couldn't find prev_id %a!" L.Report_id.pp prev_id)
      in
      let new_cmd, new_next =
        match prev.next with
        | Some (Single (None, ())) ->
            let parent = Some (prev_id, None) in
            let new_cmd = new_cmd ~parent () in
            (new_cmd, Single (Some new_cmd.data.id, ()))
        | Some (Single _) -> failwith "cmd already has next!"
        | Some (Branch nexts) -> (
            let case =
              match branch_case with
              | None ->
                  failwith "HORROR - need branch case to insert to branch cmd!"
              | Some case -> case
            in
            match List.assoc_opt case nexts with
            | Some (None, ()) ->
                let parent = Some (prev_id, Some case) in
                let new_cmd = new_cmd ~parent () in
                let next = (Some new_cmd.data.id, ()) in
                let nexts = List_utils.assoc_replace case next nexts in
                (new_cmd, Branch nexts)
            | Some _ -> failwith "colliding cases in branch cmd"
            | None -> failwith "inserted branch case not found on parent!")
        | None -> failwith "can't insert to final cmd"
      in
      let prev' = { prev with next = Some new_next } in
      let () = Hashtbl.replace map.entries prev.data.id (Node prev') in
      new_cmd

    let handle_cmd prev_id branch_case exec_data state =
      let _ = consume_cmd prev_id branch_case exec_data state in
      ()

    let package_node { data : cmd_data; next } =
      let data =
        Packaged.
          {
            id = data.id;
            all_ids = [ data.id ];
            display = data.display;
            matches = data.matches;
            errors = data.errors;
            submap = data.submap;
          }
      in
      let next =
        match next with
        | Some (Single (next, ())) -> Some (Single (next, ""))
        | Some (Branch nexts) ->
            let nexts =
              List.map
                (fun (case, (next, ())) ->
                  let case, bdata = Packaged.package_gil_case case in
                  (case, (next, bdata)))
                nexts
            in
            Some (Branch nexts)
        | None -> None
      in
      { data; next }

    let package = Packaged.package Fun.id package_node
    let get_gil_map state = package state.map
    let get_lifted_map _ = None

    let get_lifted_map_exn _ =
      failwith "get_lifted_map not implemented for GIL lifter"

    let get_matches_at_id id state = (get_exn state.map id).data.matches
    let path_of_id id state = (get_exn state.map id).data.branch_path

    let cases_at_id id state =
      let next =
        match get state.map id with
        | None -> failwith "cases_at_id: couldn't find id!"
        | Some node -> node.next
      in
      match next with
      | None -> failwith " cases_at_id: map is Final!"
      | Some (Single _) -> []
      | Some (Branch nexts) -> nexts |> List.map fst

    let memory_error_to_exception_info { error; _ } : exception_info =
      { id = Fmt.to_to_string SMemory.pp_err error; description = None }

    let add_variables ~store ~memory ~is_gil_file ~get_new_scope_id variables :
        Variable.scope list =
      let () = ignore is_gil_file in
      let store_id = get_new_scope_id () in
      let memory_id = get_new_scope_id () in
      let scopes : Variable.scope list =
        [
          { id = store_id; name = "Store" }; { id = memory_id; name = "Memory" };
        ]
      in
      let store_vars =
        store
        |> List.map (fun (var, value) : Variable.t ->
               let value = Fmt.to_to_string (Fmt.hbox Expr.pp) value in
               Variable.create_leaf var value ())
        |> List.sort (fun (v : Variable.t) w -> Stdlib.compare v.name w.name)
      in
      let memory_vars =
        [
          Variable.create_leaf ""
            (Fmt.to_to_string (Fmt.hbox SMemory.pp) memory)
            ();
        ]
      in
      let () = Hashtbl.replace variables store_id store_vars in
      let () = Hashtbl.replace variables memory_id memory_vars in
      scopes

    let select_case nexts =
      let case, _ =
        List.fold_left
          (fun acc (case, (next, _)) ->
            match (next, acc) with
            | _, (_, true) -> acc
            | None, (_, false) -> (Some case, true)
            | _, (None, false) -> (Some case, false)
            | _ -> acc)
          (None, false) nexts
      in
      Option.get case

    let find_next state id case =
      let node = get_exn state.map id in
      match (node.next, case) with
      | (None | Some (Single _)), Some _ ->
          failwith "HORROR - tried to step case for non-branch cmd"
      | Some (Single (None, _)), None -> Either.Right None
      | Some (Single (Some next, _)), None ->
          Either.Left (get_exn state.map next)
      | Some (Branch nexts), None ->
          let case = select_case nexts in
          Either.Right (Some case)
      | Some (Branch nexts), Some case -> (
          match List.assoc_opt case nexts with
          | None -> failwith "case not found"
          | Some (None, _) -> Either.Right (Some case)
          | Some (Some next, _) -> Either.Left (get_exn state.map next))
      | None, None -> Either.Left node

    let request_next state id case =
      let path = path_of_id id state in
      let exec_data = Effect.perform (Step (Some id, case, path)) in
      consume_cmd id case exec_data state

    let step state id case =
      let next_node =
        match find_next state id case with
        | Either.Left next -> next
        | Either.Right case -> request_next state id case
      in
      let id = next_node.data.id in
      match next_node.next with
      | Some (Single _) -> Either.Left [ (id, None) ]
      | Some (Branch nexts) ->
          nexts |> List.map (fun (case, _) -> (id, Some case)) |> Either.left
      | None -> Either.Right id

    let step_branch state id case =
      let case =
        Option.map
          (fun json -> json |> Branch_case.of_yojson |> Result.get_ok)
          case
      in
      let id =
        match step state id case with
        | Either.Left nexts -> nexts |> List.hd |> fst
        | Either.Right end_id -> end_id
      in
      (id, Debugger_utils.Step)

    let step_over state id = step_branch state id None
    let step_in = step_over

    let step_back { map; _ } id =
      let stop_id =
        match (get_exn map id).data.parent with
        | None -> id
        | Some (parent_id, _) -> parent_id
      in
      (stop_id, Debugger_utils.Step)

    let is_breakpoint node =
      match node.data.loc with
      | None -> false
      | Some (file, line) -> Effect.perform (IsBreakpoint (file, [ line ]))

    let continue state id =
      let open Utils.Syntaxes.Option in
      let rec aux ?(first = false) stack ends =
        match stack with
        | [] -> List.rev ends
        | (id, case) :: rest -> (
            let node = get_exn state.map id in
            if (not first) && is_breakpoint node then aux rest (id :: ends)
            else
              match step state id case with
              | Either.Left nexts -> aux (nexts @ rest) ends
              | Either.Right end_id -> aux rest (end_id :: ends))
      in
      let end_ =
        let end_, stack =
          match (get_exn state.map id).next with
          | None -> (Some id, [])
          | Some (Single _) -> (None, [ (id, None) ])
          | Some (Branch nexts) ->
              let stack =
                nexts |> List.map (fun (case, _) -> (id, Some case))
              in
              (None, stack)
        in
        let- () = end_ in
        let ends = aux ~first:true stack [] in
        List.hd ends
      in
      (end_, Debugger_utils.Step)

    let step_out = continue

    (* TODO: breakpoints *)
    let continue_back t id =
      let rec aux id = function
        | None -> (id, Debugger_utils.Step)
        | Some (id, _) ->
            let node = get_exn t.map id in
            if is_breakpoint node then (id, Breakpoint)
            else aux id node.data.parent
      in
      let node = get_exn t.map id in
      aux id node.data.parent
    (* (Option.get t.map.root, Debugger_utils.Step) *)

    let init_manual proc_name all_procs =
      let map = Exec_map.make () in
      let state = { map; root_proc = proc_name; all_procs } in
      let finish_init exec_data () =
        let exec_data =
          Option_utils.or_else
            (fun () -> Effect.perform (Step (None, None, [])))
            exec_data
        in
        let cmd = new_cmd map exec_data ~parent:None () in
        let id = cmd.data.id in
        let () = map.root <- Some id in
        (id, Debugger_utils.Step)
      in
      (state, finish_init)

    let init_exn ~proc_name ~all_procs _ _ =
      let state, finish = init_manual proc_name all_procs in
      (state, finish None)

    let init ~proc_name ~all_procs tl_ast prog =
      Some (init_exn ~proc_name ~all_procs tl_ast prog)
  end
