module L = Logging
module DL = Debugger_log
open Lifter
include Gil_lifter_intf

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

    (* Some fields are Null'd in yojson to stop huge memory inefficiency *)
    type map = (branch_case, cmd_data, unit) Exec_map.t

    and cmd_data = {
      id : L.Report_id.t;
      display : string;
      matches : matching list;
      errors : string list;
      submap : map submap;
      branch_path : branch_path;
      parent : (map * branch_case option) option; [@to_yojson fun _ -> `Null]
    }
    [@@deriving to_yojson]

    type t = {
      mutable map : map;
      root_proc : string;
      all_procs : string list;
      id_map : (L.Report_id.t, map) Hashtbl.t; [@to_yojson fun _ -> `Null]
    }
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
        id_map
        { kind; id; matches; errors; cmd_report : cmd_report; branch_path; _ }
        () =
      let display = Fmt.to_to_string Cmd.pp_indexed cmd_report.cmd in
      let data =
        { id; display; matches; errors; submap; branch_path; parent }
      in
      let cmd =
        match kind with
        | Normal -> Cmd { data; next = Nothing }
        | Branch cases ->
            let nexts = Hashtbl.create (List.length cases) in
            let () =
              List.iter
                (fun (case, _) -> Hashtbl.add nexts case ((), Nothing))
                cases
            in
            BranchCmd { data; nexts }
        | Final -> FinalCmd { data }
      in
      Hashtbl.replace id_map id cmd;
      cmd

    let get_id_opt = function
      | Nothing -> None
      | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data; _ } ->
          Some data.id

    let get_id map =
      match get_id_opt map with
      | None -> failwith "Tried to get id of Nothing!"
      | Some id -> id

    let at_id_result id state =
      match Hashtbl.find_opt state.id_map id with
      | None -> Error "id not found"
      | Some Nothing -> Error "HORROR - got Nothing at id!"
      | Some
          ((Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data }) as map)
        -> Ok (map, data.branch_path)

    let at_id_exn id state =
      match at_id_result id state with
      | Ok (map, branch_path) -> (map, branch_path)
      | Error s ->
          DL.failwith
            (fun () ->
              [ ("id", L.Report_id.to_yojson id); ("state", dump state) ])
            ("at_id: " ^ s)

    let should_skip_cmd (exec_data : cmd_report executed_cmd_data) state : bool
        =
      let proc_name = get_proc_name exec_data in
      not (List.mem proc_name state.all_procs)

    let consume_cmd prev_id branch_case exec_data state =
      let { id_map; _ } = state in
      let new_cmd = new_cmd id_map exec_data in
      let branch_case =
        Option_utils.coalesce branch_case exec_data.cmd_report.branch_case
      in
      let failwith s =
        DL.failwith
          (fun () ->
            [
              ("state", dump state);
              ("exec_data", exec_data_to_yojson exec_data);
              ("prev_id", L.Report_id.to_yojson prev_id);
              ("branch_case", opt_to_yojson branch_case_to_yojson branch_case);
            ])
          ("Gil_lifter.handle_cmd: " ^ s)
      in
      let branch_case =
        Option_utils.coalesce branch_case exec_data.cmd_report.branch_case
      in
      let map =
        match Hashtbl.find_opt id_map prev_id with
        | Some map -> map
        | None ->
            failwith
              (Fmt.str "couldn't find prev_id %a!" L.Report_id.pp prev_id)
      in
      let new_cmd =
        match map with
        | Cmd cmd when cmd.next = Nothing ->
            let parent = Some (map, None) in
            let new_cmd = new_cmd ~parent () in
            let () = cmd.next <- new_cmd in
            new_cmd
        | Cmd _ -> failwith "cmd.next not Nothing!"
        | BranchCmd { nexts; _ } -> (
            match branch_case with
            | None ->
                failwith "HORROR - need branch case to insert to branch cmd!"
            | Some case -> (
                match Hashtbl.find_opt nexts case with
                | Some ((), Nothing) ->
                    let parent = Some (map, Some case) in
                    let new_cmd = new_cmd ~parent () in
                    let () = Hashtbl.replace nexts case ((), new_cmd) in
                    new_cmd
                | Some ((), _) -> failwith "colliding cases in branch cmd"
                | None -> failwith "inserted branch case not found on parent!"))
        | _ -> failwith "can't insert to Nothing or FinalCmd"
      in
      new_cmd

    let handle_cmd prev_id branch_case exec_data state =
      let _ = consume_cmd prev_id branch_case exec_data state in
      ()

    let package_case ~bd:_ ~all_cases:_ case = Packaged.package_gil_case case

    let package_data package { id; display; matches; errors; submap; _ } =
      let submap =
        match submap with
        | NoSubmap -> NoSubmap
        | Proc p -> Proc p
        | Submap map -> Submap (package map)
      in
      Packaged.{ id; all_ids = [ id ]; display; matches; errors; submap }

    let package = Packaged.package package_data package_case
    let get_gil_map state = package state.map
    let get_lifted_map _ = None

    let get_lifted_map_exn _ =
      failwith "get_lifted_map not implemented for GIL lifter"

    let get_matches_at_id id state =
      match state |> at_id_exn id |> fst with
      | Nothing ->
          DL.failwith
            (fun () ->
              [ ("id", L.Report_id.to_yojson id); ("state", dump state) ])
            "get_matches_at_id: HORROR - map is Nothing!"
      | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
          data.matches

    let get_root_id { map; _ } =
      match map with
      | Nothing -> None
      | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
          Some data.id

    let path_of_id id state = state |> at_id_exn id |> snd

    let cases_at_id id state =
      match state |> at_id_exn id |> fst with
      | Nothing -> failwith "cases_at_id: map is Nothing!"
      | FinalCmd _ -> failwith " cases_at_id: map is Final!"
      | Cmd _ -> []
      | BranchCmd { nexts; _ } -> nexts |> Hashtbl.to_seq_keys |> List.of_seq

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
        Hashtbl.fold
          (fun case (_, map) acc ->
            match (map, acc) with
            | _, (_, true) -> acc
            | Nothing, (_, false) -> (Some case, true)
            | _, (None, false) -> (Some case, false)
            | _ -> acc)
          nexts (None, false)
      in
      Option.get case

    let find_next state id case =
      let map, _ = state |> at_id_exn id in
      match (map, case) with
      | Nothing, _ -> failwith "HORROR - map is Nothing!"
      | FinalCmd _, _ -> failwith "HORROR - tried to step from FinalCmd!"
      | Cmd _, Some _ ->
          failwith "HORROR - tried to step case for non-branch cmd"
      | Cmd { next = Nothing; _ }, None -> Either.Right None
      | Cmd { next; _ }, None -> Either.Left next
      | BranchCmd { nexts; _ }, None ->
          let case = select_case nexts in
          Either.Right (Some case)
      | BranchCmd { nexts; _ }, Some case -> (
          match Hashtbl.find_opt nexts case with
          | None -> failwith "case not found"
          | Some (_, Nothing) -> Either.Right (Some case)
          | Some (_, next) -> Either.Left next)

    let request_next state id case =
      let path = path_of_id id state in
      let exec_data = Effect.perform (Step (Some id, case, path)) in
      consume_cmd id case exec_data state

    let step state id case =
      let next =
        match find_next state id case with
        | Either.Left next -> next
        | Either.Right case -> request_next state id case
      in
      match next with
      | Nothing -> failwith "HORROR - next is Nothing!"
      | Cmd { data = { id; _ }; _ } -> Either.Left [ (id, None) ]
      | BranchCmd { data = { id; _ }; nexts } ->
          nexts |> Hashtbl.to_seq
          |> Seq.map (fun (case, _) -> (id, Some case))
          |> List.of_seq |> Either.left
      | FinalCmd { data = { id; _ } } -> Either.Right id

    let step_branch state id case =
      let case =
        Option.map
          (fun (_, json) -> json |> Branch_case.of_yojson |> Result.get_ok)
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

    let step_back { id_map; _ } id =
      match Hashtbl.find id_map id with
      | Nothing -> failwith "HORROR - map is Nothing!"
      | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> (
          match data.parent with
          | None -> failwith "HORROR - parent is None!"
          | Some (parent, _) ->
              let id = get_id parent in
              (id, Debugger_utils.Step))

    let continue state id =
      let open Utils.Syntaxes.Option in
      let rec aux stack ends =
        match stack with
        | [] -> List.rev ends
        | (id, case) :: rest -> (
            match step state id case with
            | Either.Left nexts -> aux (nexts @ rest) ends
            | Either.Right end_id -> aux rest (end_id :: ends))
      in
      let end_ =
        let end_, stack =
          match at_id_exn id state |> fst with
          | Nothing -> failwith "HORROR - map is Nothing!"
          | FinalCmd _ -> (Some id, [])
          | Cmd _ -> (None, [ (id, None) ])
          | BranchCmd { nexts; _ } ->
              let stack =
                nexts |> Hashtbl.to_seq
                |> Seq.map (fun (case, _) -> (id, Some case))
                |> List.of_seq
              in
              (None, stack)
        in
        let- () = end_ in
        let ends = aux stack [] in
        List.hd ends
      in
      (end_, Debugger_utils.Step)

    let step_out = continue

    let continue_back t _ =
      match t.map with
      | Nothing -> failwith "HORROR - map is Nothing!"
      | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
          (data.id, Debugger_utils.Step)

    let init_manual proc_name all_procs =
      let id_map = Hashtbl.create 1 in
      let state = { map = Nothing; root_proc = proc_name; all_procs; id_map } in
      let finish_init exec_data () =
        let exec_data =
          Option_utils.or_else
            (fun () -> Effect.perform (Step (None, None, [])))
            exec_data
        in
        let () = state.map <- new_cmd id_map exec_data ~parent:None () in
        (exec_data.id, Debugger_utils.Step)
      in
      (state, finish_init)

    let init_exn ~proc_name ~all_procs _ _ =
      let state, finish = init_manual proc_name all_procs in
      (state, finish None)

    let init ~proc_name ~all_procs tl_ast prog =
      Some (init_exn ~proc_name ~all_procs tl_ast prog)
  end
