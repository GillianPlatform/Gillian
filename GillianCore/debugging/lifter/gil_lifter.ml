module L = Logging
module DL = Debugger_log
open Lifter

module type S = sig
  include Lifter.S

  val pop_to_exec : t -> (Logging.Report_id.t * BranchCase.t option) option
end

module Make
    (PC : ParserAndCompiler.S)
    (Verifier : Verifier.S with type annot = PC.Annot.t)
    (SMemory : SMemory.S) :
  S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = Verifier.SAInterpreter.Logging.ConfigReport.t
     and type annot = PC.Annot.t = struct
  open Exec_map
  module Annot = PC.Annot

  type annot = PC.Annot.t
  type branch_case = BranchCase.t [@@deriving yojson]
  type branch_path = BranchCase.path [@@deriving yojson]

  (* Some fields are Null'd in yojson to stop huge memory inefficiency *)
  type map = (branch_case, cmd_data, unit) Exec_map.t

  and cmd_data = {
    id : L.Report_id.t;
    display : string;
    unifys : unification list;
    errors : string list;
    submap : map submap;
    branch_path : branch_path;
    parent : (map * branch_case option) option; [@to_yojson fun _ -> `Null]
  }
  [@@deriving to_yojson]

  type t = {
    map : map;
    root_proc : string;
    all_procs : string list;
    id_map : (L.Report_id.t, map) Hashtbl.t; [@to_yojson fun _ -> `Null]
    mutable to_exec : (L.Report_id.t * BranchCase.t option) list;
  }
  [@@deriving to_yojson]

  type memory = SMemory.t
  type tl_ast = PC.tl_ast
  type memory_error = SMemory.err_t

  type cmd_report = Verifier.SAInterpreter.Logging.ConfigReport.t
  [@@deriving yojson]

  type exec_data = cmd_report executed_cmd_data [@@deriving yojson]

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
      { kind; id; unifys; errors; cmd_report : cmd_report; branch_path }
      () =
    let display = Fmt.to_to_string Cmd.pp_indexed cmd_report.cmd in
    let data = { id; display; unifys; errors; submap; branch_path; parent } in
    let cmd, to_exec =
      match kind with
      | Normal -> (Cmd { data; next = Nothing }, [ (id, None) ])
      | Branch cases ->
          let nexts = Hashtbl.create (List.length cases) in
          let to_exec =
            cases
            |> List.map (fun (case, _) ->
                   Hashtbl.add nexts case ((), Nothing);
                   (id, Some case))
          in
          (BranchCmd { data; nexts }, to_exec)
      | Final -> (FinalCmd { data }, [])
    in
    Hashtbl.replace id_map id cmd;
    (cmd, to_exec)

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

  let at_id_opt id state =
    match at_id_result id state with
    | Ok (map, branch_path) -> Some (map, branch_path)
    | Error _ -> None

  let at_id id state =
    match at_id_result id state with
    | Ok (map, branch_path) -> (map, branch_path)
    | Error s ->
        DL.failwith
          (fun () ->
            [ ("id", L.Report_id.to_yojson id); ("state", dump state) ])
          ("at_id: " ^ s)

  let init_exn ~proc_name ~all_procs _ _ exec_data =
    let id_map = Hashtbl.create 1 in
    let map, _ = new_cmd id_map exec_data ~parent:None () in
    ({ map; root_proc = proc_name; all_procs; id_map; to_exec = [] }, Stop)

  let init ~proc_name ~all_procs tl_ast prog exec_data =
    Some (init_exn ~proc_name ~all_procs tl_ast prog exec_data)

  let should_skip_cmd (exec_data : cmd_report executed_cmd_data) state : bool =
    let annot = exec_data.cmd_report.annot in
    if Annot.is_hidden annot then (
      DL.log (fun m -> m "Gil_lifter: skipping hidden cmd");
      true)
    else
      let proc_name = get_proc_name exec_data in
      if not (List.mem proc_name state.all_procs) then (
        DL.log (fun m -> m "Gil_lifter: skipping cmd from internal proc");
        true)
      else false

  let handle_cmd prev_id branch_case exec_data state =
    let { id_map; _ } = state in
    let new_cmd = new_cmd id_map exec_data in
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
    let map =
      match Hashtbl.find_opt id_map prev_id with
      | Some map -> map
      | None ->
          failwith (Fmt.str "couldn't find prev_id %a!" L.Report_id.pp prev_id)
    in
    let to_exec =
      match map with
      | Cmd cmd when cmd.next = Nothing ->
          let parent = Some (map, None) in
          let new_cmd, to_exec = new_cmd ~parent () in
          cmd.next <- new_cmd;
          to_exec
      | Cmd _ -> failwith "cmd.next not Nothing!"
      | BranchCmd { nexts; _ } -> (
          match branch_case with
          | None ->
              failwith "HORROR - need branch case to insert to branch cmd!"
          | Some case -> (
              match Hashtbl.find_opt nexts case with
              | Some ((), Nothing) ->
                  let parent = Some (map, Some case) in
                  let new_cmd, to_exec = new_cmd ~parent () in
                  Hashtbl.replace nexts case ((), new_cmd);
                  to_exec
              | Some ((), _) -> failwith "colliding cases in branch cmd"
              | None -> failwith "inserted branch case not found on parent!"))
      | _ -> failwith "can't insert to Nothing or FinalCmd"
    in
    if should_skip_cmd exec_data state then
      state.to_exec <- to_exec @ state.to_exec;
    match state.to_exec with
    | (id, case) :: rest_to_exec ->
        state.to_exec <- rest_to_exec;
        ExecNext (Some id, case)
    | _ -> Stop

  let package_case _ case _ = Packaged.package_gil_case case

  let package_data package { id; display; unifys; errors; submap; _ } =
    let submap =
      match submap with
      | NoSubmap -> NoSubmap
      | Proc p -> Proc p
      | Submap map -> Submap (package map)
    in
    Packaged.{ ids = [ id ]; display; unifys; errors; submap }

  let package = Packaged.package package_data package_case
  let get_gil_map state = package state.map
  let get_lifted_map _ = None

  let get_lifted_map_exn _ =
    failwith "get_lifted_map not implemented for GIL lifter"

  let get_unifys_at_id id state =
    match state |> at_id id |> fst with
    | Nothing ->
        DL.failwith
          (fun () ->
            [ ("id", L.Report_id.to_yojson id); ("state", dump state) ])
          "get_unifys_at_id: HORROR - map is Nothing!"
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> data.unifys

  let get_root_id { map; _ } =
    match map with
    | Nothing -> None
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        Some data.id

  let path_of_id id state = state |> at_id id |> snd

  let existing_next_steps id state =
    match state |> at_id_opt id with
    | None -> []
    | Some (map, _) -> (
        match map with
        | Nothing -> failwith "existing_next_steps: map is Nothing!"
        | Cmd { next = Nothing; _ } | FinalCmd _ -> []
        | Cmd { next; _ } ->
            let id = get_id next in
            [ (id, None) ]
        | BranchCmd { nexts; _ } ->
            let nexts =
              Hashtbl.fold
                (fun case (_, next) acc ->
                  match get_id_opt next with
                  | None -> acc
                  | Some id -> (id, Some case) :: acc)
                nexts []
            in
            List.rev nexts)

  let next_gil_step id case _ =
    let case =
      case
      |> Option.map (fun (case : Exec_map.Packaged.branch_case) ->
             case.json |> BranchCase.of_yojson |> Result.get_ok)
    in
    (id, case)

  let previous_step id state =
    match state |> at_id id |> fst with
    | Nothing -> failwith "HORROR - map is Nothing!"
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> (
        match data.parent with
        | None -> None
        | Some (Nothing, _) -> failwith "HORROR - parent is Nothing!"
        | Some
            ((Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data }), case)
          ->
            let case = case |> Option.map Exec_map.Packaged.package_gil_case in
            Some (data.id, case))

  let select_next_path case id state =
    let map, path = state |> at_id id in
    let failwith s =
      DL.failwith
        (fun () ->
          [
            ("id", L.Report_id.to_yojson id);
            ("state", dump state);
            ("case", opt_to_yojson branch_case_to_yojson case);
          ])
        ("select_next_path: " ^ s)
    in
    match (map, case) with
    | (Nothing | FinalCmd _), _ -> failwith "no next path"
    | Cmd _, Some _ -> failwith "tried to select case for non-branch cmd"
    | Cmd _, None -> path
    | BranchCmd { nexts; _ }, None ->
        Hashtbl.find_map (fun case _ -> Some (case :: path)) nexts |> Option.get
    | BranchCmd { nexts; _ }, Some case -> (
        match Hashtbl.find_opt nexts case with
        | None -> failwith "case not found"
        | Some _ -> case :: path)

  let find_unfinished_path ?at_id state =
    let rec aux = function
      | Nothing ->
          DL.failwith
            (fun () ->
              [
                ("state", dump state);
                ("at_id", opt_to_yojson L.Report_id.to_yojson at_id);
              ])
            "find_unfinished_path: started at Nothing"
      | Cmd { data = { id; _ }; next = Nothing } -> Some (id, None)
      | Cmd { next; _ } -> aux next
      | BranchCmd { nexts; data = { id; _ } } -> (
          match
            Hashtbl.find_map
              (fun case (_, next) ->
                if next = Nothing then Some (id, Some case) else None)
              nexts
          with
          | None -> Hashtbl.find_map (fun _ (_, next) -> aux next) nexts
          | result -> result)
      | FinalCmd _ -> None
    in
    let map =
      match at_id with
      | None -> state.map
      | Some id -> Hashtbl.find state.id_map id
    in
    aux map

  let memory_error_to_exception_info { error; _ } : exception_info =
    { id = Fmt.to_to_string SMemory.pp_err error; description = None }

  let add_variables ~store ~memory ~is_gil_file ~get_new_scope_id variables :
      Variable.scope list =
    let () = ignore is_gil_file in
    let store_id = get_new_scope_id () in
    let memory_id = get_new_scope_id () in
    let scopes : Variable.scope list =
      [ { id = store_id; name = "Store" }; { id = memory_id; name = "Memory" } ]
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

  let pop_to_exec state =
    match state.to_exec with
    | [] -> None
    | (id, case) :: to_exec ->
        state.to_exec <- to_exec;
        Some (id, case)
end
