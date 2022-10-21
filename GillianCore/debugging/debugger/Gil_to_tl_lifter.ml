module L = Logging
module DL = Debugger_log

type rid = L.ReportId.t [@@deriving yojson]
type unify_result = Success | Failure [@@deriving yojson]

type ('err, 'ast) memory_error_info = {
  error : 'err;  (** The memory error that needs to be lifted *)
  command : (int Cmd.t * Annot.t) option;  (** The command where it happened *)
  tl_ast : 'ast option;
}

type 'cmd_report executed_cmd_data = {
  kind : BranchCase.t ExecMap.cmd_kind;
  id : rid;
  cmd : 'cmd_report;
  unifys : ExecMap.unifys; [@default []]
  errors : string list; [@default []]
  branch_path : BranchCase.path;
}
[@@deriving yojson]

let make_executed_cmd_data kind id cmd ?(unifys = []) ?(errors = []) branch_path
    =
  { kind; id; cmd; unifys; errors; branch_path }

type handle_cmd_result = Stop | ExecNext of (rid option * BranchCase.t option)
[@@deriving yojson]

module type S = sig
  type t
  type memory_error
  type tl_ast
  type memory
  type cmd_report

  val init : cmd_report executed_cmd_data -> t * handle_cmd_result
  val dump : t -> Yojson.Safe.t
  val handle_cmd : cmd_report executed_cmd_data -> t -> handle_cmd_result
  val get_gil_map : t -> ExecMap.Packaged.t
  val get_lifted_map : t -> ExecMap.Packaged.t option
  val get_unifys_at_id : Logging.ReportId.t -> t -> ExecMap.unifys
  val get_root_id : t -> Logging.ReportId.t option
  val path_of_id : Logging.ReportId.t -> t -> BranchCase.path
  val next_steps : rid -> t -> (rid * BranchCase.t option) list
  val previous_step : rid -> t -> (rid * BranchCase.t option) option

  val select_next_path :
    BranchCase.t option -> Logging.ReportId.t -> t -> BranchCase.path

  val find_unfinished_path :
    ?at_path:BranchCase.path ->
    t ->
    (Logging.ReportId.t * BranchCase.t option) option

  (** Take the origin [tl_ast], an origin [node_id] and returns
      a string representing the evaluation step for the exec map.
      Should never be called if [source_map_ability] is false *)
  val get_origin_node_str : tl_ast -> int option -> string

  val memory_error_to_exception_info :
    (memory_error, tl_ast) memory_error_info -> DebuggerTypes.exception_info

  val add_variables :
    store:(string * Expr.t) list ->
    memory:memory ->
    is_gil_file:bool ->
    get_new_scope_id:(unit -> int) ->
    DebuggerTypes.variables ->
    DebuggerTypes.scope list
end

module BaseGilLifter
    (Verification : Verifier.S)
    (SMemory : SMemory.S)
    (PC : ParserAndCompiler.S) :
  S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = Verification.SAInterpreter.Logging.ConfigReport.t =
struct
  open ExecMap

  type branch_case = BranchCase.t [@@deriving yojson]
  type branch_path = BranchCase.path [@@deriving yojson]

  (* Some fields are Null'd in yojson to stop huge memory inefficiency *)
  type map = (branch_case, cmd_data) ExecMap.t

  and cmd_data = {
    id : rid;
    display : string;
    unifys : unifys;
    errors : string list;
    submap : map submap;
    branch_path : branch_path;
    parent : (map * branch_case option) option; [@to_yojson fun _ -> `Null]
  }
  [@@deriving to_yojson]

  type t = {
    map : map;
    root_proc : string;
    mutable prev_id : rid option;
    id_map : (rid, map) Hashtbl.t; [@to_yojson fun _ -> `Null]
  }
  [@@deriving to_yojson]

  type memory = SMemory.t
  type tl_ast = PC.tl_ast
  type memory_error = SMemory.err_t

  type cmd_report = Verification.SAInterpreter.Logging.ConfigReport.t
  [@@deriving yojson]

  type exec_data = cmd_report executed_cmd_data [@@deriving yojson]

  let dump = to_yojson

  let get_proc_name ({ cmd; _ } : exec_data) =
    let open Verification.SAInterpreter.Logging.ConfigReport in
    let cs = cmd.callstack in
    let head = List.hd cs in
    head.pid

  let new_cmd
      ?(submap = NoSubmap)
      ~parent
      id_map
      { kind; id; unifys; errors; cmd : cmd_report; branch_path }
      () =
    let display = cmd.cmd in
    let data = { id; display; unifys; errors; submap; branch_path; parent } in
    let cmd =
      match kind with
      | Normal -> Cmd { data; next = Nothing }
      | Branch cases ->
          let nexts = Hashtbl.create (List.length cases) in
          cases |> List.iter (fun case -> Hashtbl.add nexts case Nothing);
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

  let at_id id state =
    let failwith s =
      DL.failwith
        (fun () -> [ ("id", rid_to_yojson id); ("state", dump state) ])
        ("at_id: " ^ s)
    in
    match Hashtbl.find_opt state.id_map id with
    | None -> failwith "id not found"
    | Some Nothing -> failwith "HORROR - got Nothing at id!"
    | Some
        ((Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data }) as map)
      -> (map, data.branch_path)

  let init exec_data =
    let id_map = Hashtbl.create 1 in
    let map = new_cmd id_map exec_data ~parent:None () in
    let root_proc = get_proc_name exec_data in
    ({ map; root_proc; id_map; prev_id = None }, Stop)

  let handle_cmd exec_data state =
    let { map; root_proc; prev_id; _ } = state in
    let current_proc = get_proc_name exec_data in
    if root_proc <> current_proc then ExecNext (prev_id, None)
    else (
      state.prev_id <- Some exec_data.id;
      let new_cmd = new_cmd state.id_map exec_data in
      let failwith s =
        DL.log (fun m ->
            m
              ~json:
                [
                  ("state", dump state);
                  ("exec_data", exec_data_to_yojson exec_data);
                ]
              "handle_cmd: %s" s)
      in
      let map =
        map |> ExecMap.(at_path ~stop_at:BeforeNothing) exec_data.branch_path
      in
      (match map with
      | Cmd cmd when cmd.next = Nothing ->
          let parent = Some (map, None) in
          cmd.next <- new_cmd ~parent ()
      | BranchCmd { nexts; _ } -> (
          let case = List_utils.hd_opt (List.rev exec_data.branch_path) in
          match case with
          | None -> failwith "HORROR - branch cmd has no cases!"
          | Some case -> (
              match Hashtbl.find_opt nexts case with
              | Some Nothing ->
                  let parent = Some (map, Some case) in
                  Hashtbl.replace nexts case (new_cmd ~parent ())
              | _ -> failwith "colliding cases in branch cmd"))
      | _ -> failwith "can't insert to Nothing or FinalCmd");
      Stop)

  let package state : Packaged.t =
    let { map; _ } = state in
    let rec aux = function
      | Nothing -> Nothing
      | Cmd { data; next } ->
          let data = package_data data in
          let next = aux next in
          Cmd { data; next }
      | BranchCmd { data; nexts } ->
          let data = package_data data in
          let nexts =
            nexts
            |> Hashtbl.map (fun case next ->
                   let case = Packaged.package_case case in
                   let next = aux next in
                   (case, next))
          in
          BranchCmd { data; nexts }
      | FinalCmd { data } ->
          let data = package_data data in
          FinalCmd { data }
    and package_data { id; display; unifys; errors; submap; _ } =
      let submap =
        match submap with
        | NoSubmap -> NoSubmap
        | Proc p -> Proc p
        | Submap map -> Submap (aux map)
      in
      Packaged.{ ids = [ id ]; display; unifys; errors; submap }
    in
    aux map

  let get_gil_map state = package state
  let get_lifted_map _ = None

  let get_unifys_at_id id state =
    match state |> at_id id |> fst with
    | Nothing ->
        DL.failwith
          (fun () -> [ ("id", rid_to_yojson id); ("state", dump state) ])
          "get_unifys_at_id: HORROR - map is Nothing!"
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> data.unifys

  let get_root_id { map; _ } =
    match map with
    | Nothing -> None
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } ->
        Some data.id

  let path_of_id id state = state |> at_id id |> snd

  let next_steps id state =
    let map, _ = state |> at_id id in
    match map with
    | Nothing -> failwith "next_step: HORROR - map is Nothing!"
    | Cmd { next = Nothing; _ } | FinalCmd _ -> []
    | Cmd { next; _ } ->
        let id = get_id next in
        [ (id, None) ]
    | BranchCmd { nexts; _ } ->
        let nexts =
          Hashtbl.fold
            (fun case next acc ->
              match get_id_opt next with
              | None -> acc
              | Some id -> (id, Some case) :: acc)
            nexts []
        in
        List.rev nexts

  let previous_step id state =
    match state |> at_id id |> fst with
    | Nothing -> failwith "HORROR - map is Nothing!"
    | Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data } -> (
        match data.parent with
        | None -> None
        | Some (Nothing, _) -> failwith "HORROR - parent is Nothing!"
        | Some
            ((Cmd { data; _ } | BranchCmd { data; _ } | FinalCmd { data }), case)
          -> Some (data.id, case))

  let select_next_path case id state =
    let map, path = state |> at_id id in
    let failwith s =
      DL.failwith
        (fun () ->
          [
            ("id", rid_to_yojson id);
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

  let find_unfinished_path ?(at_path = []) state =
    let rec aux = function
      | Nothing ->
          DL.failwith
            (fun () ->
              [
                ("state", dump state); ("at_path", branch_path_to_yojson at_path);
              ])
            "find_unfinished_path: started at Nothing"
      | Cmd { data = { id; _ }; next = Nothing } -> Some (id, None)
      | Cmd { next; _ } -> aux next
      | BranchCmd { nexts; data = { id; _ } } -> (
          match
            Hashtbl.find_map
              (fun case next ->
                if next = Nothing then Some (id, Some case) else None)
              nexts
          with
          | None -> Hashtbl.find_map (fun _ next -> aux next) nexts
          | result -> result)
      | FinalCmd _ -> None
    in
    let map = state.map |> ExecMap.(at_path ~stop_at:StartOfPath) at_path in
    aux map

  let get_origin_node_str _ _ = failwith "Not implemented in Default lifter"

  let memory_error_to_exception_info { error; _ } : DebuggerTypes.exception_info
      =
    { id = Fmt.to_to_string SMemory.pp_err error; description = None }

  let add_variables ~store ~memory ~is_gil_file ~get_new_scope_id variables :
      DebuggerTypes.scope list =
    let open DebuggerTypes in
    let () = ignore is_gil_file in
    let store_id = get_new_scope_id () in
    let memory_id = get_new_scope_id () in
    let scopes : scope list =
      [ { id = store_id; name = "Store" }; { id = memory_id; name = "Memory" } ]
    in
    let store_vars =
      store
      |> List.map (fun (var, value) : variable ->
             let value = Fmt.to_to_string (Fmt.hbox Expr.pp) value in
             create_leaf_variable var value ())
      |> List.sort (fun (v : DebuggerTypes.variable) w ->
             Stdlib.compare v.name w.name)
    in
    let memory_vars =
      [
        create_leaf_variable ""
          (Fmt.to_to_string (Fmt.hbox SMemory.pp) memory)
          ();
      ]
    in
    let () = Hashtbl.replace variables store_id store_vars in
    let () = Hashtbl.replace variables memory_id memory_vars in
    scopes
end
