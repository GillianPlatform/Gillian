module L = Logging
module DL = Debugger_log
module Gil_to_tl_lifter = Gil_to_tl_lifter
module DebuggerTypes = DebuggerTypes
module DebuggerUtils = DebuggerUtils
open DebuggerTypes
open Syntaxes.Option

type rid = L.ReportId.t [@@deriving show, yojson]
type branch_case = BranchCase.t [@@deriving yojson]

let ( let** ) = Result.bind
let ( let++ ) f o = Result.map o f

module type S = sig
  type tl_ast
  type debug_state

  module PackagedBranchCase : sig
    type t [@@deriving yojson]
  end

  module UnifyMap : sig
    type t [@@deriving yojson]
  end

  module ExecMap : sig
    type 'a t [@@deriving yojson]
  end

  module Inspect : sig
    type debug_state_view [@@deriving yojson]

    val get_debug_state : debug_state -> debug_state_view

    val get_unification :
      ?proc_name:string -> rid -> debug_state -> rid * UnifyMap.t
  end

  val launch : string -> string option -> (debug_state, string) result

  val jump_to_id :
    ?proc_name:string -> rid -> debug_state -> (unit, string) result

  val jump_to_start : ?proc_name:string -> debug_state -> unit
  val step_in : ?proc_name:string -> ?reverse:bool -> debug_state -> stop_reason
  val step : ?proc_name:string -> ?reverse:bool -> debug_state -> stop_reason

  val step_specific :
    ?proc_name:string ->
    PackagedBranchCase.t option ->
    Logging.ReportId.t ->
    debug_state ->
    (stop_reason, string) result

  val step_out : ?proc_name:string -> debug_state -> stop_reason

  val run :
    ?proc_name:string ->
    ?reverse:bool ->
    ?launch:bool ->
    debug_state ->
    stop_reason

  val terminate : debug_state -> unit
  val get_frames : ?proc_name:string -> debug_state -> frame list
  val get_scopes : ?proc_name:string -> debug_state -> scope list
  val get_variables : ?proc_name:string -> int -> debug_state -> variable list
  val get_exception_info : ?proc_name:string -> debug_state -> exception_info

  val set_breakpoints :
    ?proc_name:string -> string option -> int list -> debug_state -> unit
end

module Make
    (PC : ParserAndCompiler.S)
    (Verification : Verifier.S)
    (Lifter : Gil_to_tl_lifter.S
                with type memory = Verification.SAInterpreter.heap_t
                 and type memory_error = Verification.SPState.m_err_t
                 and type tl_ast = PC.tl_ast) =
struct
  open L.LoggingConstants
  open Verification.SAInterpreter
  module Breakpoints = Set.Make (Int)

  type breakpoints = (string, Breakpoints.t) Hashtbl.t
  type tl_ast = PC.tl_ast

  module PackagedBranchCase = struct
    type t = { kind : string; display : string * string; json : Yojson.Safe.t }
    [@@deriving yojson]

    let from case =
      let open Verification.SAInterpreter in
      let json = branch_case_to_yojson case in
      let kind, display =
        match case with
        | GuardedGoto b -> ("GuardedGoto", ("If/Else", Fmt.str "%B" b))
        | LCmd x -> ("LCmd", ("Logical command", Fmt.str "%d" x))
        | SpecExec fl -> ("SpecExec", ("Spec exec", Fmt.str "%a" Flag.pp fl))
        | LAction vs ->
            let vs =
              vs
              |> List_utils.map_results state_vt_of_yojson
              |> Result.get_ok |> List.map show_state_vt
            in
            ( "LAction",
              ( "Logical action",
                Fmt.str "%a" (Fmt.list ~sep:(Fmt.any ", ") Fmt.string) vs ) )
        | LActionFail x ->
            ("LActionFail", ("Logical action failure", Fmt.str "%d" x))
      in
      { kind; display; json }

    let unpackage { json; _ } =
      json |> branch_case_of_yojson
      |> Result.map_error (fun _ -> "Malformed branch case json!")
  end

  module UnifyMap = struct
    open Verification.SUnifier.Logging

    type unify_kind = Unifier.unify_kind [@@deriving yojson]
    type unify_result = Success | Failure [@@deriving yojson]

    type substitution = {
      assert_id : rid; [@key "assertId"]
      subst : string * string;
    }
    [@@deriving yojson]

    type assertion_data = {
      id : rid;
      fold : (rid * unify_result) option;
      assertion : string;
      substitutions : substitution list;
    }
    [@@deriving yojson]

    type unify_seg =
      | Assertion of assertion_data * unify_seg
      | UnifyResult of rid * unify_result
    [@@deriving yojson]

    type map = Direct of unify_seg | Fold of unify_seg list
    [@@deriving yojson]

    type t = unify_kind * map [@@deriving yojson]

    let result_of_id id =
      let rec aux id =
        let children = L.LogQueryer.get_children_of id in
        if children = [] then
          Fmt.failwith "UnifyMap.result_of_id: report %a has no children!"
            pp_rid id;
        match
          children
          |> List.find_opt (fun (_, type_, _) ->
                 type_ = ContentType.unify_result)
        with
        | Some (_, _, content) -> (
            let result_report =
              content |> Yojson.Safe.from_string |> UnifyResultReport.of_yojson
              |> Result.get_ok
            in
            match result_report with
            | Success _ -> true
            | Failure _ -> false)
        | None -> children |> List.exists (fun (id, _, _) -> aux id)
      in
      if aux id then Success else Failure

    let rec seg_result = function
      | Assertion (_, next) -> seg_result next
      | UnifyResult (_, result) -> result

    let result = function
      | _, Direct seg -> seg_result seg
      | _, Fold segs ->
          if segs |> List.exists (fun seg -> seg_result seg = Success) then
            Success
          else Failure

    let rec build_seg ?(prev_substs = []) (id, type_, content) : unify_seg =
      let module Subst = Verification.SUnifier.ESubst in
      if type_ = ContentType.assertion then
        let asrt_report =
          content |> Yojson.Safe.from_string |> AssertionReport.of_yojson
          |> Result.get_ok
        in
        let assertion =
          let asrt, _ = asrt_report.step in
          Fmt.str "%a" Asrt.pp asrt
        in
        let substitutions =
          asrt_report.subst |> Subst.to_list_pp
          |> List.map (fun subst ->
                 List.find_opt
                   (fun prev -> [%eq: string * string] prev.subst subst)
                   prev_substs
                 |> Option.value ~default:{ assert_id = id; subst })
        in
        let fold =
          let+ child_id, type_, _ =
            match L.LogQueryer.get_children_of id with
            | [] -> None
            | [ child ] -> Some child
            | _ ->
                Fmt.failwith
                  "UnifyMap.build_seg: assertion %a has multiple children!"
                  pp_rid id
          in
          if type_ <> ContentType.unify then
            Fmt.failwith
              "UnifyMap.build_seg: report %a (child of assertion %a) has type \
               %s (expected %s)!"
              pp_rid child_id pp_rid id type_ ContentType.unify;
          (child_id, result_of_id child_id)
        in
        let seg =
          match L.LogQueryer.get_next_report_ids id with
          | [] ->
              Fmt.failwith "UnifyMap.build_seg: assertion %a has no next!"
                pp_rid id
          | [ next_id ] ->
              let content, type_ =
                L.LogQueryer.get_report next_id |> Option.get
              in
              build_seg ~prev_substs:substitutions (next_id, type_, content)
          | _ ->
              Fmt.failwith
                "UnifyMap.build_seg: assertion %a has multiple nexts!" pp_rid id
        in
        Assertion ({ id; fold; assertion; substitutions }, seg)
      else if type_ = ContentType.unify_result then
        let result_report =
          content |> Yojson.Safe.from_string |> UnifyResultReport.of_yojson
          |> Result.get_ok
        in
        let result =
          match result_report with
          | Success _ -> Success
          | Failure _ -> Failure
        in
        UnifyResult (id, result)
      else
        Fmt.failwith
          "UnifyMap.build_seg: report %a has invalid type (%s) for unify_seg!"
          pp_rid id type_

    let build_case id : unify_seg =
      match L.LogQueryer.get_children_of ~roots_only:true id with
      | [] ->
          Fmt.failwith "UnifyMap.build_case: id %a has no children!" pp_rid id
      | [ child ] -> build_seg child
      | _ ->
          Fmt.failwith "UnifyMap.build_case: id %a has multiple children!"
            pp_rid id

    let build_cases id : unify_seg list =
      let rec aux id acc =
        let acc = build_case id :: acc in
        match L.LogQueryer.get_next_reports id with
        | [] -> acc
        | [ (id, type_, _) ] ->
            if type_ <> ContentType.unify_case then
              Fmt.failwith "UnifyMap.build_cases: %a has type %s (expected %s)!"
                pp_rid id type_ ContentType.unify_case
            else aux id acc
        | _ ->
            Fmt.failwith
              "UnifyMap.build_cases: unify_case %a has multiple nexts!" pp_rid
              id
      in

      aux id [] |> List.rev

    let build unify_id =
      let kind =
        let content, _ = L.LogQueryer.get_report unify_id |> Option.get in
        let unify_report =
          content |> Yojson.Safe.from_string |> UnifyReport.of_yojson
          |> Result.get_ok
        in
        unify_report.unify_kind
      in
      let map =
        let id, type_, _ =
          match L.LogQueryer.get_children_of ~roots_only:true unify_id with
          | [ child ] -> child
          | _ ->
              Fmt.failwith
                "UnifyMap.build: unify id %a should have one root child!" pp_rid
                unify_id
        in
        if type_ = ContentType.unify_case then
          let segs = build_cases id in
          Fold segs
        else
          let seg = build_case unify_id in
          Direct seg
      in
      (kind, map)
  end

  module ExecMap = struct
    type unifys = (rid * Unifier.unify_kind * UnifyMap.unify_result) list
    [@@deriving yojson]

    type 'case submap = NoSubmap | Submap of 'case t | Proc of string
    [@@deriving yojson]

    and 'case cmd_data = {
      id : rid;
      origin_id : int option; [@name "originId"]
      display : string;
      unifys : unifys;
      errors : string list;
      submap : 'case submap;
    }
    [@@deriving yojson]

    and 'case t =
      | Nothing
      | Cmd of 'case cmd_data * 'case t
      | BranchCmd of 'case cmd_data * ('case * 'case t) list
      | FinalCmd of 'case cmd_data
    [@@deriving yojson]

    type stop_at = StartOfPath | EndOfPath | BeforeNothing

    type cmd_kind = Branch of branch_case list | Normal | Final
    [@@deriving yojson]

    type 'case with_source = (string * string) option * 'case t
    [@@deriving yojson]

    let kind_of_cases = function
      | Some cases -> Branch cases
      | None -> Normal

    let new_cmd
        cmd_kind
        new_id
        display
        ?(unifys = [])
        ?(errors = [])
        ?(submap = NoSubmap)
        origin_id =
      let cmd_data =
        { id = new_id; display; unifys; errors; origin_id; submap }
      in
      match cmd_kind with
      | Normal -> Cmd (cmd_data, Nothing)
      | Branch cases ->
          let nexts = cases |> List.map (fun bc -> (bc, Nothing)) in
          BranchCmd (cmd_data, nexts)
      | Final -> FinalCmd cmd_data

    let insert_cmd_sourceless new_cmd path map =
      let fail cur_path cur_map =
        DL.failwith
          (fun () ->
            [
              ("new_cmd", to_yojson branch_case_to_yojson new_cmd);
              ("path", branch_path_to_yojson path);
              ("map", to_yojson branch_case_to_yojson map);
              ("cur_path (rev)", branch_path_to_yojson cur_path);
              ("cur_map", to_yojson branch_case_to_yojson cur_map);
            ])
          "ExecMap.insert_cmd: malformed request"
      in

      let rec aux path map =
        match (map, path) with
        | Nothing, [] -> new_cmd
        | Cmd (cmd_data, next), _ ->
            let next = aux path next in
            Cmd (cmd_data, next)
        | BranchCmd (cmd_data, nexts), case :: path -> (
            let new_nexts =
              nexts
              |> List_utils.replace_assoc_opt case (fun map -> aux path map)
            in
            match new_nexts with
            | None -> fail path map
            | Some nexts -> BranchCmd (cmd_data, nexts))
        | _ -> fail path map
      in
      aux (List.rev path) map

    let insert_cmd new_cmd path new_source ((source, map) : 'a with_source) =
      let insert () = insert_cmd_sourceless new_cmd path map in
      match source with
      | None -> Some (Some new_source, insert ())
      | Some source ->
          if new_source = source then Some (Some source, insert ())
          else
            let path, pid = source in
            let new_path, new_pid = new_source in
            DL.log (fun m ->
                m
                  ~json:
                    [
                      ("new_cmd", to_yojson branch_case_to_yojson new_cmd);
                      ("path", `String path);
                      ("new_path", `String new_path);
                      ("pid", `String pid);
                      ("new_pid", `String new_pid);
                    ]
                  "ExecMap.insert_cmd: didn't insert due to differing source");
            None

    let path_of_id_opt selected_id =
      let rec aux acc = function
        | Nothing -> None
        | Cmd ({ id; _ }, next) ->
            if id = selected_id then Some acc else aux acc next
        | BranchCmd ({ id; _ }, nexts) ->
            if id = selected_id then Some acc
            else
              nexts
              |> List.find_map (fun (case, next) -> aux (case :: acc) next)
        | FinalCmd { id; _ } -> if id = selected_id then Some acc else None
      in
      aux []

    let map_at_path_opt f ?(stop_at = EndOfPath) path map =
      let rec aux path map =
        match (map, path, stop_at) with
        | map, [], StartOfPath -> f (Some map)
        | (Cmd (_, Nothing) as map), [], BeforeNothing -> f (Some map)
        | (BranchCmd (_, nexts) as map), [ case ], BeforeNothing
          when nexts |> List.assoc_opt case = Some Nothing -> f (Some map)
        | Cmd (d, next), _, _ ->
            let next = aux path next in
            Cmd (d, next)
        | BranchCmd (d, nexts), case :: path, _ ->
            let found = ref false in
            let nexts =
              nexts
              |> List.map (fun (next_case, map) ->
                     let map =
                       if case = next_case then (
                         found := true;
                         aux path map)
                       else map
                     in
                     (next_case, map))
            in
            if not !found then f None |> ignore;
            BranchCmd (d, nexts)
        | map, [], (EndOfPath | BeforeNothing) -> f (Some map)
        | map, _, _ ->
            f None |> ignore;
            map
      in
      aux path map

    let map_at_path f =
      let f = function
        | Some map -> f map
        | None -> Nothing
      in
      map_at_path_opt f

    let at_path_opt ?(stop_at = EndOfPath) path map =
      let map_ref = ref None in
      let f map =
        map_ref := Some map;
        map
      in
      map_at_path f ~stop_at path map |> ignore;
      !map_ref

    let at_path ?(stop_at = EndOfPath) path map =
      match at_path_opt ~stop_at path map with
      | Some map -> map
      | None ->
          DL.failwith
            (fun () ->
              [
                ("path", branch_path_to_yojson path);
                ("map", to_yojson branch_case_to_yojson map);
              ])
            "ExecMap.at_path: malformed request"

    let find_unfinished path (_, map) =
      let rec aux prev_id branch_case = function
        | FinalCmd _ -> None
        | Nothing -> Some (prev_id, branch_case)
        | Cmd ({ id; _ }, next) -> aux id None next
        | BranchCmd ({ id; _ }, nexts) ->
            nexts |> List.find_map (fun (case, next) -> aux id (Some case) next)
      in
      let submap = map |> at_path ~stop_at:StartOfPath path in
      DL.log (fun m ->
          m
            ~json:
              [
                ("path", branch_path_to_yojson path);
                ("map", to_yojson branch_case_to_yojson map);
                ("submap", to_yojson branch_case_to_yojson submap);
              ]
            "ExecMap.find_unfinished: Got submap");
      match submap with
      | FinalCmd _ | Nothing ->
          DL.failwith
            (fun () ->
              [
                ("path", branch_path_to_yojson path);
                ("map", to_yojson branch_case_to_yojson map);
              ])
            "ExecMap.find_unfinished: malformed request"
      | Cmd ({ id; _ }, next) -> aux id None next
      | BranchCmd ({ id; _ }, nexts) ->
          nexts |> List.find_map (fun (case, next) -> aux id (Some case) next)

    let next_paths path map =
      match at_path path map with
      | Cmd _ ->
          DL.failwith
            (fun () ->
              [
                ("path", branch_path_to_yojson path);
                ("map", to_yojson branch_case_to_yojson map);
              ])
            "ExecMap.at_path: shouldn't get Cmd from at_path!"
      | Nothing | FinalCmd _ -> [ path ]
      | BranchCmd (_, nexts) ->
          nexts |> List.map (fun (case, _) -> case :: path)

    let path_of_id id map =
      match path_of_id_opt id map with
      | Some path -> path
      | None -> Fmt.failwith "ExecMap.path_of_id: %a not in map!" pp_rid id

    let at_id search_id map =
      let rec aux = function
        | (Cmd ({ id; _ }, _) as map)
        | (BranchCmd ({ id; _ }, _) as map)
        | (FinalCmd { id; _ } as map)
          when id = search_id -> Some map
        | Cmd (_, next) -> aux next
        | BranchCmd (_, nexts) ->
            nexts |> List.find_map (fun (_, next) -> aux next)
        | Nothing | FinalCmd _ -> None
      in
      aux map

    let unifys_at_id id (_, map) =
      match map |> at_id id with
      | None -> []
      | Some map -> (
          match map with
          | Cmd ({ unifys; _ }, _)
          | BranchCmd ({ unifys; _ }, _)
          | FinalCmd { unifys; _ } -> unifys
          | _ ->
              Fmt.failwith
                "ExecMap.unifys_at_id: HORROR - map at %a is Nothing!" pp_rid id
          )

    let rec package = function
      | Nothing -> Nothing
      | Cmd (cmd_data, next) ->
          let next = package next in
          Cmd (package_data cmd_data, next)
      | BranchCmd (cmd_data, nexts) ->
          let nexts =
            nexts
            |> List.map (fun (case, next) ->
                   (PackagedBranchCase.from case, package next))
          in
          BranchCmd (package_data cmd_data, nexts)
      | FinalCmd cmd_data -> FinalCmd (package_data cmd_data)

    and package_data cmd_data =
      let submap =
        match cmd_data.submap with
        | Submap map -> Submap (package map)
        | NoSubmap -> NoSubmap
        | Proc p -> Proc p
      in
      { cmd_data with submap }

    let id_of = function
      | Nothing -> None
      | Cmd ({ id; _ }, _) | BranchCmd ({ id; _ }, _) | FinalCmd { id; _ } ->
          Some id

    let insert_lifted tl_ast new_submap path lifted_map =
      let failwith msg =
        DL.failwith
          (fun () ->
            [
              ("path", branch_path_to_yojson path);
              ("lifted_map", to_yojson branch_case_to_yojson lifted_map);
              ("new_submap", to_yojson branch_case_to_yojson new_submap);
            ])
          ("ExecMap.insert_lifted: " ^ msg)
      in
      let lift_data cmd_data =
        let new_display =
          Lifter.get_origin_node_str tl_ast cmd_data.origin_id
        in
        let new_display =
          if new_display = "No info!" then cmd_data.display else new_display
        in
        { cmd_data with display = new_display }
      in
      let eq_opt ida idb =
        match (ida, idb) with
        | Some ida, Some idb -> Int.equal ida idb
        | _ -> false
      in
      let merge_data ~left ~right =
        {
          right with
          unifys = left.unifys @ right.unifys;
          errors = left.errors @ right.errors;
          origin_id = Option_utils.coalesce right.origin_id left.origin_id;
        }
      in
      let next, next_origin_id =
        match new_submap with
        | Cmd (data, Nothing) -> (Cmd (lift_data data, Nothing), data.origin_id)
        | BranchCmd (data, nexts)
          when nexts |> List.for_all (fun (_, next) -> next = Nothing) ->
            (BranchCmd (lift_data data, nexts), data.origin_id)
        | FinalCmd data -> (FinalCmd (lift_data data), data.origin_id)
        | _ -> failwith "New submap is invalid"
      in
      let f = function
        | Some Nothing -> next
        | Some (Cmd (data, Nothing)) ->
            if eq_opt data.origin_id next_origin_id then
              match next with
              | Cmd (data', Nothing) ->
                  let data = merge_data ~left:data ~right:data' in
                  Cmd (data, Nothing)
              | FinalCmd data' ->
                  let data = merge_data ~left:data ~right:data' in
                  FinalCmd data
              | BranchCmd (data', nexts)
                when nexts |> List.for_all (fun (_, next) -> next = Nothing) ->
                  let data = merge_data ~left:data ~right:data' in
                  BranchCmd (data, nexts)
              | _ -> failwith "New submap is invalid"
            else Cmd (data, next)
        | Some (BranchCmd (data, nexts))
          when nexts |> List.assoc_opt (List.hd path) = Some Nothing ->
            if eq_opt data.origin_id next_origin_id then
              failwith
                "BranchCmd has the same id as its next - not yet implemented"
            else
              let nexts =
                nexts
                |> List_utils.replace_assoc_opt (List.hd path) (fun _ -> next)
                |> Option.get
              in
              BranchCmd (data, nexts)
        | Some (FinalCmd _) -> failwith "Tried to insert to finished path"
        | Some _ ->
            failwith "HORROR - map has next despite stop_at:BeforeNothing"
        | None -> failwith "Path doesn't exist in map"
      in
      lifted_map |> map_at_path_opt f ~stop_at:BeforeNothing path
  end

  type exec_map = branch_case ExecMap.with_source [@@deriving yojson]

  type debug_proc_state = {
    mutable cont_func : result_t cont_func_f option;
    mutable breakpoints : breakpoints;
    mutable cur_report_id : rid;
    (* TODO: The below fields only depend on the
             cur_report_id and could be refactored to use this *)
    mutable top_level_scopes : scope list;
    mutable frames : frame list;
    mutable variables : variables;
    mutable errors : err_t list;
    mutable cur_cmd : (int Cmd.t * Annot.t) option;
    mutable exec_map : exec_map;
    mutable lifted_exec_map : branch_case ExecMap.t option;
    mutable proc_name : string option;
    mutable unify_maps : (rid * UnifyMap.t) list;
    report_state : L.report_state;
  }

  type debug_cfg = {
    source_file : string;
    source_files : SourceFiles.t option;
    prog : Verification.prog_t;
    tl_ast : tl_ast option;
    tests : (string * Verification.t) list;
    main_proc_name : string;
    report_state_base : L.report_state;
  }

  type debug_state = {
    cfg : debug_cfg;
    procs : (string, debug_proc_state) Hashtbl.t;
    mutable cur_proc_name : string;
  }

  let get_proc_state ?proc_name ?(activate_report_state = true) dbg =
    let proc_name =
      match proc_name with
      | Some proc_name ->
          dbg.cur_proc_name <- proc_name;
          proc_name
      | None -> dbg.cur_proc_name
    in
    let proc_state = Hashtbl.find dbg.procs proc_name in
    if activate_report_state then
      L.activate_report_state proc_state.report_state;
    proc_state

  module Inspect = struct
    type exec_map_pkg = PackagedBranchCase.t ExecMap.t [@@deriving yojson]
    type branch_path_pkg = PackagedBranchCase.t list [@@deriving yojson]

    let get_exec_maps_pkg { exec_map = _, map; lifted_exec_map; _ } =
      (ExecMap.package map, Option.map ExecMap.package lifted_exec_map)

    type debug_proc_state_view = {
      exec_map : exec_map_pkg; [@key "execMap"]
      lifted_exec_map : exec_map_pkg option; [@key "liftedExecMap"]
      current_cmd_id : rid; [@key "currentCmdId"]
      unifys : (rid * Unifier.unify_kind * UnifyMap.unify_result) list;
      proc_name : string; [@key "procName"]
    }
    [@@deriving yojson]

    let procs_to_yosjon procs : Yojson.Safe.t =
      let procs =
        procs |> List.map (fun (k, v) -> (k, debug_proc_state_view_to_yojson v))
      in
      `Assoc procs

    let procs_of_yojson json =
      let procs =
        json |> Yojson.Safe.Util.to_assoc
        |> List_utils.map_results (fun (k, v) ->
               let++ v' = debug_proc_state_view_of_yojson v in
               (k, v'))
      in
      procs

    type debug_state_view = {
      main_proc_name : string; [@key "mainProc"]
      current_proc_name : string; [@key "currentProc"]
      procs : (string * debug_proc_state_view) list;
          [@to_yojson procs_to_yosjon] [@of_yojson procs_of_yojson]
    }
    [@@deriving yojson]

    let get_debug_state (dbg : debug_state) : debug_state_view =
      let procs =
        Hashtbl.fold
          (fun proc_name state acc ->
            let current_cmd_id = state.cur_report_id in
            let unifys =
              state.exec_map |> ExecMap.unifys_at_id current_cmd_id
            in
            let exec_map, lifted_exec_map = get_exec_maps_pkg state in
            let proc =
              { exec_map; lifted_exec_map; current_cmd_id; unifys; proc_name }
            in
            (proc_name, proc) :: acc)
          dbg.procs []
      in
      {
        main_proc_name = dbg.cfg.main_proc_name;
        current_proc_name = dbg.cur_proc_name;
        procs;
      }

    let get_unification ?proc_name unify_id dbg =
      let state = dbg |> get_proc_state ?proc_name in
      match state.unify_maps |> List.assoc_opt unify_id with
      | Some map -> (unify_id, map)
      | None ->
          let map = UnifyMap.build unify_id in
          state.unify_maps <- (unify_id, map) :: state.unify_maps;
          (unify_id, map)
  end

  let top_level_scopes : scope list =
    let top_level_scope_names =
      (* [ "Store"; "Heap"; "Pure Formulae"; "Typing Environment"; "Predicates" ] *)
      [ "Pure Formulae"; "Typing Environment"; "Predicates" ]
    in
    List.mapi (fun i name -> { name; id = i + 1 }) top_level_scope_names

  let is_gil_file file_name = Filename.check_suffix file_name "gil"

  let get_pure_formulae_vars (state : state_t) : variable list =
    Verification.SPState.get_pfs state
    |> PFS.to_list
    |> List.map (fun formula ->
           let value = Fmt.to_to_string (Fmt.hbox Formula.pp) formula in
           { name = ""; value; type_ = None; var_ref = 0 })
    |> List.sort (fun v w -> Stdlib.compare v.value w.value)

  let get_typ_env_vars (state : state_t) : variable list =
    let typ_env = Verification.SPState.get_typ_env state in
    TypEnv.to_list typ_env
    |> List.sort (fun (v, _) (w, _) -> Stdlib.compare v w)
    |> List.map (fun (name, value) ->
           let value = Type.str value in
           { name; value; type_ = None; var_ref = 0 })
    |> List.sort (fun v w -> Stdlib.compare v.name w.name)

  let get_pred_vars (state : state_t) : variable list =
    Verification.SPState.get_preds state
    |> Preds.SPreds.to_list
    |> List.map (fun pred ->
           let value = Fmt.to_to_string (Fmt.hbox Preds.SPreds.pp_pabs) pred in
           { name = ""; value; type_ = None; var_ref = 0 })
    |> List.sort (fun v w -> Stdlib.compare v.value w.value)

  let create_variables (state : state_t option) (is_gil_file : bool) :
      scope list * variables =
    let variables = Hashtbl.create 0 in
    (* New scope ids must be higher than last top level scope id to prevent
       duplicate scope ids *)
    let scope_id = ref (List.length top_level_scopes) in
    let get_new_scope_id () =
      let () = scope_id := !scope_id + 1 in
      !scope_id
    in
    let lifted_scopes =
      match state with
      | None -> []
      | Some state ->
          let store = Verification.SPState.get_store state |> Store.bindings in
          let memory = Verification.SPState.get_heap state in
          let lifted_scopes =
            Lifter.add_variables ~store ~memory ~is_gil_file ~get_new_scope_id
              variables
          in
          let pure_formulae_vars = get_pure_formulae_vars state in
          let typ_env_vars = get_typ_env_vars state in
          let pred_vars = get_pred_vars state in
          let vars_list = [ pure_formulae_vars; typ_env_vars; pred_vars ] in
          let () =
            List.iter2
              (fun (scope : scope) vars ->
                Hashtbl.replace variables scope.id vars)
              top_level_scopes vars_list
          in
          lifted_scopes
    in
    (lifted_scopes, variables)

  (* TODO: Find a common place to put the below three functions which are
     duplicated in CommandLine.ml *)
  let convert_other_imports oi =
    List.map
      (fun (ext, f) ->
        let fun_with_exn s = Stdlib.Result.get_ok (f s) in
        (ext, fun_with_exn))
      oi

  let get_progs_or_fail = function
    | Ok progs -> (
        match progs.ParserAndCompiler.gil_progs with
        | [] ->
            Fmt.pr "Error: expected at least one GIL program\n";
            exit 1
        | _ -> progs)
    | Error err ->
        Fmt.pr "Error during compilation to GIL:\n%a" PC.pp_err err;
        exit 1

  let burn_gil prog outfile_opt =
    match outfile_opt with
    | Some outfile ->
        let outc = open_out outfile in
        let fmt = Format.formatter_of_out_channel outc in
        let () = Prog.pp_labeled fmt prog in
        close_out outc
    | None -> ()

  let preprocess_files files already_compiled outfile_opt no_unfold =
    let e_prog, source_files_opt, tl_ast =
      if not already_compiled then
        let progs = get_progs_or_fail (PC.parse_and_compile_files files) in
        let e_progs = progs.gil_progs in
        let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
        let e_prog = snd (List.hd e_progs) in
        let source_files = progs.source_files in
        (e_prog, Some source_files, Some progs.tl_ast)
      else
        let e_prog = Gil_parsing.parse_eprog_from_file (List.hd files) in
        (e_prog, None, None)
    in
    let () = burn_gil e_prog outfile_opt in
    (* Prog.perform_syntax_checks e_prog; *)
    let prog =
      Gil_parsing.eprog_to_prog
        ~other_imports:(convert_other_imports PC.other_imports)
        e_prog
    in
    let () =
      L.verbose (fun m ->
          m "@\nProgram as parsed:@\n%a@\n" Prog.pp_indexed prog)
    in
    let prog = LogicPreprocessing.preprocess prog (not no_unfold) in
    let () =
      L.verbose (fun m ->
          m "@\nProgram after logic preprocessing:@\n%a@\n" Prog.pp_indexed prog)
    in
    DL.log (fun m ->
        let proc_to_yojson = Proc.to_yojson Annot.to_yojson (fun x -> `Int x) in
        let procs_json =
          Hashtbl.fold
            (fun name proc acc -> (name, proc_to_yojson proc) :: acc)
            prog.procs []
        in
        m ~json:procs_json "Got %d procs" (Hashtbl.length prog.procs));
    (prog, source_files_opt, tl_ast)

  let has_hit_breakpoint dbg =
    match dbg.frames with
    | [] -> false
    | frame :: _ ->
        if Hashtbl.mem dbg.breakpoints frame.source_path then
          let breakpoints = Hashtbl.find dbg.breakpoints frame.source_path in
          (* Currently only one breakpoint per line is supported *)
          Breakpoints.mem frame.start_line breakpoints
        else false

  let rec call_stack_to_frames call_stack next_proc_body_idx prog =
    match call_stack with
    | [] -> []
    | (se : CallStack.stack_element) :: rest ->
        let defaults = (0, 0, 0, 0, "") in
        let proc = Prog.get_proc prog se.pid in
        let start_line, start_column, end_line, end_column, source_path =
          match proc with
          | None -> defaults
          | Some proc -> (
              let annot, _, _ = proc.proc_body.(next_proc_body_idx) in
              let loc_opt = Annot.get_origin_loc annot in
              match loc_opt with
              | None -> defaults
              | Some loc ->
                  let loc = DebuggerUtils.location_to_display_location loc in
                  ( loc.loc_start.pos_line,
                    loc.loc_start.pos_column,
                    loc.loc_end.pos_line,
                    loc.loc_end.pos_column,
                    loc.loc_source ))
        in
        let frame =
          {
            (* TODO: make this a guaranteed unique index*)
            index = se.call_index;
            name = se.pid;
            source_path;
            start_line;
            start_column;
            end_line;
            end_column;
          }
        in
        frame :: call_stack_to_frames rest se.call_index prog

  let update_report_id_and_inspection_fields report_id cfg state =
    match L.LogQueryer.get_report report_id with
    | None ->
        Fmt.failwith
          "Unable to find report id '%a'. Check the logging level is set \
           correctly"
          pp_rid report_id
    | Some (content, type_) -> (
        DL.show_report report_id ("Debugger.update...: Got report type " ^ type_);
        match type_ with
        | t when t = ContentType.cmd ->
            state.cur_report_id <- report_id;
            let cmd =
              content |> Yojson.Safe.from_string
              |> Logging.ConfigReport.of_yojson |> Result.get_ok
            in
            state.frames <-
              call_stack_to_frames cmd.callstack cmd.proc_line cfg.prog;
            let lifted_scopes, variables =
              create_variables (Some cmd.state) (is_gil_file cfg.source_file)
            in
            let () = state.variables <- variables in
            let () =
              state.top_level_scopes <-
                List.concat [ lifted_scopes; top_level_scopes ]
            in
            (* TODO: fix *)
            (* let () = dbg.errors <- cmd_result.errors in *)
            let cur_cmd =
              match cmd.callstack with
              | [] -> None
              | (se : CallStack.stack_element) :: _ -> (
                  let proc = Prog.get_proc cfg.prog se.pid in
                  match proc with
                  | None -> None
                  | Some proc ->
                      let annot, _, cmd = proc.proc_body.(cmd.proc_line) in
                      Some (cmd, annot))
            in
            state.cur_cmd <- cur_cmd
        | _ as t ->
            Fmt.failwith
              "Debugger: don't know how to handle report of type '%s'!" t)

  let launch_proc cfg proc_name =
    let report_state = L.(clone_report_state cfg.report_state_base) in
    let rec aux = function
      | Verification.SAInterpreter.Finished _ ->
          Error "HORROR: Shouldn't encounter Finished when debugging!"
      | Verification.SAInterpreter.EndOfBranch _ -> Error "Nothing to run"
      | Verification.SAInterpreter.Continue
          (cur_report_id, branch_path, new_branch_cases, cont_func) -> (
          match cur_report_id with
          | None ->
              raise
                (Failure
                   "Did not log report. Check the logging level is set \
                    correctly")
          | Some cur_report_id ->
              let content, type_ =
                Option.get @@ L.LogQueryer.get_report cur_report_id
              in
              if type_ = ContentType.proc_init then (
                DL.log (fun m -> m "Debugger.launch: Skipping proc_init...");
                aux (cont_func ~path:[] ()))
              else
                let cmd =
                  Result.get_ok
                    (content |> Yojson.Safe.from_string
                   |> Logging.ConfigReport.of_yojson)
                in
                let cmd_display = cmd.cmd in
                let origin_id = Annot.get_origin_id cmd.annot in

                let open ExecMap in
                let new_cmd =
                  new_cmd
                    (kind_of_cases new_branch_cases)
                    cur_report_id cmd_display origin_id
                in
                let map =
                  Nothing |> insert_cmd_sourceless new_cmd branch_path
                in
                if Annot.is_hidden cmd.annot then
                  failwith "HORROR: First command is hidden!";
                let lifted_map =
                  match (Lifter.source_map_ability, cfg.tl_ast) with
                  | true, Some tl_ast ->
                      Some (Nothing |> insert_lifted tl_ast new_cmd branch_path)
                  | _ -> None
                in
                let state =
                  {
                    cont_func = Some cont_func;
                    breakpoints = Hashtbl.create 0;
                    cur_report_id;
                    top_level_scopes;
                    frames = [];
                    variables = Hashtbl.create 0;
                    errors = [];
                    cur_cmd = None;
                    exec_map = (None, map);
                    lifted_exec_map = lifted_map;
                    proc_name = None;
                    unify_maps = [];
                    report_state;
                  }
                in
                state
                |> update_report_id_and_inspection_fields cur_report_id cfg;
                Ok state)
    in
    Config.Verification.(
      let procs_to_verify = !procs_to_verify in
      if not (procs_to_verify |> List.mem proc_name) then
        set_procs_to_verify (procs_to_verify @ [ proc_name ]));
    report_state
    |> L.with_report_state (fun () ->
           let cont_func =
             Verification.verify_up_to_procs ~proc_name cfg.prog
           in
           aux cont_func)

  let launch file_name proc_name =
    let () = Fmt_tty.setup_std_outputs () in
    let () = Config.current_exec_mode := Verification in
    let () = PC.initialize Verification in
    let () = Config.stats := false in
    let () = Config.lemma_proof := true in
    let () = Config.manual_proof := false in
    let () =
      match proc_name with
      | None -> ()
      | Some proc_name -> Config.Verification.set_procs_to_verify [ proc_name ]
    in
    (* If the file is a GIL file, assume it is already compiled *)
    let already_compiled = is_gil_file file_name in
    let outfile_opt = None in
    let no_unfold = false in
    (* TODO: Support debugging incremental mode *)
    (* let incremental = false in *)
    let prog, source_files_opt, tl_ast =
      preprocess_files [ file_name ] already_compiled outfile_opt no_unfold
    in
    let tests = Verification.Debug.get_tests_for_prog prog in
    let proc_name =
      match proc_name with
      | Some proc_name -> proc_name
      | None -> tests |> List.hd |> fst
    in
    let cfg =
      {
        source_file = file_name;
        source_files = source_files_opt;
        prog;
        tl_ast;
        tests;
        main_proc_name = proc_name;
        report_state_base = L.(clone_report_state global_report_state);
      }
    in
    let++ main_proc_state = launch_proc cfg proc_name in
    main_proc_state.report_state |> L.activate_report_state;
    let dbg = { cfg; procs = Hashtbl.create 0; cur_proc_name = proc_name } in
    Hashtbl.add dbg.procs proc_name main_proc_state;
    dbg

  let unify result proc_name prev_id dbg =
    match dbg.tests |> List.assoc_opt proc_name with
    | None ->
        DL.failwith
          (fun () ->
            let tests_json = Verification.proc_tests_to_yojson dbg.tests in
            [ ("tests", tests_json) ])
          (Fmt.str "No test found for proc `%s`!" proc_name)
    | Some test -> (
        match L.LogQueryer.get_unify_for prev_id with
        | Some _ ->
            DL.log (fun m ->
                m "Unification for %a already exists; skipping unify" pp_rid
                  prev_id);
            None
        | None ->
            DL.log (fun m -> m "Unifying result for %a" pp_rid prev_id);
            let success =
              Verification.Debug.analyse_result test prev_id result
            in
            let+ id, content = L.LogQueryer.get_unify_for prev_id in
            let unify_report =
              content |> Yojson.Safe.from_string
              |> Verification.SUnifier.Logging.UnifyReport.of_yojson
              |> Result.get_ok
            in
            let kind = unify_report.unify_kind in
            let result = UnifyMap.(if success then Success else Failure) in
            (id, kind, result))

  let show_result_errors = function
    | ExecRes.RSucc _ -> []
    | ExecRes.RFail { errors; _ } ->
        errors |> List.map Verification.SAInterpreter.show_err_t

  let jump_state_to_id id cfg state =
    try
      DL.log (fun m -> m "Jumping to id %a" pp_rid id);
      state.exec_map |> snd |> ExecMap.path_of_id id |> ignore;
      state |> update_report_id_and_inspection_fields id cfg;
      Ok ()
    with Failure msg -> Error msg

  let jump_to_id ?proc_name id dbg =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state ?proc_name in
    state |> jump_state_to_id id cfg

  let jump_to_start ?proc_name dbg =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state ?proc_name in
    let result =
      let** root_id =
        ExecMap.id_of (snd state.exec_map)
        |> Option.to_result ~none:"Debugger.jump_to_start: exec map is Nothing!"
      in
      state |> jump_state_to_id root_id cfg
    in
    match result with
    | Error msg -> failwith msg
    | Ok () -> ()

  let get_current_source dbg =
    let { source_path; name; _ } = List.hd dbg.frames in
    (source_path, name)

  let update_exec_maps
      cmd_kind
      cur_report_id
      (cmd : Verification.SAInterpreter.Logging.ConfigReport.t)
      ?(unifys = [])
      ?(errors = [])
      branch_path
      dbg
      state =
    let { cfg; _ } = dbg in
    let cmd_display = cmd.cmd in
    let origin_id = Annot.get_origin_id cmd.annot in
    let source = state |> get_current_source in
    let submap =
      let open ExecMap in
      match Annot.get_expansion_kind cmd.annot with
      | NoExpansion -> NoSubmap
      | Function fname -> (
          match launch_proc cfg fname with
          | Error msg ->
              DL.log (fun m -> m "Failed to launch proc %s: %s" fname msg);
              NoSubmap
          | Ok state' ->
              Hashtbl.replace dbg.procs fname state';
              Proc fname)
    in
    ExecMap.(
      let new_cmd =
        new_cmd cmd_kind cur_report_id cmd_display ~unifys ~errors ~submap
          origin_id
      in
      let exec_map = state.exec_map |> insert_cmd new_cmd branch_path source in
      match exec_map with
      | None -> false
      | Some exec_map -> (
          state.exec_map <- exec_map;
          match (cfg.tl_ast, state.lifted_exec_map) with
          | Some tl_ast, Some lifted_exec_map ->
              if Annot.is_hidden cmd.annot then false
              else
                let lifted_exec_map =
                  lifted_exec_map |> insert_lifted tl_ast new_cmd branch_path
                in
                state.lifted_exec_map <- Some lifted_exec_map;
                true
          | _ -> true))

  let rec execute_step prev_id_in_frame ?branch_case ?prev_branch_path dbg state
      =
    let open Verification.SAInterpreter in
    let { cfg; _ } = dbg in
    match state.cont_func with
    | None ->
        DL.log (fun m -> m "No cont_func; reached end");
        ReachedEnd
    | Some cont_func -> (
        DL.log (fun m ->
            m
              ~json:
                [
                  ("id", rid_to_yojson state.cur_report_id);
                  ( "map",
                    state.exec_map |> snd
                    |> ExecMap.to_yojson branch_case_to_yojson );
                ]
              "Grabbing path for step...");
        let branch_path =
          prev_branch_path
          |> Option_utils.or_else (fun () ->
                 state.exec_map |> snd |> ExecMap.path_of_id prev_id_in_frame)
        in
        let branch_path =
          match branch_case with
          | Some case -> case :: branch_path
          | None ->
              state.exec_map |> snd |> ExecMap.next_paths branch_path |> List.hd
        in
        DL.log (fun m ->
            m ~json:[ ("path", branch_path_to_yojson branch_path) ] "Got path");
        match cont_func ~path:branch_path () with
        | Finished _ ->
            state.cont_func <- None;
            failwith "HORROR: Shouldn't encounter Finished when debugging!"
        | EndOfBranch (result, cont_func) ->
            state.cont_func <- Some cont_func;
            let prev =
              let+ content, type_ = L.LogQueryer.get_report prev_id_in_frame in
              (prev_id_in_frame, content, type_)
            in
            (match prev with
            | Some (prev_id, content, type_) when type_ = ContentType.cmd ->
                let cmd =
                  content |> Yojson.Safe.from_string
                  |> Logging.ConfigReport.of_yojson |> Result.get_ok
                in
                let proc_name = (List.hd cmd.callstack).pid in
                let errors = show_result_errors result in
                let unifys =
                  unify result proc_name prev_id cfg |> Option.to_list
                in
                state |> update_report_id_and_inspection_fields prev_id cfg;
                let inserted =
                  state
                  |> update_exec_maps Final prev_id cmd ~unifys ~errors
                       branch_path dbg
                in
                if not inserted then
                  failwith "HORROR: didn't insert on EndOfBranch!"
            | Some (prev_id, _, type_) ->
                Fmt.failwith "EndOfBranch: prev cmd (%a) is '%s', not '%s'!"
                  pp_rid prev_id type_ ContentType.cmd
            | None ->
                Fmt.failwith "EndOfBranch: prev id '%a' doesn't exist!" pp_rid
                  prev_id_in_frame);
            ReachedEnd
        | Continue (cur_report_id, branch_path, new_branch_cases, cont_func)
          -> (
            match cur_report_id with
            | None ->
                failwith
                  "Did not log report. Check the logging level is set correctly"
            | Some cur_report_id ->
                state.cont_func <- Some cont_func;
                let content, type_ =
                  Option.get @@ L.LogQueryer.get_report cur_report_id
                in
                if type_ = ContentType.proc_init then (
                  DL.log (fun m -> m "(execute_step) Skipping proc_init...");
                  state |> execute_step prev_id_in_frame dbg)
                else if
                  L.LogQueryer.get_cmd_results cur_report_id
                  |> List.for_all (fun (_, content) ->
                         let result =
                           content |> Yojson.Safe.from_string
                           |> Logging.CmdResult.of_yojson |> Result.get_ok
                         in
                         result.errors <> [])
                then (
                  DL.log (fun m ->
                      m
                        "No results for cmd (or all results have error); \
                         assuming eob, stepping again...");
                  state
                  |> execute_step ~prev_branch_path:branch_path cur_report_id
                       dbg)
                else (
                  state
                  |> update_report_id_and_inspection_fields cur_report_id cfg;
                  ExecMap.(
                    let cmd =
                      Result.get_ok
                        (content |> Yojson.Safe.from_string
                       |> Logging.ConfigReport.of_yojson)
                    in
                    let cmd_kind =
                      match new_branch_cases with
                      | Some cases -> Branch cases
                      | None -> Normal
                    in
                    let unifys =
                      (DL.log (fun m ->
                           m "getting unify_result for %a" pp_rid cur_report_id);
                       let+ unify_id, _ =
                         L.LogQueryer.get_unify_for cur_report_id
                       in
                       let unify_map =
                         match state.unify_maps |> List.assoc_opt unify_id with
                         | Some map -> map
                         | None ->
                             let map = UnifyMap.build unify_id in
                             state.unify_maps <-
                               (unify_id, map) :: state.unify_maps;
                             map
                       in
                       let result = unify_map |> UnifyMap.result in
                       (unify_id, fst unify_map, result))
                      |> Option.to_list
                    in
                    let inserted =
                      state
                      |> update_exec_maps cmd_kind cur_report_id cmd ~unifys
                           branch_path dbg
                    in
                    if not inserted then
                      state
                      |> execute_step cur_report_id
                           ~prev_branch_path:branch_path dbg
                    else Step))))

  let step_in_branch_case
      prev_id_in_frame
      ?branch_case
      ?(reverse = false)
      dbg
      state =
    let { cfg; _ } = dbg in
    let stop_reason =
      if reverse then (
        let prev_report_id =
          L.LogQueryer.get_previous_report_id state.cur_report_id
        in
        match prev_report_id with
        | None -> ReachedStart
        | Some prev_report_id ->
            DL.show_report prev_report_id "Previous report";
            let _, prev_report_type =
              Option.get (L.LogQueryer.get_report prev_report_id)
            in
            if prev_report_type = ContentType.proc_init then (
              DL.log (fun m ->
                  m "Prev report is '%s'; not stepping." prev_report_type);
              ReachedStart)
            else (
              state |> update_report_id_and_inspection_fields prev_report_id cfg;
              Step))
      else
        let next_report_ids =
          L.LogQueryer.get_next_report_ids state.cur_report_id
        in
        let next_report_id =
          match branch_case with
          | None -> List_utils.hd_opt next_report_ids
          | Some branch_case ->
              next_report_ids
              |> List.find_opt (fun id ->
                     match L.LogQueryer.get_report id with
                     | Some (content, type_) when type_ = ContentType.cmd -> (
                         match
                           content |> Yojson.Safe.from_string
                           |> Logging.ConfigReport.of_yojson
                         with
                         | Error _ -> false
                         | Ok cmd ->
                             Option_utils.somes_and_eq cmd.branch_case
                               branch_case)
                     | _ -> false)
        in
        match next_report_id with
        | None ->
            DL.log (fun m -> m "No next report ID; executing next step");
            state |> execute_step ?branch_case prev_id_in_frame dbg
        | Some next_report_id ->
            DL.show_report next_report_id "Next report ID found; not executing";
            state |> update_report_id_and_inspection_fields next_report_id cfg;
            Step
    in
    if has_hit_breakpoint state then Breakpoint
    else if List.length state.errors > 0 then
      let () = state.cont_func <- None in
      ExecutionError
    else stop_reason

  let step_in_state ?(reverse = false) cfg state =
    step_in_branch_case state.cur_report_id ?branch_case:None ~reverse cfg state

  let step_in ?proc_name ?(reverse = false) dbg =
    let state = dbg |> get_proc_state ?proc_name in
    step_in_state ~reverse dbg state

  let step_until_cond
      ?(reverse = false)
      ?(branch_case : branch_case option)
      (cond : frame -> frame -> int -> int -> bool)
      (dbg : debug_state)
      (state : debug_proc_state) : stop_reason =
    let prev_id_in_frame = state.cur_report_id in
    let prev_frame =
      match state.frames with
      | [] -> failwith "Nothing in call stack, cannot step"
      | frame :: _ -> frame
    in
    let prev_stack_depth = List.length state.frames in
    let rec aux () =
      let stop_reason =
        state |> step_in_branch_case ~reverse ?branch_case prev_id_in_frame dbg
      in
      match stop_reason with
      | Step ->
          let cur_frame =
            match state.frames with
            | [] -> failwith "Nothing in call stack, cannot step"
            | cur_frame :: _ -> cur_frame
          in
          let cur_stack_depth = List.length state.frames in
          if cond prev_frame cur_frame prev_stack_depth cur_stack_depth then
            stop_reason
          else aux ()
      | other_stop_reason -> other_stop_reason
    in
    aux ()

  let step_case ?(reverse = false) ?branch_case dbg state =
    let { cfg; _ } = dbg in
    let cond =
      if is_gil_file cfg.source_file then
        (* If GIL file, step until next cmd in the same frame (like in regular
           debuggers) *)
        fun prev_frame cur_frame prev_stack_depth cur_stack_depth ->
        cur_frame.source_path = prev_frame.source_path
        && cur_frame.name = prev_frame.name
        || cur_stack_depth < prev_stack_depth
      else
        (* If target language file, step until the code origin location is
           different, indicating an actual step in the target language*)
        fun prev_frame cur_frame _ _ ->
        cur_frame.source_path = prev_frame.source_path
        && (cur_frame.start_line <> prev_frame.start_line
           || cur_frame.start_column <> prev_frame.start_column
           || cur_frame.end_line <> prev_frame.end_line
           || cur_frame.end_column <> prev_frame.end_column)
    in
    state |> step_until_cond ~reverse ?branch_case cond dbg

  let step ?proc_name ?(reverse = false) dbg =
    let state = dbg |> get_proc_state ?proc_name in
    step_case ~reverse dbg state

  let step_specific
      ?proc_name
      (branch_case : PackagedBranchCase.t option)
      prev_id
      dbg =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state ?proc_name in
    let** branch_case =
      branch_case
      |> Option.map PackagedBranchCase.unpackage
      |> Option_utils.to_result
    in
    let++ () = state |> jump_state_to_id prev_id cfg in
    state |> step_case ?branch_case dbg

  let step_out ?proc_name dbg =
    let state = dbg |> get_proc_state ?proc_name in
    let rec aux stack_depth =
      let stop_reason = state |> step_in_state dbg in
      match stop_reason with
      | Step ->
          if List.length state.frames < stack_depth then stop_reason
          else aux stack_depth
      | other_stop_reason -> other_stop_reason
    in
    aux (List.length state.frames)

  let run ?proc_name ?(reverse = false) ?(launch = false) dbg =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state ?proc_name in
    let current_id = state.cur_report_id in
    let branch_path = state.exec_map |> snd |> ExecMap.path_of_id current_id in
    DL.log (fun m ->
        m
          ~json:
            [
              ("current_id", rid_to_yojson current_id);
              ("path", branch_path_to_yojson branch_path);
              ( "map",
                state.exec_map |> snd |> ExecMap.to_yojson branch_case_to_yojson
              );
            ]
          "Debugger.run");
    let rec aux ?(launch = false) count branch_case =
      if count > 20 then failwith "Debugger.run: infinite loop?";
      (* We need to check if a breakpoint has been hit if run is called
         immediately after launching to prevent missing a breakpoint on the first
         line *)
      if launch && has_hit_breakpoint state then Breakpoint
      else
        let stop_reason = step_case ?branch_case ~reverse dbg state in
        match stop_reason with
        | Step -> aux count None
        | Breakpoint -> Breakpoint
        | other_stop_reason -> (
            if reverse then other_stop_reason
            else
              match state.exec_map |> ExecMap.find_unfinished branch_path with
              | None ->
                  DL.log (fun m ->
                      m
                        ~json:[ ("map", exec_map_to_yojson state.exec_map) ]
                        "Debugger.run: map has no unfinished branches");
                  other_stop_reason
              | Some (prev_id, branch_case) ->
                  DL.log (fun m ->
                      m
                        ~json:
                          [
                            ("prev_id", rid_to_yojson prev_id);
                            ("", opt_to_yojson branch_case_to_yojson branch_case);
                          ]
                        "Debugger.run: found unfinished path");
                  state |> jump_state_to_id prev_id cfg |> Result.get_ok;
                  aux (count + 1) branch_case)
    in
    aux ~launch 0 None

  let terminate dbg =
    L.(activate_report_state global_report_state);
    Verification.postprocess_files dbg.cfg.source_files;
    if !Config.stats then Statistics.print_statistics ();
    L.wrap_up ()

  let get_frames ?proc_name dbg =
    let state = dbg |> get_proc_state ?proc_name in
    state.frames

  let get_scopes ?proc_name dbg =
    let state = dbg |> get_proc_state ?proc_name in
    state.top_level_scopes

  let get_variables ?proc_name (var_ref : int) (dbg : debug_state) :
      variable list =
    let state = dbg |> get_proc_state ?proc_name in
    match Hashtbl.find_opt state.variables var_ref with
    | None -> []
    | Some vars -> vars

  let get_exception_info ?proc_name (dbg : debug_state) =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state ?proc_name in
    let error = List.hd state.errors in
    let non_mem_exception_info =
      { id = Fmt.to_to_string Logging.pp_err error; description = None }
    in
    match error with
    | ExecErr.ESt state_error -> (
        match state_error with
        | StateErr.EMem merr ->
            Lifter.memory_error_to_exception_info
              { error = merr; command = state.cur_cmd; tl_ast = cfg.tl_ast }
        | _ -> non_mem_exception_info)
    | _ -> non_mem_exception_info

  let set_breakpoints ?proc_name source bp_list dbg =
    let state = dbg |> get_proc_state ?proc_name in
    match source with
    (* We can't set the breakpoints if we do not know the source file *)
    | None -> ()
    | Some source ->
        let bp_set = Breakpoints.of_list bp_list in
        Hashtbl.replace state.breakpoints source bp_set
end
