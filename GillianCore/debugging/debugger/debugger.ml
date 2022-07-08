module L = Logging
module DL = Debugger_log
module Gil_to_tl_lifter = Gil_to_tl_lifter
module DebuggerTypes = DebuggerTypes
module DebuggerUtils = DebuggerUtils
open DebuggerTypes
open Syntaxes.Option

type rid = L.ReportId.t [@@deriving show, yojson]

let ( let** ) = Result.bind
let ( let++ ) f o = Result.map o f

module type S = sig
  type tl_ast
  type debugger_state

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
    type debug_state [@@deriving yojson]

    val get_debug_state : debugger_state -> debug_state
    val get_unification : rid -> debugger_state -> rid * UnifyMap.t
  end

  val launch : string -> string option -> (debugger_state, string) result
  val jump_to_id : rid -> debugger_state -> (unit, string) result
  val jump_to_start : debugger_state -> unit
  val step_in : ?reverse:bool -> debugger_state -> stop_reason
  val step : ?reverse:bool -> debugger_state -> stop_reason

  val step_specific :
    PackagedBranchCase.t option ->
    Logging.ReportId.t ->
    debugger_state ->
    (stop_reason, string) result

  val step_out : debugger_state -> stop_reason
  val run : ?reverse:bool -> ?launch:bool -> debugger_state -> stop_reason
  val terminate : debugger_state -> unit
  val get_frames : debugger_state -> frame list
  val get_scopes : debugger_state -> scope list
  val get_variables : int -> debugger_state -> variable list
  val get_exception_info : debugger_state -> exception_info
  val set_breakpoints : string option -> int list -> debugger_state -> unit
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
            let vs = vs |> List.map show_state_vt in
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

    type assertion_data = {
      id : rid;
      fold : (rid * unify_result) option;
      assertion : string;
      substitutions : (string * string) list;
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

    let rec build_seg (id, type_, content) : unify_seg =
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
        let substitutions = asrt_report.subst |> Subst.to_list_pp in
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
              build_seg (next_id, type_, content)
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

    type cmd_data = {
      id : rid;
      origin_id : int option; [@name "originId"]
      display : string;
      unifys : unifys;
      errors : string list;
    }
    [@@deriving yojson]

    type 'case t =
      | Nothing
      | Cmd of cmd_data * 'case t
      | BranchCmd of cmd_data * ('case * 'case t) list
      | FinalCmd of cmd_data
    [@@deriving yojson]

    type cmd_kind = Branch of branch_case list | Normal | Final
    [@@deriving yojson]

    type 'case with_source = (string * string) option * 'case t
    [@@deriving yojson]

    let kind_of_cases = function
      | Some cases -> Branch cases
      | None -> Normal

    let insert_cmd_sourceless
        cmd_kind
        new_id
        display
        ?(unifys = [])
        ?(errors = [])
        path
        origin_id
        map =
      let cmd_data = { id = new_id; display; unifys; errors; origin_id } in
      let new_cmd =
        match cmd_kind with
        | Normal -> Cmd (cmd_data, Nothing)
        | Branch cases ->
            let nexts = cases |> List.map (fun bc -> (bc, Nothing)) in
            BranchCmd (cmd_data, nexts)
        | Final -> FinalCmd cmd_data
      in

      let fail cur_path cur_map =
        DL.failwith
          (fun () ->
            [
              ("cmd_type", cmd_kind_to_yojson cmd_kind);
              ("new_id", rid_to_yojson new_id);
              ("display", `String display);
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

    let insert_cmd
        cmd_kind
        new_id
        display
        ?(unifys = [])
        ?(errors = [])
        path
        new_source
        origin_id
        ((source, map) : 'a with_source) =
      let insert () =
        insert_cmd_sourceless cmd_kind new_id display ~unifys ~errors path
          origin_id map
      in

      match source with
      | None -> (Some new_source, insert ())
      | Some source ->
          if new_source = source then (Some source, insert ())
          else
            let path, pid = source in
            let new_path, new_pid = new_source in
            DL.log (fun m ->
                m
                  ~json:
                    [
                      ("path", `String path);
                      ("new_path", `String new_path);
                      ("pid", `String pid);
                      ("new_pid", `String new_pid);
                    ]
                  "ExecMap.insert_cmd: didn't insert id %a due to differing \
                   source"
                  pp_rid new_id);
            (Some source, map)

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

    let at_path ?(stop_early = false) path map =
      let rec aux path map =
        match (map, path) with
        | Cmd (_, next), _ when not stop_early -> aux path next
        | map, [] -> Some map
        | Cmd (_, next), _ -> aux path next
        | BranchCmd (_, nexts), case :: path ->
            nexts
            |> List.find_map (fun (next_case, map) ->
                   if case = next_case then aux path map else None)
        | (Nothing | FinalCmd _), _ :: _ -> None
      in
      match aux (List.rev path) map with
      | None ->
          DL.failwith
            (fun () ->
              [
                ("path", branch_path_to_yojson path);
                ("map", to_yojson branch_case_to_yojson map);
              ])
            "ExecMap.at_path: malformed request"
      | Some map -> map

    let find_unfinished path (_, map) =
      let rec aux prev_id branch_case = function
        | FinalCmd _ -> None
        | Nothing -> Some (prev_id, branch_case)
        | Cmd ({ id; _ }, next) -> aux id None next
        | BranchCmd ({ id; _ }, nexts) ->
            nexts |> List.find_map (fun (case, next) -> aux id (Some case) next)
      in
      let submap = map |> at_path ~stop_early:true path in
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
          Cmd (cmd_data, next)
      | BranchCmd (cmd_data, nexts) ->
          let nexts =
            nexts
            |> List.map (fun (case, next) ->
                   (PackagedBranchCase.from case, package next))
          in
          BranchCmd (cmd_data, nexts)
      | FinalCmd cmd_data -> FinalCmd cmd_data

    let id_of = function
      | Nothing -> None
      | Cmd ({ id; _ }, _) | BranchCmd ({ id; _ }, _) | FinalCmd { id; _ } ->
          Some id

    let lift ~tl_ast exec_map =
      DL.log (fun m -> m "Lifting");
      let lift_data cmd_data =
        let new_display =
          Lifter.get_origin_node_str tl_ast cmd_data.origin_id
        in
        let new_displa =
          if new_display = "No info!" then cmd_data.display else new_display
        in
        { cmd_data with display = new_displa }
      in
      let origin_id = function
        | Nothing -> None
        | Cmd (data, _) | FinalCmd data | BranchCmd (data, _) -> data.origin_id
      in
      let eq_opt ida idb =
        match (ida, idb) with
        | Some ida, Some idb -> Int.equal ida idb
        | _ -> false
      in
      let merge_data ~left ~right =
        (* Note that we don't need to lift left here, we only need to lift right
           before calling this. *)
        (* This is a tad problematic: in case left has a unification but not right,
           the report id kept is the one of right, and it's going to be hard to look for unification there.
           We keep the left one, because that's what we might require to click on the next possible branches.
           On the other hand, if both have a unify, then I don't know what to do at all.
           I suggest, we change cmd_data with
           {
             last_id: L.ReportId.t;
             unify_ids : L.ReportId.t list // if empty, then has_unify is false
             display;
             origin_id;
           }
           In any case, this should be done after the report deadline.
        *)
        let origin_id =
          match (left.origin_id, right.origin_id) with
          | _, Some id | Some id, _ -> Some id
          | _ -> None
        in
        {
          right with
          unifys = left.unifys @ right.unifys;
          errors = left.errors @ right.errors;
          origin_id;
        }
      in
      let rec aux = function
        | Nothing -> Nothing
        | Cmd (data, next) -> (
            let next = aux next in
            if not (eq_opt data.origin_id (origin_id next)) then
              Cmd (lift_data data, next)
            else
              match next with
              | Cmd (data', next') ->
                  Cmd (merge_data ~left:data ~right:data', next')
              | FinalCmd data' -> FinalCmd (merge_data ~left:data ~right:data')
              | BranchCmd (data', branches) ->
                  BranchCmd (merge_data ~left:data ~right:data', branches)
              | Nothing ->
                  failwith "unreachable! id cannot be equal when there is no id"
            )
        | BranchCmd (data, branches) ->
            let branches = List.map (fun (c, b) -> (c, aux b)) branches in
            if
              List.exists
                (fun (_, b) -> eq_opt (origin_id b) data.origin_id)
                branches
            then
              failwith "branch_cmd has the same id as its next: unhandled yet."
            else BranchCmd (lift_data data, branches)
        | FinalCmd data -> FinalCmd (lift_data data)
      in
      aux exec_map
  end

  type exec_map = branch_case ExecMap.with_source [@@deriving yojson]

  type debugger_state = {
    source_file : string;
    source_files : SourceFiles.t option;
    prog : Verification.prog_t;
    tl_ast : tl_ast option;
    tests : (string * Verification.t) list;
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
    mutable proc_name : string option;
    mutable unify_maps : (rid * UnifyMap.t) list;
  }

  module Inspect = struct
    type exec_map_pkg = PackagedBranchCase.t ExecMap.t [@@deriving yojson]
    type branch_path_pkg = PackagedBranchCase.t list [@@deriving yojson]

    let get_exec_map_pkg { exec_map = _, map; _ } = ExecMap.package map

    let get_proc_name dbg =
      match dbg.proc_name with
      | None ->
          let proc_name =
            let+ frame = List_utils.tl_opt dbg.frames in
            frame.name
          in
          dbg.proc_name <- proc_name;
          proc_name
      | name -> name

    type debug_state = {
      exec_map : exec_map_pkg; [@key "execMap"]
      lifted_exec_map : exec_map_pkg option; [@key "liftedExecMap"]
      current_cmd_id : rid; [@key "currentCmdId"]
      unifys : (rid * Unifier.unify_kind * UnifyMap.unify_result) list;
      proc_name : string; [@key "procName"]
    }
    [@@deriving yojson]

    let get_debug_state dbg : debug_state =
      let current_cmd_id = dbg.cur_report_id in
      let unifys = dbg.exec_map |> ExecMap.unifys_at_id current_cmd_id in
      let exec_map = get_exec_map_pkg dbg in
      let lifted_exec_map =
        match (Lifter.source_map_ability, dbg.tl_ast) with
        | true, Some tl_ast -> Some (ExecMap.lift ~tl_ast exec_map)
        | _ -> None
      in
      let proc_name =
        match dbg |> get_proc_name with
        | None -> "unknown proc"
        | Some proc_name -> proc_name
      in
      { exec_map; current_cmd_id; unifys; proc_name; lifted_exec_map }

    let get_unification unify_id dbg =
      match dbg.unify_maps |> List.assoc_opt unify_id with
      | Some map -> (unify_id, map)
      | None ->
          let map = UnifyMap.build unify_id in
          dbg.unify_maps <- (unify_id, map) :: dbg.unify_maps;
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

  let update_report_id_and_inspection_fields report_id (dbg : debugger_state) =
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
            dbg.cur_report_id <- report_id;
            let cmd =
              content |> Yojson.Safe.from_string
              |> Logging.ConfigReport.of_yojson |> Result.get_ok
            in
            dbg.frames <-
              call_stack_to_frames cmd.callstack cmd.proc_line dbg.prog;
            let lifted_scopes, variables =
              create_variables (Some cmd.state) (is_gil_file dbg.source_file)
            in
            let () = dbg.variables <- variables in
            let () =
              dbg.top_level_scopes <-
                List.concat [ lifted_scopes; top_level_scopes ]
            in
            (* TODO: fix *)
            (* let () = dbg.errors <- cmd_result.errors in *)
            let cur_cmd =
              match cmd.callstack with
              | [] -> None
              | (se : CallStack.stack_element) :: _ -> (
                  let proc = Prog.get_proc dbg.prog se.pid in
                  match proc with
                  | None -> None
                  | Some proc ->
                      let annot, _, cmd = proc.proc_body.(cmd.proc_line) in
                      Some (cmd, annot))
            in
            dbg.cur_cmd <- cur_cmd
        | _ as t ->
            Fmt.failwith
              "Debugger: don't know how to handle report of type '%s'!" t)

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
    let cont_func = Verification.verify_up_to_procs prog in
    let rec init_with_no_proc_init cont_func =
      match cont_func with
      | Verification.SAInterpreter.Finished _ ->
          failwith "HORROR: Shouldn't encounter Finished when debugging!"
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
                init_with_no_proc_init (cont_func ~path:[] ()))
              else
                let cmd =
                  Result.get_ok
                    (content |> Yojson.Safe.from_string
                   |> Logging.ConfigReport.of_yojson)
                in
                let cmd_display = cmd.cmd in
                let origin_id = Annot.get_origin_id cmd.annot in
                let tests = Verification.Debug.get_tests_for_prog prog in
                let map =
                  ExecMap.(
                    Nothing
                    |> insert_cmd_sourceless
                         (kind_of_cases new_branch_cases)
                         cur_report_id cmd_display branch_path origin_id)
                in
                let dbg =
                  ({
                     source_file = file_name;
                     source_files = source_files_opt;
                     top_level_scopes;
                     cont_func = Some cont_func;
                     prog;
                     tl_ast;
                     tests;
                     frames = [];
                     cur_report_id;
                     breakpoints = Hashtbl.create 0;
                     variables = Hashtbl.create 0;
                     errors = [];
                     cur_cmd = None;
                     exec_map = (None, map);
                     proc_name = None;
                     unify_maps = [];
                   }
                    : debugger_state)
                in
                let _ =
                  update_report_id_and_inspection_fields cur_report_id dbg
                in
                Ok dbg)
    in
    init_with_no_proc_init cont_func

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

  let jump_to_id id dbg =
    try
      DL.log (fun m -> m "Jumping to id %a" pp_rid id);
      dbg.exec_map |> snd |> ExecMap.path_of_id id |> ignore;
      update_report_id_and_inspection_fields id dbg;
      Ok ()
    with Failure msg -> Error msg

  let jump_to_start dbg =
    let result =
      let** root_id =
        ExecMap.id_of (snd dbg.exec_map)
        |> Option.to_result ~none:"Debugger.jump_to_start: exec map is Nothing!"
      in
      dbg |> jump_to_id root_id
    in
    match result with
    | Error msg -> failwith msg
    | Ok () -> ()

  let get_current_source dbg =
    let { source_path; name; _ } = List.hd dbg.frames in
    (source_path, name)

  let rec execute_step prev_id_in_frame ?branch_case ?prev_branch_path dbg =
    let open Verification.SAInterpreter in
    match dbg.cont_func with
    | None ->
        DL.log (fun m -> m "No cont_func; reached end");
        ReachedEnd
    | Some cont_func -> (
        DL.log (fun m ->
            m
              ~json:
                [
                  ("id", rid_to_yojson dbg.cur_report_id);
                  ( "map",
                    dbg.exec_map |> snd
                    |> ExecMap.to_yojson branch_case_to_yojson );
                ]
              "Grabbing path for step...");
        let branch_path =
          prev_branch_path
          |> Option_utils.or_else (fun () ->
                 dbg.exec_map |> snd |> ExecMap.path_of_id prev_id_in_frame)
        in
        let branch_path =
          match branch_case with
          | Some case -> case :: branch_path
          | None ->
              dbg.exec_map |> snd |> ExecMap.next_paths branch_path |> List.hd
        in
        DL.log (fun m ->
            m ~json:[ ("path", branch_path_to_yojson branch_path) ] "Got path");
        match cont_func ~path:branch_path () with
        | Finished _ ->
            dbg.cont_func <- None;
            failwith "HORROR: Shouldn't encounter Finished when debugging!"
        | EndOfBranch (result, cont_func) ->
            dbg.cont_func <- Some cont_func;
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
                  unify result proc_name prev_id dbg |> Option.to_list
                in
                update_report_id_and_inspection_fields prev_id dbg;
                let cmd_display = cmd.cmd in
                let origin_id = Annot.get_origin_id cmd.annot in
                dbg.exec_map <-
                  (dbg.exec_map
                  |> ExecMap.(
                       let source = get_current_source dbg in
                       insert_cmd Final prev_id cmd_display ~unifys ~errors
                         branch_path source origin_id))
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
                dbg.cont_func <- Some cont_func;
                let content, type_ =
                  Option.get @@ L.LogQueryer.get_report cur_report_id
                in
                if type_ = ContentType.proc_init then (
                  DL.log (fun m -> m "(execute_step) Skipping proc_init...");
                  execute_step prev_id_in_frame dbg)
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
                  execute_step ~prev_branch_path:branch_path cur_report_id dbg)
                else (
                  update_report_id_and_inspection_fields cur_report_id dbg;
                  ExecMap.(
                    let cmd =
                      Result.get_ok
                        (content |> Yojson.Safe.from_string
                       |> Logging.ConfigReport.of_yojson)
                    in
                    let cmd_display = cmd.cmd in
                    let origin_id = Annot.get_origin_id cmd.annot in
                    let cmd_kind =
                      match new_branch_cases with
                      | Some cases -> Branch cases
                      | None -> Normal
                    in
                    let source = get_current_source dbg in
                    let unifys =
                      (DL.log (fun m ->
                           m "getting unify_result for %a" pp_rid cur_report_id);
                       let+ unify_id, _ =
                         L.LogQueryer.get_unify_for cur_report_id
                       in
                       let unify_map =
                         match dbg.unify_maps |> List.assoc_opt unify_id with
                         | Some map -> map
                         | None ->
                             let map = UnifyMap.build unify_id in
                             dbg.unify_maps <- (unify_id, map) :: dbg.unify_maps;
                             map
                       in
                       let result = unify_map |> UnifyMap.result in
                       (unify_id, fst unify_map, result))
                      |> Option.to_list
                    in
                    dbg.exec_map <-
                      dbg.exec_map
                      |> insert_cmd cmd_kind cur_report_id cmd_display ~unifys
                           branch_path source origin_id);
                  Step)))

  let step_in_branch_case prev_id_in_frame ?branch_case ?(reverse = false) dbg =
    let stop_reason =
      if reverse then (
        let prev_report_id =
          L.LogQueryer.get_previous_report_id dbg.cur_report_id
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
              update_report_id_and_inspection_fields prev_report_id dbg;
              Step))
      else
        let next_report_ids =
          L.LogQueryer.get_next_report_ids dbg.cur_report_id
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
            execute_step ?branch_case prev_id_in_frame dbg
        | Some next_report_id ->
            DL.show_report next_report_id "Next report ID found; not executing";
            update_report_id_and_inspection_fields next_report_id dbg;
            Step
    in
    if has_hit_breakpoint dbg then Breakpoint
    else if List.length dbg.errors > 0 then
      let () = dbg.cont_func <- None in
      ExecutionError
    else stop_reason

  let step_in ?(reverse = false) dbg =
    step_in_branch_case dbg.cur_report_id ?branch_case:None ~reverse dbg

  let rec step_until_cond
      ?(reverse = false)
      ?(branch_case : branch_case option)
      (prev_id_in_frame : rid)
      (prev_frame : frame)
      (prev_stack_depth : int)
      (cond : frame -> frame -> int -> int -> bool)
      (dbg : debugger_state) : stop_reason =
    let stop_reason =
      step_in_branch_case ~reverse ?branch_case prev_id_in_frame dbg
    in
    match stop_reason with
    | Step ->
        let cur_frame =
          match dbg.frames with
          | [] -> failwith "Nothing in call stack, cannot step"
          | cur_frame :: _ -> cur_frame
        in
        let cur_stack_depth = List.length dbg.frames in
        if cond prev_frame cur_frame prev_stack_depth cur_stack_depth then
          stop_reason
        else
          step_until_cond ~reverse prev_id_in_frame prev_frame prev_stack_depth
            cond dbg
    | other_stop_reason -> other_stop_reason

  let step_case ?(reverse = false) ?branch_case dbg =
    let frame =
      match dbg.frames with
      | [] -> failwith "Nothing in call stack, cannot step"
      | frame :: _ -> frame
    in
    let cond =
      if is_gil_file dbg.source_file then
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
    step_until_cond ~reverse ?branch_case dbg.cur_report_id frame
      (List.length dbg.frames) cond dbg

  let step ?(reverse = false) dbg = step_case ~reverse dbg

  let step_specific (branch_case : PackagedBranchCase.t option) prev_id dbg =
    let** branch_case =
      branch_case
      |> Option.map PackagedBranchCase.unpackage
      |> Option_utils.to_result
    in
    let++ () = dbg |> jump_to_id prev_id in
    step_case ?branch_case dbg

  let step_out dbg =
    let rec step_out stack_depth dbg =
      let stop_reason = step_in dbg in
      match stop_reason with
      | Step ->
          if List.length dbg.frames < stack_depth then stop_reason
          else step_out stack_depth dbg
      | other_stop_reason -> other_stop_reason
    in
    step_out (List.length dbg.frames) dbg

  let run ?(reverse = false) ?(launch = false) dbg =
    let current_id = dbg.cur_report_id in
    let branch_path = dbg.exec_map |> snd |> ExecMap.path_of_id current_id in
    DL.log (fun m ->
        m
          ~json:
            [
              ("current_id", rid_to_yojson current_id);
              ("path", branch_path_to_yojson branch_path);
              ( "map",
                dbg.exec_map |> snd |> ExecMap.to_yojson branch_case_to_yojson
              );
            ]
          "Debugger.run");
    let rec aux ?(launch = false) count branch_case =
      if count > 20 then failwith "Debugger.run: infinite loop?";
      (* We need to check if a breakpoint has been hit if run is called
         immediately after launching to prevent missing a breakpoint on the first
         line *)
      if launch && has_hit_breakpoint dbg then Breakpoint
      else
        let stop_reason = step_case ?branch_case ~reverse dbg in
        match stop_reason with
        | Step -> aux count None
        | Breakpoint -> Breakpoint
        | other_stop_reason -> (
            if reverse then other_stop_reason
            else
              match dbg.exec_map |> ExecMap.find_unfinished branch_path with
              | None ->
                  DL.log (fun m ->
                      m
                        ~json:[ ("map", exec_map_to_yojson dbg.exec_map) ]
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
                  dbg |> jump_to_id prev_id |> Result.get_ok;
                  aux (count + 1) branch_case)
    in
    aux ~launch 0 None

  let terminate dbg =
    let () = Verification.postprocess_files dbg.source_files in
    let () = if !Config.stats then Statistics.print_statistics () in
    L.wrap_up ()

  let get_frames dbg = dbg.frames
  let get_scopes dbg = dbg.top_level_scopes

  let get_variables (var_ref : int) (dbg : debugger_state) : variable list =
    match Hashtbl.find_opt dbg.variables var_ref with
    | None -> []
    | Some vars -> vars

  let get_exception_info (dbg : debugger_state) =
    let error = List.hd dbg.errors in
    let non_mem_exception_info =
      { id = Fmt.to_to_string Logging.pp_err error; description = None }
    in
    match error with
    | ExecErr.ESt state_error -> (
        match state_error with
        | StateErr.EMem merr ->
            Lifter.memory_error_to_exception_info
              { error = merr; command = dbg.cur_cmd; tl_ast = dbg.tl_ast }
        | _ -> non_mem_exception_info)
    | _ -> non_mem_exception_info

  let set_breakpoints source bp_list dbg =
    match source with
    (* We can't set the breakpoints if we do not know the source file *)
    | None -> ()
    | Some source ->
        let bp_set = Breakpoints.of_list bp_list in
        Hashtbl.replace dbg.breakpoints source bp_set
end
