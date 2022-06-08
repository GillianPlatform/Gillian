module L = Logging
module DL = Debugger_log
module Gil_to_tl_lifter = Gil_to_tl_lifter
module DebuggerTypes = DebuggerTypes
module DebuggerUtils = DebuggerUtils
open DebuggerTypes
open Syntaxes.Option

let ( let** ) = Result.bind
let ( let++ ) f o = Result.map o f

module type S = sig
  type tl_ast
  type debugger_state

  module PackagedBranchCase : sig
    type t [@@deriving yojson]
  end

  module ExecMap : sig
    type 'a t [@@deriving yojson]
  end

  module Inspect : sig
    type debug_state [@@deriving yojson]

    val get_debug_state : debugger_state -> debug_state
  end

  val launch : string -> string option -> (debugger_state, string) result
  val jump_to_id : L.ReportId.t -> debugger_state -> (unit, string) result
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

  module ExecMap = struct
    type 'case t =
      | Nothing
      | Cmd of { id : L.ReportId.t; display : string; next : 'case t }
      | BranchCmd of {
          id : L.ReportId.t;
          display : string;
          nexts : ('case * 'case t) list;
        }
      | FinalCmd of { id : L.ReportId.t; display : string }
    [@@deriving yojson]

    type cmd_kind = Branch of branch_case list | Normal | Final
    [@@deriving yojson]

    type 'case with_source = string option * 'case t

    let kind_of_cases = function
      | Some cases -> Branch cases
      | None -> Normal

    let insert_cmd_sourceless cmd_kind new_id display path map =
      let fail () =
        DL.failwith
          (fun () ->
            [
              ("cmd_type", cmd_kind_to_yojson cmd_kind);
              ("path", branch_path_to_yojson path);
              ("new_id", L.ReportId.to_yojson new_id);
              ("display", `String display);
              ("map", to_yojson branch_case_to_yojson map);
            ])
          "ExecMap.insert_cmd: malformed request"
      in

      let rec aux path map =
        match (map, path) with
        | Nothing, [] -> (
            match cmd_kind with
            | Branch branch_cases ->
                BranchCmd
                  {
                    id = new_id;
                    display;
                    nexts = branch_cases |> List.map (fun bc -> (bc, Nothing));
                  }
            | Final -> FinalCmd { id = new_id; display }
            | Normal -> Cmd { id = new_id; display; next = Nothing })
        | Cmd { id; display; next }, _ ->
            let next = aux path next in
            Cmd { id; display; next }
        | BranchCmd { id; display; nexts }, case :: path -> (
            let new_nexts =
              nexts
              |> List_utils.replace_assoc_opt case (fun map -> aux path map)
            in
            match new_nexts with
            | None -> fail ()
            | Some nexts -> BranchCmd { id; display; nexts })
        | _ -> fail ()
      in
      aux path map

    let insert_cmd
        cmd_kind
        new_id
        display
        path
        new_source
        ((source, map) : 'a with_source) =
      match source with
      | None ->
          ( Some new_source,
            insert_cmd_sourceless cmd_kind new_id display path map )
      | Some source ->
          if new_source == source then
            (Some source, insert_cmd_sourceless cmd_kind new_id display path map)
          else (
            DL.log (fun m -> m "TRIED TO INSERT %a" L.ReportId.pp new_id);
            (Some source, map))

    let path_of_id_opt selected_id =
      let rec aux acc = function
        | Nothing -> None
        | Cmd { id; next; _ } ->
            if id = selected_id then Some acc else aux acc next
        | BranchCmd { id; nexts; _ } ->
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
        | Cmd { next; _ }, _ when not stop_early -> aux path next
        | map, [] -> Some map
        | Cmd { next; _ }, _ -> aux path next
        | BranchCmd { nexts; _ }, case :: path ->
            nexts
            |> List.find_map (fun (next_case, map) ->
                   if case = next_case then aux path map else None)
        | (Nothing | FinalCmd _), _ :: _ -> None
      in
      match aux path map with
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
        | Cmd { id; next; _ } -> aux id None next
        | BranchCmd { id; nexts; _ } ->
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
      | Cmd { id; next; _ } -> aux id None next
      | BranchCmd { id; nexts; _ } ->
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
      | BranchCmd { nexts; _ } ->
          nexts |> List.map (fun (case, _) -> case :: path)

    let path_of_id id map =
      match path_of_id_opt id map with
      | Some path -> path
      | None -> Fmt.failwith "ID %a doesn't exist in exec map!" L.ReportId.pp id

    let rec package = function
      | Nothing -> Nothing
      | Cmd { id; display; next } ->
          let next = package next in
          Cmd { id; display; next }
      | BranchCmd { id; display; nexts } ->
          let nexts =
            nexts
            |> List.map (fun (case, next) ->
                   (PackagedBranchCase.from case, package next))
          in
          BranchCmd { id; display; nexts }
      | FinalCmd id -> FinalCmd id
  end

  type exec_map = branch_case ExecMap.with_source

  type debugger_state = {
    source_file : string;
    source_files : SourceFiles.t option;
    prog : Verification.prog_t;
    tl_ast : tl_ast option;
    tests : (string * Verification.t) list;
    mutable cont_func : result_t cont_func_f option;
    mutable breakpoints : breakpoints;
    mutable cur_report_id : L.ReportId.t;
    (* TODO: The below fields only depend on the
             cur_report_id and could be refactored to use this *)
    mutable top_level_scopes : scope list;
    mutable frames : frame list;
    mutable variables : variables;
    mutable errors : err_t list;
    mutable cur_cmd : (int Cmd.t * Annot.t) option;
    mutable exec_map : exec_map;
    mutable proc_name : string option;
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
      current_cmd_id : L.ReportId.t; [@key "currentCmdId"]
      proc_name : string; [@key "procName"]
    }
    [@@deriving yojson]

    let get_debug_state dbg : debug_state =
      let current_cmd_id = dbg.cur_report_id in
      let exec_map = get_exec_map_pkg dbg in
      let proc_name =
        match dbg |> get_proc_name with
        | None -> "unknown proc"
        | Some proc_name -> proc_name
      in
      { exec_map; current_cmd_id; proc_name }
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
          L.ReportId.pp report_id
    | Some (content, type_) -> (
        DL.show_report report_id ("Debugger.update...: Got report type " ^ type_);
        match type_ with
        | t when t = ContentType.cmd ->
            dbg.cur_report_id <- report_id;
            let cmd =
              content |> Yojson.Safe.from_string
              |> Logging.ConfigReport.of_yojson
            in
            let cmd =
              match cmd with
              | Ok cmd -> cmd
              | Error _ -> failwith "Invalid cmd content!"
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
                let tests = Verification.Debug.get_tests_for_prog prog in
                let map =
                  ExecMap.(
                    Nothing
                    |> insert_cmd_sourceless
                         (kind_of_cases new_branch_cases)
                         cur_report_id cmd_display branch_path)
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
        match L.LogQueryer.get_unification_for prev_id with
        | Some _ ->
            DL.log (fun m ->
                m "Unification for %a already exists; skipping unify"
                  L.ReportId.pp prev_id)
        | None -> (
            DL.log (fun m -> m "Unifying result for %a" L.ReportId.pp prev_id);
            let _ = Verification.Debug.analyse_result test prev_id result in
            match L.LogQueryer.get_unification_for prev_id with
            | None -> failwith "No unify report found!"
            | Some _ -> ()))

  let jump_to_id id dbg =
    try
      DL.log (fun m -> m "Jumping to id %a" L.ReportId.pp id);
      dbg.exec_map |> snd |> ExecMap.path_of_id id |> ignore;
      update_report_id_and_inspection_fields id dbg;
      Ok ()
    with Failure msg -> Error msg

  let jump_to_start dbg =
    let result =
      let** root_id =
        match dbg.exec_map |> snd with
        | Nothing -> Error "Debugger.jump_to_start: exec map is Nothing!"
        | Cmd { id; _ } | BranchCmd { id; _ } | FinalCmd { id; _ } -> Ok id
      in
      dbg |> jump_to_id root_id
    in
    match result with
    | Error msg -> failwith msg
    | Ok () -> ()

  let rec execute_step prev_id_in_frame ?branch_case dbg =
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
                  ("id", L.ReportId.to_yojson dbg.cur_report_id);
                  ( "map",
                    dbg.exec_map |> snd
                    |> ExecMap.to_yojson branch_case_to_yojson );
                ]
              "Grabbing path for step...");
        let branch_path =
          dbg.exec_map |> snd |> ExecMap.path_of_id prev_id_in_frame
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
                unify result proc_name prev_id dbg;
                update_report_id_and_inspection_fields prev_id dbg
                (* DL.log (fun m -> m "--- premature step ---"); (unify_id) |> ignore *)
            | Some (prev_id, _, type_) ->
                Fmt.failwith "EndOfBranch: prev cmd (%a) is '%s', not '%s'!"
                  L.ReportId.pp prev_id type_ ContentType.cmd
            | None ->
                Fmt.failwith "EndOfBranch: prev id '%a' doesn't exist!"
                  L.ReportId.pp prev_id_in_frame);
            ReachedEnd
        | Continue (cur_report_id, branch_path, new_branch_cases, cont_func)
          -> (
            match cur_report_id with
            | None ->
                failwith
                  "Did not log report. Check the logging level is set correctly"
            | Some cur_report_id -> (
                dbg.cont_func <- Some cont_func;
                let content, type_ =
                  Option.get @@ L.LogQueryer.get_report cur_report_id
                in
                if type_ = ContentType.proc_init then (
                  DL.log (fun m -> m "(execute_step) Skipping proc_init...");
                  execute_step prev_id_in_frame dbg)
                else
                  match L.LogQueryer.get_cmd_results cur_report_id with
                  | [] ->
                      DL.log (fun m ->
                          m
                            "No results for cmd; assuming eob, stepping \
                             again...");
                      let cmd =
                        Result.get_ok
                          (content |> Yojson.Safe.from_string
                         |> Logging.ConfigReport.of_yojson)
                      in
                      let cmd_display = cmd.cmd in
                      dbg.exec_map <-
                        (dbg.exec_map
                        |> ExecMap.(
                             let source_file =
                               (List.hd dbg.frames).source_path
                             in
                             insert_cmd Final cur_report_id cmd_display
                               branch_path source_file));
                      execute_step cur_report_id dbg
                  | _ ->
                      update_report_id_and_inspection_fields cur_report_id dbg;
                      ExecMap.(
                        let cmd =
                          Result.get_ok
                            (content |> Yojson.Safe.from_string
                           |> Logging.ConfigReport.of_yojson)
                        in
                        let cmd_display = cmd.cmd in
                        let cmd_kind =
                          match new_branch_cases with
                          | Some cases -> Branch cases
                          | None -> Normal
                        in
                        let source_file = (List.hd dbg.frames).source_path in
                        dbg.exec_map <-
                          dbg.exec_map
                          |> insert_cmd cmd_kind cur_report_id cmd_display
                               branch_path source_file);
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
                         | Ok cmd -> Option_utils.eq cmd.branch_case branch_case
                         )
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
      (prev_id_in_frame : L.ReportId.t)
      (prev_frame : frame)
      (prev_stack_depth : int)
      (cond : frame -> frame -> int -> int -> bool)
      (dbg : debugger_state) : stop_reason =
    let stop_reason =
      step_in_branch_case ~reverse ?branch_case prev_id_in_frame dbg
    in
    match stop_reason with
    | Step -> (
        match dbg.frames with
        | [] -> failwith "Nothing in call stack, cannot step"
        | cur_frame :: _ ->
            let cur_stack_depth = List.length dbg.frames in
            if cond prev_frame cur_frame prev_stack_depth cur_stack_depth then
              stop_reason
            else
              step_until_cond ~reverse prev_id_in_frame prev_frame
                prev_stack_depth cond dbg)
    | other_stop_reason -> other_stop_reason

  let step_case ?(reverse = false) ?branch_case dbg =
    match dbg.frames with
    | [] -> failwith "Nothing in call stack, cannot step"
    | frame :: _ ->
        if is_gil_file dbg.source_file then
          (* If GIL file, step until next cmd in the same frame (like in regular
             debuggers) *)
          step_until_cond ~reverse ?branch_case dbg.cur_report_id frame
            (List.length dbg.frames)
            (fun prev_frame cur_frame prev_stack_depth cur_stack_depth ->
              cur_frame.source_path = prev_frame.source_path
              && cur_frame.name = prev_frame.name
              || cur_stack_depth < prev_stack_depth)
            dbg
        else
          (* If target language file, step until the code origin location is
             different, indicating an actual step in the target language*)
          step_until_cond ~reverse ?branch_case dbg.cur_report_id frame
            (List.length dbg.frames)
            (fun prev_frame cur_frame _ _ ->
              cur_frame.source_path = prev_frame.source_path
              && (cur_frame.start_line <> prev_frame.start_line
                 || cur_frame.start_column <> prev_frame.start_column
                 || cur_frame.end_line <> prev_frame.end_line
                 || cur_frame.end_column <> prev_frame.end_column))
            dbg

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
              ("current_id", L.ReportId.to_yojson current_id);
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
              | None -> other_stop_reason
              | Some (prev_id, branch_case) ->
                  DL.log (fun m ->
                      m
                        ~json:
                          [
                            ("prev_id", L.ReportId.to_yojson prev_id);
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
