module L = Logging
module DL = Debugger_log
module Gil_to_tl_lifter = Gil_to_tl_lifter
module DebuggerTypes = DebuggerTypes
module DebuggerUtils = DebuggerUtils
module ExecMap = ExecMap
open DebuggerTypes
open Syntaxes.Option

type rid = L.ReportId.t [@@deriving show, yojson]
type branch_case = BranchCase.t [@@deriving yojson]

let ( let** ) = Result.bind
let ( let++ ) f o = Result.map o f

module type S = sig
  type tl_ast
  type debug_state

  module UnifyMap : sig
    type t [@@deriving yojson]
  end

  module Inspect : sig
    type debug_state_view [@@deriving yojson]

    val get_debug_state : debug_state -> debug_state_view
    val get_unification : rid -> debug_state -> rid * UnifyMap.t
  end

  val launch : string -> string option -> (debug_state, string) result
  val jump_to_id : rid -> debug_state -> (unit, string) result
  val jump_to_start : debug_state -> unit
  val step_in : ?reverse:bool -> debug_state -> stop_reason
  val step : ?reverse:bool -> debug_state -> stop_reason

  val step_specific :
    ExecMap.Packaged.branch_case option ->
    Logging.ReportId.t ->
    debug_state ->
    (stop_reason, string) result

  val step_out : debug_state -> stop_reason
  val run : ?reverse:bool -> ?launch:bool -> debug_state -> stop_reason
  val terminate : debug_state -> unit
  val get_frames : debug_state -> frame list
  val get_scopes : debug_state -> scope list
  val get_variables : int -> debug_state -> variable list
  val get_exception_info : debug_state -> exception_info
  val set_breakpoints : string option -> int list -> debug_state -> unit
end

module Make
    (PC : ParserAndCompiler.S)
    (Verification : Verifier.S)
    (Lifter : Gil_to_tl_lifter.S
                with type memory = Verification.SAInterpreter.heap_t
                 and type memory_error = Verification.SPState.m_err_t
                 and type tl_ast = PC.tl_ast
                 and type cmd_report =
                  Verification.SAInterpreter.Logging.ConfigReport.t) =
struct
  open L.LoggingConstants
  open Verification.SAInterpreter
  module Breakpoints = Set.Make (Int)

  type breakpoints = (string, Breakpoints.t) Hashtbl.t
  type tl_ast = PC.tl_ast

  module UnifyMap = UnifyMap.Make (Verification)

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
    mutable proc_name : string option;
    mutable unify_maps : (rid * UnifyMap.t) list;
    lifter_state : Lifter.t;
    report_state : L.ReportState.t;
  }

  type debug_cfg = {
    source_file : string;
    source_files : SourceFiles.t option;
    prog : Verification.prog_t;
    tl_ast : tl_ast option;
    tests : (string * Verification.t) list;
    main_proc_name : string;
    report_state_base : L.ReportState.t;
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
    if activate_report_state then L.ReportState.activate proc_state.report_state;
    proc_state

  module Inspect = struct
    type debug_proc_state_view = {
      exec_map : ExecMap.Packaged.t; [@key "execMap"]
      lifted_exec_map : ExecMap.Packaged.t option; [@key "liftedExecMap"]
      current_cmd_id : rid; [@key "currentCmdId"]
      unifys : ExecMap.unifys;
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
      DL.log (fun m -> m "Getting debug state");
      let procs =
        Hashtbl.fold
          (fun proc_name state acc ->
            let current_cmd_id = state.cur_report_id in
            let unifys =
              state.lifter_state |> Lifter.get_unifys_at_id current_cmd_id
            in
            let exec_map = state.lifter_state |> Lifter.get_gil_map in
            let lifted_exec_map = state.lifter_state |> Lifter.get_lifted_map in
            let proc =
              { exec_map; lifted_exec_map; current_cmd_id; unifys; proc_name }
            in
            (proc_name, proc) :: acc)
          dbg.procs []
      in
      DL.log (fun m -> m "Got debug state");
      {
        main_proc_name = dbg.cfg.main_proc_name;
        current_proc_name = dbg.cur_proc_name;
        procs;
      }

    let get_unification unify_id dbg =
      let state = dbg |> get_proc_state in
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

  let rec execute_step prev_id_in_frame ?branch_case ?branch_path dbg state =
    let open Verification.SAInterpreter in
    let { cfg; _ } = dbg in
    match state.cont_func with
    | None ->
        DL.log (fun m -> m "No cont_func; reached end");
        (ReachedEnd, None)
    | Some cont_func -> (
        DL.log (fun m ->
            m
              ~json:
                [
                  ("id", rid_to_yojson prev_id_in_frame);
                  ("lifter_state", state.lifter_state |> Lifter.dump);
                ]
              "Grabbing path for step...");
        let branch_path =
          match branch_path with
          | Some path -> path
          | None ->
              state.lifter_state
              |> Lifter.select_next_path branch_case prev_id_in_frame
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
            | Some (prev_id, content, type_) when type_ = ContentType.cmd -> (
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
                let exec_data =
                  Gil_to_tl_lifter.make_executed_cmd_data ExecMap.Final prev_id
                    cmd ~unifys ~errors branch_path
                in
                let handler_result =
                  state.lifter_state |> Lifter.handle_cmd exec_data
                in
                match handler_result with
                | Stop -> ()
                | _ ->
                    DL.log (fun m ->
                        m
                          ~json:
                            [
                              ( "handler_result",
                                Gil_to_tl_lifter.handle_cmd_result_to_yojson
                                  handler_result );
                              ("lifter_state", Lifter.dump state.lifter_state);
                            ]
                          "HORROR: Lifter didn't give Stop for Final cmd!"))
            | Some (prev_id, _, type_) ->
                Fmt.failwith "EndOfBranch: prev cmd (%a) is '%s', not '%s'!"
                  pp_rid prev_id type_ ContentType.cmd
            | None ->
                Fmt.failwith "EndOfBranch: prev id '%a' doesn't exist!" pp_rid
                  prev_id_in_frame);
            (ReachedEnd, None)
        | Continue (cur_report_id, branch_path, new_branch_cases, cont_func)
          -> (
            DL.to_file "CONTINUE";
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
                  state |> execute_step ~branch_path cur_report_id dbg)
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
                    let exec_data =
                      Gil_to_tl_lifter.make_executed_cmd_data cmd_kind
                        cur_report_id cmd ~unifys branch_path
                    in
                    let handler_result =
                      state.lifter_state |> Lifter.handle_cmd exec_data
                    in
                    match handler_result with
                    | ExecNext (id, branch_case) ->
                        let id = id |> Option.value ~default:cur_report_id in
                        execute_step id ?branch_case dbg state
                    | Stop -> (Step, Some cur_report_id)))))

  let launch_proc dbg proc_name =
    let { cfg; _ } = dbg in
    let report_state = L.ReportState.clone cfg.report_state_base in
    let open Verification.SAInterpreter in
    let rec aux = function
      | Finished _ ->
          Error "HORROR: Shouldn't encounter Finished when debugging!"
      | EndOfBranch _ -> Error "Nothing to run"
      | Continue (cur_report_id, branch_path, new_branch_cases, cont_func) -> (
          match cur_report_id with
          | None ->
              raise
                (Failure
                   "Did not log report. Check the logging level is set \
                    correctly")
          | Some id ->
              let content, type_ = Option.get @@ L.LogQueryer.get_report id in
              if type_ = ContentType.proc_init then (
                DL.log (fun m -> m "Debugger.launch: Skipping proc_init...");
                aux (cont_func ~path:[] ()))
              else
                let cmd =
                  Result.get_ok
                    (content |> Yojson.Safe.from_string
                   |> Logging.ConfigReport.of_yojson)
                in
                let lifter_state, handler_result =
                  let kind =
                    ExecMap.kind_of_cases
                    @@ Option.value ~default:[] new_branch_cases
                  in
                  let exec_data =
                    Gil_to_tl_lifter.make_executed_cmd_data kind id cmd
                      branch_path
                  in
                  Lifter.init exec_data
                in
                let state =
                  {
                    cont_func = Some cont_func;
                    breakpoints = Hashtbl.create 0;
                    cur_report_id = id;
                    top_level_scopes;
                    frames = [];
                    variables = Hashtbl.create 0;
                    errors = [];
                    cur_cmd = None;
                    proc_name = None;
                    unify_maps = [];
                    lifter_state;
                    report_state;
                  }
                in
                let id =
                  match handler_result with
                  | Stop -> id
                  | ExecNext (id', branch_case) ->
                      let id = id' |> Option.value ~default:id in
                      execute_step ?branch_case id dbg state
                      |> snd |> Option.value ~default:id
                in
                state |> update_report_id_and_inspection_fields id cfg;
                Ok state)
    in
    Config.Verification.(
      let procs_to_verify = !procs_to_verify in
      if not (procs_to_verify |> List.mem proc_name) then
        set_procs_to_verify (procs_to_verify @ [ proc_name ]));
    report_state
    |> L.ReportState.with_state (fun () ->
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
        report_state_base = L.ReportState.(clone global_state);
      }
    in
    let dbg = { cfg; procs = Hashtbl.create 0; cur_proc_name = proc_name } in
    let++ main_proc_state = launch_proc dbg proc_name in
    main_proc_state.report_state |> L.ReportState.activate;
    Hashtbl.add dbg.procs proc_name main_proc_state;
    dbg

  let jump_state_to_id id cfg state =
    try
      DL.log (fun m -> m "Jumping to id %a" pp_rid id);
      (* state.exec_map |> snd |> ExecMap.path_of_id id |> ignore; *)
      (* TODO *)
      state |> update_report_id_and_inspection_fields id cfg;
      Ok ()
    with Failure msg -> Error msg

  let jump_to_id id dbg =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state in
    state |> jump_state_to_id id cfg

  let jump_to_start dbg =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state in
    let result =
      let** root_id =
        state.lifter_state |> Lifter.get_root_id
        |> Option.to_result ~none:"Debugger.jump_to_start: No root id found!"
      in
      state |> jump_state_to_id root_id cfg
    in
    match result with
    | Error msg -> failwith msg
    | Ok () -> ()

  let step_in_branch_case
      prev_id_in_frame
      ?branch_case
      ?(reverse = false)
      dbg
      state =
    let { cfg; _ } = dbg in
    let stop_reason =
      if reverse then (
        match
          state.lifter_state |> Lifter.previous_step state.cur_report_id
        with
        | None -> ReachedStart
        | Some (prev_id, _) ->
            state |> update_report_id_and_inspection_fields prev_id cfg;
            Step)
      else
        let next_id =
          match state.lifter_state |> Lifter.next_steps state.cur_report_id with
          | [] -> None
          | nexts -> (
              match branch_case with
              | Some branch_case ->
                  let+ id, _ =
                    List.find_opt
                      (fun (_, bc) -> bc |> Option.get = branch_case)
                      nexts
                  in
                  id
              | None -> Some (List.hd nexts |> fst))
        in
        match next_id with
        | None ->
            DL.log (fun m -> m "No next report ID; executing next step");
            state |> execute_step ?branch_case prev_id_in_frame dbg |> fst
        | Some id ->
            DL.show_report id "Next report ID found; not executing";
            state |> update_report_id_and_inspection_fields id cfg;
            Step
    in
    if has_hit_breakpoint state then Breakpoint
    else if List.length state.errors > 0 then
      let () = state.cont_func <- None in
      ExecutionError
    else stop_reason

  let step_in_state ?(reverse = false) cfg state =
    step_in_branch_case state.cur_report_id ?branch_case:None ~reverse cfg state

  let step_in ?(reverse = false) dbg =
    let state = dbg |> get_proc_state in
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

  let step ?(reverse = false) dbg =
    let state = dbg |> get_proc_state in
    step_case ~reverse dbg state

  let step_specific branch_case prev_id dbg =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state in
    let id, branch_case =
      state.lifter_state |> Lifter.next_step_specific prev_id branch_case
    in
    let++ () = state |> jump_state_to_id id cfg in
    state |> step_case ?branch_case dbg

  let step_out dbg =
    let state = dbg |> get_proc_state in
    let rec aux stack_depth =
      let stop_reason = state |> step_in_state dbg in
      match stop_reason with
      | Step ->
          if List.length state.frames < stack_depth then stop_reason
          else aux stack_depth
      | other_stop_reason -> other_stop_reason
    in
    aux (List.length state.frames)

  let run ?(reverse = false) ?(launch = false) dbg =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state in
    let current_id = state.cur_report_id in
    let branch_path = state.lifter_state |> Lifter.path_of_id current_id in
    DL.log (fun m ->
        m
          ~json:
            [
              ("current_id", rid_to_yojson current_id);
              ("path", branch_path_to_yojson branch_path);
              ("lifter_state", state.lifter_state |> Lifter.dump);
            ]
          "Debugger.run");
    let rec aux ?(launch = false) count =
      if count > 100 then failwith "Debugger.run: infinite loop?";
      (* We need to check if a breakpoint has been hit if run is called
         immediately after launching to prevent missing a breakpoint on the first
         line *)
      if launch && has_hit_breakpoint state then Breakpoint
      else
        let unfinished =
          state.lifter_state |> Lifter.find_unfinished_path ~at_path:branch_path
        in
        match unfinished with
        | None ->
            DL.log (fun m -> m "Debugger.run: map has no unfinished branches");
            ReachedEnd
        | Some (prev_id, branch_case) -> (
            state |> jump_state_to_id prev_id cfg |> Result.get_ok;
            let stop_reason = step_case ?branch_case ~reverse dbg state in
            match stop_reason with
            | Step -> aux count
            | Breakpoint -> Breakpoint
            | other_stop_reason ->
                if reverse then other_stop_reason else aux (count + 1))
    in
    aux ~launch 0

  let terminate dbg =
    L.ReportState.(activate global_state);
    Verification.postprocess_files dbg.cfg.source_files;
    if !Config.stats then Statistics.print_statistics ();
    L.wrap_up ()

  let get_frames dbg =
    let state = dbg |> get_proc_state in
    state.frames

  let get_scopes dbg =
    let state = dbg |> get_proc_state in
    state.top_level_scopes

  let get_variables (var_ref : int) (dbg : debug_state) : variable list =
    let state = dbg |> get_proc_state in
    match Hashtbl.find_opt state.variables var_ref with
    | None -> []
    | Some vars -> vars

  let get_exception_info (dbg : debug_state) =
    let { cfg; _ } = dbg in
    let state = dbg |> get_proc_state in
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

  let set_breakpoints source bp_list dbg =
    let state = dbg |> get_proc_state in
    match source with
    (* We can't set the breakpoints if we do not know the source file *)
    | None -> ()
    | Some source ->
        let bp_set = Breakpoints.of_list bp_list in
        Hashtbl.replace state.breakpoints source bp_set
end
