include Debugger_intf
open Syntaxes.Option
module L = Logging
module DL = Debugger_log
module Lift = Debugger_lifter
module Exec_map = Exec_map

let ( let** ) = Result.bind
let ( let++ ) f o = Result.map o f

module Make : Make =
functor
  (ID : Init_data.S)
  (PC : ParserAndCompiler.S with type init_data = ID.t)
  (Verification : Verifier.S
                    with type SPState.init_data = ID.t
                     and type annot = PC.Annot.t)
  (Lifter : Lift.S
              with type memory = Verification.SAInterpreter.heap_t
               and type memory_error = Verification.SPState.m_err_t
               and type tl_ast = PC.tl_ast
               and type cmd_report =
                Verification.SAInterpreter.Logging.ConfigReport.t
               and type annot = PC.Annot.t)
  ->
  struct
    open L.LoggingConstants
    open Verification.SAInterpreter
    module Gil_parsing = Gil_parsing.Make (PC.Annot)
    module Breakpoints = Set.Make (Int)
    module Annot = PC.Annot

    type breakpoints = (string, Breakpoints.t) Hashtbl.t
    type tl_ast = PC.tl_ast
    type unify_result = Unify_map.unify_result = Success | Failure

    let build_unify_map =
      let module Build = Unify_map.Make_builder (Verification) in
      Build.f

    type debug_proc_state = {
      mutable cont_func : result_t cont_func_f option;
      mutable breakpoints : breakpoints; [@default Hashtbl.create 0]
      mutable cur_report_id : L.ReportId.t;
      (* TODO: The below fields only depend on the
               cur_report_id and could be refactored to use this *)
      mutable top_level_scopes : scope list;
      mutable frames : frame list;
      mutable variables : Variable.ts; [@default Hashtbl.create 0]
      mutable errors : err_t list;
      mutable cur_cmd : (int Cmd.t * Annot.t) option;
      mutable proc_name : string option;
      mutable unify_maps : (L.ReportId.t * Unify_map.t) list;
      lifter_state : Lifter.t;
      report_state : L.ReportState.t;
    }
    [@@deriving make]

    type debug_cfg = {
      source_file : string;
      source_files : SourceFiles.t option;
      prog : Verification.prog_t;
      tl_ast : tl_ast option;
      tests : (string * Verification.t) list;
      main_proc_name : string;
      report_state_base : L.ReportState.t;
      init_data : ID.t;
    }
    [@@deriving make]

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
      match Hashtbl.find_opt dbg.procs proc_name with
      | None -> Error ("get_proc_state: couldn't find proc " ^ proc_name)
      | Some proc_state ->
          if activate_report_state then
            L.ReportState.activate proc_state.report_state;
          Ok proc_state

    let get_proc_state_exn ?proc_name ?(activate_report_state = true) dbg =
      match get_proc_state ?proc_name ~activate_report_state dbg with
      | Ok proc_state -> proc_state
      | Error msg -> failwith msg

    module Inspect = struct
      type debug_proc_state_view = {
        exec_map : Exec_map.Packaged.t; [@key "execMap"]
        lifted_exec_map : Exec_map.Packaged.t option; [@key "liftedExecMap"]
        current_cmd_id : L.ReportId.t; [@key "currentCmdId"]
        unifys : Exec_map.unification list;
        proc_name : string; [@key "procName"]
      }
      [@@deriving yojson]

      let procs_to_yosjon procs : Yojson.Safe.t =
        let procs =
          procs
          |> List.map (fun (k, v) -> (k, debug_proc_state_view_to_yojson v))
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
              let lifted_exec_map =
                state.lifter_state |> Lifter.get_lifted_map
              in
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
        let state = dbg |> get_proc_state_exn in
        match state.unify_maps |> List.assoc_opt unify_id with
        | Some map -> (unify_id, map)
        | None ->
            let map = build_unify_map unify_id in
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

    let get_pure_formulae_vars (state : state_t) : Variable.t list =
      let open Variable in
      Verification.SPState.get_pfs state
      |> PFS.to_list
      |> List.map (fun formula ->
             let value = Fmt.to_to_string (Fmt.hbox Formula.pp) formula in
             { name = ""; value; type_ = None; var_ref = 0 })
      |> List.sort (fun v w -> Stdlib.compare v.value w.value)

    let get_typ_env_vars (state : state_t) : Variable.t list =
      let open Variable in
      let typ_env = Verification.SPState.get_typ_env state in
      TypEnv.to_list typ_env
      |> List.sort (fun (v, _) (w, _) -> Stdlib.compare v w)
      |> List.map (fun (name, value) ->
             let value = Type.str value in
             { name; value; type_ = None; var_ref = 0 })
      |> List.sort (fun v w -> Stdlib.compare v.name w.name)

    let get_pred_vars (state : state_t) : Variable.t list =
      let open Variable in
      Verification.SPState.get_preds state
      |> Preds.SPreds.to_list
      |> List.map (fun pred ->
             let value =
               Fmt.to_to_string (Fmt.hbox Preds.SPreds.pp_pabs) pred
             in
             { name = ""; value; type_ = None; var_ref = 0 })
      |> List.sort (fun v w -> Stdlib.compare v.value w.value)

    module Preprocess_files = struct
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

      let compile_tl_files files =
        let progs = get_progs_or_fail (PC.parse_and_compile_files files) in
        let e_progs = progs.gil_progs in
        let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
        let e_prog = snd (List.hd e_progs) in
        let source_files = progs.source_files in
        (e_prog, progs.init_data, Some source_files, Some progs.tl_ast)

      let parse_gil_file file =
        let Gil_parsing.{ labeled_prog; init_data } =
          Gil_parsing.parse_eprog_from_file file
        in
        let init_data =
          match ID.of_yojson init_data with
          | Ok d -> d
          | Error e -> failwith e
        in
        (labeled_prog, init_data, None, None)

      let burn_gil ~init_data prog outfile_opt =
        match outfile_opt with
        | Some outfile ->
            let outc = open_out outfile in
            let fmt = Format.formatter_of_out_channel outc in
            let () =
              match init_data with
              | `Null -> ()
              | init_data ->
                  Fmt.pf fmt "#begin_init_data@\n%a@\n#end_init_data@\n"
                    (Yojson.Safe.pretty_print ~std:true)
                    init_data
            in
            let () = Prog.pp_labeled fmt prog in
            close_out outc
        | None -> ()

      (* TODO: Find a common place to put the three functions here which are
         duplicated in CommandLine.ml *)
      let convert_other_imports oi =
        List.map
          (fun (ext, f) ->
            let fun_with_exn s = Stdlib.Result.get_ok (f s) in
            (ext, fun_with_exn))
          oi

      let log_procs procs =
        DL.log (fun m ->
            let proc_to_yojson =
              Proc.to_yojson Annot.to_yojson (fun x -> `Int x)
            in
            let procs_json =
              Hashtbl.fold
                (fun name proc acc -> (name, proc_to_yojson proc) :: acc)
                procs []
            in
            m ~json:procs_json "Got %d procs" (Hashtbl.length procs))

      let f files already_compiled outfile_opt no_unfold =
        let e_prog, init_data, source_files_opt, tl_ast =
          if already_compiled then parse_gil_file (List.hd files)
          else compile_tl_files files
        in
        burn_gil ~init_data:(ID.to_yojson init_data) e_prog outfile_opt;
        (* Prog.perform_syntax_checks e_prog; *)
        let other_imports = convert_other_imports PC.other_imports in
        let prog = Gil_parsing.eprog_to_prog ~other_imports e_prog in
        L.verbose (fun m ->
            m "@\nProgram as parsed:@\n%a@\n" Prog.pp_indexed prog);
        let prog = LogicPreprocessing.preprocess prog (not no_unfold) in
        L.verbose (fun m ->
            m "@\nProgram after logic preprocessing:@\n%a@\n" Prog.pp_indexed
              prog);
        log_procs prog.procs;
        (prog, init_data, source_files_opt, tl_ast)
    end

    let preprocess_files = Preprocess_files.f

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
          let start_line, start_column, end_line, end_column, source_path =
            (let* proc = Prog.get_proc prog se.pid in
             let annot, _, _ = proc.proc_body.(next_proc_body_idx) in
             let+ loc = Annot.get_origin_loc annot in
             let Location.{ loc_start; loc_end; loc_source } =
               location_to_display_location loc
             in
             ( loc_start.pos_line,
               loc_start.pos_column,
               loc_end.pos_line,
               loc_end.pos_column,
               loc_source ))
            |> Option.value ~default:(0, 0, 0, 0, "")
          in
          (* TODO: make index guaranteed to be unique *)
          let frame =
            make_frame ~index:se.call_index ~name:se.pid ~source_path
              ~start_line ~start_column ~end_line ~end_column
          in
          frame :: call_stack_to_frames rest se.call_index prog

    module Update_proc_state = struct
      let get_cmd id =
        match L.LogQueryer.get_report id with
        | None ->
            Fmt.failwith
              "Unable to find report id '%a'. Check the logging level is set \
               correctly"
              L.ReportId.pp id
        | Some (content, type_) ->
            if type_ <> ContentType.cmd then
              Fmt.failwith
                "Debugger: don't know how to handle report of type '%s'!" type_
            else
              DL.show_report id ("Debugger.update...: Got report type " ^ type_);
            content |> of_yojson_string Logging.ConfigReport.of_yojson

      let get_cur_cmd (cmd : Lifter.cmd_report) cfg =
        match cmd.callstack with
        | [] -> None
        | (se : CallStack.stack_element) :: _ -> (
            let proc = Prog.get_proc cfg.prog se.pid in
            match proc with
            | None -> None
            | Some proc ->
                let annot, _, cmd = proc.proc_body.(cmd.proc_line) in
                Some (cmd, annot))

      let create_variables (state : state_t option) (is_gil_file : bool) :
          scope list * Variable.ts =
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
              let store =
                Verification.SPState.get_store state |> Store.bindings
              in
              let memory = Verification.SPState.get_heap state in
              let lifted_scopes =
                Lifter.add_variables ~store ~memory ~is_gil_file
                  ~get_new_scope_id variables
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

      let f report_id cfg state =
        let cmd = get_cmd report_id in
        state.cur_report_id <- report_id;
        state.frames <-
          call_stack_to_frames cmd.callstack cmd.proc_line cfg.prog;
        let lifted_scopes, variables =
          create_variables (Some cmd.state) (is_gil_file cfg.source_file)
        in
        state.variables <- variables;
        state.top_level_scopes <-
          List.concat [ lifted_scopes; top_level_scopes ];
        (* TODO: fix *)
        (* let () = dbg.errors <- cmd_result.errors in *)
        state.cur_cmd <- get_cur_cmd cmd cfg
    end

    let update_proc_state = Update_proc_state.f

    module Unify = struct
      open Verification.SUnifier.Logging

      let get_test dbg proc_name =
        match dbg.tests |> List.assoc_opt proc_name with
        | None ->
            DL.failwith
              (fun () ->
                let tests_json = Verification.proc_tests_to_yojson dbg.tests in
                [ ("tests", tests_json) ])
              (Fmt.str "No test found for proc `%s`!" proc_name)
        | Some test -> test

      let is_unify_successful id =
        L.LogQueryer.get_unify_results id
        |> List.exists (fun (_, content) ->
               let result =
                 content |> Yojson.Safe.from_string
                 |> UnifyResultReport.of_yojson |> Result.get_ok
               in
               match result with
               | Success _ -> true
               | Failure _ -> false)

      let do_unify prev_id test result =
        DL.log (fun m -> m "Unifying result for %a" L.ReportId.pp prev_id);
        let success = Verification.Debug.analyse_result test prev_id result in
        let+ id, content = L.LogQueryer.get_unify_for prev_id in
        (id, content, success)

      let f result proc_name prev_id dbg =
        let test = get_test dbg proc_name in
        let+ id, content, success =
          match L.LogQueryer.get_unify_for prev_id with
          | Some (id, content) -> Some (id, content, is_unify_successful id)
          | None -> do_unify prev_id test result
        in
        let unify_report = content |> of_yojson_string UnifyReport.of_yojson in
        let kind = unify_report.unify_kind in
        let result = if success then Success else Failure in
        (id, kind, result)
    end

    let unify = Unify.f

    let show_result_errors = function
      | ExecRes.RSucc _ -> []
      | ExecRes.RFail { errors; _ } -> errors |> List.map show_err_t

    let build_final_cmd_data content result prev_id branch_path dbg =
      let { cfg; _ } = dbg in
      let cmd = content |> of_yojson_string Logging.ConfigReport.of_yojson in
      let proc_name = (List.hd cmd.callstack).pid in
      let errors = show_result_errors result in
      let unifys =
        match unify result proc_name prev_id cfg with
        | None -> []
        | Some (id, kind, result) -> [ Exec_map.{ id; kind; result } ]
      in
      let exec_data =
        Lift.make_executed_cmd_data Exec_map.Final prev_id cmd ~unifys ~errors
          branch_path
      in
      (exec_data, cmd)

    module Execute_step = struct
      open Verification.SAInterpreter.Logging

      type execute_step =
        L.ReportId.t ->
        ?branch_case:BranchCase.t ->
        ?branch_path:BranchCase.path ->
        debug_state ->
        debug_proc_state ->
        stop_reason * L.ReportId.t option

      let get_branch_path prev_id case path state =
        DL.log (fun m ->
            m
              ~json:
                [
                  ("id", L.ReportId.to_yojson prev_id);
                  ("lifter_state", state.lifter_state |> Lifter.dump);
                ]
              "Grabbing path for step...");
        let branch_path =
          match path with
          | Some path -> path
          | None -> state.lifter_state |> Lifter.select_next_path case prev_id
        in
        DL.log (fun m ->
            m ~json:[ ("path", branch_path_to_yojson branch_path) ] "Got path");
        branch_path

      let check_cur_report_id = function
        | None ->
            failwith
              "Did not log report. Check the logging level is set correctly"
        | Some id -> id

      let handle_lifter_result
          (execute_step : execute_step)
          ?default_next_id
          dbg
          state
          on_stop
          (result : Lift.handle_cmd_result) =
        match result with
        | ExecNext (id, branch_case) ->
            DL.log (fun m ->
                m "EXEC NEXT (%a, %a)" (pp_option L.ReportId.pp) id
                  (pp_option BranchCase.pp) branch_case);
            let id = Option_utils.coalesce id default_next_id |> Option.get in
            execute_step id ?branch_case dbg state
        | Stop -> on_stop ()

      module Handle_continue = struct
        let get_unifys cur_report_id state =
          let unify =
            DL.log (fun m ->
                m "getting unify_result for %a" L.ReportId.pp cur_report_id);
            let+ unify_id, _ = L.LogQueryer.get_unify_for cur_report_id in
            let unify_map =
              match state.unify_maps |> List.assoc_opt unify_id with
              | Some map -> map
              | None ->
                  let map = build_unify_map unify_id in
                  state.unify_maps <- (unify_id, map) :: state.unify_maps;
                  map
            in
            let result = unify_map |> Unify_map.result in
            (unify_id, fst unify_map, result)
          in
          match unify with
          | None -> []
          | Some (id, kind, result) -> [ Exec_map.{ id; kind; result } ]

        let get_report_and_check_type
            ?(log_context = "execute_step")
            ~on_proc_init
            ~on_eob
            ~continue
            id =
          let content, type_ = Option.get @@ L.LogQueryer.get_report id in
          if type_ = ContentType.proc_init then (
            DL.log (fun m -> m "Debugger.%s: Skipping proc_init..." log_context);
            on_proc_init ())
          else if
            L.LogQueryer.get_cmd_results id
            |> List.for_all (fun (_, content) ->
                   let result =
                     content |> of_yojson_string CmdResult.of_yojson
                   in
                   result.errors <> [])
          then (
            DL.log (fun m ->
                m "Debugger.%s: No non-error results; stepping again for EoB"
                  log_context);
            on_eob ())
          else continue content

        let f
            (execute_step : execute_step)
            prev_id_in_frame
            cur_report_id
            branch_case
            branch_path
            new_branch_cases
            cont_func
            dbg
            state =
          let { cfg; _ } = dbg in
          let cur_report_id = check_cur_report_id cur_report_id in
          state.cont_func <- Some cont_func;
          cur_report_id
          |> get_report_and_check_type
               ~on_proc_init:(fun () ->
                 state |> execute_step prev_id_in_frame dbg)
               ~on_eob:(fun () ->
                 state |> execute_step ~branch_path cur_report_id dbg)
               ~continue:(fun content ->
                 state |> update_proc_state cur_report_id cfg;
                 let open Exec_map in
                 let cmd = content |> of_yojson_string ConfigReport.of_yojson in
                 let cmd_kind =
                   new_branch_cases
                   |> Option.fold ~none:Normal ~some:Exec_map.kind_of_cases
                 in
                 let unifys = get_unifys cur_report_id state in
                 let exec_data =
                   Lift.make_executed_cmd_data cmd_kind cur_report_id cmd
                     ~unifys branch_path
                 in
                 state.lifter_state
                 |> Lifter.handle_cmd prev_id_in_frame branch_case exec_data
                 |> handle_lifter_result ~default_next_id:cur_report_id
                      execute_step dbg state (fun () ->
                        DL.log (fun m ->
                            m "STOP (%a)" L.ReportId.pp cur_report_id);
                        (Step, Some cur_report_id)))
      end

      let handle_continue = Handle_continue.f

      module Handle_end_of_branch = struct
        let get_prev prev_id =
          let prev =
            let+ content, type_ = L.LogQueryer.get_report prev_id in
            (prev_id, content, type_)
          in
          match prev with
          | Some (prev_id, content, type_) when type_ = ContentType.cmd ->
              (prev_id, content)
          | Some (prev_id, _, type_) ->
              Fmt.failwith "EndOfBranch: prev cmd (%a) is '%s', not '%s'!"
                L.ReportId.pp prev_id type_ ContentType.cmd
          | None ->
              Fmt.failwith "EndOfBranch: prev id '%a' doesn't exist!"
                L.ReportId.pp prev_id

        let f
            (execute_step : execute_step)
            prev_id_in_frame
            result
            cont_func
            branch_path
            state
            dbg =
          let { cfg; _ } = dbg in
          state.cont_func <- Some cont_func;
          let prev_id, content = get_prev prev_id_in_frame in
          let prev_prev_id =
            L.LogQueryer.get_previous_report_id prev_id |> Option.get
          in
          let exec_data, cmd =
            build_final_cmd_data content result prev_id branch_path dbg
          in
          state |> update_proc_state prev_id cfg;
          state.lifter_state
          |> Lifter.handle_cmd prev_prev_id cmd.branch_case exec_data
          |> handle_lifter_result execute_step dbg state (fun () ->
                 DL.log (fun m -> m "STOP (end)");
                 (ReachedEnd, None))
      end

      let handle_end_of_branch = Handle_end_of_branch.f

      let rec f prev_id_in_frame ?branch_case ?branch_path dbg state =
        let open Verification.SAInterpreter in
        match state.cont_func with
        | None ->
            DL.log (fun m -> m "No cont_func; reached end");
            (ReachedEnd, None)
        | Some cont_func -> (
            let branch_path =
              get_branch_path prev_id_in_frame branch_case branch_path state
            in
            match cont_func ~path:branch_path () with
            | Finished _ ->
                state.cont_func <- None;
                failwith "HORROR: Shouldn't encounter Finished when debugging!"
            | EndOfBranch (result, cont_func) ->
                handle_end_of_branch f prev_id_in_frame result cont_func
                  branch_path state dbg
            | Continue (cur_report_id, branch_path, new_branch_cases, cont_func)
              ->
                handle_continue f prev_id_in_frame cur_report_id branch_case
                  branch_path new_branch_cases cont_func dbg state)
    end

    let execute_step = Execute_step.f

    module Launch_proc = struct
      open Verification.SAInterpreter.Logging

      let handle_end_of_branch
          proc_name
          result
          prev_id
          report_state
          cont_func
          dbg =
        let { cfg; _ } = dbg in
        match prev_id with
        | None -> Error "Nothing to run"
        | Some prev_id ->
            let lifter_state, _ =
              let prev_content, _ =
                L.LogQueryer.get_report prev_id |> Option.get
              in
              let exec_data, _ =
                build_final_cmd_data prev_content result prev_id [] dbg
              in
              Lifter.init_exn proc_name cfg.tl_ast exec_data
            in
            let state =
              make_debug_proc_state ~cont_func ~cur_report_id:prev_id
                ~top_level_scopes ~lifter_state ~report_state ()
            in
            state |> update_proc_state prev_id cfg;
            Ok (state, ReachedEnd)

      let init_lifter proc_name id cmd branch_path new_branch_cases dbg =
        let { cfg; _ } = dbg in
        let kind =
          let cases = Option.value ~default:[] new_branch_cases in
          Exec_map.kind_of_cases cases
        in
        let exec_data = Lift.make_executed_cmd_data kind id cmd branch_path in
        Lifter.init_exn proc_name cfg.tl_ast exec_data

      let handle_continue
          proc_name
          new_branch_cases
          branch_path
          cur_report_id
          build_proc_state
          (cont_func : result_t cont_func_f)
          report_state
          dbg =
        let { cfg; _ } = dbg in
        let id = cur_report_id |> Execute_step.check_cur_report_id in
        let aux () = build_proc_state (Some id) (cont_func ~path:[] ()) in
        id
        |> Execute_step.Handle_continue.get_report_and_check_type
             ~log_context:"launch_proc" ~on_proc_init:aux ~on_eob:aux
             ~continue:(fun content ->
               let cmd = content |> of_yojson_string ConfigReport.of_yojson in
               let lifter_state, handler_result =
                 init_lifter proc_name id cmd branch_path new_branch_cases dbg
               in
               let proc_state =
                 make_debug_proc_state ~cont_func ~cur_report_id:id
                   ~top_level_scopes ~lifter_state ~report_state ()
               in
               let stop_reason, id =
                 handler_result
                 |> Execute_step.handle_lifter_result execute_step
                      ~default_next_id:id dbg proc_state (fun () ->
                        DL.log (fun m -> m "STOP (%a)" L.ReportId.pp id);
                        (Step, Some id))
               in
               let id = id |> Option.get in
               proc_state |> update_proc_state id cfg;
               Ok (proc_state, stop_reason))

      let rec build_proc_state proc_name report_state dbg prev_id cont_func =
        let build_proc_state = build_proc_state proc_name report_state dbg in
        match cont_func with
        | Finished _ ->
            Error "HORROR: Shouldn't encounter Finished when debugging!"
        | EndOfBranch (result, cont_func) ->
            handle_end_of_branch proc_name result prev_id report_state cont_func
              dbg
        | Continue (cur_report_id, branch_path, new_branch_cases, cont_func) ->
            handle_continue proc_name new_branch_cases branch_path cur_report_id
              build_proc_state cont_func report_state dbg

      let do_launch proc_name report_state dbg () =
        let { cfg; _ } = dbg in
        let cont_func =
          Verification.verify_up_to_procs ~init_data:cfg.init_data ~proc_name
            cfg.prog
        in
        build_proc_state proc_name report_state dbg None cont_func

      let f proc_name dbg =
        let { cfg; _ } = dbg in
        let report_state = L.ReportState.clone cfg.report_state_base in
        Config.Verification.(
          let procs_to_verify = !procs_to_verify in
          if not (procs_to_verify |> List.mem proc_name) then
            set_procs_to_verify (procs_to_verify @ [ proc_name ]));
        report_state
        |> L.ReportState.with_state (do_launch proc_name report_state dbg)
    end

    let launch_proc = Launch_proc.f

    module Launch = struct
      let build_debug_cfg file_name proc_name =
        (* If the file is a GIL file, assume it is already compiled *)
        let already_compiled = is_gil_file file_name in
        let outfile_opt, no_unfold = (None, false) in
        (* TODO: Support debugging incremental mode *)
        (* let incremental = false in *)
        let prog, init_data, source_files, tl_ast =
          preprocess_files [ file_name ] already_compiled outfile_opt no_unfold
        in
        let tests = Verification.Debug.get_tests_for_prog ~init_data prog in
        let proc_name =
          proc_name |> Option_utils.or_else (fun () -> tests |> List.hd |> fst)
        in
        let report_state_base = L.ReportState.(clone global_state) in
        let cfg =
          make_debug_cfg ~source_file:file_name ?source_files ~prog ?tl_ast
            ~tests ~main_proc_name:proc_name ~report_state_base ~init_data ()
        in
        (cfg, proc_name)

      let f file_name proc_name =
        Fmt_tty.setup_std_outputs ();
        Config.current_exec_mode := Verification;
        PC.initialize Verification;
        Config.stats := false;
        Config.lemma_proof := true;
        proc_name
        |> Option.iter (fun proc_name ->
               Config.Verification.set_procs_to_verify [ proc_name ]);
        let cfg, proc_name = build_debug_cfg file_name proc_name in
        let dbg =
          { cfg; procs = Hashtbl.create 0; cur_proc_name = proc_name }
        in
        let++ main_proc_state, _ = dbg |> launch_proc proc_name in
        main_proc_state.report_state |> L.ReportState.activate;
        Hashtbl.add dbg.procs proc_name main_proc_state;
        dbg
    end

    let launch = Launch.f

    let jump_state_to_id id cfg state =
      try
        DL.log (fun m -> m "Jumping to id %a" L.ReportId.pp id);
        (* state.exec_map |> snd |> Exec_map.path_of_id id |> ignore; *)
        (* TODO *)
        state |> update_proc_state id cfg;
        Ok ()
      with Failure msg -> Error msg

    let jump_to_id proc_name id dbg =
      let { cfg; _ } = dbg in
      let** state = dbg |> get_proc_state ~proc_name in
      state |> jump_state_to_id id cfg

    let jump_to_start dbg =
      let { cfg; _ } = dbg in
      let state = dbg |> get_proc_state_exn in
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

    module Step_in_branch_case = struct
      let step_backwards state dbg =
        let { cfg; _ } = dbg in
        match
          state.lifter_state |> Lifter.previous_step state.cur_report_id
        with
        | None -> ReachedStart
        | Some (prev_id, _) ->
            state |> update_proc_state prev_id cfg;
            Step

      let select_next branch_case nexts =
        match branch_case with
        | Some branch_case ->
            List.find_opt (fun (_, bc) -> bc |> Option.get = branch_case) nexts
            |> Option.map fst
        | None -> Some (List.hd nexts |> fst)

      let step_forwards prev_id_in_frame branch_case state dbg =
        let { cfg; _ } = dbg in
        let next_id =
          match
            state.lifter_state |> Lifter.existing_next_steps state.cur_report_id
          with
          | [] -> None
          | nexts -> select_next branch_case nexts
        in
        match next_id with
        | None ->
            DL.log (fun m -> m "No next report ID; executing next step");
            state |> execute_step ?branch_case prev_id_in_frame dbg |> fst
        | Some id ->
            DL.show_report id "Next report ID found; not executing";
            state |> update_proc_state id cfg;
            Step

      let f prev_id_in_frame ?branch_case ?(reverse = false) dbg state =
        let stop_reason =
          if reverse then step_backwards state dbg
          else step_forwards prev_id_in_frame branch_case state dbg
        in
        if has_hit_breakpoint state then Breakpoint
        else if List.length state.errors > 0 then
          let () = state.cont_func <- None in
          ExecutionError
        else stop_reason
    end

    let step_in_branch_case = Step_in_branch_case.f

    let step_in_state ?(reverse = false) cfg state =
      step_in_branch_case state.cur_report_id ?branch_case:None ~reverse cfg
        state

    let step_in ?(reverse = false) dbg =
      let state = dbg |> get_proc_state_exn in
      step_in_state ~reverse dbg state

    module Step_case = struct
      let get_top_frame state =
        match state.frames with
        | [] -> failwith "Nothing in call stack, cannot step"
        | cur_frame :: _ -> cur_frame

      let step_until_cond
          ?(reverse = false)
          ?(branch_case : BranchCase.t option)
          (cond : frame -> frame -> int -> int -> bool)
          (dbg : debug_state)
          (state : debug_proc_state) : stop_reason =
        let prev_id_in_frame = state.cur_report_id in
        let prev_frame = get_top_frame state in
        let prev_stack_depth = List.length state.frames in
        let rec aux () =
          let stop_reason =
            step_in_branch_case ~reverse ?branch_case prev_id_in_frame dbg state
          in
          match stop_reason with
          | Step ->
              let cur_frame = get_top_frame state in
              let cur_stack_depth = List.length state.frames in
              if cond prev_frame cur_frame prev_stack_depth cur_stack_depth then
                stop_reason
              else aux ()
          | other_stop_reason -> other_stop_reason
        in
        aux ()

      (* If GIL file, step until next cmd in the same frame (like in
         regular debuggers) *)
      let is_next_gil_step prev_frame cur_frame prev_stack_depth cur_stack_depth
          =
        cur_frame.source_path = prev_frame.source_path
        && cur_frame.name = prev_frame.name
        || cur_stack_depth < prev_stack_depth

      (* If target language file, step until the code origin location is
         different, indicating an actual step in the target language*)
      let is_next_tl_step prev_frame cur_frame _ _ =
        cur_frame.source_path = prev_frame.source_path
        && (cur_frame.start_line <> prev_frame.start_line
           || cur_frame.start_column <> prev_frame.start_column
           || cur_frame.end_line <> prev_frame.end_line
           || cur_frame.end_column <> prev_frame.end_column)

      let get_cond dbg =
        let { cfg; _ } = dbg in
        if is_gil_file cfg.source_file then is_next_gil_step
        else is_next_tl_step

      let f ?(reverse = false) ?branch_case dbg state =
        let cond = get_cond dbg in
        state |> step_until_cond ~reverse ?branch_case cond dbg
    end

    let step_case = Step_case.f

    let step ?(reverse = false) dbg =
      let state = dbg |> get_proc_state_exn in
      step_case ~reverse dbg state

    let step_specific proc_name branch_case prev_id dbg =
      let { cfg; _ } = dbg in
      let** state = dbg |> get_proc_state ~proc_name in
      let id, branch_case =
        state.lifter_state |> Lifter.next_gil_step prev_id branch_case
      in
      let++ () = state |> jump_state_to_id id cfg in
      state |> step_case ?branch_case dbg

    let step_out dbg =
      let state = dbg |> get_proc_state_exn in
      let rec aux stack_depth =
        let stop_reason = state |> step_in_state dbg in
        match stop_reason with
        | Step ->
            if List.length state.frames < stack_depth then stop_reason
            else aux stack_depth
        | other_stop_reason -> other_stop_reason
      in
      aux (List.length state.frames)

    module Run = struct
      let log_run current_id state =
        DL.log (fun m ->
            m
              ~json:
                [
                  ("current_id", L.ReportId.to_yojson current_id);
                  ("lifter_state", state.lifter_state |> Lifter.dump);
                ]
              "Debugger.run")

      let run_backwards ~on_step dbg state =
        let stop_reason = step_case ~reverse:true dbg state in
        match stop_reason with
        | Step -> on_step ()
        | Breakpoint -> Breakpoint
        | other_stop_reason -> other_stop_reason

      let run_forwards ~on_step ~on_other_reason current_id dbg state =
        let { cfg; _ } = dbg in
        let unfinished =
          state.lifter_state |> Lifter.find_unfinished_path ~at_id:current_id
        in
        match unfinished with
        | None ->
            DL.log (fun m -> m "Debugger.run: map has no unfinished branches");
            ReachedEnd
        | Some (prev_id, branch_case) -> (
            state |> jump_state_to_id prev_id cfg |> Result.get_ok;
            let stop_reason = step_case ?branch_case dbg state in
            match stop_reason with
            | Step -> on_step ()
            | Breakpoint -> Breakpoint
            | _ -> on_other_reason ())

      let f ?(reverse = false) ?(launch = false) dbg =
        let state = dbg |> get_proc_state_exn in
        let current_id = state.cur_report_id in
        log_run current_id state;
        let rec aux ?(launch = false) count =
          if count > 100 then failwith "Debugger.run: infinite loop?";
          let on_step () = aux count in
          let on_other_reason () = aux (count + 1) in
          (* We need to check if a breakpoint has been hit if run is called
             immediately after launching to prevent missing a breakpoint on the first
             line *)
          if launch && has_hit_breakpoint state then Breakpoint
          else if reverse then run_backwards ~on_step dbg state
          else run_forwards ~on_step ~on_other_reason current_id dbg state
        in
        aux ~launch 0
    end

    let run = Run.f

    let start_proc proc_name dbg =
      let++ proc_state, stop_reason = dbg |> launch_proc proc_name in
      Hashtbl.add dbg.procs proc_name proc_state;
      dbg.cur_proc_name <- proc_name;
      stop_reason

    let terminate dbg =
      L.ReportState.(activate global_state);
      Verification.postprocess_files dbg.cfg.source_files;
      if !Config.stats then Statistics.print_statistics ();
      L.wrap_up ()

    let get_frames dbg =
      let state = dbg |> get_proc_state_exn in
      state.frames

    let get_scopes dbg =
      let state = dbg |> get_proc_state_exn in
      state.top_level_scopes

    let get_variables (var_ref : int) (dbg : debug_state) : Variable.t list =
      let state = dbg |> get_proc_state_exn in
      match Hashtbl.find_opt state.variables var_ref with
      | None -> []
      | Some vars -> vars

    let get_exception_info (dbg : debug_state) =
      let { cfg; _ } = dbg in
      let state = dbg |> get_proc_state_exn in
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
      let state = dbg |> get_proc_state_exn in
      match source with
      (* We can't set the breakpoints if we do not know the source file *)
      | None -> ()
      | Some source ->
          let bp_set = Breakpoints.of_list bp_list in
          Hashtbl.replace state.breakpoints source bp_set
  end
