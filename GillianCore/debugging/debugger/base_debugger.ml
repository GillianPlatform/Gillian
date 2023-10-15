open Syntaxes.Option
module L = Logging
module DL = Debugger_log
module Lift = Debugger_lifter
module Exec_map = Exec_map

let ( let** ) = Result.bind
let ( let++ ) f o = Result.map o f

module Premake
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
                 and type annot = PC.Annot.t) =
struct
  open Verification.SAInterpreter
  module Gil_parsing = Gil_parsing.Make (PC.Annot)
  module Breakpoints = Set.Make (Int)
  module Annot = PC.Annot
  module Content_type = L.Logging_constants.Content_type
  module State = Verification.SPState

  type breakpoints = (string, Breakpoints.t) Hashtbl.t
  type tl_ast = PC.tl_ast

  type 'ext base_proc_state = {
    mutable cont_func : result_t cont_func_f option;
    mutable breakpoints : breakpoints; [@default Hashtbl.create 0]
    mutable cur_report_id : L.Report_id.t;
    (* TODO: The below fields only depend on the
            cur_report_id and could be refactored to use this *)
    mutable top_level_scopes : Variable.scope list;
    mutable frames : frame list;
    mutable variables : Variable.ts; [@default Hashtbl.create 0]
    mutable errors : err_t list;
    mutable cur_cmd : (int Cmd.t * Annot.t) option;
    mutable proc_name : string option;
    lifter_state : Lifter.t;
    report_state : L.Report_state.t;
    ext : 'ext;
  }
  [@@deriving make]

  type 'ext base_debug_state = {
    source_file : string;
    source_files : SourceFiles.t option;
    prog : Verification.prog_t;
    tl_ast : tl_ast option;
    main_proc_name : string;
    report_state_base : L.Report_state.t;
    init_data : ID.t;
    proc_names : string list;
    mutable cur_proc_name : string;
    ext : 'ext;
  }
  [@@deriving make]

  type ('proc_state, 'debug_state) state = {
    procs : (string, 'proc_state) Hashtbl.t;
    debug_state : 'debug_state;
  }

  module type Debugger_impl = sig
    type proc_state_ext
    type debug_state_ext

    val preprocess_prog :
      no_unfold:bool -> (annot, int) Prog.t -> (annot, int) Prog.t

    val init : unit base_debug_state -> debug_state_ext

    val init_proc :
      debug_state_ext base_debug_state -> unit base_proc_state -> proc_state_ext

    val launch_proc :
      proc_name:string -> debug_state_ext base_debug_state -> result_t cont_func

    module Unify : sig
      val unify_final_cmd :
        L.Report_id.t ->
        proc_name:string ->
        (state_t, state_vt, err_t) Exec_res.t ->
        debug_state_ext base_debug_state ->
        Exec_map.unification list

      val get_unifys :
        L.Report_id.t ->
        debug_state_ext base_debug_state ->
        proc_state_ext base_proc_state ->
        Exec_map.unification list

      val get_unify_map :
        L.Report_id.t -> debug_state_ext base_debug_state -> Unify_map.t
    end
  end

  module Make (Debugger_impl : Debugger_impl) = struct
    open Debugger_impl
    open Debugger_impl.Unify

    type nonrec breakpoints = breakpoints
    type nonrec tl_ast = tl_ast
    type proc_state = proc_state_ext base_proc_state
    type debug_state = debug_state_ext base_debug_state
    type t = (proc_state, debug_state) state

    let get_root_proc_name_of_id id =
      let content, type_ =
        L.Log_queryer.get_report id
        |> Option_utils.or_else (fun () ->
               Fmt.failwith "Couldn't get report for %a" L.Report_id.pp id)
      in
      if type_ <> L.Logging_constants.Content_type.cmd then
        Fmt.failwith "Report %a is not type '%s'" L.Report_id.pp id
          L.Logging_constants.Content_type.cmd;
      let cmd = content |> of_yojson_string Logging.ConfigReport.of_yojson in
      (List_utils.last cmd.callstack |> Option.get).pid

    let get_proc_state ?cmd_id ?(activate_report_state = true) state =
      let { debug_state; procs } = state in
      let proc_name =
        match cmd_id with
        | Some cmd_id ->
            let proc_name = get_root_proc_name_of_id cmd_id in
            debug_state.cur_proc_name <- proc_name;
            proc_name
        | None -> debug_state.cur_proc_name
      in
      match Hashtbl.find_opt procs proc_name with
      | None -> Error ("get_proc_state: couldn't find proc " ^ proc_name)
      | Some proc_state ->
          if activate_report_state then
            L.Report_state.activate proc_state.report_state;
          Ok proc_state

    let get_proc_state_exn ?cmd_id ?(activate_report_state = true) dbg =
      match get_proc_state ?cmd_id ~activate_report_state dbg with
      | Ok proc_state -> proc_state
      | Error msg -> failwith msg

    module Inspect = struct
      type debug_proc_state_view = {
        exec_map : Exec_map.Packaged.t; [@key "execMap"]
        lifted_exec_map : Exec_map.Packaged.t option; [@key "liftedExecMap"]
        current_cmd_id : L.Report_id.t; [@key "currentCmdId"]
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

      let get_debug_state ({ debug_state; procs } : t) : debug_state_view =
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
            procs []
        in
        DL.log (fun m -> m "Got debug state");
        {
          main_proc_name = debug_state.main_proc_name;
          current_proc_name = debug_state.cur_proc_name;
          procs;
        }

      let get_unify_map id { debug_state; _ } = get_unify_map id debug_state
    end

    let top_level_scopes : Variable.scope list =
      let top_level_scope_names =
        (* [ "Store"; "Heap"; "Pure Formulae"; "Typing Environment"; "Predicates" ] *)
        [ "Pure Formulae"; "Typing Environment"; "Predicates" ]
      in
      List.mapi
        (fun i name -> Variable.{ name; id = i + 1 })
        top_level_scope_names

    let is_gil_file file_name = Filename.check_suffix file_name "gil"

    let get_pure_formulae_vars (state : state_t) : Variable.t list =
      let open Variable in
      State.get_pfs state |> PFS.to_list
      |> List.map (fun formula ->
             let value = Fmt.to_to_string (Fmt.hbox Formula.pp) formula in
             { name = ""; value; type_ = None; var_ref = 0 })
      |> List.sort (fun v w -> Stdlib.compare v.value w.value)

    let get_type_env_vars (state : state_t) : Variable.t list =
      let open Variable in
      let typ_env = State.get_typ_env state in
      Type_env.to_list typ_env
      |> List.sort (fun (v, _) (w, _) -> Stdlib.compare v w)
      |> List.map (fun (name, value) ->
             let value = Type.str value in
             { name; value; type_ = None; var_ref = 0 })
      |> List.sort (fun v w -> Stdlib.compare v.name w.name)

    let get_pred_vars (state : state_t) : Variable.t list =
      let open Variable in
      State.get_preds state |> Preds.SPreds.to_list
      |> List.map (fun pred ->
             let value =
               Fmt.to_to_string (Fmt.hbox Preds.SPreds.pp_pabs) pred
             in
             { name = ""; value; type_ = None; var_ref = 0 })
      |> List.sort (fun v w -> Stdlib.compare v.value w.value)

    module Process_files = struct
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

      let f ~outfile ~no_unfold ~already_compiled files =
        let e_prog, init_data, source_files_opt, tl_ast =
          if already_compiled then parse_gil_file (List.hd files)
          else compile_tl_files files
        in
        Command_line_utils.burn_gil ~init_data:(ID.to_yojson init_data)
          ~pp_prog:Prog.pp_labeled e_prog outfile;
        (* Prog.perform_syntax_checks e_prog; *)
        let other_imports =
          Command_line_utils.convert_other_imports PC.other_imports
        in
        let prog = Gil_parsing.eprog_to_prog ~other_imports e_prog in
        L.verbose (fun m ->
            m "@\nProgram as parsed:@\n%a@\n" Prog.pp_indexed prog);
        let prog = Debugger_impl.preprocess_prog ~no_unfold prog in
        (prog, init_data, source_files_opt, tl_ast)
    end

    let process_files = Process_files.f

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
      | (se : Call_stack.stack_element) :: rest ->
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
        match L.Log_queryer.get_report id with
        | None ->
            Fmt.failwith
              "Unable to find report id '%a'. Check the logging level is set \
               correctly"
              L.Report_id.pp id
        | Some (content, type_) ->
            if type_ <> Content_type.cmd then
              Fmt.failwith
                "Debugger: don't know how to handle report of type '%s'!" type_
            else
              DL.show_report id ("Debugger.update...: Got report type " ^ type_);
            content |> of_yojson_string Logging.ConfigReport.of_yojson

      let get_cur_cmd (cmd : Lifter.cmd_report) cfg =
        match cmd.callstack with
        | [] -> None
        | (se : Call_stack.stack_element) :: _ -> (
            let proc = Prog.get_proc cfg.prog se.pid in
            match proc with
            | None -> None
            | Some proc ->
                let annot, _, cmd = proc.proc_body.(cmd.proc_line) in
                Some (cmd, annot))

      let create_variables (state : state_t option) (is_gil_file : bool) :
          Variable.scope list * Variable.ts =
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
              let store = State.get_store state |> Store.bindings in
              let memory = State.get_heap state in
              let lifted_scopes =
                Lifter.add_variables ~store ~memory ~is_gil_file
                  ~get_new_scope_id variables
              in
              let pure_formulae_vars = get_pure_formulae_vars state in
              let type_env_vars = get_type_env_vars state in
              let pred_vars = get_pred_vars state in
              let vars_list =
                [ pure_formulae_vars; type_env_vars; pred_vars ]
              in
              let () =
                List.iter2
                  (fun (scope : Variable.scope) vars ->
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

    let show_result_errors = function
      | Exec_res.RSucc _ -> []
      | Exec_res.RFail { errors; _ } -> errors |> List.map show_err_t

    let build_final_cmd_data content result prev_id branch_path debug_state =
      let cmd = content |> of_yojson_string Logging.ConfigReport.of_yojson in
      let proc_name = (List.hd cmd.callstack).pid in
      let errors = show_result_errors result in
      let unifys = unify_final_cmd prev_id ~proc_name result debug_state in
      let exec_data =
        Lift.make_executed_cmd_data Exec_map.Final prev_id cmd ~unifys ~errors
          branch_path
      in
      (exec_data, cmd)

    module Execute_step = struct
      open Verification.SAInterpreter.Logging

      type execute_step =
        L.Report_id.t ->
        ?branch_case:BranchCase.t ->
        ?branch_path:BranchCase.path ->
        debug_state ->
        proc_state ->
        stop_reason * L.Report_id.t option

      let get_branch_path prev_id case path state =
        DL.log (fun m ->
            m
              ~json:
                [
                  ("id", L.Report_id.to_yojson prev_id);
                  ("lifter_state", state.lifter_state |> Lifter.dump);
                ]
              "Grabbing path for step...");
        let branch_path =
          match path with
          | Some path -> path
          | None -> state.lifter_state |> Lifter.select_next_path case prev_id
        in
        DL.log (fun m ->
            m
              ~json:[ ("path", BranchCase.path_to_yojson branch_path) ]
              "Got path");
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
                m "EXEC NEXT (%a, %a)" (pp_option L.Report_id.pp) id
                  (pp_option BranchCase.pp) branch_case);
            let id = Option_utils.coalesce id default_next_id |> Option.get in
            execute_step id ?branch_case dbg state
        | Stop -> on_stop ()

      module Handle_continue = struct
        let get_report_and_check_type
            ?(log_context = "execute_step")
            ~on_proc_init
            ~on_eob
            ~continue
            id =
          let content, type_ = Option.get @@ L.Log_queryer.get_report id in
          if type_ = Content_type.proc_init then (
            DL.log (fun m -> m "Debugger.%s: Skipping proc_init..." log_context);
            on_proc_init ())
          else if
            L.Log_queryer.get_cmd_results id
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
            debug_state
            proc_state =
          let cur_report_id = check_cur_report_id cur_report_id in
          proc_state.cont_func <- Some cont_func;
          cur_report_id
          |> get_report_and_check_type
               ~on_proc_init:(fun () ->
                 execute_step prev_id_in_frame debug_state proc_state)
               ~on_eob:(fun () ->
                 execute_step ~branch_path cur_report_id debug_state proc_state)
               ~continue:(fun content ->
                 update_proc_state cur_report_id debug_state proc_state;
                 let open Exec_map in
                 let cmd = content |> of_yojson_string ConfigReport.of_yojson in
                 let cmd_kind = Exec_map.kind_of_cases new_branch_cases in
                 let unifys = get_unifys cur_report_id debug_state proc_state in
                 let exec_data =
                   Lift.make_executed_cmd_data cmd_kind cur_report_id cmd
                     ~unifys branch_path
                 in
                 proc_state.lifter_state
                 |> Lifter.handle_cmd prev_id_in_frame branch_case exec_data
                 |> handle_lifter_result ~default_next_id:cur_report_id
                      execute_step debug_state proc_state (fun () ->
                        DL.log (fun m ->
                            m "STOP (%a)" L.Report_id.pp cur_report_id);
                        (Step, Some cur_report_id)))
      end

      let handle_continue = Handle_continue.f

      module Handle_end_of_branch = struct
        let get_prev prev_id =
          let prev =
            let+ content, type_ = L.Log_queryer.get_report prev_id in
            (prev_id, content, type_)
          in
          match prev with
          | Some (prev_id, content, type_) when type_ = Content_type.cmd ->
              (prev_id, content)
          | Some (prev_id, _, type_) ->
              Fmt.failwith "EndOfBranch: prev cmd (%a) is '%s', not '%s'!"
                L.Report_id.pp prev_id type_ Content_type.cmd
          | None ->
              Fmt.failwith "EndOfBranch: prev id '%a' doesn't exist!"
                L.Report_id.pp prev_id

        let f
            (execute_step : execute_step)
            prev_id_in_frame
            result
            cont_func
            branch_path
            debug_state
            proc_state =
          proc_state.cont_func <- Some cont_func;
          let prev_id, content = get_prev prev_id_in_frame in
          let prev_prev_id =
            L.Log_queryer.get_previous_report_id prev_id |> Option.get
          in
          let exec_data, cmd =
            build_final_cmd_data content result prev_id branch_path debug_state
          in
          update_proc_state prev_id debug_state proc_state;
          proc_state.lifter_state
          |> Lifter.handle_cmd prev_prev_id cmd.branch_case exec_data
          |> handle_lifter_result execute_step debug_state proc_state (fun () ->
                 DL.log (fun m -> m "STOP (end)");
                 (ReachedEnd, None))
      end

      let handle_end_of_branch = Handle_end_of_branch.f

      let rec f
          prev_id_in_frame
          ?branch_case
          ?branch_path
          debug_state
          proc_state =
        let open Verification.SAInterpreter in
        match proc_state.cont_func with
        | None ->
            DL.log (fun m -> m "No cont_func; reached end");
            (ReachedEnd, None)
        | Some cont_func -> (
            let branch_path =
              get_branch_path prev_id_in_frame branch_case branch_path
                proc_state
            in
            match cont_func ~path:branch_path () with
            | Finished _ ->
                proc_state.cont_func <- None;
                failwith "HORROR: Shouldn't encounter Finished when debugging!"
            | EndOfBranch (result, cont_func) ->
                handle_end_of_branch f prev_id_in_frame result cont_func
                  branch_path debug_state proc_state
            | Continue { report_id; branch_path; new_branch_cases; cont_func }
              ->
                handle_continue f prev_id_in_frame report_id branch_case
                  branch_path new_branch_cases cont_func debug_state proc_state)
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
          debug_state =
        match prev_id with
        | None -> Error "Nothing to run"
        | Some prev_id ->
            let lifter_state, _ =
              let prev_content, _ =
                L.Log_queryer.get_report prev_id |> Option.get
              in
              let exec_data, _ =
                build_final_cmd_data prev_content result prev_id [] debug_state
              in
              Lifter.init_exn ~proc_name ~all_procs:debug_state.proc_names
                debug_state.tl_ast debug_state.prog exec_data
            in
            let proc_state =
              let make ext =
                make_base_proc_state ~cont_func ~cur_report_id:prev_id
                  ~top_level_scopes ~lifter_state ~report_state ~ext ()
              in
              let ext = Debugger_impl.init_proc debug_state (make ()) in
              make ext
            in
            proc_state |> update_proc_state prev_id debug_state;
            Ok (proc_state, ReachedEnd)

      let init_lifter
          ~proc_name
          ~report_id
          ~cmd_report
          ~branch_path
          ~new_branch_cases
          debug_state =
        let { proc_names; tl_ast; prog; _ } = debug_state in
        let kind = Exec_map.kind_of_cases new_branch_cases in
        let exec_data =
          Lift.make_executed_cmd_data kind report_id cmd_report branch_path
        in
        Lifter.init_exn ~proc_name ~all_procs:proc_names tl_ast prog exec_data

      let handle_continue
          proc_name
          new_branch_cases
          branch_path
          cur_report_id
          build_proc_state
          (cont_func : result_t cont_func_f)
          report_state
          debug_state =
        let id = cur_report_id |> Execute_step.check_cur_report_id in
        let aux () = build_proc_state (Some id) (cont_func ~path:[] ()) in
        id
        |> Execute_step.Handle_continue.get_report_and_check_type
             ~log_context:"launch_proc" ~on_proc_init:aux ~on_eob:aux
             ~continue:(fun content ->
               let cmd_report =
                 content |> of_yojson_string ConfigReport.of_yojson
               in
               let lifter_state, handler_result =
                 init_lifter ~proc_name ~report_id:id ~cmd_report ~branch_path
                   ~new_branch_cases debug_state
               in
               let proc_state =
                 let make ext =
                   make_base_proc_state ~cont_func ~cur_report_id:id
                     ~top_level_scopes ~lifter_state ~report_state ~ext ()
                 in
                 let ext = Debugger_impl.init_proc debug_state (make ()) in
                 make ext
               in
               let stop_reason, id =
                 handler_result
                 |> Execute_step.handle_lifter_result execute_step
                      ~default_next_id:id debug_state proc_state (fun () ->
                        DL.log (fun m -> m "STOP (%a)" L.Report_id.pp id);
                        (Step, Some id))
               in
               let id = id |> Option.get in
               update_proc_state id debug_state proc_state;
               Ok (proc_state, stop_reason))

      let rec build_proc_state
          proc_name
          report_state
          debug_state
          prev_id
          cont_func =
        let build_proc_state =
          build_proc_state proc_name report_state debug_state
        in
        match cont_func with
        | Finished _ ->
            Error "HORROR: Shouldn't encounter Finished when debugging!"
        | EndOfBranch (result, cont_func) ->
            handle_end_of_branch proc_name result prev_id report_state cont_func
              debug_state
        | Continue { report_id; branch_path; new_branch_cases; cont_func } ->
            handle_continue proc_name new_branch_cases branch_path report_id
              build_proc_state cont_func report_state debug_state

      let f proc_name debug_state =
        let report_state = L.Report_state.clone debug_state.report_state_base in
        report_state
        |> L.Report_state.with_state (fun () ->
               let cont_func =
                 Debugger_impl.launch_proc ~proc_name debug_state
               in
               build_proc_state proc_name report_state debug_state None
                 cont_func)
    end

    let launch_proc = Launch_proc.f

    module Launch = struct
      let build_debug_state file_name proc_name =
        (* If the file is a GIL file, assume it is already compiled *)
        let already_compiled = is_gil_file file_name in
        let outfile, no_unfold = (None, false) in
        (* TODO: Support debugging incremental mode *)
        (* let incremental = false in *)
        let prog, init_data, source_files, tl_ast =
          process_files ~outfile ~no_unfold ~already_compiled [ file_name ]
        in
        let proc_name =
          proc_name |> Option_utils.or_else (fun () -> failwith "No proc name!")
        in
        let proc_names =
          prog.procs |> Hashtbl.to_seq
          |> Seq.filter_map (fun (name, proc) ->
                 if Proc.(proc.proc_internal) then None else Some name)
          |> List.of_seq
        in
        let report_state_base = L.Report_state.(clone global_state) in
        let cfg =
          let make ext =
            make_base_debug_state ~source_file:file_name ?source_files ~prog
              ?tl_ast ~main_proc_name:proc_name ~report_state_base ~init_data
              ~proc_names ~cur_proc_name:proc_name ~ext ()
          in
          let ext = Debugger_impl.init (make ()) in
          make ext
        in
        cfg

      let make_state debug_state = { debug_state; procs = Hashtbl.create 0 }

      let f file_name proc_name : (t, string) result =
        Fmt_tty.setup_std_outputs ();
        PC.initialize !Config.current_exec_mode;
        Config.stats := false;
        let debug_state = build_debug_state file_name proc_name in
        let proc_name = debug_state.main_proc_name in
        let state = make_state debug_state in
        let++ main_proc_state, _ = launch_proc proc_name debug_state in
        main_proc_state.report_state |> L.Report_state.activate;
        Hashtbl.add state.procs proc_name main_proc_state;
        state
    end

    let launch = Launch.f

    let jump_state_to_id id cfg state =
      try
        DL.log (fun m -> m "Jumping to id %a" L.Report_id.pp id);
        (* state.exec_map |> snd |> Exec_map.path_of_id id |> ignore; *)
        (* TODO *)
        state |> update_proc_state id cfg;
        Ok ()
      with Failure msg -> Error msg

    let jump_to_id id (state : t) =
      let** proc_state = get_proc_state ~cmd_id:id state in
      jump_state_to_id id state.debug_state proc_state

    let jump_to_start (state : t) =
      let { debug_state; _ } = state in
      let proc_state = get_proc_state_exn state in
      let result =
        let** root_id =
          proc_state.lifter_state |> Lifter.get_root_id
          |> Option.to_result ~none:"Debugger.jump_to_start: No root id found!"
        in
        jump_state_to_id root_id debug_state proc_state
      in
      match result with
      | Error msg -> failwith msg
      | Ok () -> ()

    module Step_in_branch_case = struct
      let step_backwards debug_state proc_state =
        match
          let { lifter_state; cur_report_id; _ } = proc_state in
          lifter_state |> Lifter.previous_step cur_report_id
        with
        | None -> ReachedStart
        | Some (prev_id, _) ->
            update_proc_state prev_id debug_state proc_state;
            Step

      let select_next branch_case nexts =
        match branch_case with
        | Some branch_case ->
            List.find_opt (fun (_, bc) -> bc |> Option.get = branch_case) nexts
            |> Option.map fst
        | None -> Some (List.hd nexts |> fst)

      let step_forwards prev_id_in_frame branch_case debug_state proc_state =
        let next_id =
          match
            proc_state.lifter_state
            |> Lifter.existing_next_steps proc_state.cur_report_id
          with
          | [] -> None
          | nexts -> select_next branch_case nexts
        in
        match next_id with
        | None ->
            DL.log (fun m -> m "No next report ID; executing next step");
            execute_step ?branch_case prev_id_in_frame debug_state proc_state
            |> fst
        | Some id ->
            DL.show_report id "Next report ID found; not executing";
            update_proc_state id debug_state proc_state;
            Step

      let f
          prev_id_in_frame
          ?branch_case
          ?(reverse = false)
          debug_state
          proc_state =
        let stop_reason =
          if reverse then step_backwards debug_state proc_state
          else step_forwards prev_id_in_frame branch_case debug_state proc_state
        in
        if has_hit_breakpoint proc_state then Breakpoint
        else if List.length proc_state.errors > 0 then
          let () = proc_state.cont_func <- None in
          ExecutionError
        else stop_reason
    end

    let step_in_branch_case = Step_in_branch_case.f

    module Step_case = struct
      let get_top_frame state =
        match state.frames with
        | [] -> failwith "Nothing in call stack, cannot step"
        | cur_frame :: _ -> cur_frame

      let step_until_cond
          ?(reverse = false)
          ?(branch_case : BranchCase.t option)
          (cond : frame -> frame -> int -> int -> bool)
          (debug_state : debug_state)
          (proc_state : proc_state) : stop_reason =
        let prev_id_in_frame = proc_state.cur_report_id in
        let prev_frame = get_top_frame proc_state in
        let prev_stack_depth = List.length proc_state.frames in
        let rec aux () =
          let stop_reason =
            step_in_branch_case ~reverse ?branch_case prev_id_in_frame
              debug_state proc_state
          in
          match stop_reason with
          | Step ->
              let cur_frame = get_top_frame proc_state in
              let cur_stack_depth = List.length proc_state.frames in
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

      let get_cond { source_file; _ } =
        if is_gil_file source_file then is_next_gil_step else is_next_tl_step

      let f ?(reverse = false) ?branch_case debug_state proc_state =
        let cond = get_cond debug_state in
        step_until_cond ~reverse ?branch_case cond debug_state proc_state
    end

    let step_case = Step_case.f

    let step_in ?(reverse = false) state =
      let proc_state = get_proc_state_exn state in
      step_case ~reverse state.debug_state proc_state

    let step ?(reverse = false) state =
      let proc_state = get_proc_state_exn state in
      step_case ~reverse state.debug_state proc_state

    let step_specific branch_case prev_id state =
      let { debug_state; _ } = state in
      let** proc_state = state |> get_proc_state ~cmd_id:prev_id in
      let id, branch_case =
        proc_state.lifter_state |> Lifter.next_gil_step prev_id branch_case
      in
      let++ () = jump_state_to_id id debug_state proc_state in
      proc_state |> step_case ?branch_case debug_state

    let step_out state =
      let proc_state = get_proc_state_exn state in
      step_case state.debug_state proc_state

    module Run = struct
      let log_run current_id state =
        DL.log (fun m ->
            m
              ~json:
                [
                  ("current_id", L.Report_id.to_yojson current_id);
                  ("lifter_state", state.lifter_state |> Lifter.dump);
                ]
              "Debugger.run")

      let run_backwards ~on_step dbg state =
        let stop_reason = step_case ~reverse:true dbg state in
        match stop_reason with
        | Step -> on_step ()
        | Breakpoint -> Breakpoint
        | other_stop_reason -> other_stop_reason

      let run_forwards
          ~on_step
          ~on_other_reason
          current_id
          debug_state
          proc_state =
        let unfinished =
          proc_state.lifter_state
          |> Lifter.find_unfinished_path ~at_id:current_id
        in
        match unfinished with
        | None ->
            DL.log (fun m -> m "Debugger.run: map has no unfinished branches");
            ReachedEnd
        | Some (prev_id, branch_case) -> (
            jump_state_to_id prev_id debug_state proc_state |> Result.get_ok;
            let stop_reason = step_case ?branch_case debug_state proc_state in
            match stop_reason with
            | Step -> on_step ()
            | Breakpoint -> Breakpoint
            | _ -> on_other_reason ())

      let f ?(reverse = false) ?(launch = false) state =
        let { debug_state; _ } = state in
        let proc_state = get_proc_state_exn state in
        let current_id = proc_state.cur_report_id in
        log_run current_id proc_state;
        let rec aux ?(launch = false) count =
          if count > 100 then failwith "Debugger.run: infinite loop?";
          let on_step () = aux count in
          let on_other_reason () = aux (count + 1) in
          (* We need to check if a breakpoint has been hit if run is called
             immediately after launching to prevent missing a breakpoint on the first
             line *)
          if launch && has_hit_breakpoint proc_state then Breakpoint
          else if reverse then run_backwards ~on_step debug_state proc_state
          else
            run_forwards ~on_step ~on_other_reason current_id debug_state
              proc_state
        in
        aux ~launch 0
    end

    let run = Run.f

    let start_proc proc_name state =
      let { debug_state; procs } = state in
      let++ proc_state, stop_reason = launch_proc proc_name debug_state in
      Hashtbl.add procs proc_name proc_state;
      debug_state.cur_proc_name <- proc_name;
      stop_reason

    let terminate state =
      L.Report_state.(activate global_state);
      Verification.postprocess_files state.debug_state.source_files;
      if !Config.stats then Statistics.print_statistics ();
      L.wrap_up ()

    let get_frames state =
      let { frames; _ } = get_proc_state_exn state in
      frames

    let get_scopes state =
      let { top_level_scopes; _ } = get_proc_state_exn state in
      top_level_scopes

    let get_variables (var_ref : int) (state : t) : Variable.t list =
      let { variables; _ } = get_proc_state_exn state in
      match Hashtbl.find_opt variables var_ref with
      | None -> []
      | Some vars -> vars

    let get_exception_info state =
      let proc_state = get_proc_state_exn state in
      let error = List.hd proc_state.errors in
      let non_mem_exception_info =
        { id = Fmt.to_to_string Logging.pp_err error; description = None }
      in
      match error with
      | Exec_err.EState state_error -> (
          match state_error with
          | StateErr.EMem merr ->
              let tl_ast = state.debug_state.tl_ast in
              Lifter.memory_error_to_exception_info
                { error = merr; command = proc_state.cur_cmd; tl_ast }
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
end
