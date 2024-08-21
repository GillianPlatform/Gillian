open Syntaxes.Option
module L = Logging
module DL = Debugger_log
module Lift = Debugger_lifter

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
    mutable cur_report_id : L.Report_id.t option;
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

    module Match : sig
      val match_final_cmd :
        L.Report_id.t ->
        proc_name:string ->
        (state_t, state_vt, err_t) Exec_res.t ->
        debug_state_ext base_debug_state ->
        Exec_map.matching list

      val get_matches :
        L.Report_id.t ->
        debug_state_ext base_debug_state ->
        proc_state_ext base_proc_state ->
        Exec_map.matching list

      val get_match_map :
        L.Report_id.t -> debug_state_ext base_debug_state -> Match_map.t
    end
  end

  module Make (Debugger_impl : Debugger_impl) = struct
    open Debugger_impl
    open Debugger_impl.Match

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
        matches : Exec_map.matching list;
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
              let current_cmd_id = Option.get state.cur_report_id in
              let matches =
                state.lifter_state |> Lifter.get_matches_at_id current_cmd_id
              in
              let exec_map = state.lifter_state |> Lifter.get_gil_map in
              let lifted_exec_map =
                state.lifter_state |> Lifter.get_lifted_map
              in
              let proc =
                {
                  exec_map;
                  lifted_exec_map;
                  current_cmd_id;
                  matches;
                  proc_name;
                }
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

      let get_match_map id { debug_state; _ } = get_match_map id debug_state
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
      State.get_preds state |> Preds.to_list
      |> List.map (fun pred ->
             let value = Fmt.to_to_string (Fmt.hbox Preds.pp_pabs) pred in
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

    (* Currently only one breakpoint per line is supported *)
    let is_breakpoint ~file ~lines proc =
      match Hashtbl.find_opt proc.breakpoints file with
      | None -> false
      | Some breakpoints ->
          let rec aux = function
            | [] -> false
            | line :: ls -> Breakpoints.mem line breakpoints || aux ls
          in
          aux lines

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
        state.cur_report_id <- Some report_id;
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

    let jump_state_to_id id cfg state =
      try
        DL.log (fun m -> m "Jumping to id %a" L.Report_id.pp id);
        state |> update_proc_state id cfg;
        Ok ()
      with Failure msg -> Error msg

    let jump_to_id id (state : t) =
      let** proc_state = get_proc_state ~cmd_id:id state in
      jump_state_to_id id state.debug_state proc_state

    let handle_stop debug_state proc_state ?(is_end = false) id id' =
      let id =
        match id' with
        | None -> id
        | Some id' ->
            jump_state_to_id id' debug_state proc_state |> Result.get_ok;
            id
      in
      if is_end then
        let () = DL.log (fun m -> m "STOP (end)") in
        (ReachedEnd, None)
      else
        let () = DL.log (fun m -> m "STOP (%a)" L.Report_id.pp id) in
        (Step, Some id)

    let show_result_errors = function
      | Exec_res.RSucc _ -> []
      | Exec_res.RFail { errors; _ } -> errors |> List.map show_err_t

    let build_final_cmd_data content result prev_id branch_path debug_state =
      let cmd = content |> of_yojson_string Logging.ConfigReport.of_yojson in
      let exec_data =
        let proc_name = (List.hd cmd.callstack).pid in
        let errors = show_result_errors result in
        let matches = match_final_cmd prev_id ~proc_name result debug_state in
        let next_kind = Exec_map.Zero in
        Lift.make_executed_cmd_data next_kind prev_id cmd ~matches ~errors
          branch_path
      in
      (exec_data, cmd)

    module Step = struct
      open Verification.SAInterpreter.Logging

      let check_cur_report_id = function
        | None ->
            failwith
              "Did not log report. Check the logging level is set correctly"
        | Some id -> id

      (* A command step with no results *should* mean that we're returning.
         If we're at the top of the callstack, this *should* mean that we're hitting the end of the program. *)
      let is_eob ~id =
        L.Log_queryer.get_cmd_results id
        |> List.for_all (fun (_, content) ->
               let result = content |> of_yojson_string CmdResult.of_yojson in
               result.errors <> [])

      type continue_kind = ProcInit | EoB | Continue

      let get_report_and_check_type ?(log_context = "execute_step") id =
        let content, type_ = Option.get @@ L.Log_queryer.get_report id in
        let kind =
          if type_ = Content_type.proc_init then (
            DL.log (fun m -> m "Debugger.%s: Skipping proc_init..." log_context);
            ProcInit)
          else if is_eob ~id then (
            DL.log (fun m ->
                m
                  "Debugger.%s: No non-error results for %a; stepping again \
                   for EoB"
                  log_context L.Report_id.pp id);
            EoB)
          else Continue
        in
        (kind, content)

      (*
        This is a bit weird; it hinges on the fact that the ReturnNormal GIL cmd gives no results
        FIXME: I don't think g_interpreter actually logs the result?
        this means we need to get results via cont_func and needs to be changed.
      *)
      let find_or_exec_eob
          id
          (cont_func : 'a Verification.SAInterpreter.cont_func_f) =
        ignore id;
        (* TODO try to find result in log *)
        match cont_func ~selector:(IdCase (id, None)) () with
        | Finished _ ->
            failwith "HORROR: Shouldn't encounter Finished when debugging!"
        | Continue _ -> failwith "Expected EoB, got Continue!"
        | EndOfBranch (result, cont_func) -> (result, cont_func)

      let find_or_exec_next
          id
          case
          path
          (cont_func : 'a Verification.SAInterpreter.cont_func_f) =
        let next =
          let* id = id in
          L.Log_queryer.get_next_reports id
          |> List.find_map (fun (_, content, type_) ->
                 if type_ = Content_type.cmd then
                   let cmd =
                     content |> of_yojson_string Logging.ConfigReport.of_yojson
                   in
                   match (case, cmd.branch_case) with
                   | Some case, Some bc when bc = case -> Some id
                   | None, _ -> Some id
                   | _ -> None
                 else None)
        in
        match next with
        | Some id ->
            let new_branch_cases =
              L.Log_queryer.get_cmd_results id
              |> List.filter_map (fun (_, content) ->
                     let result =
                       of_yojson_string Logging.CmdResult.of_yojson content
                     in
                     result.branch_case)
            in
            let branch_path = List_utils.cons_opt case path in
            (id, branch_path, new_branch_cases, cont_func)
        | None -> (
            let selector =
              match id with
              | Some id -> IdCase (id, case)
              | None -> Path path
            in
            match cont_func ~selector () with
            | Finished _ ->
                failwith "HORROR: Shouldn't encounter Finished when debugging!"
            | EndOfBranch _ -> failwith "Unexpected EndOfBranch!"
            | Continue { report_id; branch_path; new_branch_cases; cont_func }
              ->
                let id = check_cur_report_id report_id in
                (id, branch_path, new_branch_cases, cont_func))

      let get_next_step id case path debug_state proc_state =
        let cont_func =
          Option_utils.or_else
            (fun () -> failwith "HORROR: No cont func!")
            proc_state.cont_func
        in
        let id, path, new_branch_cases, cont_func =
          find_or_exec_next id case path cont_func
        in
        let continue_kind, content = get_report_and_check_type id in
        let exec_data, cont_func =
          match continue_kind with
          | ProcInit -> failwith "Unexpected ProcInit!"
          | Continue ->
              let cmd_kind = Exec_map.kind_of_cases new_branch_cases in
              let matches = get_matches id debug_state proc_state in
              let report =
                of_yojson_string Logging.ConfigReport.of_yojson content
              in
              let exec_data =
                Lift.make_executed_cmd_data cmd_kind id report ~matches path
              in
              (exec_data, cont_func)
          | EoB ->
              let result, cont_func = find_or_exec_eob id cont_func in
              let exec_data, _ =
                build_final_cmd_data content result id path debug_state
              in
              (exec_data, cont_func)
        in
        let () = proc_state.cont_func <- Some cont_func in
        exec_data

      let handle_step_effect id case path proc_state { debug_state; _ } =
        let () =
          let> id = id in
          jump_state_to_id id debug_state proc_state |> Result.get_ok
        in
        let exec_data = get_next_step id case path debug_state proc_state in
        let () = update_proc_state exec_data.id debug_state proc_state in
        exec_data

      let with_lifter_effects f proc_state state =
        let open Lift in
        let open Lifter in
        let open Effect.Deep in
        try_with f ()
          {
            effc =
              (fun (type a) (eff : a Effect.t) ->
                match eff with
                | Step (id, case, path) ->
                    Some
                      (fun (k : (a, _) continuation) ->
                        let step_result =
                          handle_step_effect id case path proc_state state
                        in
                        continue k step_result)
                | IsBreakpoint (file, lines) ->
                    Some
                      (fun (k : (a, _) continuation) ->
                        is_breakpoint ~file ~lines proc_state |> continue k)
                | _ ->
                    let s = Printexc.to_string (Effect.Unhandled eff) in
                    Fmt.failwith "HORROR: effect leak!\n%s" s);
          }

      let lifter_call lifter_func proc_state state =
        let stop_id, stop_reason =
          with_lifter_effects lifter_func proc_state state
        in
        let++ () = jump_state_to_id stop_id state.debug_state proc_state in
        stop_reason

      let lifter_call_with_id state lifter_func =
        let proc_state = get_proc_state_exn state in
        let { cur_report_id; lifter_state; _ } = proc_state in
        let id = Option.get cur_report_id in
        let f () = lifter_func lifter_state id in
        lifter_call f proc_state state |> Result.get_ok

      let over state = lifter_call_with_id state Lifter.step_over
      let in_ state = lifter_call_with_id state Lifter.step_in
      let out state = lifter_call_with_id state Lifter.step_out

      let branch case id state =
        let proc_state = get_proc_state_exn ~cmd_id:id state in
        let { lifter_state; _ } = proc_state in
        let f () = Lifter.step_branch lifter_state id case in
        lifter_call f proc_state state

      let back state = lifter_call_with_id state Lifter.step_back
      let continue state = lifter_call_with_id state Lifter.continue
      let continue_back state = lifter_call_with_id state Lifter.continue_back
    end

    module Launch_proc = struct
      let check_init_report id =
        let** id =
          Option.to_result ~none:"HORROR: No report from initial cont!" id
        in
        match L.Log_queryer.get_report id with
        | None -> Error "HORROR: No report on initial cont_func!"
        | Some (_, type_)
          when type_ = L.Logging_constants.Content_type.proc_init -> Ok ()
        | Some _ -> Error "HORROR: Initial report is not a proc_init!"

      (* For the initial step, we should always get a blank Continue *)
      let get_cont_func proc_name debug_state =
        match Debugger_impl.launch_proc ~proc_name debug_state with
        | Continue
            { report_id; branch_path = []; new_branch_cases = []; cont_func } ->
            let++ () = check_init_report report_id in
            cont_func
        | _ -> Error "HORROR: Unexpected conf from initial cont!"

      let init_lifter proc_name debug_state =
        let { proc_names; tl_ast; prog; _ } = debug_state in
        Lifter.init_exn ~proc_name ~all_procs:proc_names tl_ast prog

      let f proc_name state =
        let { debug_state; _ } = state in
        let report_state = L.Report_state.clone debug_state.report_state_base in
        report_state
        |> L.Report_state.with_state (fun () ->
               let** cont_func = get_cont_func proc_name debug_state in
               let lifter_state, init_lifter' =
                 init_lifter proc_name debug_state
               in
               let proc_state =
                 let make ext =
                   make_base_proc_state ~cont_func ~top_level_scopes
                     ~lifter_state ~report_state ~ext ()
                 in
                 let ext = Debugger_impl.init_proc debug_state (make ()) in
                 make ext
               in
               let** stop_reason =
                 Step.lifter_call init_lifter' proc_state state
               in
               Ok (proc_state, stop_reason))
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
        let++ main_proc_state, _ = launch_proc proc_name state in
        main_proc_state.report_state |> L.Report_state.activate;
        Hashtbl.add state.procs proc_name main_proc_state;
        state
    end

    let launch = Launch.f
    let step_in = Step.in_
    let step ?(reverse = false) = if reverse then Step.back else Step.over
    let step_specific = Step.branch
    let step_out = Step.out

    let run ?(reverse = false) ?(launch = false) =
      ignore launch;
      (* TODO *)
      if reverse then Step.continue_back else Step.continue

    let start_proc proc_name state =
      let { debug_state; procs } = state in
      let++ proc_state, stop_reason = launch_proc proc_name state in
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
