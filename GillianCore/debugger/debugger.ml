module L = Logging
module Displayable = Displayable
module DisplayFilterMap = DisplayFilterMap

module type S = sig
  type stop_reason =
    | Step
    | ReachedStart
    | ReachedEnd
    | Breakpoint
    | ExecutionError

  type frame = {
    index : int;
    name : string;
    source_path : string;
    start_line : int;
    start_column : int;
    end_line : int;
    end_column : int;
  }

  type scope = { name : string; id : int }

  type variable = {
    name : string;
    value : string;
    type_ : string option;
    var_ref : int;
  }

  type exception_info = { id : string; description : string option }

  type debugger_state

  val launch : string -> string option -> (debugger_state, string) result

  val step_in : ?reverse:bool -> debugger_state -> stop_reason

  val step : debugger_state -> stop_reason

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
    (TLDisplayFilterMap : DisplayFilterMap.S)
    (SMemoryDisplayable : Displayable.S
                            with type t = Verification.SAInterpreter.heap_t) =
struct
  open Verification.SAInterpreter
  module Breakpoints = Set.Make (Int)

  type stop_reason =
    | Step
    | ReachedStart
    | ReachedEnd
    | Breakpoint
    | ExecutionError

  type frame = {
    index : int;
    name : string;
    source_path : string;
    start_line : int;
    start_column : int;
    end_line : int;
    end_column : int;
  }

  type scope = { name : string; id : int }

  type variable = {
    name : string;
    value : string;
    type_ : string option;
    var_ref : int;
  }

  type exception_info = { id : string; description : string option }

  type breakpoints = (string, Breakpoints.t) Hashtbl.t

  type scopes_to_vars = (int, variable list) Hashtbl.t

  type debugger_state = {
    source_file : string;
    source_files : SourceFiles.t option;
    top_level_scopes : scope list;
    prog : (Annot.t, int) Prog.t;
    mutable cont_func :
      (unit -> Verification.SAInterpreter.result_t cont_func) option;
    mutable cur_report_id : string;
    mutable frames : frame list;
    mutable breakpoints : breakpoints;
    mutable scopes_to_vars : scopes_to_vars;
    mutable errors : err_t list;
  }

  let top_level_scope_names =
    [ "Store"; "Heap"; "Pure Formulae"; "Typing Environment"; "Predicates" ]

  let top_level_scopes : scope list =
    List.map2
      (fun name id -> { name; id })
      top_level_scope_names
      (List_utils.range (List.length top_level_scope_names))

  let is_gil_file file_name = Filename.check_suffix file_name "gil"

  let get_store_vars (state : state_t) (is_gil_file : bool) : variable list =
    let store = Verification.SPState.get_store state in
    let store_bindings = Store.bindings store in
    let store_bindings =
      if is_gil_file then
        DisplayFilterMap.Default.filter_map_store store_bindings
      else TLDisplayFilterMap.filter_map_store store_bindings
    in
    store_bindings
    |> List.map (fun (var, value) ->
           { name = var; value; type_ = None; var_ref = 0 })
    |> List.sort (fun v w -> Stdlib.compare v.name w.name)

  let rec add_heap_vars
      (dt_list : Displayable.debugger_tree list)
      (scopes_to_vars : scopes_to_vars)
      (get_new_scope_id : unit -> int) : variable list =
    dt_list
    |> List.map (fun tree ->
           match tree with
           | Displayable.Leaf (name, value) ->
               (* Variable reference is set to 0 as there are no children variables *)
               { name; value; type_ = None; var_ref = 0 }
           | Displayable.Node (name, dt_list) ->
               let new_scope_id = get_new_scope_id () in
               let vars =
                 add_heap_vars dt_list scopes_to_vars get_new_scope_id
               in
               let () = Hashtbl.replace scopes_to_vars new_scope_id vars in
               {
                 name;
                 value = "";
                 type_ = Some "object";
                 var_ref = new_scope_id;
               })

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
    Verification.SPState.get_pp_preds state
    |> List.map (fun pred ->
           { name = ""; value = pred; type_ = None; var_ref = 0 })
    |> List.sort (fun v w -> Stdlib.compare v.value w.value)

  let create_scopes_to_vars (state : state_t option) (is_gil_file : bool) :
      scopes_to_vars =
    let scopes_to_vars = Hashtbl.create 0 in
    (* New scope ids must be higher than last top level scope id to prevent
       duplicate scope ids *)
    let scope_id = ref (List.length top_level_scopes) in
    let get_new_scope_id () =
      let () = scope_id := !scope_id + 1 in
      !scope_id
    in
    let vars_list =
      match state with
      | None       -> [ []; []; []; []; [] ]
      | Some state ->
          let store_vars = get_store_vars state is_gil_file in
          let heap = Verification.SPState.get_heap state in
          let dt_list = SMemoryDisplayable.to_debugger_tree heap in
          let heap_vars =
            add_heap_vars dt_list scopes_to_vars get_new_scope_id
          in
          let pure_formulae_vars = get_pure_formulae_vars state in
          let typ_env_vars = get_typ_env_vars state in
          let pred_vars = get_pred_vars state in
          [ store_vars; heap_vars; pure_formulae_vars; typ_env_vars; pred_vars ]
    in
    let () =
      List.iter2
        (fun (scope : scope) vars ->
          Hashtbl.replace scopes_to_vars scope.id vars)
        top_level_scopes vars_list
    in
    scopes_to_vars

  (* TODO: Find a common place to put the below three functions which are
     duplicated in CommandLine.ml *)
  let convert_other_imports oi =
    List.map
      (fun (ext, f) ->
        let fun_with_exn s = Stdlib.Result.get_ok (f s) in
        (ext, fun_with_exn))
      oi

  let get_progs_or_fail = function
    | Ok progs  -> (
        match progs.ParserAndCompiler.gil_progs with
        | [] ->
            Fmt.pr "Error: expected at least one GIL program\n";
            exit 1
        | _  -> progs)
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
    | None         -> ()

  let preprocess_files files already_compiled outfile_opt no_unfold =
    let e_prog, source_files_opt =
      if not already_compiled then
        let progs = get_progs_or_fail (PC.parse_and_compile_files files) in
        let e_progs = progs.gil_progs in
        let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
        let e_prog = snd (List.hd e_progs) in
        let source_files = progs.source_files in
        (e_prog, Some source_files)
      else
        let e_prog = Gil_parsing.parse_eprog_from_file (List.hd files) in
        (e_prog, None)
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
    (prog, source_files_opt)

  let has_hit_breakpoint dbg =
    match dbg.frames with
    | []         -> false
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
          | None      -> defaults
          | Some proc -> (
              let annot, _, _ = proc.proc_body.(next_proc_body_idx) in
              let loc_opt = Annot.get_origin_loc annot in
              match loc_opt with
              | None     -> defaults
              | Some loc ->
                  ( loc.loc_start.pos_line,
                    (* VSCode column numbers start from 1 *)
                    loc.loc_start.pos_column + 1,
                    loc.loc_end.pos_line,
                    loc.loc_end.pos_column + 1,
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

  let update_report_id_and_inspection_fields report_id dbg =
    dbg.cur_report_id <- report_id;
    match Logging.LogQueryer.get_report report_id with
    | None                  ->
        raise
          (Failure
             (Printf.sprintf
                "Unable to find report id '%s'. Check the logging level is set \
                 correctly"
                report_id))
    | Some (content, type_) -> (
        match type_ with
        | t when t = Logging.LoggingConstants.ContentType.cmd_step -> (
            let cmd_step =
              content |> Yojson.Safe.from_string |> cmd_step_of_yojson
            in
            match cmd_step with
            | Ok cmd_step ->
                let () =
                  dbg.frames <-
                    call_stack_to_frames cmd_step.call_stack
                      cmd_step.proc_body_index dbg.prog
                in
                let () =
                  dbg.scopes_to_vars <-
                    create_scopes_to_vars cmd_step.state
                      (is_gil_file dbg.source_file)
                in
                dbg.errors <- cmd_step.errors
            | Error err   -> failwith err)
        | _ as t ->
            raise
              (Failure
                 (Printf.sprintf
                    "Cannot deserialize: type '%s' does not match callstack" t))
        )

  let launch file_name proc_name =
    let () = Fmt_tty.setup_std_outputs () in
    let () = Config.current_exec_mode := Verification in
    let () = PC.initialize Verification in
    let () = Config.stats := false in
    let () = Config.lemma_proof := true in
    let () = Config.manual_proof := false in
    let () =
      match proc_name with
      | None           -> ()
      | Some proc_name -> Config.Verification.set_procs_to_verify [ proc_name ]
    in
    (* If the file is a GIL file, assume it is already compiled *)
    let already_compiled = is_gil_file file_name in
    let outfile_opt = None in
    let no_unfold = false in
    (* TODO: Support debugging incremental mode *)
    (* let incremental = false in *)
    let prog, source_files_opt =
      preprocess_files [ file_name ] already_compiled outfile_opt no_unfold
    in
    let cont_func = Verification.verify_up_to_procs prog in
    match cont_func with
    | Verification.SAInterpreter.Finished _ -> Error "Nothing to run"
    | Verification.SAInterpreter.Continue (cur_report_id, cont_func) -> (
        match cur_report_id with
        | None               ->
            raise
              (Failure
                 "Did not log report. Check the logging level is set correctly")
        | Some cur_report_id ->
            let dbg =
              ({
                 source_file = file_name;
                 source_files = source_files_opt;
                 top_level_scopes;
                 cont_func = Some cont_func;
                 prog;
                 frames = [];
                 cur_report_id;
                 breakpoints = Hashtbl.create 0;
                 scopes_to_vars = Hashtbl.create 0;
                 errors = [];
               }
                : debugger_state)
            in
            let _ = update_report_id_and_inspection_fields cur_report_id dbg in
            Ok dbg)

  let execute_step dbg =
    let open Verification.SAInterpreter in
    match dbg.cont_func with
    | None           -> ReachedEnd
    | Some cont_func -> (
        let cont_func = cont_func () in
        match cont_func with
        | Finished _ ->
            let () = dbg.cont_func <- None in
            ReachedEnd
        | Continue (cur_report_id, cont_func) -> (
            match cur_report_id with
            | None               ->
                raise
                  (Failure
                     "Did not log report. Check the logging level is set \
                      correctly")
            | Some cur_report_id ->
                let () = dbg.cont_func <- Some cont_func in
                let () =
                  update_report_id_and_inspection_fields cur_report_id dbg
                in
                Step))

  let step_in ?(reverse = false) dbg =
    let stop_reason =
      if reverse then
        let prev_report_id =
          Logging.LogQueryer.get_previous_report_id dbg.cur_report_id
        in
        match prev_report_id with
        | None                -> ReachedStart
        | Some prev_report_id ->
            let () =
              update_report_id_and_inspection_fields prev_report_id dbg
            in
            Step
      else
        let next_report_id =
          Logging.LogQueryer.get_next_report_id dbg.cur_report_id
        in
        match next_report_id with
        | None                -> execute_step dbg
        | Some next_report_id ->
            let () =
              update_report_id_and_inspection_fields next_report_id dbg
            in
            Step
    in
    if has_hit_breakpoint dbg then Breakpoint
    else if List.length dbg.errors > 0 then
      let () = dbg.cont_func <- None in
      ExecutionError
    else stop_reason

  let step dbg =
    match dbg.frames with
    | []         -> failwith "Nothing in call stack, cannot step"
    | frame :: _ ->
        let rec step prev_frame stack_depth dbg =
          let stop_reason = step_in dbg in
          match stop_reason with
          | Step              -> (
              match dbg.frames with
              | []         -> failwith "Nothing in call stack, cannot step"
              | frame :: _ ->
                  (* TODO: If the frame index is consistent, we can use that to
                           check if we are in the same frame as the previous
                           frame *)
                  if
                    frame.source_path = prev_frame.source_path
                    && frame.name = prev_frame.name
                    || List.length dbg.frames < stack_depth
                  then stop_reason
                  else step prev_frame stack_depth dbg)
          | other_stop_reason -> other_stop_reason
        in
        step frame (List.length dbg.frames) dbg

  let step_out dbg =
    let rec step_out stack_depth dbg =
      let stop_reason = step_in dbg in
      match stop_reason with
      | Step              ->
          if List.length dbg.frames < stack_depth then stop_reason
          else step_out stack_depth dbg
      | other_stop_reason -> other_stop_reason
    in
    step_out (List.length dbg.frames) dbg

  let rec run ?(reverse = false) ?(launch = false) dbg =
    (* We need to check if a breakpoint has been hit if run is called
       immediately after launching to prevent missing a breakpoint on the first
       line *)
    if launch && has_hit_breakpoint dbg then Breakpoint
    else
      let stop_reason = step_in ~reverse dbg in
      match stop_reason with
      | Step              -> run ~reverse dbg
      | other_stop_reason -> other_stop_reason

  let terminate dbg =
    let () = Verification.postprocess_files dbg.source_files in
    let () = if !Config.stats then Statistics.print_statistics () in
    Logging.wrap_up ()

  let get_frames dbg = dbg.frames

  let get_scopes dbg = dbg.top_level_scopes

  let get_variables (var_ref : int) (dbg : debugger_state) : variable list =
    match Hashtbl.find_opt dbg.scopes_to_vars var_ref with
    | None      -> []
    | Some vars -> vars

  let get_exception_info (dbg : debugger_state) =
    let error = List.hd dbg.errors in
    let id =
      match error with
      | ExecErr.ESt state_error -> (
          match state_error with
          | StateErr.EMem _ -> Fmt.to_to_string pp_err error
          | _               -> Fmt.to_to_string pp_err error)
      | error                   -> Fmt.to_to_string pp_err error
    in
    { id; description = None }

  let set_breakpoints source bp_list dbg =
    match source with
    (* We can't set the breakpoints if we do not know the source file *)
    | None -> ()
    | Some source ->
        let bp_set = Breakpoints.of_list bp_list in
        Hashtbl.replace dbg.breakpoints source bp_set
end
