module L = Logging

module type S = sig
  type stop_reason = Step | ReachedEnd | Breakpoint

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

  type variable = { name : string; value : string; type_ : string option }

  type debugger_state

  val launch : string -> (debugger_state, string) result

  val step : debugger_state -> stop_reason

  val run : debugger_state -> stop_reason

  val terminate : debugger_state -> unit

  val get_frames : debugger_state -> frame list

  val get_scopes : debugger_state -> scope list

  val get_variables : int -> debugger_state -> variable list

  val set_breakpoints : string option -> int list -> debugger_state -> unit
end

module Make (PC : ParserAndCompiler.S) (Verification : Verifier.S) = struct
  module Breakpoints = Set.Make (Int)

  type stop_reason = Step | ReachedEnd | Breakpoint

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

  type variable = { name : string; value : string; type_ : string option }

  type breakpoints = (string, Breakpoints.t) Hashtbl.t

  type debugger_state = {
    source_file : string;
    source_files : SourceFiles.t option;
    scopes : scope list;
    prog : (Annot.t, int) Prog.t;
    mutable step_func :
      unit ->
      string option * Verification.result_t Verification.SAInterpreter.cont_func;
    mutable frames : frame list;
    mutable store : Verification.SAInterpreter.store_t option;
    mutable cur_report_id : string option;
    mutable breakpoints : breakpoints;
  }

  let scopes_tbl = Hashtbl.create 0

  let store_scope = ({ name = "Store"; id = 1 } : scope)

  let heap_scope = ({ name = "Heap"; id = 2 } : scope)

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

  let launch file_name =
    Hashtbl.replace scopes_tbl store_scope.id store_scope.name;
    Hashtbl.replace scopes_tbl heap_scope.id heap_scope.name;

    let () = Fmt_tty.setup_std_outputs () in
    let () = Config.current_exec_mode := Verification in
    let () = PC.initialize Verification in
    let () = Config.stats := false in
    let () = Config.lemma_proof := true in
    let () = Config.manual_proof := false in
    let already_compiled = true in
    let outfile_opt = None in
    let no_unfold = false in
    (* TODO: Support debugging incremental mode *)
    (* let incremental = false in *)
    let prog, source_files_opt =
      preprocess_files [ file_name ] already_compiled outfile_opt no_unfold
    in
    let step_func = Verification.verify_up_to_procs prog in
    match step_func with
    | Verification.SAInterpreter.Finished _ -> Error "Nothing to run"
    | Verification.SAInterpreter.Continue step_func ->
        Ok
          ({
             source_file = file_name;
             source_files = source_files_opt;
             scopes = [ store_scope; heap_scope ];
             step_func;
             prog;
             frames = [];
             store = None;
             cur_report_id = None;
             breakpoints = Hashtbl.create 0;
           }
            : debugger_state)

  let callstack_to_frames callstack next_proc_body_idx prog =
    callstack
    |> List.map
         (fun (se : Verification.SAInterpreter.CallStack.stack_element) : frame
         ->
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
                       (* VSCode column number s start from 1 *)
                       loc.loc_start.pos_column + 1,
                       loc.loc_end.pos_line,
                       loc.loc_end.pos_column + 1,
                       loc.loc_source ))
           in
           {
             (* TODO: make this a guaranteed unique index*)
             index = se.call_index;
             name = se.pid;
             source_path;
             start_line;
             start_column;
             end_line;
             end_column;
           })

  let step dbg =
    let report_id, cont_func = dbg.step_func () in
    dbg.cur_report_id <- report_id;
    let open Verification.SAInterpreter in
    let () =
      match report_id with
      | None           -> ()
      | Some report_id -> (
          let content, type_ = Logging.LogQueryer.get_report report_id in
          match type_ with
          | t when t = Logging.LoggingConstants.ContentType.cmd_step -> (
              let cmd_step =
                content |> Yojson.Safe.from_string |> cmd_step_of_yojson
              in
              match cmd_step with
              | Ok cmd_step ->
                  let () =
                    dbg.frames <-
                      callstack_to_frames cmd_step.call_stack
                        cmd_step.proc_body_index dbg.prog
                  in
                  dbg.store <- cmd_step.store
              | Error err   -> raise (Failure err))
          | _ as t ->
              raise
                (Failure
                   (Printf.sprintf
                      "Cannot deserialize: type '%s' does not match callstack" t))
          )
    in
    match cont_func with
    | Finished _         -> ReachedEnd
    | Continue step_func ->
        let () = dbg.step_func <- step_func in
        if has_hit_breakpoint dbg then Breakpoint else Step

  let rec run dbg =
    let stop_reason = step dbg in
    match stop_reason with
    | Step              -> run dbg
    | other_stop_reason -> other_stop_reason

  let terminate dbg =
    let () = Verification.postprocess_files dbg.source_files in
    let () = if !Config.stats then Statistics.print_statistics () in
    Logging.wrap_up ()

  let get_frames dbg = dbg.frames

  let get_scopes dbg = dbg.scopes

  let get_variables var_ref dbg =
    (* TODO: Display store and heap *)
    match Hashtbl.find_opt scopes_tbl var_ref with
    | None    -> []
    | Some id ->
        if id = "Store" then
          match dbg.store with
          | None       -> []
          | Some store ->
              (* let store = Verification.SAInterpreter.State.get_store state in *)
              Verification.SAInterpreter.Store.bindings store
              |> List.map (fun (var, value) ->
                     let value =
                       Fmt.to_to_string Verification.SAInterpreter.Val.full_pp
                         value
                     in
                     ({ name = var; value; type_ = None } : variable))
        else
          [
            ({ name = id ^ "_i"; value = "21354"; type_ = Some "integer" }
              : variable);
            ({ name = id ^ "_f"; value = "4.52"; type_ = Some "float" }
              : variable);
            ({ name = id ^ "_s"; value = "hello world"; type_ = Some "string" }
              : variable);
          ]

  let set_breakpoints source bp_list dbg =
    match source with
    (* We can't set the breakpoints if we do not know the source file *)
    | None -> ()
    | Some source ->
        let bp_set = Breakpoints.of_list bp_list in
        Hashtbl.replace dbg.breakpoints source bp_set
end
