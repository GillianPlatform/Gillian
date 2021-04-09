module L = Logging

module type S = sig
  type stop_reason = Step | ReachedEnd

  type debugger_state

  val launch : string -> (debugger_state, string) result

  val step : debugger_state -> stop_reason

  val run : debugger_state -> stop_reason

  val terminate : debugger_state -> unit
end

module Make (PC : ParserAndCompiler.S) (Verification : Verifier.S) = struct
  type stop_reason = Step | ReachedEnd

  type debugger_state = {
    source_file : string;
    source_files : SourceFiles.t option;
    mutable step_func : unit -> GInterpreter.cont_func;
  }

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

  let launch file_name =
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
    | ReachedEnd         -> Error "Nothing to run"
    | Continue step_func ->
        Ok
          ({
             source_file = file_name;
             source_files = source_files_opt;
             step_func;
           }
            : debugger_state)

  let step dbg =
    let cont_func = dbg.step_func () in
    match cont_func with
    | GInterpreter.ReachedEnd -> ReachedEnd
    | GInterpreter.Continue step_func ->
        let () = dbg.step_func <- step_func in
        Step

  let rec run dbg =
    let stop_reason = step dbg in
    match stop_reason with
    | Step       -> run dbg
    | ReachedEnd -> ReachedEnd

  let terminate dbg =
    let () = Verification.postprocess_files dbg.source_files in
    let () = if !Config.stats then Statistics.print_statistics () in
    Logging.wrap_up ()
end
