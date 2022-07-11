open Cmdliner
module ParserAndCompiler = ParserAndCompiler
module L = Logging
module SS = Containers.SS

let convert_other_imports oi =
  List.map
    (fun (ext, f) ->
      let fun_with_exn s = Stdlib.Result.get_ok (f s) in
      (ext, fun_with_exn))
    oi

module Make
    (CMemory : CMemory.S)
    (SMemory : SMemory.S)
    (External : External.S)
    (PC : ParserAndCompiler.S) (Runners : sig
      val runners : Bulk.Runner.t list
    end)
    (Gil_to_tl_lifter : Debugger.Gil_to_tl_lifter.S
                          with type memory = SMemory.t
                           and type memory_error = SMemory.err_t
                           and type tl_ast = PC.tl_ast) =
struct
  module CState = CState.Make (CMemory)

  module CInterpreter =
    GInterpreter.Make (CVal.M) (CVal.CESubst) (CStore) (CState) (External)

  module SState = SState.Make (SMemory)

  module SInterpreter =
    GInterpreter.Make (SVal.M) (SVal.SESubst) (SStore) (SState) (External)

  module SPState =
    PState.Make (SVal.M) (SVal.SESubst) (SStore) (SState) (Preds.SPreds)

  module Verification = Verifier.Make (SState) (SPState) (External)
  module Abductor = Abductor.Make (SState) (SPState) (External)
  module Debugger = Debugger.Make (PC) (Verification) (Gil_to_tl_lifter)
  module DebugAdapter = DebugAdapter.Make (Debugger)

  let entry_point =
    let doc = "Entry point of execution." in
    let docv = "PROCEDURE_NAME" in
    let default = !Utils.Config.entry_point in
    Arg.(
      value & opt string default & info [ "start"; "entry-point" ] ~docv ~doc)

  let files =
    let doc = "Input file." in
    let docv = "FILE" in
    Arg.(non_empty & pos_all file [] & info [] ~docv ~doc)

  let ci =
    let doc = "Indicates that the tool is being run in CI." in
    Arg.(value & flag & info [ "ci" ] ~doc)

  let already_compiled =
    let doc =
      "If you are working on already-compiled GIL files. Otherwise, the \
       compilation process will take place before analysis."
    in
    Arg.(value & flag & info [ "a"; "already-compiled" ] ~doc)

  let logging_mode =
    let open L.Mode in
    let parse = function
      | "disabled" -> Result.ok @@ Disabled
      | "normal" -> Result.ok @@ Enabled Normal
      | "verbose" -> Result.ok @@ Enabled Verbose
      | "tmi" -> Result.ok @@ Enabled TMI
      | other -> Result.error @@ `Msg ("unknown value \"" ^ other ^ "\"")
    in
    let c = Arg.conv (parse, L.Mode.pp) in
    let default = Enabled Verbose in
    let doc =
      "Controls the verbosity level of logging. The value SETTING must be one \
       of `disabled', `normal', `verbose', `tmi'."
    in
    Arg.(value & opt c default & info [ "l"; "logging" ] ~docv:"SETTING" ~doc)

  type reporter_info = { name : string; reporter : L.Reporter.t }

  let reporters =
    let parse : string -> (reporter_info, [> `Msg of string ]) Result.t =
      function
      | "file" -> Ok { name = "file"; reporter = L.file_reporter }
      | "database" | "db" ->
          Ok { name = "database"; reporter = L.database_reporter }
      | other -> Error (`Msg ("unknown value \"" ^ other ^ "\""))
    in
    let print fmt (reporter_info : reporter_info) =
      Fmt.string fmt reporter_info.name
    in
    let c = Arg.(list & conv (parse, print)) in
    let default : reporter_info list =
      [ { name = "file"; reporter = L.file_reporter } ]
    in
    let doc =
      "Controls which reporters are used when logging. The value REPORTERS \
       must be a comma separated list (with no spaces) of REPORTER values. A \
       REPORTER value must be one of `file`, `database`, `db` (short for \
       `database`)."
    in
    Arg.(
      value & opt c default & info [ "r"; "reporters" ] ~docv:"REPORTERS" ~doc)

  let output_gil =
    let doc =
      "If specified, will write the compiled GIL program into $(docv)."
    in
    let docv = "OUTPUT" in
    Arg.(
      value & opt (some string) None & info [ "o"; "output"; "burn" ] ~doc ~docv)

  let runtime_path =
    let doc = "Specify runtime library path for imports." in
    let docv = "RUNTIME_PATH" in
    Arg.(value & opt_all string [] & info [ "R"; "runtime" ] ~doc ~docv)

  let no_heap =
    let doc = "If you want the heap output to be supressed in the logs" in
    Arg.(value & flag & info [ "no-heap" ] ~doc)

  let stats =
    let doc =
      "If you want to display statistics about the execution at the end"
    in
    Arg.(value & flag & info [ "stats" ] ~doc)

  let parallel =
    let docv = "PARALLEL" in
    let doc = "Enable parallel execution, default is false" in
    Arg.(value & flag & info [ "p"; "parallel" ] ~doc ~docv)

  let no_print_failures =
    let doc =
      "Do not print the list of all the failed tests at the end of the bulk \
       execution"
    in
    Arg.(value & flag & info [ "no-print-all-failures" ] ~doc)

  let incremental =
    let doc =
      "Perform analysis in incremental mode, where only the changed parts of \
       code are re-analysed."
    in
    Arg.(value & flag & info [ "inc"; "incremental" ] ~doc)

  let bi_unroll_depth =
    let default = 1 in
    let doc =
      "How many times are recursive calls called/loops unrolled in \
       bi-abduction."
    in
    Arg.(value & opt int default & info [ "bi-unroll" ] ~doc)

  let bi_no_spec_depth =
    let default = 0 in
    let doc = "The depth at which we start to re-use specs in bi-abduction." in
    Arg.(value & opt int default & info [ "bi-no-spec" ] ~doc)

  let result_directory =
    (* Default value is taken from the non-modified config *)
    let default = Config.results_dir () in
    let doc =
      Printf.sprintf
        "Set result directory relative path to $(docv). Defaults to \"%s\""
        default
    in
    let docv = "OUT_DIR" in
    Arg.(value & opt string default & info [ "result-dir" ] ~doc ~docv)

  let pbn =
    let doc = "Print-by-need." in
    Arg.(value & flag & info [ "pbn"; "print-by-need" ] ~doc)

  let dump_smt =
    let doc = "Dump every smt query sent to z3" in
    Arg.(value & flag & info [ "dump-smt" ] ~doc)

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

  let with_common (term : (unit -> unit) Term.t) : unit Term.t =
    let apply_common
        logging_mode
        reporters
        runtime_path
        ci
        tl_opts
        result_dir
        pbn
        dump_smt =
      Config.set_result_dir result_dir;
      Config.ci := ci;
      Logging.Mode.set_mode logging_mode;
      let reporters = List.map (fun r -> r.reporter) reporters in
      L.initialize reporters;
      Printexc.record_backtrace (Logging.Mode.enabled ());
      PC.TargetLangOptions.apply tl_opts;
      Config.set_runtime_paths ?env_var:PC.env_var_import_path runtime_path;
      Config.pbn := pbn;
      Config.dump_smt := dump_smt
    in
    let common_term =
      Term.(
        const apply_common $ logging_mode $ reporters $ runtime_path $ ci
        $ PC.TargetLangOptions.term $ result_directory $ pbn $ dump_smt)
    in
    Term.(term $ common_term)

  let burn_gil prog outfile_opt =
    match outfile_opt with
    | Some outfile ->
        let outc = open_out outfile in
        let fmt = Format.formatter_of_out_channel outc in
        let () = Prog.pp_labeled fmt prog in
        close_out outc
    | None -> ()

  module CompilerConsole = struct
    let mode =
      let open ExecMode in
      let doc = "Compile for Verification mode" in
      let verif =
        (Verification, Arg.info [ "ver"; "verif"; "verification" ] ~doc)
      in
      let doc = "Compile for Automatic Compositional Testing mode" in
      let act = (BiAbduction, Arg.info [ "bi"; "bi-abduction"; "act" ] ~doc) in
      let doc = "Compile for Concrete Execution mode" in
      let concrete = (Concrete, Arg.info [ "concrete"; "conc" ] ~doc) in
      let doc = "Compile for Whole Program Symbolic Testing mode" in
      let wpst =
        (Symbolic, Arg.info [ "symbolic"; "symb"; "wpst"; "stest" ] ~doc)
      in
      Arg.(last & vflag_all [ Verification ] [ concrete; wpst; verif; act ])

    let process_files files =
      let progs = get_progs_or_fail (PC.parse_and_compile_files files) in
      List.iter
        (fun (path, prog) -> Io_utils.save_file_pp path Prog.pp_labeled prog)
        progs.gil_progs

    let compile files mode runtime_path ci tl_opts =
      let () = Config.ci := ci in
      let () = PC.TargetLangOptions.apply tl_opts in
      let () = PC.initialize mode in
      let () = Config.current_exec_mode := mode in
      let () =
        Config.set_runtime_paths ?env_var:PC.env_var_import_path runtime_path
      in
      process_files files

    let compile_t =
      Term.(
        const compile $ files $ mode $ runtime_path $ ci
        $ PC.TargetLangOptions.term)

    let compile_info =
      let doc = "Compiles a file from the target language to GIL" in
      let man =
        [
          `S Manpage.s_description;
          `P
            "Compiles a file to GIL. Target execution mode can be specified, \
             defaults to Verification.";
        ]
      in
      Cmd.info "compile" ~doc ~man

    let compile_cmd = Cmd.v compile_info compile_t
  end

  module CInterpreterConsole = struct
    let return_to_exit (ret_ok : bool) : unit =
      match ret_ok with
      | false -> exit 1
      | true -> ()

    let valid_concrete_result (ret : CInterpreter.result_t list) : bool =
      assert (List.length ret = 1);
      let ret = List.hd ret in
      match ret with
      | ExecRes.RSucc { flag = Flag.Normal; _ } -> true
      | _ -> false

    let run debug (prog : ('a, int) Prog.t) : unit =
      let prog =
        match UP.init_prog prog with
        | Ok prog -> prog
        | _ -> failwith "Program could not be initialised"
      in
      let ret = CInterpreter.evaluate_prog prog in
      let () =
        if debug then
          Format.printf "Final state: @\n%a@\n" CInterpreter.Logging.pp_result
            ret
      in
      return_to_exit (valid_concrete_result ret)

    let exec files already_compiled debug outfile_opt no_heap entry_point () =
      let () = Config.current_exec_mode := Concrete in
      let () = PC.initialize Concrete in
      let () = Config.no_heap := no_heap in
      let () = Config.entry_point := entry_point in
      let e_prog =
        if not already_compiled then (
          let e_progs =
            (get_progs_or_fail (PC.parse_and_compile_files files)).gil_progs
          in
          Gil_parsing.cache_labelled_progs (List.tl e_progs);
          snd (List.hd e_progs))
        else Gil_parsing.parse_eprog_from_file (List.hd files)
      in
      let () = burn_gil e_prog outfile_opt in
      let prog =
        Gil_parsing.eprog_to_prog
          ~other_imports:(convert_other_imports PC.other_imports)
          e_prog
      in
      let () = run debug prog in
      Logging.wrap_up ()

    let debug =
      let doc =
        "If you want the final state of concrete execution to be printed at \
         the end"
      in
      Arg.(value & flag & info [ "debug"; "print-final-state" ] ~doc)

    let exec_t =
      Term.(
        const exec $ files $ already_compiled $ debug $ output_gil $ no_heap
        $ entry_point)

    let exec_info =
      let doc = "Concretely executes a file of the target language" in
      let man =
        [
          `S Manpage.s_description;
          `P "Concretely executes a given file, after compiling it to GIL";
        ]
      in
      Cmd.info "exec" ~doc ~man

    let exec_cmd = Cmd.v exec_info (with_common exec_t)
  end

  module SInterpreterConsole = struct
    let run (prog : UP.prog) incremental source_files =
      let open ResultsDir in
      let open ChangeTracker in
      let run_main prog =
        ignore
          (SInterpreter.evaluate_proc
             (fun x -> x)
             prog !Config.entry_point [] (SState.init ()))
      in
      if incremental && prev_results_exist () then
        (* Only re-run program if transitive callees of main proc have changed *)
        let cur_source_files =
          match source_files with
          | Some files -> files
          | None -> failwith "Cannot use -a in incremental mode"
        in
        let prev_source_files, prev_call_graph = read_symbolic_results () in
        let proc_changes =
          get_sym_changes prog.prog ~prev_source_files ~prev_call_graph
            ~cur_source_files
        in
        let changed_procs =
          SS.of_list
            (proc_changes.changed_procs @ proc_changes.new_procs
           @ proc_changes.dependent_procs)
        in
        if SS.mem !Config.entry_point changed_procs then
          let () = run_main prog in
          let cur_call_graph = SInterpreter.call_graph in
          let diff = Fmt.str "%a" ChangeTracker.pp_proc_changes proc_changes in
          write_symbolic_results cur_source_files cur_call_graph ~diff
        else write_symbolic_results cur_source_files prev_call_graph ~diff:""
      else
        (* Always re-run program *)
        let cur_source_files =
          Option.value ~default:(SourceFiles.make ()) source_files
        in
        let () = run_main prog in
        let call_graph = SInterpreter.call_graph in
        write_symbolic_results cur_source_files call_graph ~diff:""

    let process_files files already_compiled outfile_opt incremental =
      let e_prog, source_files_opt =
        if not already_compiled then
          let () =
            L.verbose (fun m ->
                m
                  "@\n\
                   *** Stage 1: Parsing program in original language and \
                   compiling to Gil. ***@\n")
          in
          let progs = get_progs_or_fail (PC.parse_and_compile_files files) in
          let e_progs = progs.gil_progs in
          let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
          let e_prog = snd (List.hd e_progs) in
          let source_files = progs.source_files in
          (e_prog, Some source_files)
        else
          let () =
            L.verbose (fun m -> m "@\n*** Stage 1: Parsing Gil program. ***@\n")
          in
          (Gil_parsing.parse_eprog_from_file (List.hd files), None)
      in
      let () = burn_gil e_prog outfile_opt in
      let () =
        L.normal (fun m -> m "*** Stage 2: Transforming the program.\n")
      in
      let prog =
        Gil_parsing.eprog_to_prog
          ~other_imports:(convert_other_imports PC.other_imports)
          e_prog
      in
      let () =
        L.normal (fun m -> m "\n*** Stage 2: DONE transforming the program.\n")
      in
      let () = L.normal (fun m -> m "*** Stage 3: Symbolic Execution.\n") in
      match UP.init_prog prog with
      | Error _ -> failwith "Creation of unification plans failed"
      | Ok prog' -> run prog' incremental source_files_opt

    let wpst
        files
        already_compiled
        outfile_opt
        no_heap
        stats
        parallel
        incremental
        entry_point
        () =
      let () = Config.current_exec_mode := Symbolic in
      let () = PC.initialize Symbolic in
      let () = Printexc.record_backtrace @@ L.Mode.enabled () in
      let () = Config.stats := stats in
      let () = Config.parallel := parallel in
      let () = Config.no_heap := no_heap in
      let () = Config.entry_point := entry_point in
      let () = process_files files already_compiled outfile_opt incremental in
      let () = if stats then Statistics.print_statistics () in
      let () = Logging.wrap_up () in
      try
        while true do
          let _ = Unix.wait () in
          ()
        done
      with Unix.Unix_error (Unix.ECHILD, "wait", _) -> ()

    let wpst_t =
      Term.(
        const wpst $ files $ already_compiled $ output_gil $ no_heap $ stats
        $ parallel $ incremental $ entry_point)

    let wpst_info =
      let doc = "Symbolically executes a file of the target language" in
      let man =
        [
          `S Manpage.s_description;
          `P "Symbolically executes a given file, after compiling it to GIL";
        ]
      in
      Cmd.info "wpst" ~doc ~man

    let wpst_cmd = Cmd.v wpst_info (with_common wpst_t)
  end

  module VerificationConsole = struct
    let no_lemma_proof =
      let doc = "Do not verify the proofs of lemmas." in
      Arg.(value & flag & info [ "no-lemma-proof" ] ~doc)

    let proc_arg =
      let doc =
        "Specifies a procedure or list of procedures that should be verified. \
         By default, everything is verieid, but only if neither --proc nor \
         --lemma is specified. A combination of --lemma and --proc can be \
         used."
      in
      let docv = "PROC_NAME" in
      Arg.(value & opt_all string [] & info [ "proc" ] ~doc ~docv)

    let lemma_arg =
      let doc =
        "Specifies a procedure or list of lemmas that should be verified. By \
         default, everything is verified, but only if neither --proc nor \
         --lemma is specified. A combination of --lemma and --proc can be \
         used."
      in
      let docv = "LEMMA_NAME" in
      Arg.(value & opt_all string [] & info [ "lemma" ] ~doc ~docv)

    let no_unfold =
      let doc = "Disable automatic unfolding of non-recursive predicates." in
      Arg.(value & flag & info [ "no-unfold" ] ~doc)

    let manual =
      let doc = "Disable automatic folding and unfolding heuristics." in
      Arg.(value & flag & info [ "m"; "manual" ] ~doc)

    let exact =
      let doc = "Exact verification" in
      Arg.(value & flag & info [ "exv"; "exact" ] ~doc)

    let process_files files already_compiled outfile_opt no_unfold incremental =
      Verification.start_time := Sys.time ();
      Fmt.pr "Parsing and compiling...\n@?";
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
      Fmt.pr "Preprocessing...\n@?";
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
            m "@\nProgram after logic preprocessing:@\n%a@\n" Prog.pp_indexed
              prog)
      in
      Verification.verify_prog prog incremental source_files_opt

    let verify
        files
        already_compiled
        outfile_opt
        no_unfold
        stats
        no_lemma_proof
        manual
        exact
        incremental
        procs_to_verify
        lemmas_to_verify
        () =
      let () = Fmt_tty.setup_std_outputs () in
      let () = Config.stats := stats in
      let () = Config.lemma_proof := not no_lemma_proof in
      let () = Config.current_exec_mode := Verification in
      let () = PC.initialize Verification in
      let () = Config.manual_proof := manual in
      let () = Config.Verification.exact := exact in
      let () = if exact then Fmt.pr "Exact verification enabled.\n" in
      let () = Config.Verification.set_procs_to_verify procs_to_verify in
      let () = Config.Verification.set_lemmas_to_verify lemmas_to_verify in
      let () =
        process_files files already_compiled outfile_opt no_unfold incremental
      in
      let () = if stats then Statistics.print_statistics () in
      Logging.wrap_up ()

    let verify_t =
      Term.(
        const verify $ files $ already_compiled $ output_gil $ no_unfold $ stats
        $ no_lemma_proof $ manual $ exact $ incremental $ proc_arg $ lemma_arg)

    let verify_info =
      let doc = "Verifies a file of the target language" in
      let man =
        [
          `S Manpage.s_description;
          `P "Verifies a given file, after compiling it to GIL";
        ]
      in
      Cmd.info "verify" ~doc ~man

    let verify_cmd = Cmd.v verify_info (with_common verify_t)
  end

  module ACTConsole = struct
    let emit_specs =
      let doc =
        "Emit the final GIL program containing all the derived specifications."
      in
      Arg.(value & flag & info [ "emit-specs" ] ~doc)

    let specs_to_stdout =
      let doc = "Emit specs to stdout, useful for testing." in
      Arg.(value & flag & info [ "specs-to-stdout" ] ~doc)

    let process_files files already_compiled outfile_opt emit_specs incremental
        =
      let file = List.hd files in
      let e_prog, source_files_opt =
        if not already_compiled then
          let () =
            L.verbose (fun m ->
                m
                  "@\n\
                   *** Stage 1: Parsing program in original language and \
                   compiling to Gil. ***@\n")
          in
          let progs = get_progs_or_fail (PC.parse_and_compile_files files) in
          let e_progs = progs.gil_progs in
          let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
          let e_prog = snd (List.hd e_progs) in
          let source_files = progs.source_files in
          (e_prog, Some source_files)
        else
          let () =
            L.verbose (fun m -> m "@\n*** Stage 1: Parsing Gil program. ***@\n")
          in
          (Gil_parsing.parse_eprog_from_file file, None)
      in
      let () = burn_gil e_prog outfile_opt in
      let () =
        L.normal (fun m -> m "*** Stage 2: Transforming the program.@\n")
      in
      let prog =
        Gil_parsing.eprog_to_prog
          ~other_imports:(convert_other_imports PC.other_imports)
          e_prog
      in
      let () =
        L.normal (fun m ->
            m "@\n*** Stage 2: DONE transforming the program.@\n")
      in
      let () = L.normal (fun m -> m "*** Stage 3: Symbolic Execution.@\n") in
      let () = Config.unfolding := false in
      let prog = LogicPreprocessing.preprocess prog true in
      match UP.init_prog prog with
      | Error _ -> failwith "Creation of unification plans failed."
      | Ok prog' ->
          let () = Abductor.test_prog prog' incremental source_files_opt in
          if emit_specs then
            let () = Prog.update_specs e_prog prog'.prog in
            let fname = Filename.chop_extension (Filename.basename file) in
            let dirname = Filename.dirname file in
            let out_path = Filename.concat dirname (fname ^ "_bi.gil") in
            Io_utils.save_file_pp out_path Prog.pp_labeled e_prog

    let act
        files
        already_compiled
        outfile_opt
        no_heap
        stats
        parallel
        emit_specs
        specs_to_stdout
        incremental
        bi_unroll_depth
        bi_no_spec_depth
        () =
      let () = Config.current_exec_mode := BiAbduction in
      let () = PC.initialize BiAbduction in
      let () = Config.stats := stats in
      let () = Config.no_heap := no_heap in
      let () = Config.parallel := parallel in
      let () = Config.bi_unroll_depth := bi_unroll_depth in
      let () = Config.bi_no_spec_depth := bi_no_spec_depth in
      let () = Config.specs_to_stdout := specs_to_stdout in
      let () =
        process_files files already_compiled outfile_opt emit_specs incremental
      in
      let () = if !Config.stats then Statistics.print_statistics () in
      Logging.wrap_up ()

    let act_t =
      Term.(
        const act $ files $ already_compiled $ output_gil $ no_heap $ stats
        $ parallel $ emit_specs $ specs_to_stdout $ incremental
        $ bi_unroll_depth $ bi_no_spec_depth)

    let act_info =
      let doc =
        "Automatic Compositional Testing of a file of the target language"
      in
      let man =
        [
          `S Manpage.s_description;
          `P
            "Uses Automatic Compositional Testing on a given file, after \
             compiling it to GIL";
        ]
      in
      Cmd.info "act" ~doc ~man

    let act_cmd = Cmd.v act_info (with_common act_t)
  end

  module BulkConsole = struct
    let make_bulk_console runner =
      let exec_mode = Runner.exec_mode runner in
      let path_t =
        let doc = "Path of the test suite." in
        let docv = "PATH" in
        Arg.(required & pos 0 (some file) None & info [] ~docv ~doc)
      in
      let run test_suite_path npaf incremental () =
        let () = Config.current_exec_mode := exec_mode in
        let () = PC.initialize exec_mode in
        let () = Config.bulk_print_all_failures := not npaf in
        let () = Logging.Mode.set_mode Disabled in
        let () = Printexc.record_backtrace false in
        Runner.run_all runner ~test_suite_path ~incremental
      in
      let run_t = Term.(const run $ path_t $ no_print_failures $ incremental) in
      let run_info =
        let doc = "Executes a predefined test-suite" in
        let man =
          [ `S Manpage.s_description; `P "Execute a predefined test-suite" ]
        in
        Cmd.info (Runner.cmd_name runner) ~doc ~man
      in
      Cmd.v run_info (with_common run_t)

    let bulk_cmds = List.map make_bulk_console Runners.runners
  end

  module DebugVerificationConsole = struct
    let debug_verify_info =
      let doc = "Starts Gillian in debugging mode for verification" in
      let man =
        [
          `S Manpage.s_description;
          `P
            "Starts Gillian in debugging mode for verification, which \
             communicates via the Debug Adapter Protocol";
        ]
      in
      Cmd.info "debugverify" ~doc ~man

    let start_debug_adapter () =
      Lwt_main.run (DebugAdapter.start Lwt_io.stdin Lwt_io.stdout)

    let debug_verify_t = with_common Term.(const start_debug_adapter)
    let debug_verify_cmd = Cmd.v debug_verify_info debug_verify_t
  end

  let main () =
    let doc = "An analysis toolchain" in

    let man =
      [
        `S Manpage.s_description;
        `P "Analysis toolchain for a given language, based on Gillian";
      ]
    in
    let info = Cmd.info (Filename.basename Sys.executable_name) ~doc ~man in

    let cmds =
      [
        CompilerConsole.compile_cmd;
        CInterpreterConsole.exec_cmd;
        SInterpreterConsole.wpst_cmd;
        VerificationConsole.verify_cmd;
        ACTConsole.act_cmd;
        DebugVerificationConsole.debug_verify_cmd;
      ]
      @ BulkConsole.bulk_cmds
    in
    exit (Cmd.eval (Cmd.group info cmds))
end
