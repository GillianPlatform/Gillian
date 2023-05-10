open Cmdliner
open Command_line_utils
module L = Logging
module SS = Containers.SS

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (SState : SState.S with type init_data = ID.t)
    (S_interpreter : G_interpreter.S
                       with type annot = PC.Annot.t
                        and type state_t = SState.t)
    (Gil_parsing : Gil_parsing.S with type annot = PC.Annot.t) : Console.S =
struct
  module Common_args = Common_args.Make (PC)
  open Common_args

  module Run = struct
    open ResultsDir
    open ChangeTracker

    let run_main prog init_data =
      ignore
        (S_interpreter.evaluate_proc
           (fun x -> x)
           prog !Config.entry_point [] (SState.init init_data))

    let run_incr source_files prog init_data =
      (* Only re-run program if transitive callees of main proc have changed *)
      let cur_source_files =
        match source_files with
        | Some files -> files
        | None -> failwith "Cannot use -a in incremental mode"
      in
      let prev_source_files, prev_call_graph = read_symbolic_results () in
      let proc_changes =
        get_sym_changes
          UP.(prog.prog)
          ~prev_source_files ~prev_call_graph ~cur_source_files
      in
      let changed_procs =
        SS.of_list
          (proc_changes.changed_procs @ proc_changes.new_procs
         @ proc_changes.dependent_procs)
      in
      if SS.mem !Config.entry_point changed_procs then
        let () = run_main prog init_data in
        let cur_call_graph = S_interpreter.call_graph in
        let diff = Fmt.str "%a" ChangeTracker.pp_proc_changes proc_changes in
        write_symbolic_results cur_source_files cur_call_graph ~diff
      else write_symbolic_results cur_source_files prev_call_graph ~diff:""

    let rerun_all source_files prog init_data =
      (* Always re-run program *)
      let cur_source_files =
        Option.value ~default:(SourceFiles.make ()) source_files
      in
      let () = run_main prog init_data in
      let call_graph = S_interpreter.call_graph in
      write_symbolic_results cur_source_files call_graph ~diff:""

    let f (prog : PC.Annot.t UP.prog) init_data incremental source_files =
      if incremental && prev_results_exist () then
        run_incr source_files prog init_data
      else rerun_all source_files prog init_data
  end

  let run = Run.f

  let parse_eprog files already_compiled =
    if not already_compiled then
      let () =
        L.verbose (fun m ->
            m
              "@\n\
               *** Stage 1: Parsing program in original language and compiling \
               to Gil. ***@\n")
      in
      let progs =
        ParserAndCompiler.get_progs_or_fail ~pp_err:PC.pp_err
          (PC.parse_and_compile_files files)
      in
      let init_data = progs.init_data in
      let e_progs = progs.gil_progs in
      let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
      let e_prog = snd (List.hd e_progs) in
      let source_files = progs.source_files in
      (e_prog, init_data, Some source_files)
    else
      let () =
        L.verbose (fun m -> m "@\n*** Stage 1: Parsing Gil program. ***@\n")
      in
      let e_prog, init_data =
        let Gil_parsing.{ labeled_prog; init_data } =
          Gil_parsing.parse_eprog_from_file (List.hd files)
        in
        let init_data =
          match ID.of_yojson init_data with
          | Ok d -> d
          | Error e -> failwith e
        in
        (labeled_prog, init_data)
      in
      (e_prog, init_data, None)

  let process_files files already_compiled outfile_opt incremental =
    let e_prog, init_data, source_files_opt =
      parse_eprog files already_compiled
    in
    let () =
      burn_gil ~pp_prog:Prog.pp_labeled ~init_data:(ID.to_yojson init_data)
        e_prog outfile_opt
    in
    let () = L.normal (fun m -> m "*** Stage 2: Transforming the program.\n") in
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
    | Ok prog' -> run prog' init_data incremental source_files_opt

  let wpst
      files
      already_compiled
      outfile_opt
      no_heap
      stats
      incremental
      entry_point
      () =
    let () = Fmt_tty.setup_std_outputs () in
    let () = Config.current_exec_mode := Symbolic in
    let () = PC.initialize Symbolic in
    let () = Printexc.record_backtrace @@ L.Mode.enabled () in
    let () = Config.stats := stats in
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
      $ incremental $ entry_point)

  let wpst_info =
    let doc = "Symbolically executes a file of the target language" in
    let man =
      [
        `S Manpage.s_description;
        `P "Symbolically executes a given file, after compiling it to GIL";
      ]
    in
    Cmd.info "wpst" ~doc ~man

  let wpst_cmd = Cmd.v wpst_info (Common_args.use wpst_t)
  let cmds = [ wpst_cmd ]
end
