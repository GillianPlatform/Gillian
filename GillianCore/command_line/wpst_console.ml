open Cmdliner
open Command_line_utils
open Utils.Syntaxes.Result
module L = Logging
module SS = Containers.SS

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (SState : SState.S with type init_data = ID.t)
    (S_interpreter : G_interpreter.S
                       with type annot = PC.Annot.t
                        and type state_t = SState.t
                        and type state_err_t = SState.err_t)
    (Gil_parsing : Gil_parsing.S with type annot = PC.Annot.t)
    (Debug_adapter : Debug_adapter.S) : Console.S = struct
  module Common_args = Common_args.Make (PC)
  open Common_args

  let start_time = ref (Sys.time ())

  let json_ui =
    let doc = "Output some of the UI in JSON." in
    Arg.(value & flag & info [ "json-ui" ] ~doc)

  let unroll_depth =
    let default = 100 in
    let doc = "How many times are recursive calls called/loops unrolled" in
    Arg.(value & opt int default & info [ "unroll" ] ~doc)

  let check_leaks =
    let doc = "Check for memory leaks" in
    Arg.(value & flag & info [ "leak-check" ] ~doc)

  module Run = struct
    open ResultsDir
    open ChangeTracker

    let counter_example res =
      let error_state, errors =
        match res with
        | Exec_res.RFail { error_state; errors; _ } -> (error_state, errors)
        | _ -> failwith "Expected failure"
      in
      let f =
        errors
        |> List.find_map (function
             | Exec_err.EState StateErr.(EPure f) -> Some f
             | _ -> None)
      in
      let fs =
        match f with
        | Some f -> [ Expr.Infix.not f ]
        | None -> []
      in
      let subst = SState.sat_check_f error_state fs in
      subst

    let run_main prog init_data =
      let all_results =
        let open Syntaxes.List in
        let+ result_before_leak_check =
          S_interpreter.evaluate_proc
            (fun x -> x)
            prog !Config.entry_point [] (SState.init init_data)
        in
        if !Config.leak_check then
          let () = L.verbose (fun m -> m "Checking for memory leaks") in
          S_interpreter.check_leaks result_before_leak_check
        else result_before_leak_check
      in
      if !Config.json_ui then (
        Fmt.pr "===JSON RESULTS===\n@?";
        let state_to_yojson _ = `Null in
        let json_results =
          List.map
            (Engine.Exec_res.to_yojson state_to_yojson
               S_interpreter.state_vt_to_yojson S_interpreter.err_t_to_yojson)
            all_results
        in
        Fmt.pr "%a" (Yojson.Safe.pretty_print ~std:false) (`List json_results));
      let success =
        List.for_all
          (function
            | Exec_res.RSucc _ -> true
            | _ -> false)
          all_results
      in
      let total_time = Sys.time () -. !start_time in
      Printf.printf "Total time (Compilation + Symbolic testing): %fs\n"
        total_time;
      if success then
        let () = Fmt.pr "%a@\n@?" (Fmt.styled `Green Fmt.string) "Success!" in
        exit 0
      else
        let () =
          Fmt.pr "%a@\n@?" (Fmt.styled `Red Fmt.string) "Errors occurred!"
        in
        let first_error =
          List.find
            (function
              | Exec_res.RFail _ -> true
              | _ -> false)
            all_results
        in
        let counter_example = counter_example first_error in
        Fmt.pr "Here's a counterexample: %a@\n@?"
          (Fmt.option
             ~none:(Fmt.any "Couldn't produce counterexample")
             SVal.SESubst.pp)
          counter_example;
        let () =
          Fmt.pr "Here's an example of final error state: %a@\n@?"
            (Exec_res.pp SState.pp S_interpreter.pp_state_vt
               S_interpreter.pp_err_t)
            first_error
        in
        exit 1

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
          MP.(prog.prog)
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

    let f (prog : PC.Annot.t MP.prog) init_data incremental source_files =
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
      let+ progs = PC.parse_and_compile_files files in
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
      let+ e_prog, init_data =
        let* Gil_parsing.{ labeled_prog; init_data } =
          Gil_parsing.parse_eprog_from_file (List.hd files)
        in
        let+ init_data =
          match ID.of_yojson init_data with
          | Ok d -> Ok d
          | Error e -> Gillian_result.compilation_error e
        in
        (labeled_prog, init_data)
      in
      (e_prog, init_data, None)

  let process_files files already_compiled outfile_opt incremental =
    let t = Sys.time () in
    let* e_prog, init_data, source_files_opt =
      parse_eprog files already_compiled
    in
    let () =
      let pp_annot fmt annot =
        Fmt.pf fmt "%a"
          (Yojson.Safe.pretty_print ?std:None)
          (PC.Annot.to_yojson annot)
      in
      burn_gil
        ~pp_prog:(Prog.pp_labeled ~pp_annot)
        ~init_data:(ID.to_yojson init_data) e_prog outfile_opt
    in
    let () = L.normal (fun m -> m "*** Stage 2: Transforming the program.\n") in
    let+ prog =
      Gil_parsing.eprog_to_prog ~other_imports:PC.other_imports e_prog
    in
    let () =
      L.normal (fun m -> m "\n*** Stage 2: DONE transforming the program.\n")
    in
    Printf.printf "Compilation time: %fs\n" (Sys.time () -. t);
    let () = L.normal (fun m -> m "*** Stage 3: Symbolic Execution.\n") in
    match MP.init_prog prog with
    | Error _ -> failwith "Creation of matching plans failed"
    | Ok prog' -> run prog' init_data incremental source_files_opt

  let wpst
      files
      already_compiled
      outfile_opt
      no_heap
      stats
      incremental
      entry_point
      json_ui
      unroll
      leak_check
      () =
    let () = Fmt_tty.setup_std_outputs () in
    let () = Config.json_ui := json_ui in
    let () = Config.current_exec_mode := Symbolic in
    let () = Printexc.record_backtrace @@ L.Mode.enabled () in
    let () = Config.stats := stats in
    let () = Config.no_heap := no_heap in
    let () = Config.entry_point := entry_point in
    let () = Config.leak_check := leak_check in
    let () = PC.initialize Symbolic in
    let () = Config.max_branching := unroll in
    let r =
      Gillian_result.try_ @@ fun () ->
      process_files files already_compiled outfile_opt incremental
    in
    let () = if stats then Statistics.print_statistics () in
    let () = Common_args.exit_on_error r in
    exit 0

  let cmd_name = "wpst"

  let wpst_t =
    Term.(
      const wpst $ files $ already_compiled $ output_gil $ no_heap $ stats
      $ incremental $ entry_point $ json_ui $ unroll_depth $ check_leaks)

  let wpst_info =
    let doc = "Symbolically executes a file of the target language" in
    let man =
      [
        `S Manpage.s_description;
        `P "Symbolically executes a given file, after compiling it to GIL";
      ]
    in
    Cmd.info ~exits:Common_args.exit_code_info cmd_name ~doc ~man

  let wpst_cmd = Console.Normal (Cmd.v wpst_info (Common_args.use wpst_t))

  module Debug = struct
    let debug_wpst_info =
      let doc =
        "Starts Gillian in debugging mode for whole-program symbolic testing"
      in
      let man =
        [
          `S Manpage.s_description;
          `P
            "Starts Gillian in debugging mode for whole-program symbolic \
             testing, which communicates via the Debug Adapter Protocol";
        ]
      in
      Cmd.info cmd_name ~doc ~man

    let start_debug_adapter () =
      Config.current_exec_mode := Utils.Exec_mode.Symbolic;
      Lwt_main.run (Debug_adapter.start Lwt_io.stdin Lwt_io.stdout)

    let debug_wpst_t = Common_args.use Term.(const start_debug_adapter)
    let debug_wpst_cmd = Console.Debug (Cmd.v debug_wpst_info debug_wpst_t)
  end

  let cmds = [ wpst_cmd; Debug.debug_wpst_cmd ]
end
