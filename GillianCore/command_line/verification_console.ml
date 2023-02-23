open Cmdliner
open Command_line_utils
module L = Logging

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (Verification : Verifier.S
                      with type annot = PC.Annot.t
                       and type SPState.init_data = ID.t)
    (Gil_parsing : Gil_parsing.S with type annot = PC.Annot.t) : Console.S =
struct
  module Common_args = Common_args.Make (PC)
  open Common_args

  let no_lemma_proof =
    let doc = "Do not verify the proofs of lemmas." in
    Arg.(value & flag & info [ "no-lemma-proof" ] ~doc)

  let proc_arg =
    let doc =
      "Specifies a procedure or list of procedures that should be verified. By \
       default, everything is verieid, but only if neither --proc nor --lemma \
       is specified. A combination of --lemma and --proc can be used."
    in
    let docv = "PROC_NAME" in
    Arg.(value & opt_all string [] & info [ "proc" ] ~doc ~docv)

  let lemma_arg =
    let doc =
      "Specifies a procedure or list of lemmas that should be verified. By \
       default, everything is verified, but only if neither --proc nor --lemma \
       is specified. A combination of --lemma and --proc can be used."
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

  let parse_eprog files already_compiled =
    if not already_compiled then
      let progs =
        ParserAndCompiler.get_progs_or_fail PC.pp_err
          (PC.parse_and_compile_files files)
      in
      let e_progs = progs.gil_progs in
      let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
      let e_prog = snd (List.hd e_progs) in
      let source_files = progs.source_files in
      (e_prog, progs.init_data, Some source_files)
    else
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

  let process_files files already_compiled outfile_opt no_unfold incremental =
    Verification.start_time := Sys.time ();
    Fmt.pr "Parsing and compiling...\n@?";
    let e_prog, init_data, source_files_opt =
      parse_eprog files already_compiled
    in
    let () =
      burn_gil ~pp_prog:Prog.pp_labeled ~init_data:(ID.to_yojson init_data)
        e_prog outfile_opt
    in
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
          m "@\nProgram after logic preprocessing:@\n%a@\n" Prog.pp_indexed prog)
    in
    Verification.verify_prog ~init_data prog incremental source_files_opt

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

  let verify_cmd = Cmd.v verify_info (Common_args.use verify_t)
  let cmds = [ verify_cmd ]
end
