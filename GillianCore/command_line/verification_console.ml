open Cmdliner
open Command_line_utils
open Syntaxes.Result
module L = Logging

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (Verification : Verifier.S
                      with type annot = PC.Annot.t
                       and type SPState.init_data = ID.t)
    (Gil_parsing : Gil_parsing.S with type annot = PC.Annot.t)
    (Debug_adapter : Debug_adapter.S) : Console.S = struct
  module Common_args = Common_args.Make (PC)
  open Common_args

  let no_lemma_proof =
    let doc = "Do not verify the proofs of lemmas." in
    Arg.(value & flag & info [ "no-lemma-proof" ] ~doc)

  let procs_only =
    let doc = "Only verify procs." in
    Arg.(value & flag & info [ "procs-only" ] ~doc)

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

  let parse_eprog files already_compiled =
    if not already_compiled then
      let+ progs = PC.parse_and_compile_files files in
      let e_progs = progs.gil_progs in
      let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
      let e_prog = snd (List.hd e_progs) in
      let source_files = progs.source_files in
      (e_prog, progs.init_data, Some source_files)
    else
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

  let verify files already_compiled outfile_opt no_unfold incremental =
    Gillian_result.try_ @@ fun () ->
    Verification.start_time := Sys.time ();
    Fmt.pr "Parsing and compiling...\n@?";
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
    (* Prog.perform_syntax_checks e_prog; *)
    Fmt.pr "Preprocessing...\n@?";
    let* prog =
      Gil_parsing.eprog_to_prog ~other_imports:PC.other_imports e_prog
    in
    let () =
      L.verbose (fun m ->
          m "@\nProgram as parsed:@\n%a@\n"
            (Prog.pp_indexed ?pp_annot:None)
            prog)
    in
    let prog = LogicPreprocessing.preprocess prog (not no_unfold) in
    let () =
      L.verbose (fun m ->
          m "@\nProgram after logic preprocessing:@\n%a@\n"
            (Prog.pp_indexed ?pp_annot:None)
            prog)
    in
    Verification.verify_prog ~init_data prog incremental source_files_opt

  let verify_once
      files
      already_compiled
      outfile_opt
      no_unfold
      stats
      no_lemma_proof
      manual
      incremental
      procs_to_verify
      lemmas_to_verify
      procs_only
      () =
    (* Attention: if you plan to add UX verification, you must be careful about predicates.
       In our current formalism, they must be stricly exact. *)
    let () = Fmt_tty.setup_std_outputs () in
    let () = Config.stats := stats in
    let () = Config.lemma_proof := not no_lemma_proof in
    let () = Config.current_exec_mode := Verification in
    let () = PC.initialize Verification in
    let () = Config.manual_proof := manual in
    let () =
      match (procs_to_verify, lemmas_to_verify, procs_only) with
      | [], [], true -> Config.Verification.things_to_verify := ProcsOnly
      | _ ->
          Config.Verification.set_procs_to_verify procs_to_verify;
          Config.Verification.set_lemmas_to_verify lemmas_to_verify
    in
    let r = verify files already_compiled outfile_opt no_unfold incremental in
    let () = if stats then Statistics.print_statistics () in
    let () = Common_args.exit_on_error r in
    exit 0

  let cmd_name = "verify"

  let verify_t =
    Term.(
      const verify_once $ files $ already_compiled $ output_gil $ no_unfold
      $ stats $ no_lemma_proof $ manual $ incremental $ proc_arg $ lemma_arg
      $ procs_only)

  let verify_info =
    let doc = "Verifies a file of the target language" in
    let man =
      [
        `S Manpage.s_description;
        `P "Verifies a given file, after compiling it to GIL";
      ]
    in
    Cmd.info ~exits:Common_args.exit_code_info cmd_name ~doc ~man

  let verify_cmd = Console.Normal (Cmd.v verify_info (Common_args.use verify_t))

  module Debug = struct
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
      Cmd.info cmd_name ~doc ~man

    let start_debug_adapter procs_only manual () =
      Config.current_exec_mode := Utils.Exec_mode.Verification;
      let () =
        if procs_only then Config.Verification.(things_to_verify := ProcsOnly)
      in
      Config.manual_proof := manual;
      Lwt_main.run (Debug_adapter.start Lwt_io.stdin Lwt_io.stdout)

    let debug_verify_t =
      Common_args.use Term.(const start_debug_adapter $ procs_only $ manual)

    let debug_verify_cmd =
      Console.Debug (Cmd.v debug_verify_info debug_verify_t)
  end

  module Lsp = struct
    let debug_verify_info =
      let doc = "Starts Gillian in language server mode for verification" in
      let man =
        [
          `S Manpage.s_description;
          `P
            "Starts Gillian in language server mode for verification, which \
             communicates via the Language Server Protocol";
        ]
      in
      Cmd.info cmd_name ~doc ~man

    let start_language_server procs_only manual () =
      Config.current_exec_mode := Utils.Exec_mode.Verification;
      let () =
        if procs_only then Config.Verification.(things_to_verify := ProcsOnly)
      in
      Config.manual_proof := manual;
      let analyse file = verify [ file ] false None false false in
      Lsp_server.run analyse

    let lsp_verify_t =
      Common_args.use Term.(const start_language_server $ procs_only $ manual)

    let lsp_verify_cmd = Console.Lsp (Cmd.v debug_verify_info lsp_verify_t)
  end

  let cmds = [ verify_cmd; Debug.debug_verify_cmd; Lsp.lsp_verify_cmd ]
end
