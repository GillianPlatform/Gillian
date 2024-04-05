open Cmdliner
open Command_line_utils

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t)
    (Gil_parsing : Gil_parsing.S with type annot = PC.Annot.t) : Console.S =
struct
  open Common_args.Make (PC)

  let mode =
    let open Exec_mode in
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

  let parse_eprog files =
    let progs =
      ParserAndCompiler.get_progs_or_fail ~pp_err:PC.pp_err
        (PC.parse_and_compile_files files)
    in
    let e_progs = progs.gil_progs in
    let () = Gil_parsing.cache_labelled_progs (List.tl e_progs) in
    let path, e_prog = List.hd e_progs in
    (path, e_prog, progs.init_data)

  let process_files files =
    let path, e_prog, init_data = parse_eprog files in
    let pp_annot fmt annot =
      Fmt.pf fmt "%a"
        (Yojson.Safe.pretty_print ?std:None)
        (PC.Annot.to_yojson annot)
    in
    burn_gil
      ~pp_prog:(Prog.pp_labeled ~pp_annot)
      ~init_data:(ID.to_yojson init_data) e_prog (Some path)

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
  let cmds = [ compile_cmd ]
end
