open Cmdliner
open Command_line_utils

module Make
    (ID : Init_data.S)
    (PC : ParserAndCompiler.S with type init_data = ID.t) : Console.S = struct
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

  let process_files files =
    let progs =
      ParserAndCompiler.get_progs_or_fail ~pp_err:PC.pp_err
        (PC.parse_and_compile_files files)
    in
    List.iteri
      (fun i (path, prog) ->
        let init_data = if i = 0 then ID.to_yojson progs.init_data else `Null in
        burn_gil ~init_data ~pp_prog:Prog.pp_labeled prog (Some path))
      progs.gil_progs

  let compile files mode runtime_path ci tl_opts =
    let () = Config.ci := ci in
    let () = PC.TargetLangOptions.apply tl_opts in
    let () = PC.initialize mode in
    let () = Config.current_exec_mode := mode in
    let () =
      Config.set_runtime_paths ?default_folders:PC.default_import_paths
        runtime_path
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
