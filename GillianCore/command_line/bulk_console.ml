open Cmdliner

module Make (PC : ParserAndCompiler.S) (Runners : Runners.S) : Console.S =
struct
  module Common_args = Common_args.Make (PC)
  open Common_args

  let make_bulk_console runner =
    let exec_mode = Runner.exec_mode runner in
    let path_t =
      let doc = "Paths of the test suite (files or directories)." in
      let docv = "PATH" in
      Arg.(non_empty & pos_all file [] & info [] ~docv ~doc)
    in
    let run test_suite_paths npaf incremental () =
      let () = Config.current_exec_mode := exec_mode in
      let () = Config.bulk_print_all_failures := not npaf in
      let () = PC.initialize exec_mode in
      let () = Logging.Mode.set_mode Disabled in
      let () = Printexc.record_backtrace false in
      Runner.run_all runner ~test_suite_paths ~incremental
    in
    let run_t = Term.(const run $ path_t $ no_print_failures $ incremental) in
    let run_info =
      let doc = "Executes a predefined test-suite" in
      let man =
        [ `S Manpage.s_description; `P "Execute a predefined test-suite" ]
      in
      Cmd.info ~exits:Common_args.exit_code_info (Runner.cmd_name runner) ~doc
        ~man
    in
    Console.Normal (Cmd.v run_info (Common_args.use run_t))

  let cmds = List.map make_bulk_console Runners.runners
end
