open Cmdliner
module L = Logging

let exit_code_info =
  [
    Cmd.Exit.info ~doc:"Success" 0;
    Cmd.Exit.info ~doc:"Analysis / runtime failure" 1;
    Cmd.Exit.info ~doc:"Compilation error" 2;
    Cmd.Exit.info ~doc:"Operation error - e.g. incorrect arguments" 124;
    Cmd.Exit.info ~doc:"Internal error - please report this!" 125;
  ]

module Make (PC : ParserAndCompiler.S) = struct
  let exit_code_info = exit_code_info

  let exit_on_error = function
    | Ok x -> x
    | Error e ->
        let () = Logging.print_to_all (Gillian_result.Error.show e) in
        exit (Gillian_result.Error.to_error_code e)

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
    let doc = "Dump every smt query sent to the solver" in
    Arg.(value & flag & info [ "dump-smt" ] ~doc)

  let dump_annots =
    let doc = "Dump annotations produced in compilation" in
    Arg.(value & flag & info [ "dump-annots" ] ~doc)

  let use (term : (unit -> unit) Term.t) : unit Term.t =
    let apply_common
        logging_mode
        reporters
        runtime_path
        ci
        tl_opts
        result_dir
        pbn
        dump_smt
        dump_annots =
      Config.set_result_dir result_dir;
      Config.ci := ci;
      Logging.Mode.set_mode logging_mode;
      let reporters = List.map (fun r -> r.reporter) reporters in
      L.initialize reporters;
      Printexc.record_backtrace (Logging.Mode.enabled ());
      PC.TargetLangOptions.apply tl_opts;
      Config.set_runtime_paths ?default_folders:PC.default_import_paths
        runtime_path;
      Config.pbn := pbn;
      Config.dump_smt := dump_smt;
      Config.dump_annots := dump_annots
    in
    let common_term =
      Term.(
        const apply_common $ logging_mode $ reporters $ runtime_path $ ci
        $ PC.TargetLangOptions.term $ result_directory $ pbn $ dump_smt
        $ dump_annots)
    in
    Term.(term $ common_term)
end
