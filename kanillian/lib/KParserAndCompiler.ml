open Gillian
open Kcommons

let initialize _ =
  let open Kanillian_compiler in
  Utils.Config.entry_point := Constants.CBMC_names.start;
  Option.iter
    (fun kstats_file -> at_exit (fun () -> Stats.report kstats_file))
    !Kconfig.kstats_file

let env_var_import_path = Some Kanillian_compiler.Imports.env_path_var
let other_imports = []

type init_data = unit
type tl_ast = Program.t

module Annot = Kanillian_compiler.Annot

module TargetLangOptions = struct
  type t = {
    kstats_file : string option;
    harness : string option;
    hide_genv : bool;
  }

  let term =
    let open Cmdliner in
    let docs = Manpage.s_common_options in
    let doc =
      "If set, write out a file containing the statistics about the \
       compilation process. If the file already exists, it adds to stats to \
       it, otherwise it creates it."
    in
    let kstats_file =
      Arg.(
        value
        & opt (some string) None
        & info [ "kstats-file"; "kstats" ] ~docs ~doc)
    in
    let doc =
      "Decides the entry point of the proof. Note that this is different from \
       the --entry-point option."
    in
    let docv = "FUNCTION_NAME" in
    let harness =
      Arg.(value & opt (some string) None & info [ "harness" ] ~docs ~doc ~docv)
    in
    let doc = "Hide the global environment from the logs" in
    let hide_genv = Arg.(value & flag & info [ "hide-genv" ] ~docs ~doc) in
    let opt kstats_file harness hide_genv =
      { kstats_file; harness; hide_genv }
    in
    Term.(const opt $ kstats_file $ harness $ hide_genv)

  let apply { kstats_file; harness; hide_genv } =
    Kconfig.kstats_file := kstats_file;
    Kconfig.harness := harness;
    Kconfig.hide_genv := hide_genv
end

type err = string

let pp_err = Fmt.string

let parse_symtab_into_goto json =
  let tbl = Irep_lib.Symtab.of_yojson json in
  Result.map
    (fun tbl ->
      let machine = Machine_model_parse.consume_from_symtab tbl in
      if not Machine_model.(equal machine archi64) then
        failwith "For now, kanillian can only run on archi64";
      Kconfig.machine_model := machine;
      Logging.normal ~severity:Warning (fun m ->
          m
            "Filtering every cprover_specific symbol!! Need to remove that in \
             the future");
      Goto_lib.Program.of_symtab ~machine tbl)
    tbl

let create_compilation_result path goto_prog gil_prog =
  let open Gillian.Command_line.ParserAndCompiler in
  let open IncrementalAnalysis in
  let source_files = SourceFiles.make () in
  let () = SourceFiles.add_source_file source_files ~path in
  let gil_path = Filename.chop_extension path ^ ".gil" in
  {
    gil_progs = [ (gil_path, gil_prog) ];
    source_files;
    tl_ast = goto_prog;
    init_data = ();
  }

let compile_c_to_symtab c_file =
  let symtab_file = c_file ^ ".symtab.json" in
  let status =
    Sys.command
      (Fmt.str "cbmc %s --show-symbol-table --json-ui > %s" c_file symtab_file)
  in
  if status <> 0 then
    Fmt.failwith "CMBC failed to compile %s with return code %d" c_file status;
  symtab_file

let load_symtab_from_file file =
  let json = Yojson.Safe.from_file file in
  let symtab =
    match json with
    | `List objects -> (
        let symtabs = Yojson.Safe.Util.filter_member "symbolTable" objects in
        match symtabs with
        | [ symtab ] -> Ok symtab
        | [] -> Error "Couldn't find 'symbolTable' field"
        | _ -> Error "Found multiple 'symbolTable' fields")
    | _ -> Error "Couldn't find list at top level"
  in
  match symtab with
  | Ok symtab -> symtab
  | Error msg -> Fmt.failwith "Malformed symtab file - %s" msg

let parse_and_compile_files files =
  let open Kanillian_compiler in
  let open Utils.Syntaxes.Result in
  (* Call CBMC ourselves *)
  let path =
    match files with
    | [ p ] -> (
        match Filename.extension p with
        | ".json" -> p
        | ".c" -> compile_c_to_symtab p
        | ext -> Fmt.failwith "Unknown file type '%s'!" ext)
    | _ -> failwith "Kanillian only handles one file at the moment"
  in
  let json = load_symtab_from_file path in
  let+ goto_prog = parse_symtab_into_goto json in
  let goto_prog = Sanitize.sanitize_program goto_prog in
  let context =
    Ctx.make
      ~exec_mode:!Kutils.Config.current_exec_mode
      ~machine:!Kconfig.machine_model ~prog:goto_prog ~harness:!Kconfig.harness
      ()
  in
  let gil_prog = Compile.compile context in
  create_compilation_result path goto_prog gil_prog
