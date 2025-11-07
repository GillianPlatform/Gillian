open Gillian
module Gil_parser = Gil_parsing.Make (Gil_syntax.Annot.Basic)

module TargetLangOptions = struct
  type t = unit

  let term = Cmdliner.Term.(const ())
  let apply () = ()
end

type init_data = unit
type tl_ast = unit

module Annot = Gil_syntax.Annot.Basic

type err = unit

let pp_err _ _ =
  failwith
    "Please implement the compiling interface to use with the '-compile' flag \
     or test suites"

let parse_and_compile_files files :
    ( Gil_parser.annot,
      tl_ast,
      init_data )
    Gillian.Command_line.ParserAndCompiler.compiled_progs
    Utils.Gillian_result.t =
  let gil_progs =
    List.map
      (fun fl ->
        let eprog = Gil_parser.parse_eprog_from_file fl in
        match eprog with
        | Ok parsing_result -> (fl, parsing_result.labeled_prog)
        | Error e ->
            let msg =
              Format.asprintf "Failed to parse file %s:%@%a" fl
                Utils.Gillian_result.Error.pp e
            in
            failwith msg)
      files
  in
  let source_files = Gillian.IncrementalAnalysis.SourceFiles.make () in
  Ok { gil_progs; source_files; tl_ast = (); init_data = () }

let other_imports = []
let default_import_paths = None
let initialize _ = ()
