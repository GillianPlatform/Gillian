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
    ( ( Gil_parser.annot,
        init_data,
        tl_ast )
      Gillian.Command_line.ParserAndCompiler.compiled_progs,
      unit )
    result =
  let eprogs =
    List.map
      (fun fl ->
        let eprog = Gil_parser.parse_eprog_from_file fl in
        (fl, eprog.labeled_prog))
      files
  in
  Ok
    {
      gil_progs = eprogs;
      source_files = Gillian.IncrementalAnalysis.SourceFiles.make ();
      tl_ast = ();
      init_data = ();
    }

let other_imports = []
let default_import_paths = None
let initialize _ = ()
