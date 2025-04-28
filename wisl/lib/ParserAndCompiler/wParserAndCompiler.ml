open Lexing
open WLexer
open Utils.Syntaxes.Result

type init_data = unit
type err = unit
type tl_ast = WProg.t

module Annot = WAnnot

let pp_err _ () = ()

let parse_with_error token lexbuf =
  let open Utils.Gillian_result in
  try Ok (token read lexbuf) with
  | SyntaxError message ->
      let loc = CodeLoc.(to_location @@ curr lexbuf) in
      compilation_error ~loc ("Syntax error: " ^ message)
  | WParser.Error ->
      let loc = CodeLoc.(to_location @@ curr lexbuf) in
      compilation_error ~loc
        ("Syntax error: Unexpected token " ^ Lexing.lexeme lexbuf)

let with_lexbuf file f =
  match Hashtbl.find_opt Utils.Config.file_content_overrides file with
  | Some content -> Lexing.from_string content |> f
  | None ->
      let inx = open_in file in
      let lexbuf = Lexing.from_channel inx in
      let x = f lexbuf in
      let () = close_in inx in
      x

let parse_file file =
  with_lexbuf file @@ fun lexbuf ->
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } in
  parse_with_error WParser.prog lexbuf

let compile = Wisl2Gil.compile

let create_compilation_result path prog wprog =
  let open Command_line.ParserAndCompiler in
  let open IncrementalAnalysis in
  let source_files = SourceFiles.make () in
  let () = SourceFiles.add_source_file source_files ~path in
  let gil_path = Filename.chop_extension path ^ ".gil" in
  {
    gil_progs = [ (gil_path, prog) ];
    source_files;
    tl_ast = wprog;
    init_data = ();
  }

let parse_and_compile_files files =
  let () = WUtils.Generators.reset () in
  let f files =
    let path = List.hd files in
    let* wprog, configs = parse_file path in
    let+ () = WConfigStmt.apply_all configs in
    create_compilation_result path (compile ~filepath:path wprog) wprog
  in
  Logging.Phase.with_normal ~title:"Program parsing and compilation" (fun () ->
      f files)

let other_imports = []
let initialize _ = ()
let default_import_paths = Some Runtime_sites.Sites.runtime

module TargetLangOptions =
  Gillian.Command_line.ParserAndCompiler.Dummy.TargetLangOptions
