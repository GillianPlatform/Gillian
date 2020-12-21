open Lexing
open WLexer

type err = unit

let pp_err _ () = ()

let parse_with_error token lexbuf =
  try token read lexbuf with
  | SyntaxError message -> failwith ("SYNTAX ERROR" ^ message)
  | WParser.Error       ->
      let range = CodeLoc.curr lexbuf in
      let message =
        Printf.sprintf "unexpected token : %s at loc %s" (Lexing.lexeme lexbuf)
          (CodeLoc.str range)
      in
      failwith ("PARSER ERROR : " ^ message)

let parse_file file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } in
  let wprog = parse_with_error WParser.prog lexbuf in
  let () = close_in inx in
  wprog

let compile = Wisl2Gil.compile

let create_compilation_result path prog =
  let open CommandLine.ParserAndCompiler in
  let open IncrementalAnalysis in
  let source_files = SourceFiles.make () in
  let () = SourceFiles.add_source_file source_files ~path in
  let gil_path = Filename.chop_extension path ^ ".gil" in
  { gil_progs = [ (gil_path, prog) ]; source_files }

let parse_and_compile_files files =
  let f files =
    let path = List.hd files in
    Ok
      (create_compilation_result path
         (compile ~filepath:path (parse_file path)))
  in
  Logging.with_normal_phase ~title:"Program parsing and compilation" (fun () ->
      f files)

let other_imports = []

let initialize _ = ()

let env_var_import_path = Some WConfig.import_env_var

module TargetLangOptions =
  Gillian.CommandLine.ParserAndCompiler.Dummy.TargetLangOptions
