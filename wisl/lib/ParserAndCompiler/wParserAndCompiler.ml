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

let parse_and_compile_files files =
  let path = List.hd files in
  Ok (compile path (parse_file path))

let other_imports = []

let initialize _ = ()

let env_var_import_path = Some WConfig.import_env_var

module TargetLangOptions =
  Gillian.CommandLine.ParserAndCompiler.Dummy.TargetLangOptions
