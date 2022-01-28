module Syntax = GJS_syntax
module PrettyPrint = PrettyPrint
module Error = Error
module Loc = Loc

let parse_string_exn
    ?(parse_annotations = true)
    ?(force_strict = false)
    ?program_path
    prog =
  let open Flow_parser in
  let parse_options =
    Some
      Parser_env.
        { default_parse_options with types = false; use_strict = force_strict }
  in
  let transform = OfFlow.transform_program ~parse_annotations ~force_strict in
  match program_path with
  | None ->
      (* Program is string passed to eval() or Function(); parse as regular script *)
      let prog, errors = Parser_flow.program ~fail:false ~parse_options prog in
      let () = Utils.check_parsing_errors errors in
      transform prog
  | Some program_path ->
      (* Parse program as CommonJS module *)
      let parse = Parser_flow.program_file ~fail:false ~parse_options in
      Modules.parse_commonjs parse transform program_path prog

let parse_string
    ?(parse_annotations = true)
    ?(force_strict = false)
    ?program_path
    program =
  try
    Ok (parse_string_exn ~parse_annotations ~force_strict ?program_path program)
  with Error.ParserError err -> Error err
