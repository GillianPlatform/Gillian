let burn_jsil = ref false

module TargetLangOptions = struct
  open Cmdliner

  type t = { jsil : bool; harness : bool; burn_jsil : bool }

  let term =
    let docs = Manpage.s_common_options in
    let doc = "If the target language is JSIL instead of JS" in
    let jsil = Arg.(value & flag & info [ "jsil"; "from-jsil" ] ~docs ~doc) in
    let doc = "Enable ES6 harness" in
    let harness = Arg.(value & flag & info [ "harness" ] ~docs ~doc) in
    let doc = "If you want to write the intermediate JSIL file" in
    let burn_jsil = Arg.(value & flag & info [ "burn-jsil" ] ~docs ~doc) in
    let f jsil harness burn_jsil = { jsil; harness; burn_jsil } in
    Term.(const f $ jsil $ harness $ burn_jsil)

  let apply { jsil; harness; burn_jsil = conf_burn_jsil } =
    burn_jsil := conf_burn_jsil;
    Javert_utils.Js_config.js := not jsil;
    Javert_utils.Js_config.js2jsil_harnessing := harness
end

type err = JSParserErr of JS_Parser.Error.t | JS2GILErr of string

let pp_err fmt = function
  | JS2GILErr s   -> Fmt.pf fmt "%s" s
  | JSParserErr s -> Fmt.pf fmt "%s" (JS_Parser.Error.str s)

let parse_and_compile_js path =
  try
    let e_str = Javert_utils.Io_utils.load_js_file path in
    let e_str =
      if !Javert_utils.Js_config.cosette then
        JS_PreParser.stringify_assume_and_assert e_str
      else e_str
    in
    let offset_converter = JS_Utils.memoized_offsetchar_to_offsetline e_str in
    let e = JS_Parser.parse_string_exn e_str in
    let (ext_prog : Jsil_syntax.EProg.t), _, _ =
      JS2JSIL_Compiler.js2jsil e offset_converter
        (ExecMode.verification_exec !Config.current_exec_mode)
    in
    let ext_prog =
      if !Config.unfolding then JSIL_PostParser.post_parse_eprog ext_prog
      else ext_prog
    in
    let ext_prog =
      if ExecMode.biabduction_exec !Config.current_exec_mode then
        JSIL_PostParser.bi_post_parse_eprog ext_prog JS2JSIL_Compiler.cc_tbl
          JS2JSIL_Compiler.vis_tbl
      else ext_prog
    in
    let () =
      if !burn_jsil then
        let jsil_file_name = Filename.chop_extension path ^ ".jsil" in

        Io_utils.save_file_pp jsil_file_name Jsil_syntax.EProg.pp ext_prog
    in
    let core_prog = JSIL2GIL.jsil2core_prog ext_prog in
    Ok core_prog
  with
  | JS_Parser.Error.ParserError e -> Error (JSParserErr e)
  | JS2JSIL_Preprocessing.EarlyError e ->
      Error
        (JS2GILErr
           (Printf.sprintf "\nParser post-processing threw an EarlyError: %s\n"
              e))
  | _ ->
      Error
        (JS2GILErr
           (Printf.sprintf "\nParsing problems with the file '%s'.\n" path))

let parse_and_compile_jsil path =
  let jsil_prog = Parsing.parse_jsil_eprog_from_file path in
  let core_prog = JSIL2GIL.jsil2core_prog jsil_prog in
  Ok core_prog

let parse_and_compile_file path =
  if !Javert_utils.Js_config.js then parse_and_compile_js path
  else parse_and_compile_jsil path

let other_imports = [ ("jsil", parse_and_compile_jsil) ]

let import_paths = Javert_utils.Js_config.import_paths

let env_var_import_path = Some "GILLIAN_JS_RUNTIME_PATH"

let initialize exec_mode =
  let open ExecMode in
  Javert_utils.Js_config.cosette :=
    biabduction_exec exec_mode || symbolic_exec exec_mode;
  if concrete_exec exec_mode then Config.unfolding := false
