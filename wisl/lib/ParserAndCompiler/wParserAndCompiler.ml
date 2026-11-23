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

let parse_file file =
  Utils.Config.with_lexbuf file @@ fun lexbuf ->
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

let fail_message (ctx : Command_line.ParserAndCompiler.fail_ctx) ~code params =
  let pp_val ft v = Expr_pp.pp ft (ctx.simplify v) in
  let pp_typed ft v =
    match ctx.get_type v with
    | Some t ->
        Fmt.pf ft "%a (type %s)" pp_val v (Gillian.Gil_syntax.Type.str t)
    | None -> Fmt.pf ft "%a (type unknown)" pp_val v
  in
  match (code, params) with
  | "InvalidAddition", [ l; r ] ->
      Some
        (Fmt.str
           "Can't add %a and %a: expected two numbers, or a pointer and a \
            number"
           pp_typed l pp_typed r)
  | "InvalidPointer", [ v ] ->
      Some (Fmt.str "Can't dereference %a: not a pointer" pp_typed v)
  | "InvalidBlockPointer", [ v ] ->
      Some
        (Fmt.str "Can't free %a: not a pointer to the start of a block" pp_val v)
  | "IncomparablePointers", [ l; r ] ->
      Some
        (Fmt.str "Can't compare pointers into different blocks %a and %a" pp_val
           l pp_val r)
  | _ -> None

let pp_expr = Expr_pp.pp

let default_import_paths =
  Some
    [
      Utils.Embedded.dir ~name:"wisl-runtime"
        (List.map
           (fun f -> (f, Option.get (Runtime_files.read f)))
           Runtime_files.file_list);
    ]

module TargetLangOptions = struct
  open Cmdliner

  type t = { loop_hack : bool }

  let term =
    let docs = Manpage.s_common_options in

    let loop_hack =
      let doc =
        "Apply the loop invariant hack (see \
         https://github.com/GillianPlatform/Gillian/issues/347)"
      in
      Arg.(value & flag & info [ "loop-hack" ] ~docs ~doc)
    in

    let opt loop_hack = { loop_hack } in
    Term.(const opt $ loop_hack)

  let apply { loop_hack } = WConfig.loop_hack := loop_hack
end
