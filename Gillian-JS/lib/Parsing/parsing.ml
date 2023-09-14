open Lexing

let log_verbose = Gillian.Logging.verbose
let col pos = pos.pos_cnum - pos.pos_bol + 1

let parse start lexbuf =
  try start Javert_Lexer.read lexbuf with
  | Javert_Lexer.Syntax_error message -> failwith ("Syntax error: " ^ message)
  | Javert_Parser.Error ->
      let loc_start = lexeme_start_p lexbuf in
      let loc_end = lexeme_end_p lexbuf in
      let unexpected_token = lexeme lexbuf in
      let message =
        Printf.sprintf
          "unexpected token : %s at loc %i:%i-%i:%i while reading %s"
          unexpected_token loc_start.pos_lnum (col loc_start) loc_end.pos_lnum
          (col loc_end)
          (if String.equal loc_start.pos_fname "" then "a string"
          else loc_start.pos_fname)
      in
      failwith ("Parser Error, " ^ message)

let parse_from_string start str =
  let lexbuf = from_string str in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" } in
  try parse start lexbuf with
  | Failure msg ->
      failwith
        (Printf.sprintf "Failed while trying to parse the string :@\n%s@\n%s"
           str msg)
  | _ ->
      failwith
        (Printf.sprintf "Unknown parsing error while parsing the string :@\n%s"
           str)

let parse_js_pre_from_string :
    string -> (string * Utils.Containers.SS.t) option * Jslogic.JSAsrt.t =
  parse_from_string Javert_Parser.top_level_js_pre_target

let parse_js_logic_assertion_from_string : string -> Jslogic.JSAsrt.t =
  parse_from_string Javert_Parser.top_level_js_assertion_target

let parse_js_logic_assertion_list_from_string : string -> Jslogic.JSAsrt.t list
    =
  parse_from_string Javert_Parser.top_level_js_assertion_list_target

let parse_js_logic_predicate_from_string : string -> Jslogic.JSPred.t =
  parse_from_string Javert_Parser.js_pred_target

let parse_js_only_spec_from_string : string -> Jslogic.JSSpec.t =
  parse_from_string Javert_Parser.js_only_spec_target

let parse_js_logic_commands_from_string : string -> Jslogic.JSLCmd.t list =
  parse_from_string Javert_Parser.js_logic_cmds_target

let parse_expr_from_string : string -> Gillian.Gil_syntax.Expr.t =
  parse_from_string Javert_Parser.top_level_expr_target

let parse_jsil_eprog_from_file (path : string) : Jsil_syntax.EProg.t =
  let extension = List.hd (List.rev (Str.split (Str.regexp "\\.") path)) in
  let file_previously_normalised = String.equal "njsil" extension in
  Utils.Config.previously_normalised := file_previously_normalised;
  (* Check that the file is of a valid type *)
  (match file_previously_normalised || String.equal "jsil" extension with
  | true -> ()
  | false ->
      raise
        (Failure
           (Printf.sprintf "Failed to import %s: not a .jsil or .njsil file."
              path)));
  let inx = open_in path in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path };
  let prog = parse Javert_Parser.jsil_main_target lexbuf in
  close_in inx;
  prog
