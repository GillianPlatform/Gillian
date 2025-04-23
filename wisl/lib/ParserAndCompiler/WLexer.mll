{
  open Lexing
  open CodeLoc
  open WParser

  exception SyntaxError of string
  let l_start_string = ref CodeLoc.dummy
}


let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let gvars = "gvar_" digit+ (* generated variables during compilation *)
let identifier = letter(letter|digit|'_')*
let lvar = '#' (letter|digit|'_'|'$')*
let integer = digit+
let loc = "$l" (letter|digit|'_')*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  (* keywords *)
  | "@config" { CONFIG (curr lexbuf) }
  | "true"   { TRUE (curr lexbuf) }
  | "false"  { FALSE (curr lexbuf) }
  | "nil"    { LSTNIL (curr lexbuf) }
  | "null"   { NULL (curr lexbuf) }
  | "while"  { WHILE (curr lexbuf) }
  | "if"     { IF (curr lexbuf) }
  | "else"   { ELSE (curr lexbuf) }
  | "skip"   { SKIP (curr lexbuf) }
  | "fresh"  { FRESH (curr lexbuf) }
  | "new"    { NEW (curr lexbuf) }
  | "free"   { DELETE (curr lexbuf) }
  | "dispose"{ DELETE (curr lexbuf) }
  | "function" { FUNCTION (curr lexbuf) }
  | "predicate" { PREDICATE (curr lexbuf) }
  | "invariant" { INVARIANT (curr lexbuf) }
  | "return" { RETURN (curr lexbuf) }
  | "fold"   { FOLD (curr lexbuf) }
  | "package" { PACKAGE (curr lexbuf) }
  | "unfold" { UNFOLD (curr lexbuf) }
  | "nounfold" { NOUNFOLD (curr lexbuf) }
  | "apply"  { APPLY (curr lexbuf) }
  | "assert" { ASSERT (curr lexbuf) }
  | "assume" { ASSUME (curr lexbuf) }
  | "assume_type" { ASSUME_TYPE (curr lexbuf) }
  | "with" { WITH (curr lexbuf) }
  | "variant" { VARIANT (curr lexbuf) }
  | "statement" { STATEMENT (curr lexbuf) }
  | "proof"  { PROOF (curr lexbuf) }
  | "lemma"  { LEMMA (curr lexbuf) }
  | "forall" { FORALL (curr lexbuf) }
  | "bind" { EXIST (curr lexbuf) }
  (* types *)
  | "List" { TLIST (curr lexbuf) }
  | "Int" { TINT (curr lexbuf) }
  | "Bool" { TBOOL (curr lexbuf) }
  | "String" { TSTRING (curr lexbuf) }
  (* strings and comments *)
  | '"'      { let () = l_start_string := curr lexbuf in
               read_string (Buffer.create 17) lexbuf }
  | "//"     { read_comment lexbuf }
  (* logical binary stuff *)
  | "-*"     { WAND }
  | "->"     { ARROW }
  | "-b>"   { BLOCK_ARROW }
  | "/\\"    { AND }
  | "\\/"    { OR }
  (* punctuation *)
  | "-{"     { SETOPEN (curr lexbuf) }
  | "}-"     { SETCLOSE (curr lexbuf) }
  | "[["     { LOGOPEN (curr lexbuf) }
  | "]]"     { LOGCLOSE(curr lexbuf) }
  | '['      { LBRACK (curr lexbuf) }
  | ']'      { RBRACK (curr lexbuf) }
  | '{'      { LCBRACE (curr lexbuf) }
  | '}'      { RCBRACE (curr lexbuf) }
  | '('      { LBRACE (curr lexbuf) }
  | ')'      { RBRACE (curr lexbuf) }
  | ":="     { ASSIGN (curr lexbuf) }
  | ':'      { COLON (curr lexbuf) }
  | ','      { COMMA (curr lexbuf) }
  | "."      { DOT (curr lexbuf) }
  | ';'      { SEMICOLON (curr lexbuf) }
  | "|-"     { VDASH (curr lexbuf) }
  (* binary operators *)
  | "::"     { LSTCONS }
  | '@'      { LSTCAT }
  | "=="     { EQUAL }
  | ">="     { GREATEREQUAL }
  | '>'      { GREATERTHAN }
  | '<'      { LESSTHAN }
  | "<="     { LESSEQUAL }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIV }
  | '%'      { MOD }
  | "&&"     { AND }
  | "||"     { OR }
  | "!="     { NEQ }
  | "lnth"   { LSTNTH }
  (* unary operators *)
  | "emp"    { EMP (curr lexbuf) }
  | "len"    { LEN (curr lexbuf) }
  | "hd"     { HEAD (curr lexbuf) }
  | "tl"     { TAIL (curr lexbuf) }
  | "rev"    { REV (curr lexbuf) }
  | "sub"    { SUB (curr lexbuf) }
  | '!'      { NOT (curr lexbuf) }
  (* identifiers *)
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | integer   { INTEGER (curr lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | gvars    { IDENTIFIER (curr lexbuf, (Lexing.lexeme lexbuf)^"_user") } (* if it has a name of generated var, we add _user *)
  | identifier { IDENTIFIER (curr lexbuf, Lexing.lexeme lexbuf) }
  | lvar       { LVAR (curr lexbuf, Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_string buf =
  parse
  | '"'       { let lend = curr lexbuf in
                let loc = merge (!l_start_string) lend in
                STRING (loc, Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment =
  parse
  | newline { new_line lexbuf; read lexbuf }
  | eof     { EOF }
  | _       { read_comment lexbuf }
