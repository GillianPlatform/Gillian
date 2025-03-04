{
open Lexing
open C2_annot_parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let in_annot = ref false

}


let digit = ['0'-'9']
let int = '-'? digit digit*
let letter = ['a'-'z''A'-'Z']
let number = int

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let lvar = '#' (letter|digit|'_'|'$')*
let loc = "$l" (letter|digit|'_')*



rule read =
  parse
  | "/*@"      { ANNOT_OPEN }
  | "*/"       { ANNOT_CLOSE }
  | "//"       { read_comment lexbuf }
  | '`'        { read_string (Buffer.create 17) lexbuf }
  | "-g>"      { GLOBALPOINTSTO }
  | "-m>"      { MALLOCPOINTSTO }
  | "->"       { POINTSTO }
  | "import"   { IMPORT }
  | "verify"   { VERIFY }
  | "nounfold" { NOUNFOLD }
  | "if"       { IF }
  | "else"     { ELSE }
  | "OR"       { BIGOR }
  | "ZEROS"    { ZEROS }
  | "MALLOCED" { MALLOCED }
  | "MARRAY"   { MALLOCED_ARRAY }
  | "ARRAY"    { ARRAY }
  | "UNDEFS"   { UNDEFS }
  | "abstract" { ABSTRACT }
  | "pred"     { PREDICATE }
  | "pure"     { PURE }
  | "lemma"    { LEMMA }
  | "hypothesis" { HYPOTHESIS }
  | "conclusions" { CONCLUSIONS }
  | "proof"    { PROOF }
  | "spec"     { SPECIFICATION }
  | "axiomatic" { AXIOMATIC }
  | "requires" { REQUIRES }
  | "ensures"  { ENSURES }
  | "struct"   { STRUCT }
  | "int16"    { INT16T }
  | "int"      { INTT }
  | "float"    { FLOATT }
  | "long"     { LONGT }
  | "single"   { SINGLET }
  | "char"     { CHART }
  | "ptr"      { PTRT }
  | "funptr"   { FUNPTRT }
  | "emp"      { EMP }
  | "True"     { TRUE }
  | "False"    { FALSE }
  | "nil"      { NIL }
  | "NULL"     { NULL }
  | "TRUE"     { CTRUE }
  | "FALSE"    { CFALSE }
  | "bind"     { BIND }
  | "unfold*" { REC_UNFOLD }
  | "unfold"   { UNFOLD }
  | "unfold_all" { UNFOLD_ALL }
  | "symb_exec" { SYMB_EXEC }
  | "fold"     { FOLD }
  | "apply"    { APPLY }
  | "invariant" { INVARIANT }
  | "for_loop" { FOR_LOOP }
  (* | "while_loop" { WHILE_LOOP }- *)
  | "assert"   { ASSERT }
  | "branch"   { BRANCH }
  | "len"      { LEN }
  | "lsub"     { LSUB }
  | "not"      { LNOT }
  | "forall"   { FORALL }
  | "Int"      { GINTT }
  | "Set"      { GSETT }
  | "List"     { GLISTT }
  | number     { let lexeme = Lexing.lexeme lexbuf in
                 try INTEGER (Z.of_string lexeme) with
                 | _ -> raise (SyntaxError ("invalid number: " ^ lexeme)) }
  | lvar       { LVAR ( Lexing.lexeme lexbuf) }
  | loc        { LOC (Lexing.lexeme lexbuf) }
  | id         { IDENTIFIER (Lexing.lexeme lexbuf) }
  | "-{"       { SETOP }
  | "}-"       { SETCL }
  | '('        { LBRACE }
  | ')'        { RBRACE }
  | '{'        { LCBRACE }
  | '}'        { RCBRACE }
  | '['        { LBRACK }
  | ']'        { RBRACK }
  | "[["       { LDBRACK }
  | "]]"       { RDBRACK }
  | ';'        { SCOLON }
  | '.'        { DOT }
  | ','        { COMMA }
  | "-u-"      { SETUNION }
  | "-s-"      { SETSUB }
  | "-d-"      { SETDIFF }
  | "-e-"      { SETMEM }
  | "--e--"    { LSETMEM }
  | "=>"       { IMPLIES }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | "p+"       { PTRPLUS }
  | '*'        { STAR }
  | '/'        { DIV }
  | "::"       { LSTCONS }
  | "@"        { LSTCAT }
  | ':'        { COLON }
  | "=="       { LEQ }
  | "="        { EQ }
  | '!'        { ENOT }
  | "<#"       { LLT }
  | "<=#"      { LLEQ }
  | "<"        { LT }
  | "||"       { LOR }
  | "&&"       { LAND }
  | "&"        { AND }
  | "|"        { OR }
  | newline    { next_line lexbuf; read lexbuf }
  | white      { read lexbuf }
  | eof        { EOF }
  | _          { read lexbuf }


and read_comment =
  parse
  | newline {  next_line lexbuf; read lexbuf }
  | eof     { EOF }
  | _       { read_comment lexbuf }

and read_string buf =
  parse
  | '`'       { STRING(Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '`' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
