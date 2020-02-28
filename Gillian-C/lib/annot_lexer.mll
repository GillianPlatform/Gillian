{
open Lexing
open Annot_parser

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
let frac = '.' digit*
let number = int frac?

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
  | '"'        { read_string (Buffer.create 17) lexbuf }
  | "-m>"      { MALLOCPOINTSTO }
  | "->"       { POINTSTO }
  | "if"       { IF }
  | "else"     { ELSE }
  | "OR"       { BIGOR }
  | "pred"     { PREDICATE }
  | "spec"     { SPECIFICATION }
  | "requires" { REQUIRES }
  | "ensures"  { ENSURES }
  | "struct"   { STRUCT }
  | "int"      { INTT }
  | "float"    { FLOATT }
  | "long"     { LONGT }
  | "single"   { SINGLET }
  | "ptr"      { PTRT }
  | "emp"      { EMP }
  | "True"     { TRUE }
  | "False"    { FALSE }
  | "nil"      { NIL }
  | "NULL"     { NULL }
  | "TRUE"     { CTRUE }
  | "FALSE"    { CFALSE }
  | "exists"   { EXISTS }
  | "unfold"   { UNFOLD }
  | "fold"     { FOLD }
  | "assert"   { ASSERT }
  | "len"      { LEN }
  | "not"      { LNOT }
  | "forall"   { FORALL }
  | "Num"      { GNUMT }
  | "Set"      { GSETT }
  | number     { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
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
  | ';'        { SCOLON }
  | '.'        { DOT }
  | ','        { COMMA }
  | "-u-"      { SETUNION }
  | "-s-"      { SETSUB }
  | "-d-"      { SETDIFF }
  | "--e--"    { SETMEM }
  | "=>"       { IMPLIES }
  | '+'        { PLUS }
  | '*'        { STAR }
  | "::"       { LSTCONS }
  | "@"        { LSTCAT }
  | ':'        { COLON }
  | "=="       { LEQ }
  | "="        { EQ }
  | '!'        { ENOT }
  | "<#"       { LLT }
  | "<=#"      { LLEQ }
  | newline    { next_line lexbuf; read lexbuf }
  | white      { read lexbuf }
  | eof        { EOF }
  | _          { UNKOWN }


and read_comment =
  parse
  | newline {  next_line lexbuf; read lexbuf }
  | eof     { EOF }
  | _       { read_comment lexbuf }

and read_string buf =
  parse
  | '"'       { if !in_annot then
                failwith ("We do not handle strings such as \"" ^ (Buffer.contents buf) ^ "\" in the logic yet")
                else read lexbuf }
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
