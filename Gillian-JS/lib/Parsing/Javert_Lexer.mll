{
  open Lexing

  exception Syntax_error of string

  let keyword_table = Hashtbl.create 1

  let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
  [ (* JS Logic tokens *)
    "scope",    Javert_Parser.SCOPE;
      "schain",   Javert_Parser.SCHAIN;
      "this",     Javert_Parser.THIS;
      "closure",  Javert_Parser.CLOSURE;
      "sc_scope", Javert_Parser.SCSCOPE;
      "o_chains", Javert_Parser.OCHAINS;

      (* Type literals *)
      "Undefined", Javert_Parser.UNDEFTYPELIT;
      "Null",      Javert_Parser.NULLTYPELIT;
      "Empty",     Javert_Parser.EMPTYTYPELIT;
      "None",      Javert_Parser.NONETYPELIT;
      "Bool",      Javert_Parser.BOOLTYPELIT;
      "Num",       Javert_Parser.NUMTYPELIT;
      "Str",       Javert_Parser.STRTYPELIT;
      "Obj",       Javert_Parser.OBJTYPELIT;
      "List",      Javert_Parser.LISTTYPELIT;
      "Type",      Javert_Parser.TYPETYPELIT;
      "Set",       Javert_Parser.SETTYPELIT;

      (* Literals *)
      "undefined", Javert_Parser.UNDEFINED;
      "null",      Javert_Parser.NULL;
      "empty",     Javert_Parser.EMPTY;
      "true",      Javert_Parser.TRUE;
      "false",     Javert_Parser.FALSE;
      "nan",       Javert_Parser.NAN;
      "inf",       Javert_Parser.INFINITY;
      "nil",       Javert_Parser.LSTNIL;

      (* Binary operators *)
      "and",     Javert_Parser.AND;
      "or",      Javert_Parser.OR;
      "m_atan2", Javert_Parser.M_ATAN2;

      (* Unary operators *)
      "not",           Javert_Parser.NOT;
      "isNaN",         Javert_Parser.M_ISNAN;
      "m_abs",         Javert_Parser.M_ABS;
      "m_acos",        Javert_Parser.M_ACOS;
      "m_asin",        Javert_Parser.M_ASIN;
      "m_atan",        Javert_Parser.M_ATAN;
      "m_ceil",        Javert_Parser.M_CEIL;
      "m_cos",         Javert_Parser.M_COS;
      "m_exp",         Javert_Parser.M_EXP;
      "m_floor",       Javert_Parser.M_FLOOR;
      "m_log",         Javert_Parser.M_LOG;
      "m_round",       Javert_Parser.M_ROUND;
      "m_sgn",         Javert_Parser.M_SGN;
      "m_sin",         Javert_Parser.M_SIN;
      "m_sqrt",        Javert_Parser.M_SQRT;
      "m_tan",         Javert_Parser.M_TAN;
      "num_to_string", Javert_Parser.TOSTRING;
      "num_to_int",    Javert_Parser.TOINT;
      "num_to_uint16", Javert_Parser.TOUINT16;
      "num_to_int32",  Javert_Parser.TOINT32;
      "num_to_uint32", Javert_Parser.TOUINT32;
      "string_to_num", Javert_Parser.TONUMBER;
      "car",           Javert_Parser.CAR;
      "cdr",           Javert_Parser.CDR;
      "set_to_list",   Javert_Parser.SETTOLIST;

      (* Expression keywords *)
      "typeOf", Javert_Parser.TYPEOF;


      (* Command keywords *)
      "skip",         Javert_Parser.SKIP;
      "new",          Javert_Parser.NEW;
      "delete",       Javert_Parser.DELETE;
      "deleteObject", Javert_Parser.DELETEOBJ;
      "hasField",     Javert_Parser.HASFIELD;
      "getFields",    Javert_Parser.GETFIELDS;
      "metadata",     Javert_Parser.METADATA;
      "args",         Javert_Parser.ARGUMENTS;
      "goto",         Javert_Parser.GOTO;
      "with",         Javert_Parser.WITH;
      "apply",        Javert_Parser.APPLY;
      "PHI",          Javert_Parser.PHI;
      "return",       Javert_Parser.RETURN;
      "throw",        Javert_Parser.THROW;
      "extern",       Javert_Parser.EXTERN;

      (* Logical expressions: most match with the program expressions *)
      "none", Javert_Parser.LNONE;

      (* Logic assertions *)
      "True",         Javert_Parser.LTRUE;
      "False",        Javert_Parser.LFALSE;
      "emp",          Javert_Parser.LEMP;
      "types",        Javert_Parser.LTYPES;
      "forall",       Javert_Parser.LFORALL;
      "empty_fields", Javert_Parser.EMPTYFIELDS;
      "MetaData",     Javert_Parser.LMETADATA;

      (* Logic predicates *)
      "abstract", Javert_Parser.ABSTRACT;
      "pure", Javert_Parser.PURE;
      "pred", Javert_Parser.PRED;
      "nounfold", Javert_Parser.NOUNFOLD;
      "facts", Javert_Parser.FACTS;

      (* Logic commands *)
      "fold",         Javert_Parser.FOLD;
      "flash",        Javert_Parser.FLASH;
      "unfold",       Javert_Parser.UNFOLD;
      "unfold_all",   Javert_Parser.UNFOLDALL;
      "if",           Javert_Parser.LIF;
      "then",         Javert_Parser.LTHEN;
      "else",         Javert_Parser.LELSE;
      "macro",        Javert_Parser.MACRO;
      "invariant",    Javert_Parser.INVARIANT;
      "assert",       Javert_Parser.ASSERT;
      "assume",       Javert_Parser.ASSUME;
      "assume_type",  Javert_Parser.ASSUME_TYPE;
      "fresh_svar",   Javert_Parser.FRESH_SVAR;
      "bind",         Javert_Parser.BIND;
      "existentials", Javert_Parser.EXISTENTIALS;
      "sep_assert",   Javert_Parser.SEPASSERT;
      "branch",       Javert_Parser.BRANCH;
      "use_subst",    Javert_Parser.USESUBST;

     (* JS axiomatic spec specifics *)
     "js_only_spec", Javert_Parser.JSOS;

      (* Procedure specification keywords *)
      "axiomatic",    Javert_Parser.AXIOMATIC;
      "incomplete",   Javert_Parser.INCOMPLETE;
      "lemma",        Javert_Parser.LEMMA;
      "variant",      Javert_Parser.VARIANT;
      "spec",         Javert_Parser.SPEC;
      "bispec",       Javert_Parser.BISPEC;
      "normal",       Javert_Parser.NORMAL;
      "error",        Javert_Parser.ERROR;

      (* JS axiomatic spec specifics *)
      "js_only_spec", Javert_Parser.JSOS;

      (* Procedure definition keywords *)
      "proc", Javert_Parser.PROC;

    (* Others *)
    "import", Javert_Parser.IMPORT
    ]
}

let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let identifier = letter(letter|digit|'_')*

let float = '-'? digit+ ('.' digit*)?

let var2 = "_pvar_" (letter|digit|'_')*
let filename = (letter|digit|'_')+ '.' (letter|digit|'_')+
let lvar = '#' (letter|digit|'_'|'$')*
let lvar2 = "_lvar_" (letter|digit|'_')*
let normalised_lvar = "##NORMALISED_LVAR" (letter|digit|'_'|'$')*
let loc = "$l" (letter|digit|'_')*
let aloc = "_$l_" (letter|digit|'_')*
let normalised_aloc = "_$l_#" (letter|digit|'_')*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white                { read lexbuf }
  | newline              { new_line lexbuf; read lexbuf }

  (* JS Logic tokens *)
  | "$$scope"            { Javert_Parser.SCOPELEXPR    }
  | "_"                  { Javert_Parser.UNDERSCORE    }

  (* Literals *)
  | "{{"        { Javert_Parser.LSTOPEN   }
  | "}}"        { Javert_Parser.LSTCLOSE  }

(* Constants *)
  | "$$min_float"        { Javert_Parser.MIN_FLOAT     }
  | "$$max_float"        { Javert_Parser.MAX_FLOAT     }
  | "$$max_safe_integer" { Javert_Parser.MAX_SAFE_INTEGER }
  | "$$epsilon"          { Javert_Parser.EPSILON       }
  | "$$random"           { Javert_Parser.RANDOM        }
  | "$$pi"               { Javert_Parser.PI            }
  | "$$UTCTime"          { Javert_Parser.UTCTIME       }
  | "$$LocalTime"        { Javert_Parser.LOCALTIME     }

(* Binary operators *)
  | "="                  { Javert_Parser.EQUAL         }
  | "<"                  { Javert_Parser.LESSTHAN      }
  | ">"                  { Javert_Parser.GREATERTHAN   }
  | "<="                 { Javert_Parser.LESSTHANEQUAL }
  | ">="                 { Javert_Parser.GREATERTHANEQUAL }
  | "s<"                 { Javert_Parser.LESSTHANSTRING}
  | "+"                  { Javert_Parser.PLUS          }
  | "-"                  { Javert_Parser.MINUS         }
  | "*"                  { Javert_Parser.TIMES         }
  | "/"                  { Javert_Parser.DIV           }
  | "%"                  { Javert_Parser.MOD           }
  | "&"                  { Javert_Parser.BITWISEAND    }
  | "|"                  { Javert_Parser.BITWISEOR     }
  | "^"                  { Javert_Parser.BITWISEXOR    }
  | "<<"                 { Javert_Parser.LEFTSHIFT     }
  | ">>"                 { Javert_Parser.SIGNEDRIGHTSHIFT }
  | ">>>"                { Javert_Parser.UNSIGNEDRIGHTSHIFT }
  | "**"                 { Javert_Parser.M_POW         }
  | "::"                 { Javert_Parser.LSTCONS       }
  | "l+"                 { Javert_Parser.LSTCAT        }
  | "++"                 { Javert_Parser.STRCAT        }
  | "-u-"                { Javert_Parser.SETUNION      }
  | "-i-"                { Javert_Parser.SETINTER      }
  | "-d-"                { Javert_Parser.SETDIFF       }
  | "-e-"                { Javert_Parser.SETMEM        }
  | "-s-"                { Javert_Parser.SETSUB        }
  | "--e--"              { Javert_Parser.LSETMEM       }
  | "--s--"              { Javert_Parser.LSETSUB       }
  | "-{"                 { Javert_Parser.SETOPEN       }
  | "}-"                 { Javert_Parser.SETCLOSE      }
(* Unary operators *)
  (* Unary minus uses the same symbol as binary minus, token MINUS *)
  | "~"                  { Javert_Parser.BITWISENOT    }
  | "l-len"              { Javert_Parser.LSTLEN }
  | "l-rev"              { Javert_Parser.LSTREV }
  | "l-sub"              { Javert_Parser.LSTSUB }
  | "s-len"              { Javert_Parser.STRLEN }
(* Expression keywords *)
  | "l-nth"              { Javert_Parser.LSTNTH }
  | "s-nth"              { Javert_Parser.STRNTH }
(* Command keywords *)
  | ":="                 { Javert_Parser.DEFEQ }
(* Logic assertions *)
  | "[["                 { Javert_Parser.OASSERT }
  | "]]"                 { Javert_Parser.CASSERT }
  | "/\\"                { Javert_Parser.LAND }
  | "\\/"                { Javert_Parser.LOR }
  | "!"                  { Javert_Parser.LNOT }
  | "=="                 { Javert_Parser.LEQUAL }
  | "<#"                 { Javert_Parser.LLESSTHAN       }
  | "<=#"                { Javert_Parser.LLESSTHANEQUAL  }
  | "s<#"                { Javert_Parser.LLESSTHANSTRING }
  (* Separating conjunction uses the same symbol as product, token TIMES *)
  | "->"                 { Javert_Parser.LARROW      }
(* Logic commands *)
  | "[*"                 { Javert_Parser.OLCMD     }
  | "*]"                 { Javert_Parser.CLCMD     }
  | "unfold*"            { Javert_Parser.RECUNFOLD }
  (**
    macro, assert are elsewhere
  *)
(* Separators *)
  | "(*"                 { read_comment lexbuf   }
  | '.'                  { Javert_Parser.DOT       }
  | ','                  { Javert_Parser.COMMA     }
  | ':'                  { Javert_Parser.COLON     }
  | ';'                  { Javert_Parser.SCOLON    }
  | '('                  { Javert_Parser.LBRACE    }
  | ')'                  { Javert_Parser.RBRACE    }
  | '['                  { Javert_Parser.LBRACKET  }
  | ']'                  { Javert_Parser.RBRACKET  }
  | '{'                  { Javert_Parser.CLBRACKET }
  | '}'                  { Javert_Parser.CRBRACKET }
(* Literals (cont.) *)
  | float                { let n = float_of_string (Lexing.lexeme lexbuf) in
                             Javert_Parser.FLOAT n }
  | '"'                  { read_string (Buffer.create 17) lexbuf }
  | loc                  { Javert_Parser.LOC (Lexing.lexeme lexbuf) }
  | aloc                  { Javert_Parser.ALOC (Lexing.lexeme lexbuf) }
  | normalised_aloc       { Javert_Parser.ALOC (Lexing.lexeme lexbuf) }
(* Filenames *)
  | filename             { Javert_Parser.FILENAME (Lexing.lexeme lexbuf) }

  (* Variables: THIS IS NEW *)
  | identifier           { let candidate = Lexing.lexeme lexbuf in
                            (match (Hashtbl.mem keyword_table candidate) with
                            | true  -> Hashtbl.find keyword_table candidate
                            | false -> Javert_Parser.VAR (Lexing.lexeme lexbuf)) }

  | var2                 { Javert_Parser.VAR (Lexing.lexeme lexbuf) }
(* Logic variables *)
  | lvar                 { Javert_Parser.LVAR (Lexing.lexeme lexbuf) }
  | lvar2                { Javert_Parser.LVAR (Lexing.lexeme lexbuf) }
  | normalised_lvar      { Javert_Parser.LVAR (Lexing.lexeme lexbuf) }
(* EOF *)
  | eof                  { Javert_Parser.EOF }
  | _                    { raise (Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and
(* Read strings *)
read_string buf =
  parse
  | '"'                  { Javert_Parser.STRING (Buffer.contents buf) }
  | '\\' '/'             { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'            { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'             { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'             { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'             { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'             { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'             { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\"'            { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | [^ '"' '\\']+        {
                           Buffer.add_string buf (Lexing.lexeme lexbuf);
                           read_string buf lexbuf
                         }
  | _                    { raise (Syntax_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof                  { raise (Syntax_error ("String is not terminated")) }
and
(* Read comments *)
read_comment =
  parse
  | newline              { new_line lexbuf; read_comment lexbuf }
  | "*)"                 { read lexbuf }
  | eof                  { raise (Syntax_error ("Comment is not terminated")) }
  | _                    { read_comment lexbuf }
