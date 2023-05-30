type t =
  | NEQ
  | EQUAL
  | LESSTHAN
  | FLESSTHAN
  | GREATERTHAN
  | FGREATERTHAN
  | LESSEQUAL
  | FLESSEQUAL
  | GREATEREQUAL
  | FGREATEREQUAL
  | PLUS
  | FPLUS
  | MINUS
  | FMINUS
  | TIMES
  | FTIMES
  | DIV
  | FDIV
  | MOD
  | FMOD
  | AND
  | OR
  (* Lists are only for the logic *)
  | LSTCONS
  (* list construction a::l, only for logic *)
  | LSTCAT
  | LSTNTH

(* list concatenation, only for logic *)

let pp fmt b =
  let s = Format.fprintf fmt "@[%s@]" in
  match b with
  | EQUAL -> s "="
  | LESSTHAN -> s "<"
  | FLESSTHAN -> s "f<"
  | GREATERTHAN -> s ">"
  | FGREATERTHAN -> s "f>"
  | LESSEQUAL -> s "<="
  | FLESSEQUAL -> s "f<="
  | GREATEREQUAL -> s ">="
  | FGREATEREQUAL -> s "f>="
  | PLUS -> s "+"
  | FPLUS -> s "f+"
  | MINUS -> s "-"
  | FMINUS -> s "f-"
  | TIMES -> s "*"
  | FTIMES -> s "f*"
  | DIV -> s "/"
  | FDIV -> s "f/"
  | MOD -> s "%"
  | FMOD -> s "f%"
  | AND -> s "&&"
  | OR -> s "||"
  | NEQ -> s "!="
  | LSTCAT -> s "@"
  | LSTCONS -> s "::"
  | LSTNTH -> s "lnth"

let str = Format.asprintf "%a" pp

let is_logic_only b =
  match b with
  | LSTCONS | LSTCAT -> true
  | _ -> false
