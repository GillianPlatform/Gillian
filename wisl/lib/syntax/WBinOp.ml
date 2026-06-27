type t =
  | EQUAL
  | LESSTHAN
  | GREATERTHAN
  | LESSEQUAL
  | GREATEREQUAL
  | FLESSTHAN
  | FGREATERTHAN
  | FLESSEQUAL
  | FGREATEREQUAL
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | FPLUS
  | FMINUS
  | FTIMES
  | FDIV
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
  | EQUAL -> s "=="
  | LESSTHAN -> s "<"
  | GREATERTHAN -> s ">"
  | LESSEQUAL -> s "<="
  | GREATEREQUAL -> s ">="
  | FLESSTHAN -> s "f<"
  | FGREATERTHAN -> s "f>"
  | FLESSEQUAL -> s "f<="
  | FGREATEREQUAL -> s "f>="
  | PLUS -> s "+"
  | MINUS -> s "-"
  | TIMES -> s "*"
  | DIV -> s "/"
  | MOD -> s "%"
  | FPLUS -> s "f+"
  | FMINUS -> s "f-"
  | FTIMES -> s "f*"
  | FDIV -> s "f/"
  | FMOD -> s "f%"
  | AND -> s "&&"
  | OR -> s "||"
  | LSTCAT -> s "@"
  | LSTCONS -> s "::"
  | LSTNTH -> s "lnth"

let str = Format.asprintf "%a" pp

let is_logic_only b =
  match b with
  | LSTCONS | LSTCAT -> true
  | _ -> false
