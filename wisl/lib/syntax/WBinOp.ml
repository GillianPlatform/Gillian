type t =
  | EQUAL
  | LESSTHAN
  | GREATERTHAN
  | LESSEQUAL
  | GREATEREQUAL
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
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
  | PLUS -> s "+"
  | MINUS -> s "-"
  | TIMES -> s "*"
  | DIV -> s "/"
  | MOD -> s "%"
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
