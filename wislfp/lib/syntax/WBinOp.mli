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

val pp : Format.formatter -> t -> unit
val str : t -> string
val is_logic_only : t -> bool
