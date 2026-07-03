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

val pp : Format.formatter -> t -> unit
val str : t -> string
val is_logic_only : t -> bool
