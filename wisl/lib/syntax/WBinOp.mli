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

val pp : Format.formatter -> t -> unit
val str : t -> string
val is_logic_only : t -> bool
