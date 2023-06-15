type t = NOT | LEN | REV | HEAD | TAIL

val is_logic_only : t -> bool
val pp : Format.formatter -> t -> unit
val str : t -> string
