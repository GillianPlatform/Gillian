type t = NOT | LEN | REV | HEAD | TAIL [@@deriving yojson]

val is_logic_only : t -> bool
val pp : Format.formatter -> t -> unit
val str : t -> string
