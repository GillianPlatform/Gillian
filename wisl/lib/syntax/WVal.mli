type t = Bool of bool | Int of int | Str of string | Null | VList of t list
[@@deriving yojson]

val pp : Format.formatter -> t -> unit
val str : t -> string
