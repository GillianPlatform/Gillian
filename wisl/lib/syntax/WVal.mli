type t = Bool of bool | Num of int | Str of string | Null | VList of t list

val pp : Format.formatter -> t -> unit

val str : t -> string
