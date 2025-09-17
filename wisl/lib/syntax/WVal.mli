type t =
  | Bool of bool
  | Int of int
  | Float of float
  | Str of string
  | Null
  | VList of t list

val pp : Format.formatter -> t -> unit
val str : t -> string
