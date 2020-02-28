(** GIL Variables *)

type t = string

module Set = Set.Make (String)

let str t = t
