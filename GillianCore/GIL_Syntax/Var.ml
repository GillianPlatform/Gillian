(** GIL Variables *)

type t = string [@@deriving yojson]

module Set = Set.Make (String)

let str t = t
