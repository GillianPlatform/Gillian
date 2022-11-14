(** GIL Variables *)

type t = string [@@deriving yojson, show]

module Set = Containers.SS

let str t = t
