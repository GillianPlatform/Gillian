(** GIL Variables *)

type t = string [@@deriving yojson]

module Set = Containers.SS

let str t = t
