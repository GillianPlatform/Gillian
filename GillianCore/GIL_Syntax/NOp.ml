(* GIL n-ary Operators *)

type t = TypeDef__.nop =
  (* List concatenation *)
  | LstCat
  (* Set management *)
  | SetUnion
  | SetInter
[@@deriving eq, ord]

let to_yojson = TypeDef__.nop_to_yojson
let of_yojson = TypeDef__.nop_of_yojson

let str (x : t) =
  match x with
  | LstCat -> "l+"
  | SetUnion -> "-u-"
  | SetInter -> "-i-"
