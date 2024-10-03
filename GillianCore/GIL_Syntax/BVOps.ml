type t = TypeDef__.bvop = BVPlus [@@deriving eq, ord]

let str (x : t) =
  match x with
  | BVPlus -> "bvadd"

let to_yojson = TypeDef__.bvop_to_yojson
let of_yojson = TypeDef__.bvop_of_yojson
