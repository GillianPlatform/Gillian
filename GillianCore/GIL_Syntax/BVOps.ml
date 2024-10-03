type t = TypeDef__.bvop = BVPlus [@@deriving eq, ord]

let str (x : t) =
  match x with
  | BVPlus -> "bvadd"
