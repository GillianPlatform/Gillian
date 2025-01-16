type t = TypeDef__.bvop =
  | BVConcat
  | BVExtract
  | BVNot
  | BVAnd
  | BVOr
  | BVNeg
  | BVPlus
  | BVMul
  | BVUDiv
  | BVUrem
  | BVShl
  | BVLShr
  | BVXor
  | BVSrem
  | BVSub
[@@deriving eq, ord]

let str (x : t) =
  match x with
  | BVConcat -> "bvconcat"
  | BVExtract -> "bvextract"
  | BVNot -> "bvnot"
  | BVAnd -> "bvand"
  | BVOr -> "bvor"
  | BVNeg -> "bvneg"
  | BVPlus -> "bvplus"
  | BVMul -> "bvmul"
  | BVUDiv -> "bvudiv"
  | BVUrem -> "bvurem"
  | BVShl -> "bvshl"
  | BVLShr -> "bvlshr"
  | BVXor -> "bvxor"
  | BVSrem -> "bvsrem"
  | BVSub -> "bvsub"

let to_yojson = TypeDef__.bvop_to_yojson
let of_yojson = TypeDef__.bvop_of_yojson
