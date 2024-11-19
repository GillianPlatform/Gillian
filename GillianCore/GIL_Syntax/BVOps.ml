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
  | BVNegO
  | BVUAddO
  | BVSAddO
  | BVUMulO
  | BVSMulO
  | BVShl
  | BVLShr
  | BVUlt
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
  | BVNegO -> "bvnego"
  | BVUAddO -> "bvuaddo"
  | BVSAddO -> "bvsaddo"
  | BVUMulO -> "bvumulo"
  | BVSMulO -> "bvsmulo"
  | BVShl -> "bvshl"
  | BVLShr -> "bvlshr"
  | BVUlt -> "bvult"

let to_yojson = TypeDef__.bvop_to_yojson
let of_yojson = TypeDef__.bvop_of_yojson
