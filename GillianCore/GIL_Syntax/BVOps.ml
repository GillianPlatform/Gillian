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
  | BVSignExtend
  | BVZeroExtend
  | BVSdiv
  | BVSmod
  | BVAshr
  | BVUlt
  | BVUleq
  | BVSlt
  | BVSleq
  | BVUMulO
  | BVSMulO
  | BVNegO
  | BVUAddO
  | BVSAddO
  | BVToInt
  | IntToBV
[@@deriving eq, ord]

let str (x : t) =
  match x with
  | BVConcat -> "bvconcat"
  | BVExtract -> "bvextract"
  | BVNot -> "bvnot"
  | BVAnd -> "bvand"
  | BVOr -> "bvor"
  | BVNeg -> "bvneg"
  | BVPlus -> "bvadd"
  | BVMul -> "bvmul"
  | BVUDiv -> "bvudiv"
  | BVUrem -> "bvurem"
  | BVShl -> "bvshl"
  | BVLShr -> "bvlshr"
  | BVXor -> "bvxor"
  | BVSrem -> "bvsrem"
  | BVSub -> "bvsub"
  | BVSignExtend -> "bvsext"
  | BVZeroExtend -> "bvzext"
  | BVSdiv -> "bvsdiv"
  | BVSmod -> "bvsmod"
  | BVAshr -> "bvashr"
  | BVUlt -> "bvult"
  | BVUleq -> "bvuleq"
  | BVSlt -> "bvslt"
  | BVSleq -> "bvsleq"
  | BVUMulO -> "bvumulo"
  | BVSMulO -> "bvsmulo"
  | BVNegO -> "bvnego"
  | BVUAddO -> "bvuaddo"
  | BVSAddO -> "bvsaddo"
  | BVToInt -> "bvtoint"
  | IntToBV -> "inttobv"

let to_yojson = TypeDef__.bvop_to_yojson
let of_yojson = TypeDef__.bvop_of_yojson
