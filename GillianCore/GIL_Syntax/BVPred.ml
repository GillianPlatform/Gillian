type t = TypeDef__.bvpred =
  | BVUlt
  | BVUleq
  | BVSlt
  | BVSleq
  | BVUMulO
  | BVSMulO
  | BVNegO
  | BVUAddO
  | BVSAddO
[@@deriving eq, ord]

let str (x : t) =
  match x with
  | BVUlt -> "bvult"
  | BVUleq -> "bvuleq"
  | BVSlt -> "bvslt"
  | BVSleq -> "bvsleq"
  | BVUMulO -> "bvumulo"
  | BVSMulO -> "bvsmulo"
  | BVNegO -> "bvnego"
  | BVUAddO -> "bvuaddo"
  | BVSAddO -> "bvsaddo"

let to_yojson = TypeDef__.bvpred_to_yojson
let of_yojson = TypeDef__.bvpred_of_yojson
