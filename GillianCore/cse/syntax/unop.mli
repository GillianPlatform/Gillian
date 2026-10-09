type t =
| Not
| Length
| IsNat
| RatToNat
| NatToRat

val to_extracted : t -> Extracted.op1
