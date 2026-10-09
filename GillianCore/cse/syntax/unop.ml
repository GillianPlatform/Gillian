type t =
| Not
| Length
| IsNat
| RatToNat
| NatToRat

let to_extracted op = match op with
  | Not -> Extracted.Op1Not
  | Length -> Extracted.Op1Length
  | IsNat -> Extracted.Op1IsNat
  | RatToNat -> Extracted.Op1RatToNat
  | NatToRat -> Extracted.Op1NatToRat
