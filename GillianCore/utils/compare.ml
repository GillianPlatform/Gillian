(** Convenience function for comparison
    
  Returns a variant that can easily be matched against *)

type comparison = Gt | Lt | Eq

let cmp x y =
  let z = compare x y in
  if z = 0 then Eq else if z < 0 then Lt else Gt
