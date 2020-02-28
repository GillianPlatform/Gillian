type t = Concrete | Verification | Symbolic | BiAbduction

let concrete_exec = function
  | Concrete -> true
  | _        -> false

let symbolic_exec = function
  | Symbolic -> true
  | _        -> false

let verification_exec = function
  | Verification -> true
  | _            -> false

let biabduction_exec = function
  | BiAbduction -> true
  | _           -> false

let all_exec = [ Concrete; Verification; Symbolic; BiAbduction ]

let non_bi_exec = [ Concrete; Verification; Symbolic ]

let exec_with_preds = [ Verification; BiAbduction ]

let bi_exec = [ BiAbduction ]
