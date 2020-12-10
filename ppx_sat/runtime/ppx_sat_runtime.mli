open Monadic.Delayed
open Gil_syntax

val if_then_else : Formula.t -> then_branch:'a t -> else_branch:'a t -> 'a t

val if_sure_then_else :
  Formula.t -> then_branch:'a t -> else_branch:'a t -> 'a t

val branch_entailment : (Formula.t * 'a t) list -> 'a t

val true_formula : Gil_syntax.Formula.t
