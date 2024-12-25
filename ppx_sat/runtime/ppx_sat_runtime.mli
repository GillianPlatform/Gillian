open Monadic.Delayed
open Gil_syntax

val if_then_else :
  Expr.t -> then_branch:(unit -> 'a t) -> else_branch:(unit -> 'a t) -> 'a t

val if_sure_then_else :
  Expr.t -> then_branch:(unit -> 'a t) -> else_branch:(unit -> 'a t) -> 'a t

val branch_entailment : (Expr.t * (unit -> 'a t)) list -> 'a t
val true_formula : Expr.t
