open Gil_syntax

(* Similar to Gil_syntax.Asrt.t, but made to be simpler and nicer to use:
   - typed predicate types
   - removed star -- for our use (fixes), lists are nicer to use anyways
   - removed wands + predicates as using them from within a state model seems complicated+uneeded *)
type 'cp t =
  | Emp  (** Empty heap *)
  | Pure of Formula.t  (** Pure formula *)
  | Types of (Expr.t * Type.t) list  (** Typing assertion *)
  | CorePred of 'cp * Expr.t list * Expr.t list  (** Core predicate  *)

let map_cp
    (f : 'cp1 * Expr.t list * Expr.t list -> 'cp2 * Expr.t list * Expr.t list)
    (a : 'cp1 t) : 'cp2 t =
  match a with
  | Emp -> Emp
  | Pure f -> Pure f
  | Types tys -> Types tys
  | CorePred (cp, i, o) ->
      let cp', i', o' = f (cp, i, o) in
      CorePred (cp', i', o')
