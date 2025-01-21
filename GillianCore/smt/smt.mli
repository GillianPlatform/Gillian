open Gil_syntax

exception SMT_unknown

type typenv := (Id.any_var Id.t, Type.t) Hashtbl.t

val exec_sat : Expr.Set.t -> typenv -> Sexplib.Sexp.t option
val is_sat : Expr.Set.t -> typenv -> bool
val check_sat : Expr.Set.t -> typenv -> Sexplib.Sexp.t option

val lift_model :
  Sexplib.Sexp.t -> typenv -> (LVar.t -> Expr.t -> unit) -> LVar.Set.t -> unit

val pp_sexp : Sexplib.Sexp.t Fmt.t
