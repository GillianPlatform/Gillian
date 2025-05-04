open Gil_syntax

exception SMT_unknown

val init : unit -> unit
val exec_sat : Expr.Set.t -> Type_env.t -> Sexplib.Sexp.t option
val is_sat : Expr.Set.t -> Type_env.t -> bool
val check_sat : Expr.Set.t -> Type_env.t -> Sexplib.Sexp.t option

val lift_model :
  Sexplib.Sexp.t ->
  Type_env.t ->
  (string -> Expr.t -> unit) ->
  Expr.Set.t ->
  unit

val pp_sexp : Sexplib.Sexp.t Fmt.t
