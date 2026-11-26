open Gil_syntax

exception SMT_unknown
exception SMT_error of string

val exec_sat :
  Expr.Set.t -> (Id.any_var Id.t, Type.t) Hashtbl.t -> Sexplib.Sexp.t option

val is_sat : Expr.Set.t -> (Id.any_var Id.t, Type.t) Hashtbl.t -> bool

val check_sat :
  Expr.Set.t -> (Id.any_var Id.t, Type.t) Hashtbl.t -> Sexplib.Sexp.t option

val lift_model :
  Sexplib.Sexp.t ->
  (Id.any_var Id.t, Type.t) Hashtbl.t ->
  (LVar.t -> Expr.t -> unit) ->
  Expr.Set.t ->
  unit

val pp_sexp : Sexplib.Sexp.t Fmt.t
