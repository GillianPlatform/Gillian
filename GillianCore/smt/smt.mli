open Gil_syntax

exception SMT_unknown

val exec_sat :
  Formula.Set.t -> (string, Type.t) Hashtbl.t -> Sexplib.Sexp.t option

val is_sat : Formula.Set.t -> (string, Type.t) Hashtbl.t -> bool

val check_sat :
  Formula.Set.t -> (string, Type.t) Hashtbl.t -> Sexplib.Sexp.t option

val lift_model :
  Sexplib.Sexp.t ->
  (string, Type.t) Hashtbl.t ->
  (string -> Expr.t -> unit) ->
  Expr.Set.t ->
  unit

val pp_sexp : Sexplib.Sexp.t Fmt.t
