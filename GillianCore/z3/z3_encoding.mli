(** [check_sat_core pfs gamma]
    checks whether or not the pure formulae [pfs] are satisfiable
    under the typing environment [gamma]. If this is the case,
    the function returns the appropriate z3 model. *)
val check_sat_core :
  Gil_syntax.Formula.Set.t ->
  (string, Gil_syntax.Type.t) Hashtbl.t ->
  Z3.Model.model option

val check_sat :
  Gil_syntax.Formula.Set.t -> (string, Gil_syntax.Type.t) Hashtbl.t -> bool

(** [lift_z3_model model gamma update_subst target_vars]
    attempts to lift the variables [target_vars] from the
    z3 model [model] and sends the found bindings to the [update_subst] functions
    given the typing environment [gamma]. *)
val lift_z3_model :
  Z3.Model.model ->
  (string, Gil_syntax.Type.t) Hashtbl.t ->
  (string -> Gil_syntax.Expr.t -> unit) ->
  Gil_syntax.Expr.Set.t ->
  unit
