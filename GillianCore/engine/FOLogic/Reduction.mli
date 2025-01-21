(** [ReductionException (e, msg)] denotes an exception raised
     due to an expression [e] being malformed with explanation [msg] *)
exception ReductionException of Gil_syntax.Expr.t * string

(** [resolve_to_location pfs e] attempts to resolve the expression
    denoted by [e] to a location given the pure formulae [pfs].
    If successful, it returns that location, together with any
    bindings learned during the resolution. *)
val resolve_expr_to_location :
  PFS.t -> Type_env.t -> Gil_syntax.Expr.t -> Id.any_loc Id.t option

(** [get_equal_expressions pfs e] returns a list of expressions that
    equal [e] under the pure formulae [pfs]. *)
val get_equal_expressions : PFS.t -> Gil_syntax.Expr.t -> Gil_syntax.Expr.t list

val understand_lstcat :
  PFS.t ->
  Type_env.t ->
  Expr.t list ->
  Expr.t list ->
  (Expr.t * LVar.Set.t) option

(** [reduce_lexpr ?matching ?reduce_lvars ?pfs ?gamma e] reduces the
    expression [e] given (optional) pure formulae [pfs] and typing environment [gamma].
    The [reduce_lvars] and [matching] flags should not be used by Gillian instantiation developers. *)
val reduce_lexpr :
  ?matching:bool ->
  ?reduce_lvars:bool ->
  ?pfs:PFS.t ->
  ?gamma:Type_env.t ->
  Gil_syntax.Expr.t ->
  Gil_syntax.Expr.t

(** [reduce_assertion ?matching ?pfs ?gamma a] reduces the assertion [a]
    given (optional) pure formulae [pfs] and typing environment [gamma].
    The [matching] flag should not be used by Gillian instantiation developers. *)
val reduce_assertion :
  ?matching:bool ->
  ?pfs:PFS.t ->
  ?gamma:Type_env.t ->
  Gil_syntax.Asrt.t ->
  Gil_syntax.Asrt.t

val is_tautology : ?pfs:PFS.t -> ?gamma:Type_env.t -> Gil_syntax.Expr.t -> bool
