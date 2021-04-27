(** [check_satisfiability_with_model pfs gamma]
    checks whether or not the pure formulae [pfs] are satisfiable
    under the typing environment [gamma]. If this is the case,
    the function returns the appropriate logical environment. *)
val check_satisfiability_with_model :
  Gil_syntax.Formula.t list -> TypEnv.t -> SVal.SESubst.t option

(** [check_satisfiability ?unification pfs gamma]
    checks whether or not the pure formulae [pfs] are satisfiable
    under the typing environment [gamma]. The [unification] flag should
    not be used by Gillian instantiation developers. *)
val check_satisfiability :
  ?unification:bool ->
  ?time:string ->
  ?relevant_info:Containers.SS.t * Containers.SS.t * Containers.SS.t ->
  Gil_syntax.Formula.t list ->
  TypEnv.t ->
  bool

(** A different API for [check_satisfiability] better adapted for usage in memory models *)
val sat :
  unification:bool ->
  pfs:PFS.t ->
  gamma:TypEnv.t ->
  Gil_syntax.Formula.t list ->
  bool

(** [check_entailment existentials lpfs rpfs gamma] checks whether or not
    the entailment << ∃ [existentials]. [lpfs] => [rpfs] >> holds
    under the typing environment [gamma]. *)
val check_entailment :
  ?unification:bool ->
  Utils.Containers.SS.t ->
  PFS.t ->
  Gil_syntax.Formula.t list ->
  TypEnv.t ->
  bool

(** [is_equal e1 e2 pfs gamma] checks whether or not
    << pfs, gamma |- e1 = e2 >>. *)
val is_equal :
  pfs:PFS.t -> gamma:TypEnv.t -> Gil_syntax.Expr.t -> Gil_syntax.Expr.t -> bool

(** [is_different e1 e2 pfs gamma] checks whether or not
    << pfs, gamma |- e1 <> e2 >>. *)
val is_different :
  pfs:PFS.t -> gamma:TypEnv.t -> Gil_syntax.Expr.t -> Gil_syntax.Expr.t -> bool

val is_less_or_equal :
  pfs:PFS.t -> gamma:TypEnv.t -> Gil_syntax.Expr.t -> Gil_syntax.Expr.t -> bool

val resolve_loc_name :
  pfs:PFS.t -> gamma:TypEnv.t -> Gil_syntax.Expr.t -> string option
