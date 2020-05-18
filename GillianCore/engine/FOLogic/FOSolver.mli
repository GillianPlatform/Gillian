(** [check_satisfiability_with_model pfs gamma]
    checks whether or not the pure formulae [pfs] are satisfiable
    under the typing environment [gamma]. If this is the case,
    the function returns the appropriate logical environment. *)
val check_satisfiability_with_model :
  Gil_syntax.Formula.t list -> TypEnv.t -> SVal.SSubst.t option

(** [check_satisfiability ?unification pfs gamma]
    checks whether or not the pure formulae [pfs] are satisfiable
    under the typing environment [gamma]. The [unification] flag should
    not be used by Gillian instantiation developers. *)
val check_satisfiability :
  ?unification:bool -> Gil_syntax.Formula.t list -> TypEnv.t -> bool

(** [check_entailment existentials lpfs rpfs gamma] checks whether or not
    the entailment << ∃ [existentials]. [lpfs] => [rpfs] >> holds
    under the typing environment [gamma]. *)
val check_entailment :
  Utils.Containers.SS.t ->
  Gil_syntax.Formula.t list ->
  Gil_syntax.Formula.t list ->
  TypEnv.t ->
  bool

(** [is_equal e1 e2 pfs gamma] checks whether or not
    << pfs, gamma |- e1 = e2 >>. *)
val is_equal :
  Gil_syntax.Expr.t -> Gil_syntax.Expr.t -> PFS.t -> TypEnv.t -> bool

(** [is_different e1 e2 pfs gamma] checks whether or not
    << pfs, gamma |- e1 <> e2 >>. *)
val is_different :
  Gil_syntax.Expr.t -> Gil_syntax.Expr.t -> PFS.t -> TypEnv.t -> bool
