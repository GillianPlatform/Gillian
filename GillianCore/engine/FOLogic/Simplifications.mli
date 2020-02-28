val simplify_pfs_and_gamma :
  ?unification:bool ->
  ?kill_new_lvars:bool ->
  ?save_spec_vars:Utils.Containers.SS.t * bool ->
  ?existentials:Utils.Containers.SS.t ->
  PFS.t ->
  ?rpfs:PFS.t ->
  TypEnv.t ->
  SVal.SSubst.t * Utils.Containers.SS.t
(** [simplify_pfs_and_gamma ?unification ?kill_new_lvars ?save_spec_vars ?existentials lpfs ?rpfs gamma]
    simplifies the pure formule [lpfs] and the typing environment [gamma], attempting to instantiate
    the (optional) [existentials] and returning the learned bindings and a (possibly smaller) new set
    of existentials. If provided, it will also try to simplify the pure formule [rpfs], assuming that
    that they are in the context of the entailment << ∃ [existentials]. [lpfs] => [rpfs] >>.
    If the [kill_new_lvars] flag is set, the logical variables introduced by the simplification will be
    removed if learned during the simplification, and kept otherwise.
    If the [save_spec_vars] parameter is not provided, all learned spec variables will be removed.
    If the [save_spec_vars] parameter is [(_, true)], all spec variables will be preserved.
    If the [save_spec_vars] parameter is [(var_set, false)], only the spec variables in [var_set] will be preserved.
    The [unification] flag should not be used by Gillian instantiation developers. *)

val simplify_implication :
  Utils.Containers.SS.t -> PFS.t -> PFS.t -> TypEnv.t -> Utils.Containers.SS.t
(** [simplify_implication existentials lpfs rpfs gamma]
    simplifies the entailment << ∃ [existentials]. [lpfs] => [rpfs] >>
    under the typing environment [gamma], attempting to instantiate the
    [existentials] and returning a (possibly smaller) new set of existentials *)

val admissible_assertion : Gil_syntax.Asrt.t -> bool
(** [admissible_assertion a] checks whether or not the assertion [a] is
    a contradiction only using the reductions/simplifications *)
