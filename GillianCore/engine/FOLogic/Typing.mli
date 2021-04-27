(*************************************
 * Typing Algorithm                  *
**************************************)

val type_lexpr : TypEnv.t -> Expr.t -> Type.t option * bool * Formula.t list

val infer_types_expr : TypEnv.t -> Expr.t -> unit

val infer_types_formula : TypEnv.t -> Formula.t -> unit

val reverse_type_lexpr :
  bool -> TypEnv.t -> (Expr.t * Type.t) list -> TypEnv.t option

val te_of_list : (Expr.t * Type.t) list -> TypEnv.t option

val naively_infer_type_information : PFS.t -> TypEnv.t -> unit

val substitution_in_place : SVal.SESubst.t -> TypEnv.t -> unit
