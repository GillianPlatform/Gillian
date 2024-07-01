(*************************************
 * Typing Algorithm                  *
**************************************)

(** Given a typing environment and an expressions, returns a triple containing:
    - A type if we found a necessary type
    - A boolean: true if the expression is typable, false if is definitely a type error
    - A list of corrections missing (not sure about that one ?) *)
val type_lexpr : Type_env.t -> Expr.t -> Type.t option * bool

val infer_types_expr : Type_env.t -> Expr.t -> unit
val infer_types_formula : Type_env.t -> Formula.t -> unit

val reverse_type_lexpr :
  bool -> Type_env.t -> (Expr.t * Type.t) list -> Type_env.t option

val te_of_list : (Expr.t * Type.t) list -> Type_env.t option
val naively_infer_type_information : PFS.t -> Type_env.t -> unit
val substitution_in_place : SVal.SESubst.t -> Type_env.t -> unit
