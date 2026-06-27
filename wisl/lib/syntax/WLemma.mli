type t = {
  lemma_name : string;
  lemma_params : string list;
  lemma_proof : WLCmd.t list option;
  lemma_variant : WLExpr.t option;
  lemma_hypothesis : WLAssert.t;
  lemma_conclusion : WLAssert.t;
  lemma_id : int;
  lemma_loc : CodeLoc.t;
}

val get_id : t -> int
val get_loc : t -> CodeLoc.t

val get_by_id :
  int ->
  t ->
  [> `None
  | `WExpr of WExpr.t
  | `WLAssert of WLAssert.t
  | `WLCmd of WLCmd.t
  | `WLExpr of WLExpr.t
  | `WLemma of t ]
