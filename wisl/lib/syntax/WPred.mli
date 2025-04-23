type t = {
  pred_name : string;
  pred_params : (string * WType.t option) list;
  pred_definitions : WLAssert.t list;
  pred_ins : int list;
  pred_nounfold : bool;
  pred_loc : CodeLoc.t;
  pred_id : int;
}

val get_id : t -> int
val get_loc : t -> CodeLoc.t
val get_name : t -> string
val get_ins : t -> int list

val get_by_id :
  int ->
  t ->
  [> `None | `WLAssert of WLAssert.t | `WLExpr of WLExpr.t | `WPred of t ]
