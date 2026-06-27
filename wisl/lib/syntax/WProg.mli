type t = {
  context : WFun.t list;
  predicates : WPred.t list;
  lemmas : WLemma.t list;
}

val get_context : t -> WFun.t list

val get_by_id :
  ?fname:string option ->
  t ->
  int option ->
  [> `None
  | `Return of WExpr.t
  | `WExpr of WExpr.t
  | `WFun of WFun.t
  | `WLAssert of WLAssert.t
  | `WLCmd of WLCmd.t
  | `WLExpr of WLExpr.t
  | `WLemma of WLemma.t
  | `WPred of WPred.t
  | `WSpec of WSpec.t
  | `WStmt of WStmt.t ]

val get_pred : t -> string -> WPred.t option
val get_fun : t -> string -> WFun.t option
val never_called_during_symb : t -> WFun.t list
val pp_context : Format.formatter -> WFun.t list -> unit
val pp : Format.formatter -> t -> unit
val get_function_name_of_element : t -> int -> string
