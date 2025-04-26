type t = {
  context : WProc.t list;
  predicates : WPred.t list;
  lemmas : WLemma.t list;
  datatypes : WDatatype.t list;
}

val get_context : t -> WProc.t list

val get_by_id :
  ?proc_name:string option ->
  t ->
  int option ->
  [> `None
  | `Return of WExpr.t
  | `WExpr of WExpr.t
  | `WProc of WProc.t
  | `WLAssert of WLAssert.t
  | `WLCmd of WLCmd.t
  | `WLExpr of WLExpr.t
  | `WLFormula of WLFormula.t
  | `WLemma of WLemma.t
  | `WPred of WPred.t
  | `WSpec of WSpec.t
  | `WStmt of WStmt.t ]

val get_pred : t -> string -> WPred.t option
val get_proc : t -> string -> WProc.t option
val never_called_during_symb : t -> WProc.t list
val pp_context : Format.formatter -> WProc.t list -> unit
val pp : Format.formatter -> t -> unit
val get_proc_name_of_element : t -> int -> string
