type t = {
  name : string;
  params : string list;
  body : WStmt.t list;
  spec : WSpec.t option;
  return_expr : WExpr.t;
  floc : CodeLoc.t;
  fid : int;
  is_loop_body : bool;
}

val get_id : t -> int
val get_loc : t -> CodeLoc.t
val get_name : t -> string
val get_spec : t -> WSpec.t option

val add_spec :
  t -> WLAssert.t -> WLAssert.t -> WLExpr.t option -> CodeLoc.t -> t

val functions_called : t -> string list
val has_spec : t -> bool

val get_by_id :
  int ->
  t ->
  [> `None
  | `Return of WExpr.t
  | `WExpr of WExpr.t
  | `WFun of t
  | `WLAssert of WLAssert.t
  | `WLCmd of WLCmd.t
  | `WLExpr of WLExpr.t
  | `WSpec of WSpec.t
  | `WStmt of WStmt.t ]

val pp : Format.formatter -> t -> unit
