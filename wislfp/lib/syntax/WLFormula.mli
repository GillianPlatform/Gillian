type tt =
  | LTrue
  | LFalse
  | LNot of t
  | LAnd of t * t
  | LOr of t * t
  | LEq of WLExpr.t * WLExpr.t
  | LLess of WLExpr.t * WLExpr.t
  | LGreater of WLExpr.t * WLExpr.t
  | LLessEq of WLExpr.t * WLExpr.t
  | LGreaterEq of WLExpr.t * WLExpr.t

and t

val get : t -> tt
val get_loc : t -> CodeLoc.t
val get_id : t -> int
val make : tt -> CodeLoc.t -> t
val lexpr_is_true : ?codeloc:PwUtils.CodeLoc.t -> WLExpr.t -> t
val not : t -> t
val get_vars_and_lvars : t -> Set.Make(String).t * Set.Make(String).t
val get_by_id : int -> t -> [> `None | `WLExpr of WLExpr.t | `WLFormula of t ]
val pp : Format.formatter -> t -> unit
val str : t -> string
val substitution : (string, WLExpr.tt) Hashtbl.t -> t -> t
