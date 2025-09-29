type tt =
  | LVal of WVal.t
  | LVar of string
  | PVar of string
  | LBinOp of t * WBinOp.t * t
  | LUnOp of WUnOp.t * t
  | LLSub of t * t * t
  | LEList of t list
  | LESet of t list

and t

val get : t -> tt
val get_loc : t -> CodeLoc.t
val get_id : t -> int
val make : tt -> CodeLoc.t -> t
val from_expr : WExpr.t -> t
val get_vars_and_lvars : t -> Set.Make(String).t * Set.Make(String).t
val get_by_id : int -> t -> [> `None | `WLExpr of t ]
val pp : Format.formatter -> t -> unit
val str : t -> string
val substitution : (string, tt) Hashtbl.t -> t -> t
val not : t -> t
val as_bool_fml : ?codeloc:CodeLoc.t -> t -> t
