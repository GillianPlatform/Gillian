type tt =
  | LEmp
  | LStar of t * t
  | LWand of { lhs : string * WLExpr.t list; rhs : string * WLExpr.t list }
  | LPred of string * WLExpr.t list
  | LPointsTo of WLExpr.t * WLExpr.t list
      (** x -> a, b <=> (x -> a) * (x+1 -> b) *)
  | LBlockPointsTo of WLExpr.t * WLExpr.t list
  | LPure of WLExpr.t

and t

val get : t -> tt
val get_id : t -> int
val get_loc : t -> CodeLoc.t
val make : tt -> CodeLoc.t -> t
val copy : t -> t
val get_vars_and_lvars : t -> Set.Make(String).t * Set.Make(String).t
val get_by_id : int -> t -> [> `None | `WLExpr of WLExpr.t | `WLAssert of t ]
val pp : Format.formatter -> t -> unit
val str : t -> string
val substitution : (string, WLExpr.tt) Hashtbl.t -> t -> t
