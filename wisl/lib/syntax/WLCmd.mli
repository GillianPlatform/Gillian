type tt =
  | Fold of string * WLExpr.t list
  | Unfold of string * WLExpr.t list
  | Package of { lhs : string * WLExpr.t list; rhs : string * WLExpr.t list }
  | ApplyLem of string * WLExpr.t list * string list
      (** apply \{exists: ...\} ... *)
  | LogicIf of WLExpr.t * t list * t list
  | Assert of WLAssert.t * string list  (**  assert \{exists: ...\} ... *)
  | Invariant of WLAssert.t * string list * WLExpr.t option
      (** invariant \{exists: ... \} ...*)

and t

val get : t -> tt
val get_id : t -> int
val get_loc : t -> CodeLoc.t
val make : tt -> CodeLoc.t -> t
val is_inv : t -> bool
val is_fold : t -> bool
val is_unfold : t -> bool

val get_by_id :
  int ->
  t ->
  [> `None | `WLExpr of WLExpr.t | `WLAssert of WLAssert.t | `WLCmd of t ]

val pp : Format.formatter -> t -> unit
val str : t -> string
val substitution : (string, WLExpr.tt) Hashtbl.t -> t -> t
