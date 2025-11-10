type tt =
  | Skip
  | VarAssign of string * WExpr.t  (** x := e *)
  | Fresh of string  (** fresh x *)
  | New of string * int  (** x := new(k) *)
  | Dispose of WExpr.t  (** free(e) *)
  | Lookup of string * WExpr.t  (** x := [e] *)
  | Update of WExpr.t * WExpr.t  (** [e] := [e] *)
  | FunCall of string * string * WExpr.t list * (string * string list) option
      (** x := f(e1, ..., en), last bit should be ignored *)
  | While of WExpr.t * t list  (** while (e) \{ s \} *)
  | If of WExpr.t * t list * t list  (** if (e) \{ s \} else \{ s \} *)
  | Logic of WLCmd.t  (** logic command *)
  | Assert of WExpr.t  (** non-SL assertion *)
  | Assume of WExpr.t  (** non-SL assumption *)
  | AssumeType of WExpr.t * WType.t  (** type assumption *)

and t = { sid : int; sloc : CodeLoc.t; snode : tt }

val get : t -> tt
val get_id : t -> int
val get_loc : t -> CodeLoc.t
val make : tt -> CodeLoc.t -> t
val pp_list : Format.formatter -> t list -> unit
val pp : Format.formatter -> t -> unit
val pp_head : Format.formatter -> t -> unit
val is_while : t -> bool
val is_fold : t -> bool
val is_unfold : t -> bool

val get_by_id :
  int ->
  t ->
  [> `None
  | `WExpr of WExpr.t
  | `WLAssert of WLAssert.t
  | `WLCmd of WLCmd.t
  | `WLExpr of WLExpr.t
  | `WStmt of t ]

val functions_called_by_list : t list -> string list

(* val check_consistency : t list -> CodeLoc.t -> unit *)
