type t = {
  pre : WLAssert.t;  (** Precondition *)
  post : WLAssert.t;  (** Postcondition *)
  variant : WLExpr.t option;  (** Variant *)
  existentials : (string * string list) option;  (** Existentials in the spec *)
  spid : int;  (** Unique identifier of AST el *)
  fname : string;  (** Name of the function the spec is attached to *)
  fparams : string list;
      (** Parameters of the function the spec is attached to *)
  sploc : CodeLoc.t;  (** Code location of the spec *)
}

val get_id : t -> int
val get_pre : t -> WLAssert.t
val get_post : t -> WLAssert.t
val get_loc : t -> CodeLoc.t

val get_by_id :
  int ->
  t ->
  [> `None | `WLAssert of WLAssert.t | `WLExpr of WLExpr.t | `WSpec of t ]

val make :
  ?existentials:string * string list ->
  WLAssert.t ->
  WLAssert.t ->
  WLExpr.t option ->
  string ->
  string list ->
  CodeLoc.t ->
  t
