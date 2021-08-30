type rt = RNormal | RError

type kind = Correctness | Incorrectness

type t = {
  pre : WLAssert.t;  (** Precondition *)
  posts : WLAssert.t list;  (** Postcondition *)
  return_mode : rt;  (** Return mode *)
  existentials : (string * string list) option;  (** Existentials in the spec *)
  spid : int;  (** Unique identifier of AST el *)
  fname : string;  (** Name of the function the spec is attached to *)
  fparams : string list;
      (** Parameters of the function the spec is attached to *)
  sploc : CodeLoc.t;  (** Code location of the spec *)
  kind : kind;
}

val get_id : t -> int

val get_pre : t -> WLAssert.t

val get_posts : t -> WLAssert.t list

val get_loc : t -> CodeLoc.t

val get_by_id :
  int ->
  t ->
  [> `None
  | `WLAssert  of WLAssert.t
  | `WLExpr    of WLExpr.t
  | `WLFormula of WLFormula.t
  | `WSpec     of t ]

val make :
  kind:kind ->
  ?existentials:string * string list ->
  WLAssert.t ->
  WLAssert.t list ->
  rt ->
  string ->
  string list ->
  CodeLoc.t ->
  t
