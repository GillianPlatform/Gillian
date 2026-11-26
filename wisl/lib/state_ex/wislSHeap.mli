open Gil_syntax
open Gillian.Monadic

type t [@@deriving yojson]

type err =
  | MissingResource of (WislLActions.ga * Id.Any_loc.t * Expr.t option)
  | DoubleFree of Id.Any_loc.t
  | UseAfterFree of Id.Any_loc.t
  | MemoryLeak
  | OutOfBounds of (int option * Id.Any_loc.t * Expr.t)
  | InvalidLocation of Expr.t
[@@deriving yojson, show]

val init : unit -> t
val alloc : t -> int -> t * ALoc.t
val dispose : t -> Id.Any_loc.t -> (t, err) Delayed_result.t
val is_empty : ?freed_is_empty:bool -> t -> bool

val get_cell :
  t ->
  Id.Any_loc.t ->
  Expr.t ->
  (Id.Any_loc.t * Expr.t * Expr.t, err) Delayed_result.t

val set_cell :
  alloc_if_missing:bool ->
  t ->
  Id.Any_loc.t ->
  Expr.t ->
  Expr.t ->
  (t, err) Delayed_result.t

val rem_cell : t -> Id.Any_loc.t -> Expr.t -> (t, err) Delayed_result.t
val get_bound : t -> Id.Any_loc.t -> (int, err) Delayed_result.t

val set_bound :
  alloc_if_missing:bool -> t -> Id.Any_loc.t -> int -> (t, err) Delayed_result.t

val rem_bound : t -> Id.Any_loc.t -> (t, err) Delayed_result.t
val get_freed : t -> Id.Any_loc.t -> (unit, err) Delayed_result.t

val set_freed :
  alloc_if_missing:bool -> t -> Id.Any_loc.t -> (t, err) Delayed_result.t

val rem_freed : t -> Id.Any_loc.t -> (t, err) Delayed_result.t
val pp : t Fmt.t
val lvars : t -> LVar.Set.t
val alocs : t -> ALoc.Set.t
val substitution : Gillian.Symbolic.Subst.t -> t -> t Delayed.t
val assertions : t -> Gillian.Gil_syntax.Asrt.t
val to_seq : t -> (Id.Any_loc.t * (SFVL.t * int option) option) Seq.t
