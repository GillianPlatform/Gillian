open Gil_syntax
open Gillian.Debugger.Utils
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
val is_empty : t -> bool
val alloc : t -> int -> t * ALoc.t
val dispose : t -> Id.Any_loc.t -> (t, err) Delayed_result.t

val load :
  t ->
  Id.Any_loc.t ->
  Expr.t ->
  (Expr.t * Expr.t * Expr.t, err) Delayed_result.t

val store :
  t ->
  Id.Any_loc.t ->
  Expr.t ->
  Expr.t ->
  (t * Expr.t * Expr.t, err) Delayed_result.t

val get_cell :
  t ->
  Id.Any_loc.t ->
  Expr.t ->
  Expr.t ->
  (Id.Any_loc.t * Expr.t * Expr.t, err) Delayed_result.t

val set_cell :
  t -> Id.Any_loc.t -> Expr.t -> Expr.t -> Expr.t -> (t, err) Delayed_result.t

val rem_cell :
  t -> Id.Any_loc.t -> Expr.t -> Expr.t -> (t, err) Delayed_result.t

val get_bound :
  t -> Id.Any_loc.t -> Expr.t -> (int * Expr.t, err) Delayed_result.t

val set_bound : t -> Id.Any_loc.t -> int -> Expr.t -> (t, err) Delayed_result.t
val rem_bound : t -> Id.Any_loc.t -> Expr.t -> (t, err) Delayed_result.t
val get_freed : t -> Id.Any_loc.t -> (unit, err) Delayed_result.t
val set_freed : t -> Id.Any_loc.t -> t Delayed.t
val rem_freed : t -> Id.Any_loc.t -> (t, err) Delayed_result.t
val pp : t Fmt.t
val lvars : t -> LVar.Set.t
val alocs : t -> ALoc.Set.t
val substitution : Gillian.Symbolic.Subst.t -> t -> t Delayed.t
val assertions : t -> Asrt.atom list
val to_seq : t -> (Id.Any_loc.t * (SFVL.t * int option) option) Seq.t

val add_debugger_variables :
  store:(string * Expr.t) list ->
  memory:t ->
  is_gil_file:bool ->
  get_new_scope_id:(unit -> int) ->
  Variable.ts ->
  Variable.scope list
