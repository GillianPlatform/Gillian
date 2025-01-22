open Gillian.Symbolic
open Gil_syntax
open Gillian.Debugger.Utils

type loc_t := Id.any_loc Id.t
type t [@@deriving yojson]

type err =
  | MissingResource of (WislLActions.ga * loc_t * Expr.t option)
  | DoubleFree of loc_t
  | UseAfterFree of loc_t
  | MemoryLeak
  | OutOfBounds of (int option * loc_t * Expr.t)
  | InvalidLocation of Expr.t
[@@deriving yojson, show]

val init : unit -> t
val alloc : t -> int -> loc_t
val dispose : t -> loc_t -> (unit, err) Result.t
val clean_up : Expr.Set.t -> t -> Expr.Set.t * Expr.Set.t
val is_empty : t -> bool

val get_cell :
  pfs:Pure_context.t ->
  gamma:Type_env.t ->
  t ->
  loc_t ->
  Expr.t ->
  (loc_t * Expr.t * Expr.t, err) result

val set_cell :
  pfs:Pure_context.t ->
  gamma:Type_env.t ->
  t ->
  loc_t ->
  Expr.t ->
  Expr.t ->
  (unit, err) result

val rem_cell : t -> loc_t -> Expr.t -> (unit, err) result
val get_bound : t -> loc_t -> (int, err) result
val set_bound : t -> loc_t -> int -> (unit, err) result
val rem_bound : t -> loc_t -> (unit, err) result
val get_freed : t -> loc_t -> (unit, err) result
val set_freed : t -> loc_t -> unit
val rem_freed : t -> loc_t -> (unit, err) result
val pp : t Fmt.t
val copy : t -> t
val lvars : t -> LVar.Set.t
val alocs : t -> ALoc.Set.t

val substitution_in_place :
  Gillian.Symbolic.Subst.t ->
  t ->
  (t * Expr.Set.t * (Id.any_var Id.t * Type.t) list) list

val assertions : t -> Asrt.t

val add_debugger_variables :
  store:(Var.t * Expr.t) list ->
  memory:t ->
  is_gil_file:bool ->
  get_new_scope_id:(unit -> int) ->
  Variable.ts ->
  Variable.scope list
