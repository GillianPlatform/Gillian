open Gillian.Symbolic
open Gil_syntax

type t [@@deriving yojson]

type err =
  | MissingResource of WislLActions.ga
  | DoubleFree      of string
  | UseAfterFree    of string
  | MemoryLeak
  | OutOfBounds     of (int option * string * Expr.t)
  | InvalidLocation
[@@deriving yojson]

val init : unit -> t

val alloc : t -> int -> string

val dispose : t -> string -> (unit, err) Result.t

val get_cell :
  pfs:PureContext.t ->
  gamma:TypEnv.t ->
  t ->
  string ->
  Expr.t ->
  (string * Expr.t * Expr.t, err) result

val set_cell :
  pfs:PureContext.t ->
  gamma:TypEnv.t ->
  t ->
  string ->
  Expr.t ->
  Expr.t ->
  (unit, err) result

val rem_cell : t -> string -> Expr.t -> (unit, err) result

val get_bound : t -> string -> (int, err) result

val set_bound : t -> string -> int -> (unit, err) result

val rem_bound : t -> string -> (unit, err) result

val get_freed : t -> string -> (unit, err) result

val set_freed : t -> string -> unit

val rem_freed : t -> string -> (unit, err) result

val pp : t Fmt.t

val copy : t -> t

val substitution_in_place :
  Gillian.Symbolic.Subst.t ->
  t ->
  (t
  * Gillian.Gil_syntax.Formula.Set.t
  * (string * Gillian.Gil_syntax.Type.t) list)
  list

val assertions : t -> Gillian.Gil_syntax.Asrt.t list

val to_debugger_tree : t -> Gillian.Debugger.Displayable.debugger_tree list
