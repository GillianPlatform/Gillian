open Gillian.Symbolic
open Gil_syntax
open Gillian.Debugger.Utils

type t [@@deriving yojson]

type err =
  | MissingResource of (WislLActions.ga * string * Expr.t option)
  | DoubleFree of string
  | UseAfterFree of string
  | MemoryLeak
  | OutOfBounds of (int option * string * Expr.t)
  | InvalidLocation
  | DuplicatedResource
[@@deriving yojson, show]

val init : unit -> t
val alloc : t -> int -> string
val dispose : 
pfs:Pure_context.t ->
gamma:Type_env.t ->
t -> 
string ->
(unit, err) Result.t

val clean_up : Expr.Set.t -> t -> Expr.Set.t * Expr.Set.t

val load:
  pfs:Pure_context.t ->
  gamma:Type_env.t ->
  t ->
  string ->
  Expr.t ->
  (Expr.t, err) result

val store:
  pfs:Pure_context.t ->
  gamma:Type_env.t ->
  t ->
  string ->
  Expr.t ->
  Expr.t ->
  (unit, err) result

val get_cell :
  pfs:Pure_context.t ->
  gamma:Type_env.t ->
  t ->
  string ->
  Expr.t ->
  Expr.t ->
  (string * Expr.t * Expr.t * Expr.t, err) result

val set_cell :
  pfs:Pure_context.t ->
  gamma:Type_env.t ->
  t ->
  string ->
  Expr.t ->
  Expr.t ->
  Expr.t ->
  (Formula.t list, err) result

val rem_cell : t -> string -> Expr.t -> Expr.t -> (unit, err) result
val get_bound :
  pfs:Pure_context.t ->
  gamma:Type_env.t ->
  t ->
  string ->
  Expr.t ->
  (int, err) result

val set_bound :
  pfs:Pure_context.t ->
  gamma:Type_env.t ->
  t ->
  string ->
  int ->
  (unit, err) result
val rem_bound : t -> string -> Expr.t -> (unit, err) result
val get_freed : t -> string -> (unit, err) result
val set_freed : t -> string -> unit
val rem_freed : t -> string -> (unit, err) result
val pp : t Fmt.t
val copy : t -> t
val lvars : t -> SS.t
val alocs : t -> SS.t

val substitution_in_place :
  Gillian.Symbolic.Subst.t ->
  t ->
  (t
  * Gillian.Gil_syntax.Formula.Set.t
  * (string * Gillian.Gil_syntax.Type.t) list)
  list

val assertions : t -> Gillian.Gil_syntax.Asrt.t list

val add_debugger_variables :
  store:(string * Gillian.Gil_syntax.Expr.t) list ->
  memory:t ->
  is_gil_file:bool ->
  get_new_scope_id:(unit -> int) ->
  Variable.ts ->
  Variable.scope list