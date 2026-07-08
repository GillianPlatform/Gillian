open Gil_syntax
open Gillian.Debugger.Utils
open Gillian.Monadic

type t [@@deriving yojson]

type err =
  | MissingResource of (WislLActions.ga * string * Expr.t option)
  | DoubleFree of string
  | UseAfterFree of string
  | MemoryLeak
  | OutOfBounds of (int option * string * Expr.t)
  | InvalidLocation of Expr.t
[@@deriving yojson, show]

val init : unit -> t
val is_empty : t -> bool
val alloc : t -> int -> t * string
val dispose : t -> string -> (t, err) Delayed_result.t

val load :
  t -> string -> Expr.t -> (Expr.t * Expr.t * Expr.t, err) Delayed_result.t

val store :
  t -> string -> Expr.t -> Expr.t -> (t * Expr.t * Expr.t, err) Delayed_result.t

val get_cell :
  t ->
  string ->
  Expr.t ->
  Expr.t ->
  (string * Expr.t * Expr.t, err) Delayed_result.t

val set_cell :
  t -> string -> Expr.t -> Expr.t -> Expr.t -> (t, err) Delayed_result.t

val rem_cell : t -> string -> Expr.t -> Expr.t -> (t, err) Delayed_result.t
val get_bound : t -> string -> Expr.t -> (int * Expr.t, err) Delayed_result.t
val set_bound : t -> string -> int -> Expr.t -> (t, err) Delayed_result.t
val rem_bound : t -> string -> Expr.t -> (t, err) Delayed_result.t
val get_freed : t -> string -> (unit, err) Delayed_result.t
val set_freed : t -> string -> t Delayed.t
val rem_freed : t -> string -> (t, err) Delayed_result.t
val pp : t Fmt.t
val lvars : t -> SS.t
val alocs : t -> SS.t
val substitution : Gillian.Symbolic.Subst.t -> t -> t Delayed.t
val assertions : t -> Asrt.atom list
val to_seq : t -> (string * (SFVL.t * int option) option) Seq.t

val add_debugger_variables :
  store:(string * Expr.t) list ->
  memory:t ->
  is_gil_file:bool ->
  get_new_scope_id:(unit -> int) ->
  Variable.ts ->
  Variable.scope list
