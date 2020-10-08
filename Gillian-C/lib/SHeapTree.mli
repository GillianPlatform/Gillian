open Gil_syntax
open Utils.Containers
open Monadic

type err =
  | UseAfterFree
  | BufferOverrun
  | InsufficientPermission of { required : Perm.t; actual : Perm.t }
  | InvalidAlignment       of { alignment : int; offset : Expr.t }
  | MissingResource
  | Unhandled              of string
  | RemovingNotOwned
  | HoleNotUndefined
  | MemoryNotFreed
  
val pp_err : err Fmt.t

val err_equal : err -> err -> bool

type 'a or_error = ('a, err) Result.t
type 'a d_or_error = ('a, err) Delayed_result.t

module Range : sig
  
  type t = Expr.t * Expr.t
  
end

type t

val pp : t Fmt.t

val empty : t

val freed : t

val is_empty : t -> bool

val lvars : t -> SS.t

val get_bounds : t -> Range.t or_error

val set_bounds : t -> Range.t -> t or_error

val alloc : t -> Expr.t -> Expr.t -> t

val free : t -> Expr.t -> Expr.t -> t d_or_error

