open Gil_syntax

type 'a t

val resolve : curr_pc:Pc.t -> 'a t -> 'a Branch.t list

val return :
  ?learned:Formula.t list -> ?learned_types:(string * Type.t) list -> 'a -> 'a t

val resolve_loc : Expr.t -> string option t
val reduce : Expr.t -> Expr.t t
val reduce_formula : Formula.t -> Formula.t t
val entails : Formula.t list -> Formula.t -> bool t
val check_sat : Formula.t -> bool t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : 'a t -> ('a -> 'b) -> 'b t
val branches : 'a t list -> 'a t
val all : 'a t list -> 'a list t
val vanish : unit -> 'a t
val if_sure : Formula.t -> then_:(unit -> 'a t) -> else_:(unit -> 'a t) -> 'a t
val branch_entailment : (Formula.t * (unit -> 'a t)) list -> 'a t
val leak_pc_copy : unit -> Engine.Gpc.t t

val branch_on :
  Formula.t -> then_:(unit -> 'a t) -> else_:(unit -> 'a t) -> 'a t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end
