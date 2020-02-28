(**
    Interface for GIL General States.
    They are considered to be mutable.
*)
module type S = sig
  include State.S

  type state_t

  type preds_t

  type abs_t = string * vt list

  val initialise : state_t -> preds_t -> UP.preds_tbl_t option -> t

  val get_preds : t -> preds_t
  (** Get preds of given symbolic state *)

  val set_preds : t -> preds_t -> t
  (** Set preds of given symbolic state *)

  val unify : t -> st -> UP.t -> bool

  val add_pred_defs : UP.preds_tbl_t -> t -> t

  val deabstract : t -> state_t * bool

  val get_all_preds : ?keep:bool -> (abs_t -> bool) -> t -> abs_t list

  val set_pred : t -> abs_t -> unit

  val automatic_unfold : t -> vt list -> (t list, string) result
end

module Make
    (Val : Val.S)
    (Subst : Subst.S with type vt = Val.t and type t = Val.st)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = Subst.t
                and type store_t = Store.t)
    (Preds : Preds.S with type vt = Val.t and type st = Subst.t) :
  S
    with type vt = Val.t
     and type st = Subst.t
     and type store_t = Store.t
     and type state_t = State.t
     and type preds_t = Preds.t
