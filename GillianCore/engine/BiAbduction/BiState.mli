module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (BaseState : State.S
                   with type vt = Val.t
                    and type st = ESubst.t
                    and type store_t = Store.t) : sig
  include
    State.S
      with type vt = Val.t
       and type st = ESubst.t
       and type store_t = Store.t

  val initialise : Containers.SS.t -> BaseState.t -> UP.preds_tbl_t option -> t

  val get_components : t -> BaseState.t * BaseState.t
end
