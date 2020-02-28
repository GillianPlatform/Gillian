module Make
    (Val : Val.S)
    (Subst : Subst.S with type vt = Val.t and type t = Val.st)
    (Store : Store.S with type vt = Val.t)
    (BaseState : State.S
                   with type vt = Val.t
                    and type st = Subst.t
                    and type store_t = Store.t) : sig
  include
    State.S
      with type vt = Val.t
       and type st = Subst.t
       and type store_t = Store.t

  val initialise : Containers.SS.t -> BaseState.t -> UP.preds_tbl_t option -> t

  val get_components : t -> BaseState.t * BaseState.t
end
