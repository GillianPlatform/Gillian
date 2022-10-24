module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (BaseState : PState.S
                   with type vt = Val.t
                    and type st = ESubst.t
                    and type store_t = Store.t) : sig
  include
    State.S
      with type vt = Val.t
       and type st = ESubst.t
       and type store_t = Store.t
       and type init_data = BaseState.init_data

  val make : procs:SS.t -> state:BaseState.t -> init_data:init_data -> t
  val get_components : t -> BaseState.t * BaseState.t
end
