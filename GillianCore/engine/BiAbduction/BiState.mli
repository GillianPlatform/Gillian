type 'a _t

module Make (BaseState : PState.S) : sig
  include
    State.S
      with type t = BaseState.t _t
       and type vt = Expr.t
       and type st = SVal.SESubst.t
       and type store_t = SStore.t
       and type init_data = BaseState.init_data
       and type m_err_t = BaseState.t _t * BaseState.m_err_t

  val make : procs:SS.t -> state:BaseState.t -> init_data:init_data -> t
  val get_components : t -> BaseState.t * BaseState.t
end
