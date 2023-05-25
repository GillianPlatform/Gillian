module type S = sig
  include State.S

  val make_s :
    init_data:init_data ->
    store:store_t ->
    pfs:PFS.t ->
    gamma:Type_env.t ->
    spec_vars:SS.t ->
    t

  val init : init_data -> t
  val clear_resource : t -> t
  val get_typ_env : t -> Type_env.t
  val get_pfs : t -> PFS.t
end

module Make (SMemory : SMemory.S) :
  S
    with type st = SVal.SESubst.t
     and type vt = SVal.M.t
     and type store_t = SStore.t
     and type heap_t = SMemory.t
     and type m_err_t = SMemory.err_t
     and type init_data = SMemory.init_data
