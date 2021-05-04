module type S = sig
  include State.S

  val get_typ_env : t -> TypEnv.t
end

module Make (SMemory : SMemory.S) :
  S
    with type st = SVal.SESubst.t
     and type vt = SVal.M.t
     and type store_t = SStore.t
     and type heap_t = SMemory.t
