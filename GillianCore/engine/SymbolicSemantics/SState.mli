module Make (SMemory : SMemory.S) :
  State.S
    with type st = SVal.SSubst.t
     and type vt = SVal.M.t
     and type store_t = SStore.t
