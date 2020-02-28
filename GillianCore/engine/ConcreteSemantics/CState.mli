module Make (CMemory : CMemory.S) :
  State.S
    with type st = CVal.CSubst.t
     and type vt = Literal.t
     and type store_t = CStore.t
