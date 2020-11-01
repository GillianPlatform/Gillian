module Make (CMemory : CMemory.S) :
  State.S
    with type st = CVal.CESubst.t
     and type vt = Literal.t
     and type store_t = CStore.t
