module Make (SMemory : Gillian.Symbolic.Memory_S) :
  Gillian.Debugger.Displayable.S with type t = SMemory.t
