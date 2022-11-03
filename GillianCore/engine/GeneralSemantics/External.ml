module type S = sig
  type annot

  module Make : functor
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (CallStack : CallStack.S with type vt = Val.t and type store_t = Store.t)
    -> sig
    val execute :
      (annot, int) Prog.t ->
      State.t ->
      CallStack.t ->
      int ->
      string ->
      string ->
      Val.t list ->
      int option ->
      (State.t * CallStack.t * int * int) list
  end
end

module Dummy (Annot : Annot.S) = struct
  type annot = Annot.t

  module Make
      (Val : Val.S)
      (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
      (Store : Store.S with type vt = Val.t)
      (State : State.S
                 with type vt = Val.t
                  and type st = ESubst.t
                  and type store_t = Store.t)
      (Callstack : CallStack.S with type vt = Val.t and type store_t = Store.t) =
  struct
    let execute _ _ _ _ _ _ _ _ = failwith "Unimplemented External module"
  end
end
