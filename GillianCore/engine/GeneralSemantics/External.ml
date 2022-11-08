module T (Annot : Annot.S) = struct
  module type S = functor
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
      (Annot.t, int) Prog.t ->
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

module Dummy
    (Annot : Annot.S)
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
