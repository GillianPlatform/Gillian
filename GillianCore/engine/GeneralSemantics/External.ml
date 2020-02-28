module type S = functor
  (Val : Val.S)
  (Subst : Subst.S with type vt = Val.t and type t = Val.st)
  (Store : Store.S with type vt = Val.t)
  (State : State.S
             with type vt = Val.t
              and type st = Subst.t
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

module Dummy
    (Val : Val.S)
    (Subst : Subst.S with type vt = Val.t and type t = Val.st)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = Subst.t
                and type store_t = Store.t)
    (Callstack : CallStack.S with type vt = Val.t and type store_t = Store.t) =
struct
  let execute _ _ _ _ _ _ _ _ = failwith "Unimplemented External module"
end
