(** @canonical Gillian.General.External

  Interface for executing external functions *)

module T (Annot : Annot.S) = struct
  module type S = functor
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (Call_stack : Call_stack.S with type vt = Val.t and type store_t = Store.t)
    -> sig
    val execute :
      (Annot.t, int) Prog.t ->
      State.t ->
      Call_stack.t ->
      int ->
      Var.t ->
      string ->
      Val.t list ->
      int option ->
      (State.t * Call_stack.t * int * int) list
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
    (Callstack : Call_stack.S with type vt = Val.t and type store_t = Store.t) =
struct
  let execute _ _ _ _ _ _ _ _ = failwith "Unimplemented External module"
end
