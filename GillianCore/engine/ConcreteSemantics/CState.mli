(** @canonical Gillian.Concrete.State.S *)
module type S = sig
  include
    State.S
      with type st = CVal.CESubst.t
       and type vt = Literal.t
       and type store_t = CStore.t

  val init : init_data -> t
end

(** @canonical Gillian.Concrete.State.Make *)
module Make (CMemory : CMemory.S) : sig
  include
    State.S
      with type st = CVal.CESubst.t
       and type vt = Literal.t
       and type store_t = CStore.t
       and type init_data = CMemory.init_data

  val init : init_data -> t
end
