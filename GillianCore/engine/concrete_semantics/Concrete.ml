(** @canonical Gillian.Concrete *)

(** @canonical Gillian.Concrete.Values *)
module Values = struct
  (** Concrete values *)

  (** @inline *)
  include CVal.M
end

(** @canonical Gillian.Concrete.Subst *)
module Subst = struct
  (** Substitutions
      
    A mapping of GIL variables to GIL values *)

  (** @inline *)
  include CVal.CSubst
end

(** @canonical Gillian.Concrete.Store *)
module Store = struct
  (** Concrete variable store *)

  (** @inline *)
  include CStore
end

(** @canonical Gillian.Concrete.State *)
module State = struct
  (** Concrete state *)

  (** @inline *)
  include CState
end

(** @canonical Gillian.Concrete.Memory_S
  
  Interface for a concrete memory model *)
module type Memory_S = CMemory.S
