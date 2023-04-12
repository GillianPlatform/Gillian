(** @canonical Gillian.Symbolic *)

(** @canonical Gillian.Symbolic.Values *)
module Values = struct
  (** Symbolic values *)

  (** @inline *)
  include SVal.M
end

(** @canonical Gillian.Symbolic.Subst *)
module Subst = struct
  (** Substitutions
      
    A mapping of GIL variables to GIL values *)

  (** @inline *)
  include SVal.SESubst
end

(** @canonical Gillian.Symbolic.Pure_context *)
module Pure_context = struct
  (** @inline *)
  include PFS
end

(** @canonical Gillian.Symbolic.Type_env *)
module Type_env = struct
  (** @inline *)
  include Type_env
end

module FO_logic = struct
  (** First-order logic *)

  (** @canonical Gillian.Symbolic.FO_logic.Reduction *)
  module Reduction = struct
    (** @inline *)
    include Reduction
  end
end

(** @canonical Gillian.Symbolic.Memory_S
  
  Interface for a symbolic memory model *)
module type Memory_S = SMemory.S

(** @canonical Gillian.Symbolic.Dummy_memory *)
module Dummy_memory = SMemory.Dummy
