(** @canonical Gillian.Symbolic *)

(** @canonical Gillian.Symbolic.Values *)
module Values = struct
  (** Symbolic values *)

  (** @inline *)
  include SVal.M

  module Subst = SVal.SESubst
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

  (** @canonical Gillian.Symbolic.FO_logic.FOSolver *)
  module FOSolver = struct
    (** @inline *)
    include FOSolver
  end
end

(** @canonical Gillian.Symbolic.Memory_S

    Interface for a symbolic memory model *)
module type Memory_S = SMemory.S

(** @canonical Gillian.Symbolic.Dummy_memory *)
module Dummy_memory = SMemory.Dummy

(** @canonical Gillian.Symbolic.Legacy_s_memory *)
module Legacy_s_memory = Legacy_s_memory

(** @canonical Gillian.Symbolic.Store *)
module Store = SStore

module SState = SState
module PState = PState
