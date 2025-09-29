(** @canonical Gillian.Debugger.Lifter

    Interface for lifting execution of GIL commands to a target language *)

include Lifter

module Gil_lifter = struct
  (** @inline*)
  include Gil_lifter
end

module Gil_fallback_lifter = struct
  (** @inline *)
  include Gil_fallback_lifter
end
