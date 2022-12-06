(** @canonical Gillian.Debugger.Lifter *)

include Lifter

module Gil_lifter = struct
  (** @inline*)
  include Gil_lifter
end

module Gil_fallback_lifter = struct
  (** @inline *)
  include Gil_fallback_lifter
end
