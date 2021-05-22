module type S = sig
  type merr

  val error_to_exception_info :
    merr -> int Cmd.t option -> DebuggerTypes.exception_info
end

module Dummy (SMemory : SMemory.S) : S with type merr = SMemory.err_t
