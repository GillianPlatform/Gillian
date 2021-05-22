module type S = sig
  type merr

  val error_to_exception_info :
    merr -> int Cmd.t option -> DebuggerTypes.exception_info
end

module Dummy (SMemory : SMemory.S) = struct
  type merr = SMemory.err_t

  let error_to_exception_info merr _ : DebuggerTypes.exception_info =
    { id = Fmt.to_to_string SMemory.pp_err merr; description = None }
end
