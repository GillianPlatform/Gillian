module type S = sig
  type merr

  val error_to_string : merr -> int Cmd.t option -> string
end

module Dummy (SMemory : SMemory.S) = struct
  type merr = SMemory.err_t

  let error_to_string merr _ = Fmt.to_to_string SMemory.pp_err merr
end
