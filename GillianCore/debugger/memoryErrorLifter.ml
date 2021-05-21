module type S = sig
  type merr

  val error_to_string : merr -> string
end

module Dummy (SMemory : SMemory.S) = struct
  type merr = SMemory.err_t

  let error_to_string merr = Fmt.to_to_string SMemory.pp_err merr
end
