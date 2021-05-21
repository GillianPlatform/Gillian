module type S = sig
  type merr

  val error_to_string : merr -> string
end

module Dummy (SMemory : SMemory.S) : S with type merr = SMemory.err_t
