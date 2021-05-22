module type S = sig
  type merr

  val error_to_string : merr -> int Cmd.t option -> string
end

module Dummy (SMemory : SMemory.S) : S with type merr = SMemory.err_t
