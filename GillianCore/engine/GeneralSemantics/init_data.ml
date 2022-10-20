module type S = sig
  type t

  val parse : string -> t
  val to_string : t -> string
end

module Dummy : S with type t = unit = struct
  type t = unit

  let parse _ = ()
  let to_string _ = ""
end
