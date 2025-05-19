module type S = sig
  val start : unit -> unit
end

module type Make = functor (Debugger : Debugger.S) -> S

module type Intf = sig
  module type S = S
  module type Make = Make

  module Make : Make
end
