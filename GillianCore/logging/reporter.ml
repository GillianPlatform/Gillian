module type S = sig
  val enable : unit -> unit

  val initialize : unit -> unit

  val log : Report.t -> unit

  val wrap_up : unit -> unit
end
