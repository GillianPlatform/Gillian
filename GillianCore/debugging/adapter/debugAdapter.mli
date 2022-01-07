module type S = sig
  val start : Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t
end

module Make (Debugger : Debugger.S) : S
