module type S = sig
  val initialize : unit -> unit

  val log : Report.t -> unit

  val wrap_up : unit -> unit
end

let initialize (reporter : (module S)) : unit =
  let (module R) = reporter in
  R.initialize ()

let log (reporter : (module S)) (report : Report.t) : unit =
  let (module R) = reporter in
  R.log report

let wrap_up (reporter : (module S)) : unit =
  let (module R) = reporter in
  R.wrap_up ()
