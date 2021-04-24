(**
    Interface for a reporter
*)

module type S = sig
  (** Initializes the reporter *)
  val initialize : unit -> unit

  (** Logs a report *)
  val log : Report.t -> unit

  (** Runs any clean up code *)
  val wrap_up : unit -> unit
end

(** Calls a given reporter module's `initialize` function *)
let initialize (reporter : (module S)) : unit =
  let (module R) = reporter in
  R.initialize ()

(** Calls a given reporter module's `log` function *)
let log (reporter : (module S)) (report : Report.t) : unit =
  let (module R) = reporter in
  R.log report

(** Calls a given reporter module's `wrap_up` function *)
let wrap_up (reporter : (module S)) : unit =
  let (module R) = reporter in
  R.wrap_up ()
