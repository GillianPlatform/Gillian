(**
    Interface for a reporter
*)

module type S = sig
  (** Initializes the reporter *)
  val initialize : unit -> unit

  (** Returns whether a specified content type will be logged *)
  val will_log : string -> bool

  (** Logs a report *)
  val log : Report.t -> unit

  (** Runs any clean up code *)
  val wrap_up : unit -> unit
end

type t = (module S)

(** Calls a given reporter module's `initialize` function *)
let initialize (reporter : t) : unit =
  let (module R) = reporter in
  R.initialize ()

(** Calls a given reporter module's `will_log` function *)
let will_log (reporter : t) (type_ : string) : bool =
  let (module R) = reporter in
  R.will_log type_

(** Calls a given reporter module's `log` function *)
let log (reporter : t) (report : Report.t) : unit =
  let (module R) = reporter in
  R.log report

(** Calls a given reporter module's `wrap_up` function *)
let wrap_up (reporter : t) : unit =
  let (module R) = reporter in
  R.wrap_up ()
