module LoggingConstants : sig
  (** Allowed strings for the type_ field of a report *)
  module ContentType : sig
    val debug : string

    val phase : string

    val cmd_step : string
  end
end

module Mode : sig
  (** Logging levels *)
  type level =
    | Normal  (** Normal output *)
    | Verbose  (** Verbose output *)
    | TMI  (** Too much information *)

  (** Type specifying the logging mode *)
  type t = Disabled | Enabled of level

  (** Returns whether logging is enabled or not *)
  val enabled : unit -> bool

  (** Sets the logging mode *)
  val set_mode : t -> unit

  (** Pretty print function for logging mode type  *)
  val pp : Format.formatter -> t -> unit
end

module Reporter : sig
  module type S = sig
    (** Initializes the reporter *)
    val initialize : unit -> unit

    (** Logs a report *)
    val log : Report.t -> unit

    (** Runs any clean up code *)
    val wrap_up : unit -> unit
  end
end

module DatabaseReporter : Reporter.S

module FileReporter : Reporter.S

module Loggable : sig
  (** Module specifying functions required for a type to be loggable *)
  module type t = sig
    (** Type to be logged *)
    type t [@@deriving yojson]

    (** Pretty printer for the type *)
    val pp : Format.formatter -> t -> unit
  end

  (** Type for a module which specifies functions required for a type to be loggable *)
  type 'a t = (module t with type t = 'a)

  (** Type storing the functions required to log the specified type and the
      actual content to be logged *)
  type loggable = L : ('a t * 'a) -> loggable

  (** Converts a loggable to Yojson *)
  val loggable_to_yojson : loggable -> Yojson.Safe.t

  (** Returns a loggable, given the required functions and content *)
  val make :
    (Format.formatter -> 'a -> unit) ->
    (Yojson.Safe.t -> 'a) ->
    ('a -> Yojson.Safe.t) ->
    'a ->
    loggable
end

module LogQueryer : sig
  (* Returns the content and the content type given the report id *)
  val get_report : string -> (string * string) option

  (* Returns the previous report id which has type cmd_step given the current
     report id *)
  val get_previous_report_id : string -> string option

  (* Returns the next report id which has type cmd_step given the current
     report id *)
  val get_next_report_id : string -> string option
end

(** Initializes the logging module with the specified reporters and initializes
    the reporters *)
val initialize : (module Reporter.S) list -> unit

(** Runs any clean up code *)
val wrap_up : unit -> unit

(** Logs a message at the `Normal` logging level given a message format *)
val normal :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** Logs a message at the `Verbose` logging level given a message format *)
val verbose :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** Logs a message at the `TMI` logging level given a message format *)
val tmi :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** Logs a type at the `Normal` logging level given a loggable and its content
    type (which should be one of the predefined strings in the
    `LoggingConstants.ContentType`). Returns the logged report id if it has
    been logged. *)
val normal_specific :
  ?title:string ->
  ?severity:Report.severity ->
  Loggable.loggable ->
  string ->
  string option

(** Logs a type at the `Verbose` logging level given a loggable and its content
    type (which should be one of the predefined strings in the
    `LoggingConstants.ContentType`). Returns the logged report id if it has
    been logged. *)
val verbose_specific :
  ?title:string ->
  ?severity:Report.severity ->
  Loggable.loggable ->
  string ->
  string option

(** Logs a type at the `TMI` logging level given a loggable and its content
    type (which should be one of the predefined strings in the
    `LoggingConstants.ContentType`). Returns the logged report id if it has
    been logged. *)
val tmi_specific :
  ?title:string ->
  ?severity:Report.severity ->
  Loggable.loggable ->
  string ->
  string option

(** Writes the string and then raises a failure. *)
val fail : string -> 'a

(** Output the strings in every file and prints it to stdout *)
val print_to_all : string -> unit

(** Starts a phase with logging level set to `Normal` *)
val normal_phase :
  ?title:string -> ?severity:Report.severity -> unit -> Report.uuidm option

(** Starts a phase with logging level set to `Verbose` *)
val verbose_phase :
  ?title:string -> ?severity:Report.severity -> unit -> Report.uuidm option

(** Starts a phase with logging level set to `TMI` *)
val tmi_phase :
  ?title:string -> ?severity:Report.severity -> unit -> Report.uuidm option

(** Ends the phase corresponding to the specified report id *)
val end_phase : Report.uuidm option -> unit

(** Runs the specified function within a phase with logging level set to
    `Normal` *)
val with_normal_phase :
  ?title:string -> ?severity:Report.severity -> (unit -> 'a) -> 'a

(** Runs the specified function within a phase with logging level set to
    `Verbose` *)
val with_verbose_phase :
  ?title:string -> ?severity:Report.severity -> (unit -> 'a) -> 'a

(** Runs the specified function within a phase with logging level set to
    `TMI` *)
val with_tmi_phase :
  ?title:string -> ?severity:Report.severity -> (unit -> 'a) -> 'a
