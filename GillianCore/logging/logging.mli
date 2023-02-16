(** @canonical Gillian.Logging.Logging_constants *)
module Logging_constants = Logging_constants

open Logging_constants

(** @canonical Gillian.Logging.Loggable *)

(** @canonical Gillian.Logging.Mode *)
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

(** @canonical Gillian.Logging.Report_id *)
module Report_id : sig
  type t [@@deriving yojson]

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Reporter : sig
  type t

  (** Calls a given reporter module's [initialize] function *)
  val initialize : t -> unit

  (** Calls a given reporter module's [log] function *)
  val log : t -> Report.t -> unit

  (** Calls a given reporter module's [wrap_up] function *)
  val wrap_up : t -> unit
end

val database_reporter : Reporter.t
val html_reporter : Reporter.t
val file_reporter : Reporter.t

(** @canonical Gillian.Logging.Loggable *)
module Loggable : module type of Loggable

(** @canonical Gillian.Logging.Log_queryer *)
module Log_queryer : sig
  (** Gets content and type (in that order) of the given ID *)
  val get_report : Report_id.t -> (string * string) option

  (** Gets the ID of the report that precedes the given ID *)
  val get_previous_report_id : Report_id.t -> Report_id.t option

  (** Gets the ID, content and type (in that order) of all reports
    that directly succeed the given ID *)
  val get_next_reports : Report_id.t -> (Report_id.t * string * string) list

  (** As with {!get_next_reports}, but only gives IDs *)
  val get_next_report_ids : Report_id.t -> Report_id.t list

  (** Gets the ID of the "first" report that succeeds the given ID *)
  val get_next_report_id : Report_id.t -> Report_id.t option

  (** Gets the annotation corresponding to the previous set-freed action
     for a given location in the current phase, if it exists *)
  val get_previously_freed_annot : string -> string option

  (** Gets the ID, type, and content (in that order) of any children of the given ID

    If [roots_only] is true, only gets children with no previous; defaults to [false] *)
  val get_children_of :
    ?roots_only:bool -> Report_id.t -> (Report_id.t * string * string) list

  (** Gets the ID and content of any children of the given ID with type ["cmd_result"] *)
  val get_cmd_results : Report_id.t -> (Report_id.t * string) list

  (** Gets the ID and content of a child, with type ["unify"], of the given ID (if it exists) *)
  val get_unify_for : Report_id.t -> (Report_id.t * string) option

  (** Returns the ID and content of all children of the given ID with type ["unify_result"] *)
  val get_unify_results : Report_id.t -> (Report_id.t * string) list
end

module Report_state : sig
  type t

  val make : unit -> t
  val clone : t -> t
  val global_state : t
  val activate : t -> unit
  val with_state : (unit -> 'a) -> t -> 'a
end

(** Initializes the logging module with the specified reporters and initializes
    the reporters *)
val initialize : Reporter.t list -> unit

(** Runs any clean up code *)
val wrap_up : unit -> unit

val normal :
  ?title:string ->
  ?severity:Severity.t ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

val verbose :
  ?title:string ->
  ?severity:Severity.t ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** Logs a message at the [TMI] logging level given a message format *)
val tmi :
  ?title:string ->
  ?severity:Severity.t ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** @canonical Gillian.Logging.Specific *)
module Specific : sig
  (** Logs a {!Loggable.t} at the [Normal] log level *)
  val normal :
    ?title:string ->
    ?severity:Severity.t ->
    Loggable.t ->
    string ->
    Report_id.t option

  (** Logs a {!Loggable.t} at the [Verbose] log level *)
  val verbose :
    ?title:string ->
    ?severity:Severity.t ->
    Loggable.t ->
    string ->
    Report_id.t option

  (** Logs a {!Loggable.t} at the [TMI] log level *)
  val tmi :
    ?title:string ->
    ?severity:Severity.t ->
    Loggable.t ->
    string ->
    Report_id.t option
end

(** Writes the string and then raises a failure. *)
val fail : string -> 'a

(** Output the strings in every file and prints it to stdout *)
val print_to_all : string -> unit

(** @canonical Gillian.Logging.Phase *)
module Phase : sig
  (** Starts a phase with logging level set to [Normal] *)
  val normal :
    ?title:string -> ?severity:Severity.t -> unit -> Report_id.t option

  (** Starts a phase with logging level set to [Verbose] *)
  val verbose :
    ?title:string -> ?severity:Severity.t -> unit -> Report_id.t option

  (** Starts a phase with logging level set to [TMI] *)
  val tmi : ?title:string -> ?severity:Severity.t -> unit -> Report_id.t option

  (** Ends the phase corresponding to the specified report id *)
  val stop : Report_id.t option -> unit

  (** Runs the specified function within a phase with logging level set to
      [Normal] *)
  val with_normal : ?title:string -> ?severity:Severity.t -> (unit -> 'a) -> 'a

  (** Runs the specified function within a phase with logging level set to
      [Verbose] *)
  val with_verbose : ?title:string -> ?severity:Severity.t -> (unit -> 'a) -> 'a

  (** Runs the specified function within a phase with logging level set to
      [TMI] *)
  val with_tmi : ?title:string -> ?severity:Severity.t -> (unit -> 'a) -> 'a
end

val set_previous : Report_id.t option -> unit

(** @canonical Gillian.Logging.Parent *)
module Parent : sig
  val get : unit -> Report_id.t option
  val set : Report_id.t -> unit
  val release : Report_id.t option -> unit
  val with_id : Report_id.t option -> (unit -> 'a) -> 'a

  val with_specific :
    ?title:string ->
    ?lvl:Mode.level ->
    ?severity:Severity.t ->
    Loggable.t option ->
    string ->
    (unit -> 'a) ->
    'a
end

val dummy_pp : Format.formatter -> 'a -> unit
