module LoggingConstants = LoggingConstants

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

module ReportId : sig
  type t [@@deriving yojson]

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Reporter : sig
  type t

  (** Calls a given reporter module's `initialize` function *)
  val initialize : t -> unit

  (** Calls a given reporter module's `log` function *)
  val log : t -> Report.t -> unit

  (** Calls a given reporter module's `wrap_up` function *)
  val wrap_up : t -> unit
end

val database_reporter : Reporter.t
val file_reporter : Reporter.t

module Loggable : sig
  (** Type storing the functions required to log the specified type and the
      actual content to be logged *)
  type t

  (** Returns a loggable, given the required functions and content *)
  val make :
    (Format.formatter -> 'a -> unit) ->
    (Yojson.Safe.t -> ('a, string) result) ->
    ('a -> Yojson.Safe.t) ->
    'a ->
    t

  val make_string : string -> t
end

module LogQueryer : sig
  (* Returns the content and the content type given the report id *)
  val get_report : ReportId.t -> (string * string) option

  (* Returns the previous report id which has type cmd_step given the current
     report id *)
  val get_previous_report_id : ReportId.t -> ReportId.t option
  val get_next_reports : ReportId.t -> (ReportId.t * string * string) list

  (* Returns the all next report ids which have type cmd_step, given the current
     report id *)
  val get_next_report_ids : ReportId.t -> ReportId.t list

  (* Returns the 'first' next report id which has type cmd_step, given the current
     report id *)
  val get_next_report_id : ReportId.t -> ReportId.t option

  (* Returns the annotation corresponding to the previous set freed action
     for a given location in the current phase if it exists *)
  val get_previously_freed_annot : string -> string option

  (* Returns the ids, types, and content of any children of the given report id;
     if `roots_only` is true, only get children with no previous*)
  val get_children_of :
    ?roots_only:bool -> ReportId.t -> (ReportId.t * string * string) list

  (* Returns the list of IDs and content of any children of the given report
     ID who have type 'cmd_result' *)
  val get_cmd_results : ReportId.t -> (ReportId.t * string) list

  (* Returns a 'unify' report that is the direct child of the given report id,
     if it exists *)
  val get_unify_for : ReportId.t -> (ReportId.t * string) option
end

(** Initializes the logging module with the specified reporters and initializes
    the reporters *)
val initialize : Reporter.t list -> unit

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
  Loggable.t ->
  string ->
  ReportId.t option

(** Logs a type at the `Verbose` logging level given a loggable and its content
    type (which should be one of the predefined strings in the
    `LoggingConstants.ContentType`). Returns the logged report id if it has
    been logged. *)
val verbose_specific :
  ?title:string ->
  ?severity:Report.severity ->
  Loggable.t ->
  string ->
  ReportId.t option

(** Logs a type at the `TMI` logging level given a loggable and its content
    type (which should be one of the predefined strings in the
    `LoggingConstants.ContentType`). Returns the logged report id if it has
    been logged. *)
val tmi_specific :
  ?title:string ->
  ?severity:Report.severity ->
  Loggable.t ->
  string ->
  ReportId.t option

(** Writes the string and then raises a failure. *)
val fail : string -> 'a

(** Output the strings in every file and prints it to stdout *)
val print_to_all : string -> unit

(** Starts a phase with logging level set to `Normal` *)
val normal_phase :
  ?title:string -> ?severity:Report.severity -> unit -> ReportId.t option

(** Starts a phase with logging level set to `Verbose` *)
val verbose_phase :
  ?title:string -> ?severity:Report.severity -> unit -> ReportId.t option

(** Starts a phase with logging level set to `TMI` *)
val tmi_phase :
  ?title:string -> ?severity:Report.severity -> unit -> ReportId.t option

(** Ends the phase corresponding to the specified report id *)
val end_phase : ReportId.t option -> unit

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

val set_previous : ReportId.t option -> unit
val get_parent : unit -> ReportId.t option
val set_parent : ReportId.t -> unit
val release_parent : ReportId.t option -> unit
val with_parent_id : ReportId.t option -> (unit -> 'a) -> 'a

val with_parent :
  ?title:string ->
  ?lvl:Mode.level ->
  ?severity:Report.severity ->
  Loggable.t option ->
  string ->
  (unit -> 'a) ->
  'a

val dummy_pp : Format.formatter -> 'a -> unit
