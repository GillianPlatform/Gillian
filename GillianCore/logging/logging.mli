module Mode : sig
  (** Logging levels *)
  type level =
    | Normal  (** Normal output *)
    | Verbose  (** Verbose output *)
    | Verboser  (** More verbose output *)
    | TMI  (** Too much information *)

  type t = Disabled | Enabled of level

  val enabled : unit -> bool

  val set_mode : t -> unit

  val should_log : level -> bool
end

module Report : sig
  type 'a report_builder

  val info :
    string ->
    ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
    'a report_builder

  val log :
    string ->
    ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
    'a report_builder

  val success :
    string ->
    ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
    'a report_builder

  val error :
    string ->
    ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
    'a report_builder

  val warning :
    string ->
    ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
    'a report_builder

  val enter_node : unit -> unit

  val exit_node : unit -> unit
end

val wrap_up : unit -> unit
(** Closes all the files *)

val normal : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `normal` is just `log Normal` *)

val verbose : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `verbose` is just `log Verbose` *)

val verboser : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `verboser` is just `log Verboser` *)

val tmi : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `tmi` is just `log TMI` *)

val fail : string -> 'a
(** Writes the string and then raises a failure. *)

val print_to_all : string -> unit
(** Output the strings in every file and prints it to stdout *)
