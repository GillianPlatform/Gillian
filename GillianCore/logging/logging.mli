module Mode : sig
  (** Logging levels *)
  type level =
    | Normal  (** Normal output *)
    | Verbose  (** Verbose output *)
    | TMI  (** Too much information *)

  type t = Disabled | Enabled of level

  val enabled : unit -> bool

  val set_mode : t -> unit

  val should_log : level -> bool
end

module Report : sig
  type phase = ParsingAndCompiling | Parsing | Preprocessing | Verification
end

val wrap_up : unit -> unit
(** Closes all the files *)

val normal : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `normal` is just `log Normal` *)

val verbose : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `verbose` is just `log Verbose` *)

val tmi : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
(** `tmi` is just `log TMI` *)

val fail : string -> 'a
(** Writes the string and then raises a failure. *)

val print_to_all : string -> unit
(** Output the strings in every file and prints it to stdout *)

val normal_phase : Report.phase -> unit

val verbose_phase : Report.phase -> unit

val tmi_phase : Report.phase -> unit

val end_phase : Report.phase -> unit
