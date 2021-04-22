module Mode : sig
  (** Logging levels *)
  type level =
    | Normal  (** Normal output *)
    | Verbose  (** Verbose output *)
    | TMI  (** Too much information *)

  type t = Disabled | Enabled of level

  val enabled : unit -> bool

  val set_mode : t -> unit
end

(*
module Report : sig
  type id

  type severity = Info | Log | Success | Error | Warning

  type 'a t
end *)

module Reporter : sig
  type 'a t =
    < log : 'a Report.t -> unit
    ; log_specific : Loggable.loggable -> unit
    ; wrap_up : unit >
end

module FileReporter : sig
  val enable : unit -> unit

  class virtual ['a] t :
    object
      method log : 'a Report.t -> unit

      method log_specific : Loggable.loggable -> unit

      method wrap_up : unit

      method private formatter : Format.formatter
    end
end

module DatabaseReporter : sig
  val enable : unit -> unit

  class virtual ['a] t :
    object
      method log : 'a Report.t -> unit

      method log_specific : Loggable.loggable -> unit

      method wrap_up : unit

      method virtual private specific_serializer : 'a -> Yojson.Safe.t
    end
end

module Loggable : sig
  module type t = sig
    (* Type to be logged *)
    type t [@@deriving yojson]

    (* Pretty prints the value *)
    val pp : Format.formatter -> t -> unit
  end

  type 'a t = (module t with type t = 'a)

  type loggable = L : ('a t * 'a) -> loggable

  (* val pp : 'a t -> Format.formatter -> 'a -> unit

     val of_yojson : 'a t -> Yojson.Safe.t -> ('a, string) result

     val to_yojson : 'a t -> 'a -> Yojson.Safe.t *)

  val to_yojson : loggable -> Yojson.Safe.t

  (* val make :
     (Format.formatter -> 'a -> unit) ->
     (Yojson.Safe.t -> ('a, string) result) ->
     ('a -> Yojson.Safe.t) ->
     'a t *)

  val make :
    (Format.formatter -> 'a -> unit) ->
    (Yojson.Safe.t -> ('a, string) result) ->
    ('a -> Yojson.Safe.t) ->
    'a ->
    loggable
end

(** Closes all the files *)
val wrap_up : unit -> unit

(** `normal` is just `log Normal` *)
val normal :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** `verbose` is just `log Verbose` *)
val verbose :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

(** `tmi` is just `log TMI` *)
val tmi :
  ?title:string ->
  ?severity:Report.severity ->
  ((('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

val log_specific :
  Mode.level ->
  ?title:string ->
  ?severity:Report.severity ->
  Loggable.loggable ->
  'a ->
  unit

(** Writes the string and then raises a failure. *)
val fail : string -> 'a

(** Output the strings in every file and prints it to stdout *)
val print_to_all : string -> unit

val normal_phase :
  ?title:string -> ?severity:Report.severity -> unit -> Report.id option

val verbose_phase :
  ?title:string -> ?severity:Report.severity -> unit -> Report.id option

val tmi_phase :
  ?title:string -> ?severity:Report.severity -> unit -> Report.id option

val end_phase : Report.id option -> unit

val with_normal_phase :
  ?title:string -> ?severity:Report.severity -> (unit -> 'a) -> 'a

val with_verbose_phase :
  ?title:string -> ?severity:Report.severity -> (unit -> 'a) -> 'a

val with_tmi_phase :
  ?title:string -> ?severity:Report.severity -> (unit -> 'a) -> 'a

(*
module Make : functor
  (TargetLang : sig
     type t

     val file_reporter : t FileReporter.t option

     val database_reporter : t DatabaseReporter.t option
   end)
  -> sig
  val normal :
    ?title:string -> ?severity:Report.severity -> TargetLang.t -> unit

  val verbose :
    ?title:string -> ?severity:Report.severity -> TargetLang.t -> unit

  val tmi : ?title:string -> ?severity:Report.severity -> TargetLang.t -> unit

  val wrap_up : unit -> unit
end *)
