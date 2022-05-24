module JsonMap : sig
  type t = (string * Yojson.Safe.t) list
end

exception FailureJson of string * JsonMap.t

val info : string -> unit
val enabled : unit -> bool
val setup : Debug_rpc.t -> unit

val log :
  ((?json:JsonMap.t -> ('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

val show_report : Logging.ReportId.t -> string -> unit
val failwith : (unit -> JsonMap.t) -> string -> 'a
