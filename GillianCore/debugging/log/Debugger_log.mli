module JsonMap : sig
  type t = (string * Yojson.Safe.t) list
end

exception FailureJson of string * JsonMap.t

val info : string -> unit
val enabled : unit -> bool
val setup : Debug_rpc.t -> unit
val show_report : string -> Logging.ReportId.t -> unit
val log : (unit -> string * JsonMap.t) -> unit
val failwith : (unit -> JsonMap.t) -> string -> 'a
