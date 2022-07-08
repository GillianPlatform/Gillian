module JsonMap : sig
  type t = (string * Yojson.Safe.t) list
end

exception FailureJson of string * JsonMap.t

val to_file : string -> unit
val enabled : unit -> bool
val setup : Debug_rpc.t -> unit

val log_async :
  ((?json:JsonMap.t -> ('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit Lwt.t

val log :
  ((?json:JsonMap.t -> ('a, Format.formatter, unit) format -> 'a) -> unit) ->
  unit

val show_report : Logging.ReportId.t -> string -> unit

val set_rpc_command_handler :
  Debug_rpc.t ->
  ?name:string ->
  (module Debug_protocol.COMMAND
     with type Arguments.t = 'a
      and type Result.t = 'b) ->
  ('a -> 'b Lwt.t) ->
  unit

val failwith : (unit -> JsonMap.t) -> string -> 'a
