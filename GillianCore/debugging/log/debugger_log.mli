module Public : sig
  (** @canonical Gillian.Debugger.Logging

    Functions for sending log messages to the debugger frontend*)

  (** A mapping of strings to [Yojson] values *)
  module JsonMap : sig
    type t = (string * Yojson.Safe.t) list
  end

  (** Sends a log message to the debugger frontend via a custom event.
    Optionally includes some accompanying JSON.
    
    A couple of things to bear in mind:
    - If debugging isn't enabled (or {!setup} hasn't been called yet), this function does nothing, i.e. the passed function isn't called.
    - The message is sent asynchronously; it may, for example, arrive after an error is raised, despite being called before.
    - The message is also logged to the debugger log file (see {!to_file}), though without the JSON - if JSON is provided, the message is suffixed with [(+)]. *)
  val log :
    ((?json:JsonMap.t -> ('a, Format.formatter, unit) format -> 'a) -> unit) ->
    unit

  (** Logs a message to the debugger log file ([gillian-debugger.log] in the current working directory).
    This is more reliable than {!log}, but more unwieldy, and doesn't support attached JSON. *)
  val to_file : string -> unit

  (** Logs a message (as with {!log}), but attaches the type and (parsed) content of the specified report. *)
  val show_report : Logging.Report_id.t -> string -> unit

  (** Raises an exception with a string message and attached JSON.

    Note that if debugging isn't enabled, then the JSON function is ignored,
    and a regular `Failure` is raised instead. *)
  val failwith : (unit -> JsonMap.t) -> string -> 'a
end

include module type of Public

val setup : Debug_rpc.t -> unit
val set_debug_state_dumper : (unit -> Yojson.Safe.t) -> unit

val set_rpc_command_handler :
  Debug_rpc.t ->
  ?name:string ->
  (module Debug_protocol.COMMAND
     with type Arguments.t = 'a
      and type Result.t = 'b) ->
  ('a -> 'b Lwt.t) ->
  unit
