module JsonMap : sig
  type t = (string * Yojson.Safe.t) list
end

val info : string -> unit
val setup : Debug_rpc.t -> unit
val to_rpc : ?json:JsonMap.t -> string -> unit
val show_report : string -> Logging.ReportId.t -> unit