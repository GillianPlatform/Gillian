(** @canonical Gillian.Logging.ReportId *)

type t = int64 [@@deriving yojson]

val next : unit -> t
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
