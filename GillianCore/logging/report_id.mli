(** @canonical Gillian.Logging.Report_id *)

type t = int64 [@@deriving yojson]

val next : unit -> t
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val of_string_opt : string -> t option
