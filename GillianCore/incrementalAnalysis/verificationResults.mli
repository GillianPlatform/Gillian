type t

val make : unit -> t

val reset : t -> unit

val prune : t -> string list -> unit
(** [prune results proc_names] *)

val merge : t -> t -> t

val set_result : t -> string -> int -> bool -> unit

val check_previously_verified :
  ?printer:(bool -> unit) -> t -> Containers.SS.t -> bool

val to_yojson : t -> Yojson.Safe.t

val of_yojson_exn : Yojson.Safe.t -> t
