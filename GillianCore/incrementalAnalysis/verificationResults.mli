type t [@@deriving yojson]

val make : unit -> t

val reset : t -> unit

(** [prune results proc_names] *)
val prune : t -> string list -> unit

val merge : t -> t -> t

val set_result : t -> string -> int -> bool -> unit

val check_previously_verified :
  ?printer:(bool -> unit) -> t -> Containers.SS.t -> bool
