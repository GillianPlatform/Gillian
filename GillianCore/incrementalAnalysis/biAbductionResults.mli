type t

val make : unit -> t

val reset : t -> unit

val set_spec : t -> string -> Spec.t -> unit

val get_all_specs : ?filter:(string -> bool) -> t -> Spec.t list

val remove : t -> string -> unit

val prune : t -> string list -> unit
(** [prune results proc_names] *)

val merge : t -> t -> t

val to_yojson : t -> Yojson.Safe.t

val of_yojson_exn : Yojson.Safe.t -> t
