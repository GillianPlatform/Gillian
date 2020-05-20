type t [@@deriving yojson]

val make : unit -> t

val reset : t -> unit

val set_spec : t -> string -> Spec.t -> unit

val get_all_specs : ?filter:(string -> bool) -> t -> Spec.t list

val remove : t -> string -> unit

(** [prune results proc_names] *)
val prune : t -> string list -> unit

val merge : t -> t -> t
