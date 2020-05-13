(** {3 Type definitions} *)

type t

type id

module IdSet : Set.S with type elt = id

(** {3 General manipulations } *)

val make : unit -> t

val reset : t -> unit

val to_reverse_graph : t -> t

val prune : t -> string list -> unit
(** [prune call_graph proc_names]  *)

val merge : t -> t -> t

(** {3 Getters and Setters} *)

val add_proc_call : t -> string -> string -> unit

val add_pred_call : t -> string -> string -> unit

val add_proc_pred_use : t -> string -> string -> unit

val get_children : t -> id -> id list

(** {3 Filters} *)

val contains_proc : t -> string -> bool

val contains_pred : t -> string -> bool

val is_pred : t -> id -> bool

val is_proc : t -> id -> bool

(** {3 Name/id manipulation} *)

val id_of_proc_name : string -> id

val id_of_pred_name : string -> id

val get_name : t -> id -> string

val get_proc_names : t -> string list

(** {3 Serialization} *)

val pp : Format.formatter -> t -> unit

val to_yojson : t -> Yojson.Safe.t

val of_yojson_exn : Yojson.Safe.t -> t
