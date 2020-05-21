(** {3 Type definitions} *)

type t [@@deriving yojson]

type id

module IdSet : Set.S with type elt = id

(** {3 General manipulations } *)

val make : unit -> t

val reset : t -> unit

val to_reverse_graph : t -> t

val merge : t -> t -> t

(** {3 Getters and Setters} *)

val add_proc : t -> string -> unit

val add_pred : t -> string -> unit

val add_lemma : t -> string -> unit

val add_proc_call : t -> string -> string -> unit

val add_pred_call : t -> string -> string -> unit

val add_lemma_call : t -> string -> string -> unit

val add_proc_pred_use : t -> string -> string -> unit

val add_proc_lemma_use : t -> string -> string -> unit

val add_lemma_pred_use : t -> string -> string -> unit

val get_children : t -> id -> id list

val get_proc_names : t -> string list

val get_pred_names : t -> string list

val get_lemma_names : t -> string list

val prune_procs : t -> string list -> unit

val prune_lemmas : t -> string list -> unit

(** {3 Filters} *)

val contains_proc : t -> string -> bool

val contains_pred : t -> string -> bool

val contains_lemma : t -> string -> bool

val is_pred : t -> id -> bool

val is_proc : t -> id -> bool

val is_lemma : t -> id -> bool

(** {3 Name/id manipulation} *)

val id_of_proc_name : string -> id

val id_of_pred_name : string -> id

val id_of_lemma_name : string -> id

val get_name : t -> id -> string

(** {3 Serialization} *)

val pp : Format.formatter -> t -> unit
