(** Unique name generators

    These functions generate a unique name of a particular format on each call
*)

(** Generates a literal location name *)
val fresh_loc : unit -> string

(** Generates a program variable name *)
val fresh_pvar : unit -> string

(** Generates a logical variable name *)
val fresh_lvar : unit -> string

(** Generates a logical variable name (for bi-abduction) *)
val fresh_svar : unit -> string

(** Resets all generators *)
val reset : unit -> unit
