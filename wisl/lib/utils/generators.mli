(** generates a new random id *)
val gen_id : unit -> int

(** Generates a new string with prefix
    @param fname Function name, context in which you create labels and variables
    @param pre Prefix for the variable/label name *)
val gen_str : string -> string -> string

val reset : unit -> unit
