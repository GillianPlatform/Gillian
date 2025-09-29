(** generates a new random id *)
val gen_id : unit -> int

(** Generates a new string with prefix [gen_str fname pre]
    @param fname Function name, context in which you create labels and variables
    @param pre Prefix for the variable/label name *)
val gen_str : fname:string -> string -> string

val reset : unit -> unit
