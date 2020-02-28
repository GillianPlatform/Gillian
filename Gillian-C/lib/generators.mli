val gen_id : unit -> int
(** generates a new random id *)

val gen_str : string -> string -> string
(** Generates a new string with prefix 
    [gen_str fname pre]
    @param fname Function name, context in which you create labels and variables
    @param pre   Prefix for the variable/label name
*)

val reset : unit -> unit
