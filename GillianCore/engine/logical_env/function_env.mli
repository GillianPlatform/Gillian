type t = (string, Func.t) Hashtbl.t

val init : t -> unit
val is_initialised : unit -> bool
val get_function_param_types : string -> Type.t option list option
