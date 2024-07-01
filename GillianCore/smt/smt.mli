open Gil_syntax

type model

val setup : unit -> unit
val check_sat_core : Formula.Set.t -> (string, Type.t) Hashtbl.t -> model option
val check_sat : Formula.Set.t -> (string, Type.t) Hashtbl.t -> bool
val check_sat_with_model : Formula.Set.t -> (string, Type.t) Hashtbl.t -> bool * model option
