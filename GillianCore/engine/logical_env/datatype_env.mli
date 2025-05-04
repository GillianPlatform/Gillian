type constructors_tbl = (string, Constructor.t) Hashtbl.t
type datatypes_tbl = (string, Datatype.t) Hashtbl.t

val init : datatypes_tbl -> unit
val is_initialised : unit -> bool
val get_constructor_type : string -> Type.t option
val get_constructor_type_unsafe : string -> Type.t
val get_constructor_field_types : string -> Type.t option list option
val get_constructor_field_types_unsafe : string -> Type.t option list
val get_datatypes : unit -> Datatype.t list
