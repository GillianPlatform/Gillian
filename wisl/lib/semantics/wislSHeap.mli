type t

val init : unit -> t

val get_fvl : t -> string -> SFVL.t option

val set_fvl : t -> string -> SFVL.t -> unit

val alloc : t -> int -> string

val remove : t -> string -> unit

val str : t -> string

val copy : t -> t

val substitution_in_place : Gillian.Symbolic.Subst.t -> t -> unit

val assertions : t -> Gillian.Gil_syntax.Asrt.t list
