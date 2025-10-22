type t

val init : unit -> t
val get : t -> string -> int -> Gillian.Concrete.Values.t option
val set : t -> string -> int -> Gillian.Concrete.Values.t -> unit
val alloc : t -> int -> string
val remove : t -> string -> int -> unit
val dispose : t -> string -> unit
val str : t -> string
val copy : t -> t
