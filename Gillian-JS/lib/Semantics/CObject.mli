open Gillian.Concrete

type t

val pp : Format.formatter -> string * t * Values.t -> unit
val init : unit -> t
val get : t -> string -> Values.t option
val set : t -> string -> Values.t -> unit
val remove : t -> string -> unit
val properties : t -> string list
