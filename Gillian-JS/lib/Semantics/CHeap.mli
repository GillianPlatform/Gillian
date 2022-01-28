open Gillian.Concrete

type t

val pp : Format.formatter -> t -> unit
val init : unit -> t
val get : t -> string -> (CObject.t * Values.t) option
val set : t -> string -> CObject.t * Values.t -> unit
val remove : t -> string -> unit
val copy : t -> t
