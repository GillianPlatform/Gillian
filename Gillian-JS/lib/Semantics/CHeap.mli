open Gillian.Concrete

type t

val pp : Format.formatter -> t -> unit
val init : unit -> t
val get : t -> Gillian.Gil_syntax.Id.Loc.t -> (CObject.t * Values.t) option
val set : t -> Gillian.Gil_syntax.Id.Loc.t -> CObject.t * Values.t -> unit
val remove : t -> Gillian.Gil_syntax.Id.Loc.t -> unit
val copy : t -> t
