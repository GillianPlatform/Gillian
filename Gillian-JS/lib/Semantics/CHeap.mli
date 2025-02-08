open Gillian.Concrete
module Loc := Gil_syntax.Loc

type t

val pp : Format.formatter -> t -> unit
val init : unit -> t
val get : t -> Loc.t -> (CObject.t * Values.t) option
val set : t -> Loc.t -> CObject.t * Values.t -> unit
val remove : t -> Loc.t -> unit
val copy : t -> t
