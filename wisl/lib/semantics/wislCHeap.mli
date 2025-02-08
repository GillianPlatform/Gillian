type t

val init : unit -> t
val get : t -> Gil_syntax.Loc.t -> int -> Gillian.Concrete.Values.t option
val set : t -> Gil_syntax.Loc.t -> int -> Gillian.Concrete.Values.t -> unit
val alloc : t -> int -> Gil_syntax.Loc.t
val remove : t -> Gil_syntax.Loc.t -> int -> unit
val dispose : t -> Gil_syntax.Loc.t -> unit
val str : t -> string
val copy : t -> t
