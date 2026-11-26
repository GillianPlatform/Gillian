type t

val init : unit -> t

val get :
  t -> Gillian.Gil_syntax.Id.Loc.t -> int -> Gillian.Concrete.Values.t option

val set :
  t -> Gillian.Gil_syntax.Id.Loc.t -> int -> Gillian.Concrete.Values.t -> unit

val alloc : t -> int -> Gillian.Gil_syntax.Id.Loc.t
val remove : t -> Gillian.Gil_syntax.Id.Loc.t -> int -> unit
val dispose : t -> Gillian.Gil_syntax.Id.Loc.t -> unit
val str : t -> string
val copy : t -> t
