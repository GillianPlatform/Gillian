type 'a t = { pc : Pc.t; value : 'a }

val value : 'a t -> 'a

val pc : 'a t -> Pc.t

val learned : 'a t -> Gil_syntax.Formula.Set.t

val learned_types : 'a t -> (string * Gil_syntax.Type.t) list

val pp : 'a Fmt.t -> 'a t Fmt.t
