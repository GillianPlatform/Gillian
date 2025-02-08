open Gil_syntax

type 'a t = { pc : Pc.t; value : 'a }

val make : pc:Pc.t -> value:'a -> 'a t
val value : 'a t -> 'a
val pc : 'a t -> Pc.t
val learned : 'a t -> Expr.Set.t
val learned_types : 'a t -> (Id.any_var Id.t * Type.t) list
val pp : 'a Fmt.t -> 'a t Fmt.t
