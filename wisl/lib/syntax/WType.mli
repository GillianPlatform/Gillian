type t = WList | WNull | WBool | WString | WPtr | WInt | WAny | WSet

val compatible : t -> t -> bool
val strongest : t -> t -> t
val pp : Format.formatter -> t -> unit
val to_gil : t -> Gillian.Gil_syntax.Type.t
