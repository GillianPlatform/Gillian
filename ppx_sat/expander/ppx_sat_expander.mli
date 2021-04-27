open Ppxlib

module Extension_name : sig
  type t = Sat | Ent

  val to_string : t -> string
end

val expand : ext:Extension_name.t -> expression -> expression
