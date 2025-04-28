type t =
  | WList
  | WNull
  | WBool
  | WString
  | WPtr
  | WInt
  | WAny
  | WSet
  | WDatatype of string

val compatible : t -> t -> bool
val strongest : t -> t -> t
val pp : Format.formatter -> t -> unit
val to_gil : t -> Gillian.Gil_syntax.Type.t

exception Unmatching_types

module TypeMap : sig
  type key = WLExpr.tt
  type +'a t
end

val of_variable : string -> t TypeMap.t -> t option

val infer_types_pred :
  (string * t option) list -> WLAssert.t list -> t TypeMap.t

val infer_types_func : (string * t option) list -> WLExpr.t -> t TypeMap.t
