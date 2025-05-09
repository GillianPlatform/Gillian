module TypeMap : sig
  type key = WLExpr.tt
  type +'a t
end

type t = WType.t TypeMap.t

val type_of_variable : string -> t -> WType.t option
val infer_types_pred : (string * WType.t option) list -> WLAssert.t list -> t
