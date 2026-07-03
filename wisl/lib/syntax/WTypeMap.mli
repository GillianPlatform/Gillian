module TypeMap : sig
  type key = WLExpr.tt
  type +'a t
end

type t = WType.t TypeMap.t

val type_of_variable : string -> t -> WType.t option
val infer_types_pred : (string * WType.t option) list -> WLAssert.t list -> t
val infer_types_pure_fun : (string * WType.t option) list -> WLExpr.t -> t
