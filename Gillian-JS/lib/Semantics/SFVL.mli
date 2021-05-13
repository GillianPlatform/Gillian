(****************************************
 * Interface for JSIL Symbolic FV-lists *
*****************************************)

type t [@@deriving yojson]

type field_name = Gillian.Gil_syntax.Expr.t

type field_value = Gillian.Gil_syntax.Expr.t

val add : field_name -> field_value -> t -> t

val empty : t

val field_names : t -> field_name list

val fold : (field_name -> field_value -> 'a -> 'a) -> t -> 'a -> 'a

val get : field_name -> t -> field_value option

val get_first : (field_name -> bool) -> t -> (field_name * field_value) option

val is_empty : t -> bool

val iter : (field_name -> field_value -> unit) -> t -> unit

val partition : (field_name -> field_value -> bool) -> t -> t * t

val remove : field_name -> t -> t

val pp : Format.formatter -> t -> unit

val union : t -> t -> t

val lvars : t -> Containers.SS.t

val alocs : t -> Containers.SS.t

val assertions : Gillian.Gil_syntax.Expr.t -> t -> Jsil_syntax.Asrt.t list

val substitution : Gillian.Symbolic.Subst.t -> bool -> t -> t

val selective_substitution : Gillian.Symbolic.Subst.t -> bool -> t -> t

val is_well_formed : t -> bool

val wf_assertions : t -> Gillian.Gil_syntax.Formula.t list

val to_list : t -> (field_name * field_value) list
