(****************************************
 * Interface for JSIL Symbolic FV-lists *
*****************************************)

open Gillian.Gil_syntax
open Gillian.Symbolic

type field_name = Expr.t
type field_value = Expr.t
type t = field_value Expr.Map.t [@@deriving yojson]

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
val assertions : Expr.t -> t -> Asrt.t
val substitution : Subst.t -> bool -> t -> t
val selective_substitution : Subst.t -> bool -> t -> t
val is_well_formed : t -> bool
val wf_assertions : t -> Expr.t list
val to_list : t -> (field_name * field_value) list
