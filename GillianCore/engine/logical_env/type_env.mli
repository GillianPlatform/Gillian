(** @canonical Gillian.Symbolic.Type_env

    Interface for typing environments *)

open SVal

(** @canonical Gillian.Symbolic.Type_env.t *)
type t [@@deriving yojson]

type constructors_tbl_t = (string, Constructor.t) Hashtbl.t [@@deriving yojson]
type datatypes_tbl_t = (string, Datatype.t) Hashtbl.t [@@deriving yojson]
type k := Id.any_var Id.t

val as_hashtbl : t -> (k, Type.t) Hashtbl.t
val copy : t -> t
val extend : t -> t -> unit
val filter : t -> (k -> bool) -> t
val filter_in_place : t -> (k -> bool) -> unit
val filter_vars : t -> LVar.Set.t -> t
val filter_vars_in_place : t -> LVar.Set.t -> unit
val get : t -> [< Id.any_var ] Id.t -> Type.t option
val get_exn : t -> [< Id.any_var ] Id.t -> Type.t
val get_var_type_pairs : t -> (k * Type.t) Seq.t
val get_vars_of_type : t -> Type.t -> k list
val init : unit -> t
val mem : t -> [< Id.any_var ] Id.t -> bool
val empty : t -> bool
val pp : Format.formatter -> t -> unit
val pp_by_need : Var.Set.t * LVar.Set.t -> Format.formatter -> t -> unit
val update : t -> [< Id.any_var ] Id.t -> Type.t -> unit
val remove : t -> [< Id.any_var ] Id.t -> unit
val reset : t -> (k * Type.t) list -> unit
val iter : t -> (k -> Type.t -> unit) -> unit
val fold : t -> (k -> Type.t -> 'a -> 'a) -> 'a -> 'a
val lvars : t -> LVar.Set.t
val vars : t -> Id.Any_var.Set.t
val matchables : t -> Id.Any_var.Set.t
val to_list : t -> (k * Type.t) list
val to_list_expr : t -> (Expr.t * Type.t) list
val substitution : t -> SESubst.t -> bool -> t
val is_well_formed : t -> bool
val filter_with_info : Var.Set.t * LVar.Set.t * Id.Any_loc.Set.t -> t -> t
