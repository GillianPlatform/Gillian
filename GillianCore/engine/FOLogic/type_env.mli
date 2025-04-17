(** @canonical Gillian.Symbolic.Type_env
  
  Interface for typing environments *)

open SVal

(** @canonical Gillian.Symbolic.Type_env.t *)
type t [@@deriving yojson]

type constructors_tbl_t [@@deriving yojson]

val as_hashtbl : t -> (string, Type.t) Hashtbl.t
val copy : t -> t
val extend : t -> t -> unit
val filter : t -> (string -> bool) -> t
val filter_in_place : t -> (string -> bool) -> unit
val filter_vars : t -> Containers.SS.t -> t
val filter_vars_in_place : t -> Containers.SS.t -> unit
val get : t -> string -> Type.t option
val get_unsafe : t -> string -> Type.t
val get_var_type_pairs : t -> (string * Type.t) Seq.t
val get_vars_of_type : t -> Type.t -> string list
val init : ?constructor_defs:constructors_tbl_t -> unit -> t
val mem : t -> string -> bool
val empty : t -> bool
val pp : Format.formatter -> t -> unit
val pp_by_need : Containers.SS.t -> Format.formatter -> t -> unit
val update : t -> string -> Type.t -> unit
val remove : t -> string -> unit
val reset : t -> (Var.t * Type.t) list -> unit
val iter : t -> (string -> Type.t -> unit) -> unit
val fold : t -> (string -> Type.t -> 'a -> 'a) -> 'a -> 'a
val lvars : t -> Containers.SS.t
val vars : t -> Containers.SS.t
val matchables : t -> Containers.SS.t
val to_list : t -> (Var.t * Type.t) list
val to_list_expr : t -> (Expr.t * Type.t) list
val substitution : t -> SESubst.t -> bool -> t
val is_well_formed : t -> bool

val filter_with_info :
  Containers.SS.t * Containers.SS.t * Containers.SS.t -> t -> t

val get_constructor_type : t -> string -> Type.t option
val get_constructor_type_unsafe : t -> string -> Type.t
val get_constructor_field_types : t -> string -> Type.t option list option
val copy_constructors : t -> t
