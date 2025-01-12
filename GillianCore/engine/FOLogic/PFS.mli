(** @canonical Gillian.Symbolic.Pure_context

GIL pure formulae *)

(** @canonical Gillian.Symbolic.Pure_context.t *)
type t [@@deriving yojson]

(** [init ()] returns a fresh empty collection of pure formulae *)
val init : unit -> t

(** [equal pfs_a pfs_b] returns true iff pfs_a = pfs_b *)
val equal : t -> t -> bool

(** [to_list pfs] serialises the pure formulae [pfs] into a list *)
val to_list : t -> Expr.t list

(** [of_list fs] deserialises a list of formulae [fs] into pure formulae *)
val of_list : Expr.t list -> t

val to_set : t -> Expr.Set.t

(** [mem pfs f] return true iff the formula [f] is part of the pure formulae [pfs] *)
val mem : t -> Expr.t -> bool

(** [extend pfs f] extends the pure formulae [pfs] with the formula [f] *)
val extend : t -> Expr.t -> unit

(*
(** [nth_get pfs n] returns the n-th pure formula of [pfs] *)
val nth_get : t -> int -> Formula.t

(** [nth_set pfs n f] sets the n-th pure formula of [pfs] to equal [f] *)
val nth_set : t -> int -> Formula.t -> unit

(** [nth_delete pfs n f] deletes the n-th pure formula of [pfs] *)
val nth_delete : t -> int -> unit *)

(** [clear pfs] empties the pure formulae pfs *)
val clear : t -> unit

(** [length pfs] returns the length of the pure formulae [pfs] *)
val length : t -> int

(** [copy pfs] Returns a copy of the pure formulae [pfs] *)
val copy : t -> t

(** [merge_into_left pfsl pfsr] merges the pure formulae [pfsr] with the pure formulae [pfsl] *)
val merge_into_left : t -> t -> unit

(** [set pfs fs] sets the pure formulae [pfs] to [fs] *)
val set : t -> Expr.t list -> unit

(** [iter f pfs] iterates over the pure formulae [pfs] using the function [f] *)
val iter : (Expr.t -> unit) -> t -> unit

(** [fold_left f ac pfs] folds over the pure formulae [pfs] using the function [f] and initial accumulator [ac] *)
val fold_left : ('a -> Expr.t -> 'a) -> 'a -> t -> 'a

(** [map_inplace f pfs] is like a map operation, but performing in place *)
val map_inplace : (Expr.t -> Expr.t) -> t -> unit

(** [substitution subst pfs] substitutes the substutition subst in the pure formulae [pfs] in-place *)
val substitution : SVal.SESubst.t -> t -> unit

(** [subst_expr_for_expr e_to_subst e_subst pfs] substitutes the expression [e_to_subst] with the expression [e_subst] in the pure formulae [pfs] in-place *)
val subst_expr_for_expr : Expr.t -> Expr.t -> t -> unit

(** [lvars pfs] returns the set containing all the lvars occurring in [pfs] *)
val lvars : t -> Containers.SS.t

(** Returns the set containing all the alocs occurring in --pfs-- *)
val alocs : t -> Containers.SS.t
(** [alocs pfs] returns the set containing all the abstract locations occurring in [pfs] *)

(** [clocs pfs] returns the set containing all the concrete locations occurring in [pfs] *)
val clocs : t -> Containers.SS.t

(** [pp fmt pfs] prints the pure formulae [pfs] *)
val pp : Format.formatter -> t -> unit

(** [pp pvars lvars locs fmt pfs] prints the pure formulae [pfs] relevnt to [pvars], [lvars] and [locs] *)
val pp_by_need :
  Containers.SS.t * Containers.SS.t * Containers.SS.t ->
  Format.formatter ->
  t ->
  unit

(** [filter_with_info pvars lvars locs pfs] returns only the pfs relevant to [pvars], [lvars], and [locs]*)
val filter_with_info :
  Containers.SS.t * Containers.SS.t * Containers.SS.t -> t -> t

(** [sort pfs] sorts the pure formulae [pfs] *)
val sort : t -> unit

val remove_duplicates : t -> unit
val clean_up : t -> unit

val get_relevant_info :
  Containers.SS.t ->
  Containers.SS.t ->
  Containers.SS.t ->
  t ->
  Containers.SS.t * Containers.SS.t * Containers.SS.t

val filter_map_stop :
  (Expr.t -> [ `Stop | `Filter | `Replace of Expr.t ]) -> t -> bool

(** See Gillian.Utils.Ext_list.filter_stop_cond *)
val filter_stop_cond :
  keep:(Expr.t -> bool) -> cond:(Expr.t -> bool) -> t -> bool

val filter : (Expr.t -> bool) -> t -> unit
val filter_map : (Expr.t -> Expr.t option) -> t -> unit
val exists : (Expr.t -> bool) -> t -> bool

(** Gets the nths formula. There are very few good use cases for this function, and uses should generaly use iterators instead.
    O(n) *)
val get_nth : int -> t -> Expr.t option
