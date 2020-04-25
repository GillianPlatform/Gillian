(** Type of Gillian pure formulae *)
type t

val init : unit -> t
(** [init ()] returns a fresh empty collection of pure formulae *)

val equal : t -> t -> bool
(** [equal pfs_a pfs_b] returns true iff pfs_a = pfs_b *)

val to_list : t -> Formula.t list
(** [to_list pfs] serialises the pure formulae [pfs] into a list *)

val of_list : Formula.t list -> t
(** [of_list fs] deserialises a list of formulae [fs] into pure formulae *)

val mem : t -> Formula.t -> bool
(** [mem pfs f] return true iff the formula [f] is part of the pure formulae [pfs] *)

val extend : t -> Formula.t -> unit
(** [extend pfs f] extends the pure formulae [pfs] with the formula [f] *)

val nth_get : t -> int -> Formula.t
(** [nth_get pfs n] returns the n-th pure formula of [pfs] *)

val nth_set : t -> int -> Formula.t -> unit
(** [nth_set pfs n f] sets the n-th pure formula of [pfs] to equal [f] *)

val nth_delete : t -> int -> unit
(** [nth_delete pfs n f] deletes the n-th pure formula of [pfs] *)

val clear : t -> unit
(** [clear pfs] empties the pure formulae pfs *)

val length : t -> int
(** [length pfs] returns the length of the pure formulae [pfs] *)

val copy : t -> t
(** [copy pfs] Returns a copy of the pure formulae [pfs] *)

val merge_into_left : t -> t -> unit
(** [merge_into_left pfsl pfsr] merges the pure formulae [pfsr] with the pure formulae [pfsl] *)

val set : t -> Formula.t list -> unit
(** [set pfs fs] sets the pure formulae [pfs] to [fs] *)

val iter : (Formula.t -> unit) -> t -> unit
(** [iter f pfs] iterates over the pure formulae [pfs] using the function [f] *)

val iteri : (int -> Formula.t -> unit) -> t -> unit
(** [iteri f pfs] iterates over the pure formulae [pfs] using the function [f], keeping track of the index *)

val fold_left : ('a -> Formula.t -> 'a) -> 'a -> t -> 'a
(** [fold_left f ac pfs] folds over the pure formulae [pfs] using the function [f] and initial accumulator [ac] *)

val substitution : SVal.SSubst.t -> t -> unit
(** [substitution subst pfs] substitutes the substutition subst in the pure formulae [pfs] in-place *)

val subst_expr_for_expr : Expr.t -> Expr.t -> t -> unit
(** [subst_expr_for_expr e_to_subst e_subst pfs] substitutes the expression [e_to_subst] with the expression [e_subst] in the pure formulae [pfs] in-place *)

val lvars : t -> Containers.SS.t
(** [lvars pfs] returns the set containing all the lvars occurring in [pfs] *)

(** Returns the set containing all the alocs occurring in --pfs-- *)
val alocs : t -> Containers.SS.t
(** [alocs pfs] returns the set containing all the abstract locations occurring in [pfs] *)

val clocs : t -> Containers.SS.t
(** [clocs pfs] returns the set containing all the concrete locations occurring in [pfs] *)

val count_lvar : t -> Var.t -> int
(** [count_lvar pfs x] returns the number of formulae in which the variable x occurs *)

val pp : Format.formatter -> t -> unit
(** [pp fmt pfs] prints the pure formulae [pfs] *)

val sort : t -> unit
(** [sort pfs] sorts the pure formulae [pfs] *)
