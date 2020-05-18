(** Type of Gillian pure formulae *)
type t

(** [init ()] returns a fresh empty collection of pure formulae *)
val init : unit -> t

(** [equal pfs_a pfs_b] returns true iff pfs_a = pfs_b *)
val equal : t -> t -> bool

(** [to_list pfs] serialises the pure formulae [pfs] into a list *)
val to_list : t -> Formula.t list

(** [of_list fs] deserialises a list of formulae [fs] into pure formulae *)
val of_list : Formula.t list -> t

(** [mem pfs f] return true iff the formula [f] is part of the pure formulae [pfs] *)
val mem : t -> Formula.t -> bool

(** [extend pfs f] extends the pure formulae [pfs] with the formula [f] *)
val extend : t -> Formula.t -> unit

(** [nth_get pfs n] returns the n-th pure formula of [pfs] *)
val nth_get : t -> int -> Formula.t

(** [nth_set pfs n f] sets the n-th pure formula of [pfs] to equal [f] *)
val nth_set : t -> int -> Formula.t -> unit

(** [nth_delete pfs n f] deletes the n-th pure formula of [pfs] *)
val nth_delete : t -> int -> unit

(** [clear pfs] empties the pure formulae pfs *)
val clear : t -> unit

(** [length pfs] returns the length of the pure formulae [pfs] *)
val length : t -> int

(** [copy pfs] Returns a copy of the pure formulae [pfs] *)
val copy : t -> t

(** [merge_into_left pfsl pfsr] merges the pure formulae [pfsr] with the pure formulae [pfsl] *)
val merge_into_left : t -> t -> unit

(** [set pfs fs] sets the pure formulae [pfs] to [fs] *)
val set : t -> Formula.t list -> unit

(** [iter f pfs] iterates over the pure formulae [pfs] using the function [f] *)
val iter : (Formula.t -> unit) -> t -> unit

(** [iteri f pfs] iterates over the pure formulae [pfs] using the function [f], keeping track of the index *)
val iteri : (int -> Formula.t -> unit) -> t -> unit

(** [fold_left f ac pfs] folds over the pure formulae [pfs] using the function [f] and initial accumulator [ac] *)
val fold_left : ('a -> Formula.t -> 'a) -> 'a -> t -> 'a

(** [substitution subst pfs] substitutes the substutition subst in the pure formulae [pfs] in-place *)
val substitution : SVal.SSubst.t -> t -> unit

(** [subst_expr_for_expr e_to_subst e_subst pfs] substitutes the expression [e_to_subst] with the expression [e_subst] in the pure formulae [pfs] in-place *)
val subst_expr_for_expr : Expr.t -> Expr.t -> t -> unit

(** [lvars pfs] returns the set containing all the lvars occurring in [pfs] *)
val lvars : t -> Containers.SS.t

(** Returns the set containing all the alocs occurring in --pfs-- *)
val alocs : t -> Containers.SS.t
(** [alocs pfs] returns the set containing all the abstract locations occurring in [pfs] *)

(** [clocs pfs] returns the set containing all the concrete locations occurring in [pfs] *)
val clocs : t -> Containers.SS.t

(** [count_lvar pfs x] returns the number of formulae in which the variable x occurs *)
val count_lvar : t -> Var.t -> int

(** [pp fmt pfs] prints the pure formulae [pfs] *)
val pp : Format.formatter -> t -> unit

(** [sort pfs] sorts the pure formulae [pfs] *)
val sort : t -> unit
