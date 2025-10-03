val internal_imports : string list

module Prefix : sig
  (** This module contains the prefix for different kind of strings that are
      generated during compilation *)

  (** Prefix for generated variables *)
  val gvar : string

  (** Prefix for generated symbolic variables *)
  val sgvar : string

  (** Prefix for loop invariant labels *)
  val loopinv_lab : string

  (** Prefix for loop begining labels *)
  val loop_lab : string

  (** Prefix for continue labels *)
  val ctn_lab : string

  (** Prefix for fail labels *)
  val fail_lab : string

  (** Prefix for loop body labels *)
  val lbody_lab : string

  (** Prefix for end of loop body labels *)
  val end_lab : string

  (** Prefix for end of if/else bloc labels *)
  val endif_lab : string

  (** Prefix for then labels *)
  val then_lab : string

  (** Prefix for else labels *)
  val else_lab : string
end

module InternalProcs : sig
  (** This module contains the name of the internal procedures of WISL
      implemented in GIL They take into account pointer arithmetics. For
      example, the function that has the name [internal_add] adds either to
      numbers, or a pointer and a number. *)

  (** add(a,b) computes a + b (taking pointer arithmetics into account) *)
  val internal_add : string

  (** minus(a,b) computes a - b *)
  val internal_minus : string

  (** gt(a,b) computes a > b *)
  val internal_gt : string

  (** lt(a,b) computes a < b *)
  val internal_lt : string

  (** leq(a, b) computes a <= b *)
  val internal_leq : string

  (** geq(a, b) computes a >= b *)
  val internal_geq : string
end

module InternalPreds : sig
  (** This module contains the name of internal predicates for WISL implemented
      in GIL. They take pointer arithmetics into account. For example,
      [internal_pred_add(x, y, z)] is true if [internal_add(x, y)] would return
      [z]. *)

  (** cell predicate. [cell(ptr, value)] in gil means [ptr -> value] in wisl *)
  val internal_pred_cell : string

  (** [internal_pred_add(x, y, z)] is true if executing [internal_add(x, y)]
      would return [z]. *)
  val internal_pred_add : string

  (** [internal_pred_minus(x, y, z)] is true if executing [internal_minus(x, y)]
      would return [z]. *)
  val internal_pred_minus : string

  (** [internal_pred_lt(x, y, z)] is true if executing [internal_lt(x, y)] would
      return [z]. *)
  val internal_pred_lt : string

  (** [internal_pred_gt(x, y, z)] is true if executing [internal_gt(x, y)] would
      return [z]. *)
  val internal_pred_gt : string

  (** [internal_pred_leq(x, y, z)] is true if executing [internal_leq(x, y)]
      would return [z]. *)
  val internal_pred_leq : string

  (** [internal_pred_geq(x, y, z)] is true if executing [internal_geq(x, y)]
      would return [z]. *)
  val internal_pred_geq : string
end
