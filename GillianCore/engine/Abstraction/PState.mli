(**
    Interface for GIL General States.
    They are considered to be mutable.
*)
module type S = sig
  include SState.S

  type state_t
  type abs_t = string * vt list

  val expose : t -> state_t * Preds.t * Wands.t * UP.preds_tbl_t * variants_t

  val make_p :
    preds:UP.preds_tbl_t ->
    init_data:init_data ->
    store:store_t ->
    pfs:PFS.t ->
    gamma:Type_env.t ->
    spec_vars:SS.t ->
    unit ->
    t

  val init_with_pred_table : UP.preds_tbl_t -> init_data -> t

  (** Get preds of given symbolic state *)
  val get_preds : t -> Preds.t

  (** Set preds of given symbolic state *)
  val set_preds : t -> Preds.t -> t

  (** Set variants of given symbolic state *)
  val set_variants : t -> variants_t -> t

  val unifies : t -> st -> UP.t -> Unifier.unify_kind -> bool
  val add_pred_defs : UP.preds_tbl_t -> t -> t
  val get_all_preds : ?keep:bool -> (abs_t -> bool) -> t -> abs_t list
  val set_pred : t -> abs_t -> unit
  val try_recovering : t -> vt Recovery_tactic.t -> (t list, string) result
end

module Make (State : SState.S) :
  S
    with type state_t = State.t
     and type heap_t = State.heap_t
     and type m_err_t = State.m_err_t
     and type init_data = State.init_data
