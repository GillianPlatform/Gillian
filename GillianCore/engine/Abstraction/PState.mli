(** Interface for GIL General States. They are considered to be mutable. *)
module type S = sig
  type state_t
  type abs_t = string * SVal.M.t list

  module SMatcher : Matcher.S with type state_t = state_t

  type t = SMatcher.t = {
    state : state_t;
    preds : Preds.t;
    wands : Wands.t;
    pred_defs : MP.preds_tbl_t;
  }

  include SState.S with type t := t

  val make_p :
    preds:MP.preds_tbl_t ->
    init_data:init_data ->
    store:store_t ->
    pfs:PFS.t ->
    gamma:Type_env.t ->
    spec_vars:SS.t ->
    unit ->
    t

  val init_with_pred_table : MP.preds_tbl_t -> init_data -> t

  (** Get preds of given symbolic state *)
  val get_preds : t -> Preds.t

  (** Set preds of given symbolic state *)
  val set_preds : t -> Preds.t -> t

  (** Set wands of given symbolic state *)
  val set_wands : t -> Wands.t -> t

  val matches : t -> st -> MP.t -> Matcher.match_kind -> bool
  val add_pred_defs : MP.preds_tbl_t -> t -> t
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
