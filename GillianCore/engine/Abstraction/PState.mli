(** Interface for GIL General States. They are considered to be mutable. *)
module type S = sig
  type state_t
  type abs_t = string * SVal.M.t list

  module SMatcher : Matcher.S with type state_t = state_t

  type t = SMatcher.t

  include SState.S with type t := t

  val make_p :
    init_data:init_data ->
    store:store_t ->
    pfs:PFS.t ->
    gamma:Type_env.t ->
    spec_vars:SS.t ->
    unit ->
    t

  val make_p_from_heap :
    store:store_t ->
    heap:heap_t ->
    spec_vars:SS.t ->
    wands:Wands.t ->
    preds:Preds.t ->
    pfs:PFS.t ->
    gamma:Type_env.t ->
    t

  (** Get preds of given symbolic state *)
  val get_preds : t -> Preds.t

  (** Set preds of given symbolic state *)
  val set_preds : t -> Preds.t -> t

  val get_wands : t -> Wands.t

  (** Set wands of given symbolic state *)
  val set_wands : t -> Wands.t -> t

  val matches : t -> st -> MP.t -> Matcher.match_kind -> bool option
  val try_recovering : t -> vt Recovery_tactic.t -> (t list, string) result
end

module Make (State : SState.S) :
  S
    with type state_t = State.t
     and type heap_t = State.heap_t
     and type m_err_t = State.m_err_t
     and type init_data = State.init_data
