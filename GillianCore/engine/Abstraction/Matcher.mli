type match_kind = Matching_walker.match_kind =
  | Postcondition of string
  | Fold of string
  | FunctionCall of string
  | Invariant
  | LogicCommand
  | PredicateGuard
[@@deriving yojson]

type recovery_tactic = Matching_walker.recovery_tactic =
  | Try_fold of string * Expr.t list
  | Try_unfold of string * Expr.t list
[@@deriving yojson]

(** What the matching engine requires of a state. This is a strict subset of
    {!SState.S}, spelled out so that this module does not depend on the [SState]
    compilation unit — which lets [SState] itself instantiate {!Make}. *)
module type MatchableState = sig
  type t [@@deriving yojson]
  type m_err_t
  type err_t = (m_err_t, Expr.t) StateErr.t [@@deriving yojson, show]

  val pp : Format.formatter -> t -> unit

  val pp_by_need :
    Containers.SS.t ->
    Containers.SS.t ->
    Containers.SS.t ->
    Format.formatter ->
    t ->
    unit

  val pp_err : Format.formatter -> err_t -> unit
  val copy : t -> t
  val get_store : t -> SStore.t
  val set_store : t -> SStore.t -> t

  val simplify :
    ?save:bool ->
    ?kill_new_lvars:bool ->
    ?matching:bool ->
    t ->
    SVal.SESubst.t * t list

  val simplify_val : t -> Expr.t -> Expr.t

  val assume_a :
    ?matching:bool ->
    ?production:bool ->
    ?time:string ->
    t ->
    Expr.t list ->
    t option

  val assume_t : t -> Expr.t -> Type.t -> t option
  val assert_a : t -> Expr.t list -> bool
  val get_type : t -> Expr.t -> Type.t option
  val unfolding_vals : t -> Expr.t list -> Expr.t list
  val can_fix : err_t -> bool
  val get_recovery_tactic : t -> err_t list -> Expr.t Recovery_tactic.t

  val execute_action :
    string -> t -> Expr.t list -> (t * Expr.t list, err_t) Res_list.t

  val consume_core_pred :
    string -> t -> Expr.t list -> (t * Expr.t list, err_t) Res_list.t

  val produce_core_pred : string -> t -> Expr.t list -> t list
end

module type S = sig
  type err_t
  type state_t
  type t = state_t
  type post_res = (Flag.t * Asrt.t list) option
  type search_state = (t * SVal.SESubst.t * MP.t) list * err_t list

  module Logging : sig
    module AstateRec : sig
      type t = { state : state_t } [@@deriving yojson]
    end

    module AssertionReport : sig
      type t = { step : MP.step; subst : SVal.SESubst.t; astate : AstateRec.t }
      [@@deriving yojson]
    end

    module MatchReport : sig
      type t = {
        astate : AstateRec.t;
        subst : SVal.SESubst.t;
        mp : MP.t;
        match_kind : match_kind;
      }
      [@@deriving yojson]
    end

    module MatchRecoveryReport : sig
      type t = {
        astate : AstateRec.t;
        tactic : recovery_tactic;
        num_results : int;
      }
      [@@deriving yojson]
    end

    module MatchResultReport : sig
      type remaining_state = {
        astate : AstateRec.t;
        subst : SVal.SESubst.t;
        mp : MP.t;
      }
      [@@deriving yojson]

      type t =
        | Success of {
            astate : AstateRec.t;
            subst : SVal.SESubst.t;
            posts : (Flag.t * Asrt.t list) option;
            remaining_states : remaining_state list;
          }
        | Failure of {
            cur_step : MP.step option;
            subst : SVal.SESubst.t;
            astate : AstateRec.t;
            errors : err_t list;
          }
      [@@deriving yojson]
    end
  end

  val produce_assertion :
    t -> SVal.SESubst.t -> Asrt.atom -> (t, err_t) Res_list.t

  val produce : t -> SVal.SESubst.t -> Asrt.t -> (t, err_t) Res_list.t
  val produce_posts : t -> SVal.SESubst.t -> Asrt.t list -> t list

  (** Tries recovering from an error using the provided recovery tactic, by
      delegating to the predicate-carrying memory (through the reserved recover
      action) and simplifying the recovered states. *)
  val try_recovering :
    t -> Expr.t Recovery_tactic.t -> (t list * recovery_tactic, string) result

  val match_assertion :
    ?no_auto_fold:bool ->
    t ->
    SVal.SESubst.t ->
    MP.step ->
    (t, err_t) Res_list.t

  val match_ :
    ?in_matching:bool ->
    t ->
    SVal.SESubst.t ->
    MP.t ->
    match_kind ->
    (t * SVal.SESubst.t * post_res, err_t) Res_list.t
end

module Make (State : MatchableState) :
  S with type state_t = State.t and type err_t = State.err_t
