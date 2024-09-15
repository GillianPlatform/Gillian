type match_kind =
  | Postcondition
  | Fold
  | FunctionCall
  | Invariant
  | LogicCommand
  | PredicateGuard
[@@deriving yojson]

module type S = sig
  type err_t
  type state_t
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]

  type t = {
    state : state_t;
    preds : Preds.t;
    wands : Wands.t;
    pred_defs : MP.preds_tbl_t;
    variants : variants_t;
  }

  type post_res = (Flag.t * Asrt.t list) option
  type search_state = (t * SVal.SESubst.t * MP.t) list * err_t list

  module Logging : sig
    module AstateRec : sig
      type t = {
        state : state_t;
        preds : Preds.t;
        wands : Wands.t;
        variants : variants_t;
      }
      [@@deriving yojson]
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

    module MatchCaseReport : sig
      type t = { astate : AstateRec.t; subst : SVal.SESubst.t; mp : MP.t }
      [@@deriving yojson]
    end

    module MatchResultReport : sig
      type remaining_state = MatchCaseReport.t [@@deriving yojson]

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

  type unfold_info_t = (string * string) list

  val produce_assertion :
    t -> SVal.SESubst.t -> Asrt.simple -> (t, err_t) Res_list.t

  val produce : t -> SVal.SESubst.t -> Asrt.t -> (t, err_t) Res_list.t
  val produce_posts : t -> SVal.SESubst.t -> Asrt.t list -> t list

  (** [unfold state name args unfold_info] returns a
      list of pairs (subst, state), resulting from unfolding
      the predicate [name(..args..)] from the given state.
      unfold_info contains information about how to bind new variables. *)
  val unfold :
    ?additional_bindings:unfold_info_t ->
    t ->
    string ->
    Expr.t list ->
    (SVal.SESubst.t * t, err_t) Res_list.t

  val rec_unfold :
    ?fuel:int -> t -> string -> Expr.t list -> (t, err_t) Res_list.t

  val unfold_all : t -> string -> (t, err_t) Res_list.t

  (** Tries recovering from an error using the provided recovery tactic. *)
  val try_recovering : t -> Expr.t Recovery_tactic.t -> (t list, string) result

  (** Tries to unfold the given predicate in the state.
      If it manages, it returns the new set of states and corresponding
      substitutions, otherwise, it returns None. *)
  val unfold_with_vals :
    auto_level:[ `High | `Low ] ->
    t ->
    Expr.t list ->
    (SVal.SESubst.t * t) list option

  (** Unfolds 1 predicate for which all arguments are concrete.
      - If it finds one:
        - if it succeeds to unfold, it returns Some (Some subst, new_state )
        - if it fails to unfold it returns None
      - If it doesn't find one, it returns Some (None, input_state) *)
  val unfold_concrete_preds : t -> (SVal.SESubst.t option * t) option

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

  (** Folds a predicate in the state, consuming its definition and
      producing the folded predicate.
      If the predicate has a guard, the guard is produced. *)
  val fold :
    ?in_matching:bool ->
    ?additional_bindings:(Expr.t * Expr.t) list ->
    match_kind:match_kind ->
    state:t ->
    MP.pred ->
    Expr.t list ->
    (t, err_t) Res_list.t

  (** Consumes a predicate from the state.
      If the predicate is not "verbatim" in our set of preds,
      and it is not abstract and we are not in manual mode,
      we attempt to fold it. *)
  val consume_pred :
    ?in_matching:bool ->
    ?fold_outs_info:SVal.SESubst.t * MP.step * Expr.t list ->
    ?no_auto_fold:bool ->
    t ->
    string ->
    Expr.t option list ->
    (t * Expr.t list, err_t) Res_list.t

  val package_wand : t -> Wands.wand -> (t, err_t) List_res.t
end

module Make (State : SState.S) :
  S with type state_t = State.t and type err_t = State.err_t
