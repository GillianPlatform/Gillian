type unify_kind =
  | Postcondition
  | Fold
  | FunctionCall
  | Invariant
  | LogicCommand
  | PredicateGuard
[@@deriving yojson]

module type S = sig
  module Val : Val.S
  module ESubst : ESubst.S with type vt = Val.t and type t = Val.et

  type vt
  type st
  type err_t
  type state_t
  type preds_t
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type t = state_t * preds_t * UP.preds_tbl_t * variants_t
  type post_res = (Flag.t * Asrt.t list) option
  type search_state = (t * st * UP.t) list * err_t list
  type up_u_res = ((t * st * post_res) list, err_t list) result

  module Logging : sig
    module AstateRec : sig
      type t = { state : state_t; preds : preds_t; variants : variants_t }
      [@@deriving yojson]
    end

    module AssertionReport : sig
      type t = { step : UP.step; subst : ESubst.t; astate : AstateRec.t }
      [@@deriving yojson]
    end

    module UnifyReport : sig
      type t = {
        astate : AstateRec.t;
        subst : ESubst.t;
        up : UP.t;
        unify_kind : unify_kind;
      }
      [@@deriving yojson]
    end

    module UnifyCaseReport : sig
      type t = { astate : AstateRec.t; subst : st; up : UP.t }
      [@@deriving yojson]
    end

    module UnifyResultReport : sig
      type remaining_state = UnifyCaseReport.t [@@deriving yojson]

      type t =
        | Success of {
            astate : AstateRec.t;
            subst : st;
            posts : (Flag.t * Asrt.t list) option;
            remaining_states : remaining_state list;
          }
        | Failure of {
            cur_step : UP.step option;
            subst : st;
            astate : AstateRec.t;
            errors : err_t list;
          }
      [@@deriving yojson]
    end
  end

  type gp_ret = ((t * vt list) list, err_t list) result
  type u_res = UWTF | USucc of t | UFail of err_t list
  type unfold_info_t = (string * string) list

  val produce_assertion : t -> st -> Asrt.t -> (t list, err_t list) result
  val produce : t -> st -> Asrt.t -> (t list, err_t list) result
  val produce_posts : t -> st -> Asrt.t list -> t list

  (** [unfold state name args unfold_info] returns a 
      list of pairs (subst, state), resulting from unfolding
      the predicate [name(..args..)] from the given state.
      unfold_info contains information about how to bind new variables. *)
  val unfold :
    ?additional_bindings:unfold_info_t ->
    t ->
    string ->
    vt list ->
    (st * t, err_t) List_res.t

  val rec_unfold : ?fuel:int -> t -> string -> vt list -> (t, err_t) List_res.t
  val unfold_all : t -> string -> (t, err_t) List_res.t

  (** Tries recovering from an error using the provided recovery tactic. *)
  val try_recovering : t -> vt Recovery_tactic.t -> (t list, string) result

  (** Tries to unfold the given predicate in the state.
      If it manages, it returns the new set of states and corresponding
      substitutions, otherwise, it returns None. *)
  val unfold_with_vals : t -> vt list -> (st * t) list option

  (** Unfolds 1 predicate for which all arguments are concrete.
      - If it finds one:
        - if it succeeds to unfold, it returns Some (Some subst, new_state )
        - if it fails to unfold it returns None
      - If it doesn't find one, it returns Some (None, input_state) *)
  val unfold_concrete_preds : t -> (st option * t) option

  val unify_assertion :
    ?is_post:bool -> t -> st -> string list option -> UP.step -> u_res

  val unify :
    ?is_post:bool ->
    ?in_unification:bool ->
    t ->
    st ->
    UP.t ->
    unify_kind ->
    up_u_res

  (** Folds a predicate in the state, consuming its definition and
      producing the folded predicate.
      If the predicate has a guard, the guard is produced. *)
  val fold :
    ?is_post:bool ->
    ?in_unification:bool ->
    ?additional_bindings:(Expr.t * vt) list ->
    unify_kind:unify_kind ->
    state:t ->
    UP.pred ->
    vt list ->
    (t, err_t) List_res.t

  (** Consumes a predicate from the state.
      If the predicate is not "verbatim" in our set of preds,
      and it is not abstract and we are not in manual mode,
      we attempt to fold it. *)
  val consume_pred :
    ?is_post:bool ->
    ?in_unification:bool ->
    t ->
    string ->
    vt option list ->
    (st * UP.step * UP.outs * Expr.t list) option ->
    gp_ret
end

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : SState.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (Preds : Preds.S with type vt = Val.t and type st = ESubst.t) :
  S
    with type vt = Val.t
     and type st = ESubst.t
     and type state_t = State.t
     and type preds_t = Preds.t
     and type err_t = State.err_t
