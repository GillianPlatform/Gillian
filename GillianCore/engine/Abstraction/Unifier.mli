type unify_kind =
  | Postcondition
  | Fold
  | FunctionCall
  | Invariant
  | LogicCommand
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
  type up_u_res = UPUSucc of (t * st * post_res) list | UPUFail of err_t list

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

  type gp_ret = GPSucc of (t * vt list) list | GPFail of err_t list
  type u_res = UWTF | USucc of t | UFail of err_t list
  type unfold_info_t = (string * string) list

  val produce_assertion : t -> st -> Asrt.t -> (t list, string) result
  val produce : t -> st -> Asrt.t -> (t list, string) result
  val produce_posts : t -> st -> Asrt.t list -> t list
  val unfold : t -> string -> vt list -> unfold_info_t option -> (st * t) list
  val rec_unfold : ?fuel:int -> t -> string -> vt list -> t list
  val unfold_all : t -> string -> t list
  val unfold_with_vals : t -> vt list -> (st * t) list * bool
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

  val get_pred :
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
