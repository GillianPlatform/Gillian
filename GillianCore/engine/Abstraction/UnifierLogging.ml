module Types = struct
  type unify_kind = Postcondition | Fold | FunctionCall | Invariant | LogicCommand [@@deriving yojson]
end

open Types

module type S = sig
  type vt
  type st
  type err_t
  type state_t
  type preds_t
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type astate_t = state_t * preds_t * UP.preds_tbl_t * variants_t
  type astate_t' = state_t * preds_t * unit * variants_t
  type 'a fmt = Format.formatter -> 'a -> unit
  
  module AstateRec : sig
    type t = {
      state : state_t;
      preds : preds_t;
      variants : variants_t;
    } [@@deriving yojson]

    val from : astate_t -> t
  end

  module AssertionReport : sig
    type t = {
      step : UP.step;
      subst : st;
      astate : AstateRec.t;
    } [@@deriving yojson]

    val to_loggable : st fmt
      -> astate_t' fmt
      -> t -> Logging.Loggable.t
  end

  module UnifyReport : sig
    type t = {
      astate : AstateRec.t;
      subst : st;
      up : UP.t;
      unify_kind : unify_kind;
    } [@@deriving yojson]

    val to_loggable : string -> t -> Logging.Loggable.t
  end

  module UnifyCaseReport : sig
    type t = {
      astate : AstateRec.t;
      subst : st;
      up : UP.t;
    } [@@deriving yojson]

    val log : t -> Logging.ReportId.t option
  end

  module UnifyResultReport : sig
    type report_state = UnifyCaseReport.t

    type t =
      Success of {
        astate : AstateRec.t;
        subst : st;
        posts : (Flag.t * Asrt.t list) option;
        remaining_states : report_state list;
      } 
      | Failure of {
        cur_step : UP.step option;
        subst : st;
        astate : AstateRec.t;
        errors : err_t list;
      }
    [@@deriving yojson]

    val to_loggable : astate_t' fmt -> t -> Logging.Loggable.t
  end

  val structure_unify_case_reports : Logging.ReportId.t list ref
    -> int
    -> bool
    -> astate_t
    -> st
    -> UP.t
    -> int
end

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : SState.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (Preds : Preds.S with type vt = Val.t and type st = ESubst.t):
  S
    with type vt = Val.t
     and type st = ESubst.t
     and type state_t = State.t
     and type preds_t = Preds.t
     and type err_t = State.err_t = struct
  module L = Logging

  type vt = Val.t
  type st = ESubst.t [@@deriving yojson]
  type state_t = State.t [@@deriving yojson]
  type preds_t = Preds.t [@@deriving yojson]
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type err_t = State.err_t [@@deriving yojson]
  type astate_t = state_t * preds_t * UP.preds_tbl_t * variants_t
  type astate_t' = state_t * preds_t * unit * variants_t
  type 'a fmt = Format.formatter -> 'a -> unit

  module AstateRec = struct
    type t = {
      state : state_t;
      preds : preds_t;
      variants : variants_t;
    } [@@deriving yojson]

    let from (state, preds, _, variants) = { state; preds; variants }

    let pp pp_astate fmt astate =
      let { state; preds; variants } = astate in
        pp_astate fmt (state, preds, (), variants)
  end

  module AssertionReport = struct
    type t = {
      step : UP.step;
      subst : st;
      astate : AstateRec.t;
    } [@@deriving yojson]

    let pp subst_pp pp_astate fmt u_assert_log =
      Fmt.pf fmt "Unify assertion: @[<h>%a@]@\nSubst:@\n%a@\n@[<v 2>STATE:@\n%a@]"
        UP.step_pp u_assert_log.step subst_pp u_assert_log.subst (AstateRec.pp pp_astate) u_assert_log.astate

    let to_loggable subst_pp pp_astate = L.Loggable.make (pp subst_pp pp_astate) of_yojson to_yojson
  end

  module UnifyReport = struct
    type t = {
      astate : AstateRec.t;
      subst : st;
      up : UP.t;
      unify_kind : unify_kind;
    } [@@deriving yojson]

    let to_loggable str = L.Loggable.make (fun fmt _ -> Fmt.pf fmt "%s" str) of_yojson to_yojson
  end

  module UnifyCaseReport = struct
    type t = {
      astate : AstateRec.t;
      subst : st;
      up : UP.t;
    } [@@deriving yojson]

    let log report = L.normal_specific
      (L.Loggable.make L.dummy_pp of_yojson to_yojson report)
      L.LoggingConstants.ContentType.unify_case
  end

  module UnifyResultReport = struct
    type report_state = UnifyCaseReport.t [@@deriving yojson]

    type t =
      Success of {
        astate : AstateRec.t;
        subst : st;
        posts : (Flag.t * Asrt.t list) option;
        remaining_states : report_state list;
      } 
      | Failure of {
        cur_step : UP.step option;
        subst : st;
        astate : AstateRec.t;
        errors : err_t list;
      }
    [@@deriving yojson]

    let pp pp_astate fmt report =
      match report with
      | Success data ->
        Fmt.pf fmt "Unifier.unify_up: Unification successful: %d states left"
          (1 + (List.length data.remaining_states))
      | Failure { cur_step; subst; astate; errors } ->
        Fmt.pf fmt 
          "@[<v 2>WARNING: Unify Assertion Failed: @[<h>%a@] with \
          subst @\n\
          %a in state @\n\
          %a with errors:@\n\
          %a@]"
          Fmt.(
            option
              ~none:(any "no assertion - phantom node")
              UP.step_pp)
          cur_step ESubst.pp subst (AstateRec.pp pp_astate) astate
          Fmt.(list ~sep:(any "@\n") State.pp_err)
          errors
      
    let to_loggable pp_astate = L.Loggable.make (pp pp_astate) of_yojson to_yojson
  end

  let structure_unify_case_reports parent_ids_ref target_case_depth is_new_case astate subst up =
    let target_case_depth = (if is_new_case then target_case_depth - 1 else target_case_depth) in 
    let case_depth = List.length !parent_ids_ref in (
      assert (target_case_depth <= case_depth);
      for _ = case_depth downto (target_case_depth + 1) do
        match !parent_ids_ref with
        | [] -> raise (Failure "Mismatched case depth and parent_id list!")
        | (parent_id :: rest) -> (
          L.release_parent (Some parent_id);
          parent_ids_ref := rest
        )
      done;
      if is_new_case then (
        let new_parent_id = UnifyCaseReport.log
          {
            astate = AstateRec.from astate;
            subst; up
          }
        in match new_parent_id with
        | Some new_parent_id -> (
          L.set_parent new_parent_id;
          parent_ids_ref := (new_parent_id :: !parent_ids_ref);
          target_case_depth
        )
        | None -> target_case_depth
      ) else target_case_depth
    );
end