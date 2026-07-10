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
  S with type state_t = State.t and type err_t = State.err_t = struct
  open Literal
  open Containers
  module L = Logging
  module W = Matching_walker

  type state_t = State.t [@@deriving yojson]
  type err_t = State.err_t [@@deriving yojson, show]
  type t = State.t
  type post_res = (Flag.t * Asrt.t list) option
  type s_state = t * SVal.SESubst.t * MP.t
  type search_state = s_state list * err_t list

  module Logging = struct
    let pp_astate = State.pp

    let pp_astate_by_need (pvars : SS.t) (lvars : SS.t) (locs : SS.t) fmt astate
        =
      State.pp_by_need pvars lvars locs fmt astate

    module AstateRec = struct
      type t' = t
      type t = { state : state_t } [@@deriving yojson]

      let from (state : t') = { state }
      let pp_custom pp_astate fmt { state } = pp_astate fmt state
      let pp = pp_custom pp_astate
    end

    module AssertionReport = struct
      type t = { step : MP.step; subst : SVal.SESubst.t; astate : AstateRec.t }
      [@@deriving yojson]

      let pp_custom pp_astate pp_subst fmt { step; subst; astate } =
        Fmt.pf fmt
          "Match assertion: @[<h>%a@]@\nSubst:@\n%a@\n@[<v 2>STATE:@\n%a@]"
          MP.pp_step step pp_subst subst
          (AstateRec.pp_custom pp_astate)
          astate

      let to_loggable pp_astate pp_subst =
        L.Loggable.make (pp_custom pp_astate pp_subst) of_yojson to_yojson
    end

    module MatchReport = struct
      type t = {
        astate : AstateRec.t;
        subst : SVal.SESubst.t;
        mp : MP.t;
        match_kind : match_kind;
      }
      [@@deriving yojson]

      let pp fmt _ = Fmt.pf fmt "Matcher.match_: about to match MP."
      let to_loggable = L.Loggable.make pp of_yojson to_yojson

      let as_parent report f =
        L.Parent.with_specific
          (Some (to_loggable report))
          L.Logging_constants.Content_type.match_ f
    end

    module MatchRecoveryReport = struct
      type t = {
        astate : AstateRec.t;
        tactic : recovery_tactic;
        num_results : int;
      }
      [@@deriving yojson]

      let pp fmt { tactic; num_results; _ } =
        match tactic with
        | Try_unfold _ ->
            Fmt.pf fmt "Unfolding successful: %d results" num_results
        | Try_fold _ -> Fmt.pf fmt "Folding successful"

      let to_loggable = L.Loggable.make pp of_yojson to_yojson

      let log report =
        L.Specific.normal (to_loggable report)
          L.Logging_constants.Content_type.match_recovery
    end

    module MatchResultReport = struct
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

      let pp fmt report =
        match report with
        | Success data ->
            Fmt.pf fmt "Matcher.match_mp: Matching successful: %d states left"
              (1 + List.length data.remaining_states)
        | Failure { cur_step; subst; astate; errors } ->
            Fmt.pf fmt
              "@[<v 2>WARNING: Match Assertion Failed: @[<h>%a@] with subst @\n\
               %a in state @\n\
               %a with errors:@\n\
               %a@]"
              Fmt.(option ~none:(any "no assertion - phantom node") MP.pp_step)
              cur_step SVal.SESubst.pp subst AstateRec.pp astate
              Fmt.(list ~sep:(any "@\n") State.pp_err)
              errors

      let to_loggable = L.Loggable.make pp of_yojson to_yojson

      let log report =
        L.Specific.normal (to_loggable report)
          L.Logging_constants.Content_type.match_result
    end
  end

  open Logging

  let update_store (astate : t) (x : string) (v : Expr.t) : t =
    let store = State.get_store astate in
    let () = SStore.put store x v in
    State.set_store astate store

  let simplify_astate ?(save = false) ?(matching = false) (astate : t) :
      SVal.SESubst.t * t list =
    State.simplify ~save ~kill_new_lvars:false ~matching astate

  let copy_astate (astate : t) : t = State.copy astate

  let log_hooks : (t, err_t) W.log_hooks =
    {
      with_assertion_parent =
        (fun astate subst step f ->
          let open Syntaxes.Option in
          let assertion_loggable =
            let+ () = if L.Mode.enabled () then Some () else None in
            let a = fst step in
            (* Get pvars, lvars, locs from the assertion *)
            let a_pvars, a_lvars, a_locs =
              (Asrt.pvars [ a ], Asrt.lvars [ a ], Asrt.locs [ a ])
            in
            let filter_vars = SS.union a_pvars (SS.union a_lvars a_locs) in

            (* From the subst, we take any pair that has any of those and
               collect the pvars, lvars, and alocs, from their values *)
            let s_pvars, s_lvars, s_locs =
              SVal.SESubst.fold subst
                (fun e v (s_pvars, s_lvars, s_locs) ->
                  let pvars, lvars, locs =
                    (Expr.pvars e, Expr.lvars e, Expr.locs e)
                  in
                  if
                    Containers.SS.inter
                      (List.fold_left SS.union SS.empty [ pvars; lvars; locs ])
                      filter_vars
                    <> SS.empty
                  then
                    ( SS.union s_pvars (Expr.pvars v),
                      SS.union s_lvars (Expr.lvars v),
                      SS.union s_locs (Expr.locs v) )
                  else (s_pvars, s_lvars, s_locs))
                (SS.empty, SS.empty, SS.empty)
            in

            let subst_pp =
              match !Config.pbn with
              | false -> SVal.SESubst.pp
              | true ->
                  SVal.SESubst.pp_by_need
                    (SS.union a_pvars (SS.union a_lvars a_locs))
            in

            let pp_str_list = Fmt.(brackets (list ~sep:comma string)) in

            L.verbose (fun fmt ->
                fmt "Substs:\n%a\n%a\n%a" pp_str_list (SS.elements s_pvars)
                  pp_str_list (SS.elements s_lvars) pp_str_list
                  (SS.elements s_locs));

            let pp_astate =
              match !Config.pbn with
              | false -> pp_astate
              | true -> pp_astate_by_need s_pvars s_lvars s_locs
            in

            AssertionReport.to_loggable pp_astate subst_pp
              { step; subst; astate = AstateRec.from astate }
          in
          L.Parent.with_specific assertion_loggable
            L.Logging_constants.Content_type.assertion f);
      with_match_parent =
        (fun astate subst mp match_kind f ->
          MatchReport.as_parent
            { astate = AstateRec.from astate; subst; mp; match_kind }
            (fun _ -> f ()));
      log_success =
        (fun astate subst posts rest ->
          let remaining_states =
            List.map
              (fun (astate, subst, mp) ->
                MatchResultReport.{ astate = AstateRec.from astate; subst; mp })
              rest
          in
          let _ =
            MatchResultReport.log
              (Success
                 {
                   remaining_states;
                   astate = AstateRec.from astate;
                   subst;
                   posts;
                 })
          in
          ());
      log_failure =
        (fun astate subst cur_step errors ->
          let report =
            MatchResultReport.Failure
              { astate = AstateRec.from astate; cur_step; subst; errors }
          in
          let _ = MatchResultReport.log report in
          ());
      log_recovery =
        (fun astate tactic num_results ->
          MatchRecoveryReport.(
            log { astate = AstateRec.from astate; num_results; tactic }));
    }

  (* The state-level instantiation of the matching walker. Predicates and
     wands live in the memory, so this is purely the generic walker plus the
     state-level recovery channel (the reserved recover action). This forms
     one big recursive knot. *)
  let rec state_ops : (t, err_t) W.ops =
    {
      assume_pure =
        (fun ~production ?time astate fs ->
          State.assume_a ~matching:true ~production ?time astate fs);
      assert_pure = (fun astate fs -> State.assert_a astate fs);
      assume_type = (fun astate e t -> State.assume_t astate e t);
      get_type = (fun astate e -> State.get_type astate e);
      simplify_val = (fun astate v -> State.simplify_val astate v);
      consume_core_pred =
        (fun ~no_auto_fold:_ a_id astate vs_ins ->
          State.consume_core_pred a_id astate vs_ins);
      produce_core_pred =
        (fun a_id astate vs ->
          State.produce_core_pred a_id astate vs |> List.map Result.ok);
      copy = copy_astate;
      update_store;
      mk_asrt_err = (fun vs pf -> StateErr.EAsrt (vs, pf));
      mk_other_err = (fun msg -> StateErr.EOther msg);
      can_fix = State.can_fix;
      unfolding_vals = (fun astate fs -> State.unfolding_vals astate fs);
      get_recovery_tactic =
        (fun astate errs -> State.get_recovery_tactic astate errs);
      try_recovering =
        (fun astate ~tried tactic -> try_recovering astate ~tried tactic);
      (* Predicates live in the memory: there is nothing to eagerly unfold at
         the state level (the memory runs its own concrete-unfolding pass). *)
      unfold_concrete_preds = (fun astate -> Some (None, astate));
      pp = pp_astate;
      pp_err = pp_err_t;
      log = log_hooks;
    }

  and produce (astate : t) (subst : SVal.SESubst.t) (a : Asrt.t) :
      (t, err_t) Res_list.t =
    W.produce state_ops astate subst a

  and match_
      ?(in_matching = false)
      (astate : t)
      (subst : SVal.SESubst.t)
      (mp : MP.t)
      (match_kind : match_kind) :
      (t * SVal.SESubst.t * post_res, err_t) Res_list.t =
    W.match_ state_ops ~in_matching astate subst mp match_kind

  and try_recovering_in_memory
      (astate : t)
      ~(tried : (string * Expr.t list) list)
      (tactic : Expr.t Recovery_tactic.t) :
      (t list * (string * Expr.t list) list * recovery_tactic, string) result =
    let enc_opt = function
      | None -> Expr.Lit Nono
      | Some vs -> Expr.EList vs
    in
    let enc_tried =
      Expr.EList
        (List.map
           (fun (n, args) -> Expr.EList [ Expr.string n; Expr.EList args ])
           tried)
    in
    let enc_args =
      [
        enc_opt tactic.try_fold;
        enc_opt tactic.try_unfold;
        Expr.Lit (String "high");
        enc_tried;
      ]
    in
    let results = State.execute_action SLCmd.recover_action astate enc_args in
    let oks =
      List.filter_map
        (function
          | Ok ok -> Some ok
          | Error _ -> None)
        results
    in
    match (oks, results) with
    | [], _ :: _ -> Error "In-memory recovery failed"
    | _ ->
        (* The chosen candidate (if any) comes back in the action's return
           values, so that the retry loop excludes it from later attempts. *)
        let tried =
          match oks with
          | (_, [ Expr.Lit (String n); Expr.EList args ]) :: _ ->
              (n, args) :: tried
          | _ -> tried
        in
        let states = List.map fst oks in
        (* The legacy unfold simplified the resulting states (collapsing the
           equality chains an unfolding introduces); do the same here, at the
           state level. *)
        let states =
          List.concat_map
            (fun astate -> snd (simplify_astate ~matching:true astate))
            states
        in
        Ok
          ( states,
            tried,
            Try_unfold ("<memory>", Option.value ~default:[] tactic.try_unfold)
          )

  and try_recovering
      (astate : t)
      ~(tried : (string * Expr.t list) list)
      (tactic : Expr.t Recovery_tactic.t) :
      (t list * (string * Expr.t list) list * recovery_tactic, string) result =
    if !Config.under_approximation then
      L.fail "Recovery tactics not handled in UX mode";
    try_recovering_in_memory astate ~tried tactic

  (* The public [try_recovering] (used for the interpreter's action-failure
     retries) keeps its historical signature; the exclusion set is only
     threaded by the matching retry loop. *)
  let try_recovering (astate : t) (tactic : Expr.t Recovery_tactic.t) :
      (t list * recovery_tactic, string) result =
    try_recovering astate ~tried:[] tactic
    |> Result.map (fun (states, _, witness) -> (states, witness))

  let produce_assertion (astate : t) (subst : SVal.SESubst.t) (a : Asrt.atom) :
      (t, err_t) Res_list.t =
    W.produce_assertion state_ops astate subst a

  let produce_posts (state : t) (subst : SVal.SESubst.t) (asrts : Asrt.t list) :
      t list =
    W.produce_posts state_ops state subst asrts

  let match_assertion
      ?(no_auto_fold = false)
      (astate : t)
      (subst : SVal.SESubst.t)
      (step : MP.step) : (t, err_t) Res_list.t =
    W.match_assertion state_ops ~no_auto_fold astate subst step
end
