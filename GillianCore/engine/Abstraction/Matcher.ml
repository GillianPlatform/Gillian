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

module type S = sig
  type err_t
  type state_t
  type t = state_t Pred_state.t
  type post_res = (Flag.t * Asrt.t list) option
  type search_state = (t * SVal.SESubst.t * MP.t) list * err_t list

  module Logging : sig
    module AstateRec : sig
      type t = { state : state_t; preds : Preds.t; wands : Wands.t }
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

  type unfold_info_t = (string * string) list

  val produce_assertion :
    t -> SVal.SESubst.t -> Asrt.atom -> (t, err_t) Res_list.t

  val produce : t -> SVal.SESubst.t -> Asrt.t -> (t, err_t) Res_list.t
  val produce_posts : t -> SVal.SESubst.t -> Asrt.t list -> t list

  val unfold :
    ?additional_bindings:unfold_info_t ->
    t ->
    string ->
    Expr.t list ->
    (SVal.SESubst.t * t, err_t) Res_list.t

  val rec_unfold :
    ?fuel:int -> t -> string -> Expr.t list -> (t, err_t) Res_list.t

  val unfold_all : t -> string -> (t, err_t) Res_list.t

  val try_recovering :
    t -> Expr.t Recovery_tactic.t -> (t list * recovery_tactic, string) result

  val unfold_with_vals :
    auto_level:[ `High | `Low ] ->
    t ->
    Expr.t list ->
    (SVal.SESubst.t * t) list option

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

  val fold :
    ?in_matching:bool ->
    ?additional_bindings:(Expr.t * Expr.t) list ->
    match_kind:match_kind ->
    state:t ->
    MP.pred ->
    Expr.t list ->
    (t, err_t) Res_list.t

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
  S with type state_t = State.t and type err_t = State.err_t = struct
  open Literal
  open Containers
  module L = Logging
  module W = Matching_walker

  type state_t = State.t [@@deriving yojson]
  type abs_t = string * Expr.t list
  type err_t = State.err_t [@@deriving yojson, show]
  type t = State.t Pred_state.t
  type post_res = (Flag.t * Asrt.t list) option
  type s_state = t * SVal.SESubst.t * MP.t
  type search_state = s_state list * err_t list
  type unfold_info_t = (string * string) list

  module Logging = struct
    let pp_astate = Pred_state.pp State.pp

    let pp_astate_by_need (pvars : SS.t) (lvars : SS.t) (locs : SS.t) fmt astate
        =
      Pred_state.pp State.(pp_by_need pvars lvars locs) fmt astate

    module AstateRec = struct
      type t' = t

      type t = { state : state_t; preds : Preds.t; wands : Wands.t }
      [@@deriving yojson]

      let from ({ state; preds; wands; _ } : t') = { state; preds; wands }

      let pp_custom pp_astate fmt { state; preds; wands } =
        pp_astate fmt Pred_state.{ state; preds; wands }

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

  let clear_resource (astate : t) =
    Pred_state.clear_resource State.clear_resource astate

  let update_store (astate : t) (x : string) (v : Expr.t) : t =
    let store = State.get_store astate.state in
    let () = SStore.put store x v in
    let state' = State.set_store astate.state store in
    { astate with state = state' }

  let simplify_astate ?(save = false) ?(matching = false) (astate : t) :
      SVal.SESubst.t * t list =
    let Pred_state.{ state; preds; wands } = astate in
    let subst, states =
      State.simplify ~save ~kill_new_lvars:false ~matching state
    in
    Preds.substitution_in_place subst preds;
    Wands.substitution_in_place subst wands;
    match states with
    | [] -> (subst, [])
    | [ state ] -> (subst, [ { astate with state } ])
    | states ->
        ( subst,
          List.map
            (fun state ->
              Pred_state.
                { state; preds = Preds.copy preds; wands = Wands.copy wands })
            states )

  let copy_astate (astate : t) : t =
    {
      state = State.copy astate.state;
      preds = Preds.copy astate.preds;
      wands = Wands.copy astate.wands;
    }

  let subst_in_expr_opt (astate : t) (subst : SVal.SESubst.t) (e : Expr.t) :
      Expr.t option =
    let v = SVal.SESubst.subst_in_expr_opt subst e in
    Option.map (State.simplify_val astate.state) v

  let subst_in_expr (subst : SVal.SESubst.t) (le : Expr.t) : Expr.t =
    SVal.SESubst.subst_in_expr subst ~partial:false le

  module Predicate_selection_strategies = struct
    let print_local_info (i : int) (name : string) (args : Expr.t list) : unit =
      L.verbose (fun m ->
          m "Strategy %d: Examining %s(@[<h>%a@])" i name
            Fmt.(list ~sep:comma Expr.pp)
            args)

    let get_pred_def ~pred_defs (name : string) : Pred.t =
      match Hashtbl.find_opt pred_defs name with
      | Some pred -> pred.MP.pred
      | None -> failwith "ERROR: get_pred_with_vs: Predicate doesn't exist."

    (* Strategy 1: The values that we are looking for are in the in-parameters *)
    let strategy_1
        ~pred_defs
        ~state
        ~values
        ((name, args) : string * Expr.t list) : int =
      print_local_info 1 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let one_level_list_expander args =
        List.concat_map
          (fun (x : Expr.t) ->
            match x with
            | EList ls -> ls
            | _ -> [ x ])
          args
      in
      let in_args = one_level_list_expander (Pred.in_args pred_def args) in

      L.verbose (fun fmt ->
          fmt "Original values: %a"
            Fmt.(brackets (list ~sep:comma Expr.pp))
            values);
      let vs = State.get_equal_values state values in
      let vs = vs @ List.concat_map Expr.base_elements vs in
      let vs = List.sort_uniq compare vs in
      L.verbose (fun fmt ->
          fmt "Extended values: %a" Fmt.(brackets (list ~sep:comma Expr.pp)) vs);
      let vs_inter = List_utils.intersect vs in_args in
      let es_inter =
        List.fold_left (fun ac e -> Expr.Set.add e ac) Expr.Set.empty vs_inter
      in
      let es_inter =
        Expr.Set.filter
          (fun e ->
            match e with
            | Lit _ -> false
            | _ -> true)
          es_inter
      in
      L.verbose (fun m ->
          m "get_pred_with_vs. Strategy 1. Intersection of cardinal %i: %a"
            (Expr.Set.cardinal es_inter)
            (Fmt.Dump.list Expr.pp)
            (Expr.Set.elements es_inter));

      Expr.Set.cardinal es_inter

    (* Strategy 2: Predicate has all literals as in-parameters *)
    let strategy_2 ~pred_defs ((name, args) : string * Expr.t list) : int =
      print_local_info 2 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let in_args = Pred.in_args pred_def args in
      let all_literals =
        List.for_all
          (fun (x : Expr.t) ->
            match x with
            | Lit _ -> true
            | _ -> false)
          in_args
      in
      if all_literals then 1 else 0

    (* Strategy 3: The values that we are looking for are in the out-parameters *)
    let strategy_3 ~pred_defs ~values ((name, args) : string * Expr.t list) :
        int =
      print_local_info 3 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let out_args = Pred.out_args pred_def args in
      let vs_inter = List_utils.intersect values out_args in
      let es_inter =
        List.fold_left (fun ac e -> Expr.Set.add e ac) Expr.Set.empty vs_inter
      in
      let es_inter =
        Expr.Set.filter
          (fun e ->
            match e with
            | Lit _ -> false
            | _ -> true)
          es_inter
      in
      L.verbose (fun m ->
          m "get_pred_with_vs. Strategy 3. Intersection: %s"
            (String.concat ", "
               (List.map (Fmt.to_to_string Expr.pp)
                  (Expr.Set.elements es_inter))));
      Expr.Set.cardinal es_inter

    (* Strategy 4: Predicate has non-literal parameters in pure formulae *)
    let strategy_4 ~state ((name, args) : string * Expr.t list) : int =
      print_local_info 4 name args;
      let lvars_state = State.get_spec_vars state in
      let lvars_args =
        List.fold_left SS.union SS.empty (List.map Expr.lvars args)
      in
      let inter = SS.inter lvars_args lvars_state in
      SS.cardinal inter
  end

  let consume_pred_with_vs
      ~(auto_level : [ `Low | `High ])
      (astate : t)
      (values : Expr.t list) : abs_t option =
    let Pred_state.{ state; preds; _ } = astate in
    let pred_defs = MP.get_pred_defs () in

    let wrap_strategy f (name, args) =
      let pred = Predicate_selection_strategies.get_pred_def ~pred_defs name in
      if pred.pred_abstract then 0 else f (name, args)
    in

    let apply_strategies (strategies : (string * Expr.t list -> int) list) :
        (string * Expr.t list) option =
      List.find_map (Preds.strategic_choice ~consume:true preds) strategies
    in
    let open Predicate_selection_strategies in
    let strategies =
      match auto_level with
      | `High ->
          [
            strategy_1 ~state ~values ~pred_defs;
            strategy_2 ~pred_defs;
            strategy_3 ~pred_defs ~values;
            strategy_4 ~state;
          ]
      | `Low -> [ strategy_1 ~state ~values ~pred_defs; strategy_2 ~pred_defs ]
    in
    let strategies = List.map wrap_strategy strategies in
    apply_strategies strategies

  let select_guarded_predicate_to_fold (astate : t) (values : Expr.t list) :
      abs_t option =
    let Pred_state.{ state; preds; _ } = astate in
    let pred_defs = MP.get_pred_defs () in
    let wrap_strat f (name, args) =
      if Option.is_some (Pred.pred_name_from_close_token_name name) then
        f (name, args)
      else 0
    in
    let strategies =
      let open Predicate_selection_strategies in
      List.map wrap_strat
        [
          strategy_1 ~state ~values ~pred_defs;
          strategy_2 ~pred_defs;
          strategy_3 ~pred_defs ~values;
          strategy_4 ~state;
        ]
    in
    let close_token =
      List.find_map (Preds.strategic_choice ~consume:false preds) strategies
    in
    match close_token with
    | None -> None
    | Some (close_token, args) ->
        let actual_pred =
          Option.get (Pred.pred_name_from_close_token_name close_token)
        in
        Some (actual_pred, args)

  (* TODO: why is this not EPure (Expr.false_) ? *)
  let resource_fail = Res_list.error_with (StateErr.EAsrt ([], Expr.false_))

  (** [extend_subts_with_bindings unfold_info pred state subst] takes:
      - A state
      - A substitution
      - A list of pairs of lvar names And extends the substitution with the
        pairs [(#y, eval_expr #x)] for each pair [(x, y)] *)
  let extend_subst_with_bindings
      (state : State.t)
      (subst : SVal.SESubst.t)
      (bindings : (string * string) list) : unit =
    let bindings =
      List.map
        (fun (x, y) -> (Expr.LVar y, State.eval_expr state (Expr.LVar x)))
        bindings
    in
    SVal.SESubst.extend subst bindings;
    L.verbose (fun m ->
        m "@[<v 2>Using unfold info, obtained subst:@\n%a@]@\n" SVal.SESubst.pp
          subst)

  (* The logging hooks of the matching walker: this (state-level)
     instantiation emits the structured reports the verification debugger
     consumes. *)
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

  (* The state-level instantiation of the matching walker, and the
     predicate/wand reasoning that (for now) lives outside the memory: the
     [Pred_state]-based arms are passed to the walker as hooks, and the
     fold/unfold machinery below goes through the walker for assertion-level
     production and matching. This forms one big recursive knot. *)
  let rec state_ops : (t, err_t) W.ops =
    {
      assume_pure =
        (fun ~production ?time astate fs ->
          State.assume_a ~matching:true ~production ?time astate.state fs
          |> Option.map (fun state -> { astate with state }));
      assert_pure = (fun astate fs -> State.assert_a astate.state fs);
      assume_type =
        (fun astate e t ->
          State.assume_t astate.state e t
          |> Option.map (fun state -> { astate with state }));
      get_type = (fun astate e -> State.get_type astate.state e);
      simplify_val = (fun astate v -> State.simplify_val astate.state v);
      consume_core_pred =
        (fun ~no_auto_fold:_ a_id astate vs_ins ->
          let open Res_list.Syntax in
          let** state'', vs_outs =
            State.consume_core_pred a_id astate.state vs_ins
          in
          Res_list.return ({ astate with state = state'' }, vs_outs));
      produce_core_pred =
        (fun a_id astate vs ->
          State.produce_core_pred a_id astate.state vs
          |> List.map (fun state' ->
                 Ok
                   Pred_state.
                     {
                       state = state';
                       preds = Preds.copy astate.preds;
                       wands = Wands.copy astate.wands;
                     }));
      consume_upred_hook = Some consume_upred_arm;
      consume_wand_hook = Some consume_wand_arm;
      produce_upred_hook = Some produce_upred_arm;
      produce_wand_hook = Some produce_wand_arm;
      copy = copy_astate;
      update_store;
      mk_asrt_err = (fun vs pf -> StateErr.EAsrt (vs, pf));
      mk_other_err = (fun msg -> StateErr.EOther msg);
      can_fix = State.can_fix;
      unfolding_vals = (fun astate fs -> State.unfolding_vals astate.state fs);
      get_recovery_tactic =
        (fun astate errs -> State.get_recovery_tactic astate.state errs);
      try_recovering = (fun astate tactic -> try_recovering astate tactic);
      unfold_concrete_preds = (fun astate -> unfold_concrete_preds astate);
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

  (* Legacy produce arm for wand assertions (preds/wands outside the memory). *)
  and produce_wand_arm (astate : t) (subst : SVal.SESubst.t) (a : Asrt.atom) :
      (t, err_t) Res_list.t =
    match a with
    | CorePred (name, _, _) ->
        if !Config.under_approximation then
          L.fail "Wand assertions are not supported in under-approximation mode";
        L.verbose (fun m -> m "Wand assertion.");
        (* Reconstruct the raw wand from its semantic ins/outs (needs the rhs
           predicate's number of in-parameters) before storing it. *)
        let pred_defs = MP.get_pred_defs () in
        let _, rname = Option.get (Asrt.as_wand_name name) in
        let rhs_ins_number =
          (MP.get_pred_def pred_defs rname).pred.ins_number
        in
        let (lname, largs), (rname, rargs) =
          Option.get (Asrt.as_wand ~rhs_ins_number a)
        in
        let largs = List.map (subst_in_expr subst) largs in
        let rargs = List.map (subst_in_expr subst) rargs in
        Wands.extend astate.wands
          Wands.{ lhs = (lname, largs); rhs = (rname, rargs) };
        Res_list.return astate
    | _ -> raise (Failure "Impossible: produce_wand_arm on non-wand")

  (* Legacy produce arm for user-predicate assertions. *)
  and produce_upred_arm (astate : t) (subst : SVal.SESubst.t) (a : Asrt.atom) :
      (t, err_t) Res_list.t =
    match a with
    | CorePred (cp_name, ins, outs) ->
        let open Res_list.Syntax in
        let pred_defs = MP.get_pred_defs () in
        let pname = Option.get (Asrt.as_user_pred_name cp_name) in
        L.verbose (fun fmt -> fmt "Predicate assertion.");
        let les = ins @ outs in
        let vs = List.map (subst_in_expr subst) les in
        let pred_def = Hashtbl.find pred_defs pname in
        let++ ({ state; preds; wands } : t) =
          match pred_def.pred.pred_facts with
          | [] -> Res_list.return astate
          | facts ->
              let params =
                List.map
                  (fun p ->
                    let x, _ = p in
                    Expr.PVar x)
                  pred_def.pred.pred_params
              in
              let facts =
                List.fold_left2
                  (fun facts param le ->
                    let subst =
                      Expr.subst_expr_for_expr ~to_subst:param ~subst_with:le
                    in
                    List.map subst facts)
                  facts params les
              in
              let facts = Asrt.Pure (Expr.conjunct facts) in
              W.produce_assertion state_ops astate subst facts
        in
        let pure = pred_def.pred.pred_pure in
        let preds = Preds.copy preds in
        let wands = Wands.copy wands in
        let state = State.copy state in
        Preds.extend ~pure preds (pname, vs);
        Pred_state.{ state; preds; wands }
    | _ -> raise (Failure "Impossible: produce_upred_arm on non-pred")

  (* Legacy consume arm for wand assertions. *)
  and consume_wand_arm
      ~no_auto_fold:_
      (astate : t)
      (subst : SVal.SESubst.t)
      (step : MP.step) : (t, err_t) Res_list.t =
    match (fst step : Asrt.atom) with
    | CorePred (name, _ins, outs) ->
        if !Config.under_approximation then L.fail "Wand in under-approx";
        let pred_defs = MP.get_pred_defs () in
        let _, rname = Option.get (Asrt.as_wand_name name) in
        let rhs_ins_number =
          (MP.get_pred_def pred_defs rname).pred.ins_number
        in
        let lhs, rhs = Option.get (Asrt.as_wand ~rhs_ins_number (fst step)) in
        (* The wand's outs are exactly the rhs out-args, i.e. the stored
           core-predicate outs. *)
        let les_outs = outs in
        let fold_outs_info = (subst, step, les_outs) in
        consume_wand ~fold_outs_info astate subst Wands.{ lhs; rhs }
    | _ -> raise (Failure "Impossible: consume_wand_arm on non-wand")

  (* Legacy consume arm for user-predicate assertions. *)
  and consume_upred_arm
      ~(no_auto_fold : bool)
      (astate : t)
      (subst : SVal.SESubst.t)
      (step : MP.step) : (t, err_t) Res_list.t =
    match (fst step : Asrt.atom) with
    | CorePred (cp_name, ins, outs) ->
        let pname = Option.get (Asrt.as_user_pred_name cp_name) in
        let les = ins @ outs in
        L.verbose (fun m -> m "Matching predicate assertion");
        (* Perform substitution in all predicate parameters *)
        L.verbose (fun fmt -> fmt "ARGS: %a" Fmt.(list ~sep:comma Expr.pp) les);
        L.verbose (fun fmt -> fmt "SUBST:\n%a" SVal.SESubst.pp subst);
        let vs = List.map (subst_in_expr_opt astate subst) les in
        (* The in/out split is carried by the assertion's syntax. *)
        let vs_ins = List.map (subst_in_expr_opt astate subst) ins in
        let les_outs = outs in
        (* All of which must have survived substitution *)
        let failure = List.exists (fun x -> x = None) vs_ins in
        if failure then (
          L.verbose (fun m -> m "Cannot match: not all in-parameters known");
          resource_fail)
        else
          let vs_ins = List.map Option.get vs_ins in
          L.verbose (fun m ->
              m "Looking for ins: %a"
                Fmt.(brackets (list ~sep:comma Expr.pp))
                vs_ins);
          let consume_pred_res =
            consume_pred ~no_auto_fold astate pname vs
              ~fold_outs_info:(subst, step, les_outs)
          in
          if List.is_empty consume_pred_res then
            L.verbose ~severity:Warning (fun m -> m "Consume_pred vanished!");
          let open Res_list.Syntax in
          let++ astate', _ = consume_pred_res in
          astate'
    | _ -> raise (Failure "Impossible: consume_upred_arm on non-pred")

  and consume_wand
      ~fold_outs_info
      (astate : t)
      (subst : SVal.SESubst.t)
      (wand : Wands.wand) =
    let open Res_list.Syntax in
    L.verbose (fun m -> m "Matching wand assertion");
    let pred_defs = MP.get_pred_defs () in
    (* We start by building the query *)
    let** query =
      let query_opt =
        Wands.make_query ~pred_defs ~subst:(subst_in_expr_opt astate subst) wand
      in
      match query_opt with
      | None ->
          L.verbose (fun m ->
              m "Cannot match: not all in-parameters known for wand");
          resource_fail
      | Some query -> Res_list.return query
    in
    let semantic_eq = State.equals astate.state in
    L.tmi (fun m -> m "Matcher.consume_wand @[<h>%a@]" Wands.pp_query query);
    match Wands.consume_wand ~pred_defs ~semantic_eq astate.wands query with
    | Some wand -> (
        (* The wand was found *)
        L.verbose (fun m ->
            m "Returning the following wand (before checking outs equality): %a"
              Wands.pp_wand wand);
        let _, wand_outs = Wands.wand_ins_outs ~pred_defs wand in
        let subst, step, les_outs = fold_outs_info in
        L.verbose (fun m ->
            m
              "learnd the outs of the magic wand. going to match (@[<h>%a@]) \
               against (@[<h>%a@])!!!"
              Fmt.(list ~sep:comma Expr.pp)
              wand_outs
              Fmt.(list ~sep:comma Expr.pp)
              les_outs);
        match
          W.match_ins_outs_lists state_ops astate subst step wand_outs les_outs
        with
        | W.Success astate' -> Res_list.return astate'
        | W.Abort fail_pf ->
            (* TODO: why is this not EPure (fail_pf) ? *)
            let error = StateErr.EAsrt ([], fail_pf) in
            Res_list.error_with error
        | W.Vanish -> Res_list.vanish)
    | None ->
        L.verbose (fun m ->
            m "Could not find any match for the required wand!!!");
        Res_list.error_with (StateErr.EPure Expr.false_)

  (** Consumes a predicate from the state. If the predicate is not "verbatim" in
      our set of preds, and it is not abstract and we are not in manual mode, we
      attempt to fold it. *)
  and consume_pred
      ?(in_matching = false)
      ?(fold_outs_info : (SVal.SESubst.t * MP.step * Expr.t list) option)
      ?(no_auto_fold = false)
      (astate : t)
      (pname : string)
      (vs : Expr.t option list) : (t * Expr.t list, err_t) Res_list.t =
    L.tmi (fun m ->
        m "Matcher.consume_pred %s. args: @[<h>%a@]" pname
          Fmt.(list ~sep:comma (Dump.option Expr.pp))
          vs);

    let Pred_state.{ state; preds; wands = _ } = astate in
    let pred_defs = MP.get_pred_defs () in
    let pred = MP.get_pred_def pred_defs pname in
    let pred_def = pred.pred in
    let pred_pure = pred_def.pred_pure in
    (* we attempt to consume the pred as-is from our state. *)
    match
      Preds.consume_pred ~maintain:pred_pure preds pname vs
        (Containers.SI.of_list (Pred.ins_indexes pred_def))
        (State.equals state)
    with
    | Some (_, vs) -> (
        (* It was in our set of preds! *)
        L.verbose (fun m ->
            m "Returning the following vs: @[<h>%a@]"
              Fmt.(list ~sep:comma Expr.pp)
              vs);
        let vs = Pred.out_args pred_def vs in
        match fold_outs_info with
        | None -> Res_list.return (astate, vs)
        | Some (subst, step, les_outs) -> (
            L.verbose (fun m ->
                m
                  "learned the outs of a predicate. going to match (@[<h>%a@]) \
                   against (@[<h>%a@])!!!@\n"
                  Fmt.(list ~sep:comma Expr.pp)
                  vs
                  Fmt.(list ~sep:comma Expr.pp)
                  les_outs);
            match
              W.match_ins_outs_lists state_ops astate subst step vs les_outs
            with
            | W.Success astate' -> Res_list.return (astate', vs)
            | W.Abort fail_pf ->
                (* TODO: why is this not EPure (fail_pf) ? *)
                let error = StateErr.EAsrt ([], fail_pf) in
                Res_list.error_with error
            | W.Vanish -> Res_list.vanish))
    | None
      when (not !Config.manual_proof)
           && (not pred_def.pred_abstract)
           && not no_auto_fold ->
        (* Recursive Case - Folding required *)
        (* The predicate will be folded (if possible) and then removed from the
           state. Interestingly, if the predicate has a guard, this will
           produce it but not remove it. *)
        let () =
          L.verbose (fun fmt ->
              fmt "Auto-folding predicate: %s\n" pred.pred.pred_name)
        in
        L.verbose (fun m -> m "Recursive case - attempting to fold.");

        let open Res_list.Syntax in
        let vs_ins = Pred.in_args pred.pred vs in
        let vs_ins = List.map Option.get vs_ins in
        let** folded =
          fold ~in_matching:true ~state:astate ~match_kind:(Fold pname) pred
            vs_ins
        in
        (* Supposedly, we don't need a guard to make sure we're not looping
           indefinitely: if the fold worked, then consume_pred should not take
           this branch on the next try. We should still be keeping an eye on
           this in case something loops indefinitely. *)
        consume_pred ~no_auto_fold ?fold_outs_info ~in_matching folded pname vs
    | _ ->
        let values = List.filter_map Fun.id vs in
        (* The `True` as second parameter is required for the fixing mechanism
           to trigger *)
        Res_list.error_with (StateErr.EAsrt (values, Expr.true_))

  (* WARNING: At the moment, unfold behaves over-approximately, it will return
     only success or only error. We only use unfold and fold in OX mode right
     now, and we don't quite know the meaning of UX fold/unfold. *)
  and unfold
      ?(additional_bindings = [])
      (astate : t)
      (pname : string)
      (args : Expr.t list) : (SVal.SESubst.t * t, err_t) Res_list.t =
    let pred_defs = MP.get_pred_defs () in
    let pred = MP.get_pred_def pred_defs pname in
    let params = List.map (fun (x, _) -> Expr.PVar x) pred.pred.pred_params in

    let open Res_list.Syntax in
    let** { state; preds; wands } =
      match pred.pred.pred_guard with
      | None -> Res_list.return astate
      | Some _ ->
          let in_params = Pred.in_params pred.pred in
          let in_params = List.map (fun x -> Expr.PVar x) in_params in
          let in_args = Pred.in_args pred.pred args in
          let subst = SVal.SESubst.init (List.combine in_params in_args) in
          let++ s, _, _ =
            match_ ~in_matching:true astate subst (Option.get pred.guard_mp)
              PredicateGuard
          in
          s
    in
    L.verbose (fun m ->
        m
          "Combine going to explode. PredName: @[<h>%s@]. Params: @[<h>%a@]. \
           Args: @[<h>%a@]"
          pname
          Fmt.(list ~sep:comma Expr.pp)
          params
          Fmt.(list ~sep:comma Expr.pp)
          args);
    let subst_i = SVal.SESubst.init (List_utils.right_combine params args) in

    L.verbose (fun m ->
        m "unfold with unfold_info with additional bindings@\n%a@\n"
          Fmt.(Dump.list (pair string string))
          additional_bindings);

    let new_spec_vars =
      List.to_seq additional_bindings |> Seq.map fst |> SS.of_seq
    in
    let () = extend_subst_with_bindings state subst_i additional_bindings in
    let definitions =
      List.map (fun (_, def) -> def) pred.pred.pred_definitions
    in
    let open Syntaxes.List in
    let rets =
      match definitions with
      | [] ->
          Fmt.failwith "Cannot Unfold Predicate %s with No Definitions"
            pred.pred.pred_name
      | first_def :: rest_defs -> (
          (* We separate the first case from the rest because we
             only copy the state for the remaining branches if there are more
             than 1 definition *)
          L.verbose (fun m ->
              m "Going to produce %d definitions with subst@\n%a"
                (List.length (first_def :: rest_defs))
                SVal.SESubst.pp subst_i);
          L.tmi (fun m ->
              m "%a" Fmt.(list ~sep:(any "\n;\n") Asrt.pp) definitions);
          let state' = State.add_spec_vars state new_spec_vars in
          let astate = Pred_state.{ state = state'; preds; wands } in
          let rest_results =
            let* def = rest_defs in
            produce (copy_astate astate) (SVal.SESubst.copy subst_i) def
          in
          let first_results = produce astate subst_i first_def in
          let* result = first_results @ rest_results in
          match result with
          | Error err ->
              (* If a production fails, it means this branch is not
                 possible, we log and ignore. *)
              L.verbose (fun m -> m "Warning: %a" pp_err_t err);
              Res_list.vanish
          | Ok state ->
              let subst, states = simplify_astate ~matching:true state in
              let+ state = states in
              Ok (subst, state))
    in

    L.verbose (fun m ->
        m "Results of unfolding %s(@[<h>%a@]):@\n@[%a@]" pname
          Fmt.(list ~sep:comma Expr.pp)
          params
          Fmt.(
            iter_bindings ~sep:(any "@\n ") List.iteri (fun f' (i, res) ->
                let subst, astate = Result.get_ok res in
                Fmt.pf f' "Result %d@\nSTATE:@\n  @[%a@]@\nSUBST:@[<h>%a@]@\n" i
                  pp_astate astate SVal.SESubst.pp subst))
          rets);
    rets

  and fold_guarded_with_vals (astate : t) (vs : Expr.t list) :
      string option * (t, string) Res_list.t =
    L.verbose (fun m ->
        m "@[<v 2>Starting fold_guarded_with_vals: @[<h>%a@]@\n%a.@\n"
          Fmt.(list ~sep:comma Expr.pp)
          vs pp_astate astate);
    if !Config.manual_proof then (None, Res_list.error_with "Manual proof")
    else
      match select_guarded_predicate_to_fold astate vs with
      | Some (pname, v_args) ->
          L.verbose (fun m -> m "FOUND STH TO FOLD: %s!!!!\n" pname);
          let pred = MP.get_pred_def (MP.get_pred_defs ()) pname in
          let rets =
            fold ~in_matching:true ~match_kind:(Fold pname)
              ~state:(copy_astate astate) pred v_args
          in
          let rets =
            Res_list.map_error
              (fun _ -> "fold_guarded_with_vals: Failed to fold")
              rets
          in
          (Some pname, rets)
      | None ->
          L.verbose (fun m -> m "No predicate found to fold!");
          (None, Res_list.error_with "No predicate found to fold!")

  and unfold_with_vals'
      ~(auto_level : [ `High | `Low ])
      (astate : t)
      (vs : Expr.t list) : (string * (SVal.SESubst.t * t) list) option =
    L.verbose (fun m ->
        m "@[<v 2>Starting unfold_with_vals: @[<h>%a@]@\n%a.@\n"
          Fmt.(list ~sep:comma Expr.pp)
          vs pp_astate astate);

    if !Config.manual_proof then None
    else
      match consume_pred_with_vs ~auto_level astate vs with
      | Some (pname, v_args) -> (
          L.verbose (fun m -> m "FOUND STH TO UNFOLD: %s!!!!\n" pname);
          let rets = unfold (copy_astate astate) pname v_args in
          let only_successes, only_errors = Res_list.split rets in
          match only_errors with
          | [] ->
              L.verbose (fun m ->
                  m "Unfold complete: %s(@[<h>%a@]): %d" pname
                    Fmt.(list ~sep:comma Expr.pp)
                    v_args (List.length rets));
              Some (pname, only_successes)
          | _ :: _ ->
              L.verbose (fun m ->
                  m "Unfolding failed in unfold_with_vals: %a"
                    Fmt.(list ~sep:(any "\n") pp_err_t)
                    only_errors);
              None)
      | None ->
          L.verbose (fun m -> m "NOTHING TO UNFOLD!!!!\n");
          None

  and fold
      ?(in_matching = false)
      ?(additional_bindings = [])
      ~match_kind
      ~(state : t)
      (pred : MP.pred)
      (args : Expr.t list) : (t, err_t) Res_list.t =
    let pred_name = pred.pred.pred_name in
    L.verbose (fun fmt -> fmt "Folding predicate: %s\n" pred_name);
    if pred.pred.pred_abstract then
      Fmt.failwith "Impossible: Folding abstract predicate %s" pred_name;
    L.verbose (fun m -> m "Predicate matching plan: %a" MP.pp pred.def_mp);
    let params = List.map (fun (x, _) -> x) pred.pred.pred_params in
    let param_bindings =
      if List.compare_lengths params args = 0 then List.combine params args
      else
        try List.combine (Pred.in_params pred.pred) args
        with Invalid_argument _ ->
          Fmt.failwith "invalid number of parameter while folding: %s%a"
            pred.pred.pred_name
            Fmt.(parens @@ hbox @@ list ~sep:comma Expr.pp)
            args
    in
    let param_bindings =
      List.map (fun (x, v) -> (Expr.PVar x, v)) param_bindings
    in
    let subst = SVal.SESubst.init (additional_bindings @ param_bindings) in
    let match_result = match_ ~in_matching state subst pred.def_mp match_kind in
    let open Res_list.Syntax in
    let** astate', subst', _ = match_result in
    let Pred_state.{ preds = preds'; _ } = astate' in
    let arg_vs =
      if List.compare_lengths params args = 0 then args
      else
        let out_params = Pred.out_params pred.pred in
        let vs_outs =
          List.map
            (fun x ->
              match SVal.SESubst.get subst' (PVar x) with
              | Some v_x -> v_x
              | None ->
                  failwith "DEATH. Didnt learn all the outs while folding.")
            out_params
        in
        L.verbose (fun m ->
            m "Out parameters : @[<h>%a@]" Fmt.(list ~sep:comma Expr.pp) vs_outs);
        Pred.combine_ins_outs pred.pred args vs_outs
    in
    (* We extend the list of predicates with our newly folded predicate. *)
    Preds.extend ~pure:pred.pred.pred_pure preds' (pred_name, arg_vs);
    (* If the predicate has a guard, we also produce it in our state,
       otherwise we return the just current state *)
    match pred.pred.pred_guard with
    | None -> Res_list.return astate'
    | Some guard -> produce astate' subst' guard

  and unfold_concrete_preds (astate : t) : (SVal.SESubst.t option * t) option =
    let Pred_state.{ preds; _ } = astate in
    let pred_defs = MP.get_pred_defs () in

    let is_unfoldable_lit lit =
      match lit with
      | Loc _ | LList _ -> false
      | _ -> true
    in

    let should_unfold (pname, vs) =
      (* Find a predicate with only concrete args
         and without a guard. *)
      let pred = MP.get_pred_def pred_defs pname in
      Option.is_none pred.pred.pred_guard
      && Pred.in_args pred.pred vs
         |> List.for_all (fun in_arg ->
                match Expr.to_literal in_arg with
                | None -> false
                | Some lit -> is_unfoldable_lit lit)
    in

    let pred_to_unfold = Preds.pop preds should_unfold in
    match pred_to_unfold with
    | Some (name, vs) -> (
        let next_states = unfold astate name vs in
        match next_states with
        | [] -> None
        | [ Ok (subst, astate'') ] ->
            L.verbose (fun m ->
                m "unfold_concrete_preds WORKED. Unfolded: %s(@[<h>%a])" name
                  Fmt.(list ~sep:comma Expr.pp)
                  vs);
            Some (Some subst, astate'')
        | next_states -> (
            let oks =
              List.filter_map
                (function
                  | Ok x -> Some x
                  | _ -> None)
                next_states
            in
            match oks with
            | _ :: _ ->
                failwith
                  "Impossible: pred with concrete ins unfolded to multiple \
                   states."
            | [] ->
                let errs = List.map Result.get_error next_states in
                Fmt.failwith
                  "Impossible: pred with concrete ins and no guard failed to \
                   unfold with errors: %a"
                  Fmt.(Dump.list pp_err_t)
                  errs))
    | None -> Some (None, astate)

  and try_recovering (astate : t) (tactic : Expr.t Recovery_tactic.t) :
      (t list * recovery_tactic, string) result =
    let open Syntaxes.Result in
    if !Config.under_approximation then
      L.fail "Recovery tactics not handled in UX mode";
    L.verbose (fun m -> m "Attempting to recover");
    let- fold_error =
      match tactic.try_fold with
      | Some fold_values -> (
          let pname, res = fold_guarded_with_vals astate fold_values in
          let pname = Option.value ~default:"!UNKNOWN!" pname in
          let successes, errors = Res_list.split res in
          match errors with
          | [] -> Ok (successes, Try_fold (pname, fold_values))
          | _ ->
              let error_string = Fmt.str "%a" Fmt.(Dump.list string) errors in
              Error error_string)
      | None ->
          L.verbose (fun m -> m "No fold recovery tactic");
          Error "None"
    in
    (* This matches the legacy behaviour *)
    let unfold_values = Option.value ~default:[] tactic.try_unfold in
    match unfold_with_vals' ~auto_level:`High astate unfold_values with
    | None ->
        Fmt.error "try_fold: %s\ntry_unfold: Automatic unfold failed" fold_error
    | Some (pname, next_states) ->
        let sp = List.map snd next_states in
        Ok (sp, Try_unfold (pname, unfold_values))

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

  let unfold_with_vals
      ~(auto_level : [ `High | `Low ])
      (astate : t)
      (vs : Expr.t list) : (SVal.SESubst.t * t) list option =
    unfold_with_vals' ~auto_level astate vs |> Option.map snd

  let rec rec_unfold
      ?(fuel = 10)
      (astate : t)
      (pname : string)
      (args : Expr.t list) : (t, err_t) Res_list.t =
    if fuel = 0 then failwith "RECURSIVE UNFOLD: OUT OF FUEL"
    else
      let open Res_list.Syntax in
      let** _, astate = unfold astate pname args in
      let Pred_state.{ preds; _ } = astate in
      match Preds.remove_by_name preds pname with
      | Some (pname, vs) -> rec_unfold ~fuel:(fuel - 1) astate pname vs
      | None -> Res_list.return astate

  let unfold_all (astate : t) (pname : string) : (t, err_t) Res_list.t =
    match Preds.remove_by_name astate.preds pname with
    | None -> Res_list.return astate
    | Some (pname, vs) -> rec_unfold astate pname vs

  module Wand_packaging = struct
    let non_empty_message =
      "The magic wand didn't swallow the whole footprint of its lhs. This \
       means it is very probably uninteresting. However, it doesn't mean that \
       the package does not hold! This is an error because it is probably a \
       mistake but it would not be unsound to continue."

    type package_state = {
      lhs_state : t;
      current_state : t;
      subst : SVal.SESubst.t;
    }

    let copy_package_state pstate =
      {
        lhs_state = copy_astate pstate.lhs_state;
        current_state = copy_astate pstate.current_state;
        subst = SVal.SESubst.copy pstate.subst;
      }

    let get_defs (pred : Pred.t) largs =
      if pred.pred_abstract || Option.is_some pred.pred_guard then
        [
          [
            Asrt.pred pred.pred_name (Pred.in_args pred largs)
              (Pred.out_args pred largs);
          ];
        ]
      else
        let unfolded_pred =
          Hashtbl.find_opt Unfolded_preds.tbl pred.pred_name
        in
        let pred = Option.value ~default:pred unfolded_pred in
        List.map snd pred.pred_definitions

    let make_lhs_states ~pred_defs ~empty_state (lname, largs) =
      let open Syntaxes.List in
      let lhs_pred = (MP.get_pred_def pred_defs lname).pred in
      let subst =
        let params =
          List.map (fun (x, _) -> Expr.PVar x) lhs_pred.pred_params
        in
        let bindings = List.combine params largs in
        SVal.SESubst.init bindings
      in
      let* lhs_def = get_defs lhs_pred largs in
      let subst = SVal.SESubst.copy subst in
      let astate = copy_astate empty_state in
      let* produced = produce astate subst lhs_def in
      match produced with
      | Error _ -> []
      | Ok state ->
          let _, simplified = simplify_astate ~matching:true state in
          simplified

    let match_assertion astate subst step =
      (* We are in OX mode, matching must not branch. If it does, something is
         very wrong. Mainly because the substitution is performed in place.
         This function simplifies the return type of match-assertion: it
         returns a single outcome if it's a success. *)
      let res, _ =
        W.match_assertion_safely state_ops ~no_auto_fold:true astate subst step
      in
      let successes, errors = Res_list.split res in
      let r =
        match (successes, errors) with
        | [ x ], [] -> Ok x
        | [], errs -> Error errs
        | _ ->
            Fmt.failwith
              "Impossible: match-assertion branched in OX mode: %d successes \
               and %d errors"
              (List.length successes) (List.length errors)
      in
      r

    type split_answer = {
      init_subst : State.st;
      mp : MP.t;
      fold_outs_info : State.st * MP.step * string list * Expr.t list;
    }

    let matchables expr =
      let lvars =
        Expr.lvars expr |> SS.to_seq |> Seq.map (fun x -> Expr.LVar x)
      in
      let alocs =
        Expr.alocs expr |> SS.to_seq |> Seq.map Expr.loc_from_loc_name
      in
      Seq.append lvars alocs

    (* If we can't fully consume something from the lhs, maybe we can still consume a {b fragment} of it.
        This function tries to split the step into smaller steps, some of which can be consumed from the
        lhs and some from the current state.
        The way it behaves is complicated.
        For example let's say the core predicate [(x, []) ↦ [a, b]] (with 2 ins and 1 out) can be split into
       - [(x, [0]) ↦ [a]]
       - [(x, [1]) ↦ [b]]
       [State.split_core_pred_further] will return something of the form
       [ ([ [x, [0]], [x, [1]] ], [  {{ l-nth(PVar("0:0"), 0), l-nth(PVar("1:0"), 0) }}  ] ]
       Indicating that the new ins and how to learn the old outs.
       In particular, we learn the old out ([[a, b]]) by applying [λx. x[0]]
       to both the 0th out of the 0th new core pred (0:0)  and the 0th out of the 1st new core pred (1:0).
    *)
    let try_split_step ~subst ~astate ~errs (step : MP.step) :
        split_answer option =
      let open Syntaxes.Option in
      match (step, errs) with
      | (CorePred (cp_name, ins, outs), _), _
        when Option.is_some (Asrt.as_user_pred_name cp_name) ->
          let name = Option.get (Asrt.as_user_pred_name cp_name) in
          let MP.{ pred; def_mp; _ } =
            MP.get_pred_def (MP.get_pred_defs ()) name
          in
          let* () =
            if pred.pred_abstract || Option.is_some pred.pred_guard then None
            else Some ()
          in
          let in_params =
            Pred.in_params pred |> List.map (fun x -> Expr.PVar x)
          in
          (* The in/out split is carried by the assertion's syntax. *)
          let in_args =
            ins
            |> List.map (SVal.SESubst.subst_in_expr_opt subst)
            |> List.map Option.get
          in
          let init_subst =
            List.combine in_params in_args |> SVal.SESubst.init
          in
          let out_params = Pred.out_params pred in
          let out_args = outs in
          Some
            {
              mp = def_mp;
              init_subst;
              fold_outs_info = (subst, step, out_params, out_args);
            }
      | (CorePred (core_pred, ins, outs), _), [ err ] ->
          (* What we do here is simulate the idea that the core predicate is actually a folded core-predicate *)
          let kb =
            List.to_seq ins
            |> Seq.map (fun x -> subst_in_expr_opt astate subst x |> Option.get)
            |> Seq.fold_left
                 (fun acc x -> MP.KB.add_seq (matchables x) acc)
                 MP.KB.empty
          in
          let init_subst =
            MP.KB.to_seq kb |> Seq.map (fun x -> (x, x)) |> SVal.SESubst.of_seq
          in
          let out_params =
            List.mapi (fun i _ -> "out___" ^ string_of_int i) outs
          in
          (* Now we build our assertion *)
          let+ new_ins_l, new_outs_learn =
            let vs_ins =
              List.map
                (fun x -> subst_in_expr_opt astate subst x |> Option.get)
                ins
            in
            State.split_core_pred_further astate.state core_pred vs_ins err
          in
          let out_amount = List.length outs in
          let cp_amount = List.length new_ins_l in
          let all_new_outs =
            Array.init (cp_amount * out_amount) (fun _ ->
                Expr.LVar (LVar.alloc ()))
          in
          let pvar_subst =
            let seq =
              Seq.concat
              @@ Seq.init cp_amount (fun i ->
                     Seq.init out_amount (fun j ->
                         let id = Fmt.str "%d:%d" i j in
                         (Expr.PVar id, all_new_outs.((i * out_amount) + j))))
            in
            SVal.SESubst.of_seq seq
          in
          let new_cps =
            List.mapi
              (fun cp_i ins ->
                let outs =
                  List.init out_amount (fun o_i ->
                      all_new_outs.((cp_i * out_amount) + o_i))
                in
                Asrt.CorePred (core_pred, ins, outs))
              new_ins_l
          in
          let learning_equalities =
            List.map2
              (fun old_out new_out ->
                Asrt.Pure
                  (Expr.Infix.( == ) (Expr.PVar old_out)
                     (subst_in_expr pvar_subst new_out)))
              out_params new_outs_learn
          in
          let atoms = List.rev_append new_cps learning_equalities in
          let mp =
            let steps = MP.s_init_atoms kb atoms |> Result.get_ok in
            MP.of_step_list steps
          in
          { init_subst; mp; fold_outs_info = (subst, step, out_params, outs) }
      | _ -> None

    let match_ins_outs_lists
        (state : package_state)
        (subst : SVal.SESubst.t)
        (step : MP.step)
        (obtained : Expr.t list)
        (expected : Expr.t list) : (package_state, err_t list) Result.t =
      L.verbose (fun m ->
          m
            "About to match ins-outs after splitting!@\n\
             SUBST: %a@\n\
             OBTAINED: %a@\n\
             EXPECTED: %a@\n"
            SVal.SESubst.pp subst (Fmt.Dump.list Expr.pp) obtained
            (Fmt.Dump.list Expr.pp) expected);
      let open Syntaxes.List in
      let outs = snd step in
      let pvar_subst =
        List.mapi (fun i v -> (Expr.PVar (string_of_int i), v)) obtained
        |> SVal.SESubst.init
      in
      let outs =
        let+ u, e = outs in
        let se = SVal.SESubst.subst_in_expr pvar_subst ~partial:true e in
        let se = try Reduction.reduce_lexpr ~matching:true se with _ -> se in
        (u, se)
      in
      List.iter (fun (u, v) -> SVal.SESubst.put subst u v) outs;
      let expected =
        let+ e = expected in
        match SVal.SESubst.subst_in_expr_opt subst e with
        | None -> Fmt.failwith "Did not learn %a!" Expr.pp e
        | Some e -> e
      in
      List.fold_left2
        (fun acc vd od ->
          let open Syntaxes.Result in
          let* acc = acc in
          let equality = Expr.BinOp (vd, Equal, od) in
          if
            State.assert_a state.lhs_state.state [ equality ]
            || State.assert_a state.current_state.state [ equality ]
          then Ok acc
          else
            (* TODO: why is this not EPure (equality) ? *)
            Error [ StateErr.EAsrt ([], equality) ])
        (Ok state) obtained expected

    let rec package_case_step
        { lhs_state; current_state; subst }
        (step : MP.step) : (package_state list, err_t list) Result.t =
      let open Syntaxes.Result in
      L.verbose (fun m ->
          m "Wand about to consume RHS step: %a" Asrt.pp_atom (fst step));
      (* States are modified in place unfortunately.. so we have to copy them just in case *)
      (* First we try to consume from the lhs_state *)
      let- lhs_errs =
        let subst = SVal.SESubst.copy subst in
        let+ new_lhs_state =
          match_assertion (copy_astate lhs_state) subst step
        in
        [ { lhs_state = new_lhs_state; current_state; subst } ]
      in
      (* If it fails, we try splitting the step and we try again *)
      let- split_errs =
        let split_option =
          try_split_step ~astate:lhs_state ~subst ~errs:lhs_errs step
        in
        match split_option with
        | Some { mp; init_subst; fold_outs_info } ->
            L.verbose (fun m ->
                m "We found a way to split, here is the MP:@\n%a" MP.pp mp);
            let temporary_state =
              {
                lhs_state = copy_astate lhs_state;
                current_state = copy_astate current_state;
                subst = init_subst;
              }
            in
            let* states = package_mp mp temporary_state in
            let old_subst, step, out_params, expected = fold_outs_info in
            Result_utils.map_bind
              (fun state ->
                let obtained =
                  List.map
                    (fun x ->
                      match SVal.SESubst.get state.subst (PVar x) with
                      | Some x -> x
                      | None -> Fmt.failwith "Did not learn %s ??" x)
                    out_params
                in
                let+ state =
                  match_ins_outs_lists state old_subst step obtained expected
                in
                { state with subst = old_subst })
              states
        | None -> Error []
      in
      L.verbose (fun m ->
          m
            "Wand: failed to consume from LHS! Going to try and consume from \
             current state!");
      let- current_errs =
        let matching_outcome = match_assertion current_state subst step in
        match matching_outcome with
        | Ok new_current_state ->
            Ok [ { lhs_state; current_state = new_current_state; subst } ]
        | Error errs when !Config.unfolding && List.exists State.can_fix errs
          -> (
            (* We go with the usual tactic of trying to unfold.
               Careful, after that we need *all* cases to be successful!
               That is the insight from the Viper paper correcting their old error
               "Sound automation of magic wands" *)
            let tactics = State.get_recovery_tactic current_state.state errs in
            L.verbose (fun m ->
                m
                  "Trying to recover failing on the current state, obtained \
                   recovery tactics: %a"
                  (Recovery_tactic.pp Expr.pp)
                  tactics);
            let unfold_values = Option.value ~default:[] tactics.try_unfold in
            match
              unfold_with_vals ~auto_level:`High current_state unfold_values
            with
            | None -> Error []
            | Some current_states ->
                L.verbose (fun m -> m "Successfully unfolded! Let's continue!");
                let should_copy =
                  match current_states with
                  | _ :: _ :: _ -> true
                  | _ -> false
                in
                Result_utils.map_bind
                  (fun (_, current_state) ->
                    let lhs_state, subst =
                      if should_copy then
                        (copy_astate lhs_state, SVal.SESubst.copy subst)
                      else (lhs_state, subst)
                    in
                    let+ new_current_state =
                      match_assertion current_state subst step
                    in
                    { lhs_state; current_state = new_current_state; subst })
                  current_states)
        | Error errs ->
            L.verbose (fun m -> m "Cannot recover!");
            Error errs
      in
      L.verbose (fun m -> m "Couldn't consume from anywhere!!");
      Error (lhs_errs @ split_errs @ current_errs)

    and package_mp mp (state : package_state) :
        (package_state list, err_t list) Result.t =
      let open List_res.Syntax in
      match mp with
      | MP.LabelStep _ -> L.fail "Labeled in wand RHS, can't handle that yet!"
      | Finished (Some _) -> L.fail "Finished with posts in wand RHS!"
      | Finished None -> List_res.return state
      | Choice (left_mp, right_mp) ->
          let ( let- ) = Result_utils.bind_error in
          let state_copy = copy_package_state state in
          L.verbose (fun m -> m "Trying the left-hand-side first");
          let- left_errs = package_mp left_mp state in
          L.verbose (fun m ->
              m "Left-hand-side failed, trying right-hand-side!");
          let- right_errs = package_mp right_mp state_copy in
          Error (left_errs @ right_errs)
      | ConsumeStep (step, rest_mp) ->
          let* state = package_case_step state step in
          package_mp rest_mp state

    let astate_sure_is_nonempty (astate : t) =
      State.sure_is_nonempty astate.state

    let package_wand (astate : t) (wand : Wands.wand) : (t, err_t) List_res.t =
      let open Syntaxes.Result in
      if !Config.under_approximation then
        Fmt.failwith "Wand packaging not handled in UX mode";
      (* First, we create a state that matches the lhs,
         trying to unfold the content if possible. *)
      let pred_defs = MP.get_pred_defs () in
      let lhs_states =
        make_lhs_states
          ~empty_state:(clear_resource (copy_astate astate))
          ~pred_defs wand.lhs
      in
      let rpred = MP.get_pred_def pred_defs (fst wand.rhs) in
      (* let lpred = MP.get_pred_def pred_defs (fst wand.lhs) in *)
      let rhs_mp =
        if Option.is_some rpred.pred.pred_guard then
          L.fail "Magic Wand rhs is guarded!";
        rpred.def_mp
      in
      let subst =
        let rparams =
          List.map (fun (x, _) -> Expr.PVar x) rpred.pred.pred_params
        in
        L.verbose (fun m ->
            m "RPARAMS: %a; RARGS: %a" (Fmt.Dump.list Expr.pp) rparams
              (Fmt.Dump.list Expr.pp) (snd wand.rhs));
        let all_bindings = List.combine rparams (snd wand.rhs) in
        let in_bindings = Pred.in_args rpred.pred all_bindings in
        SVal.SESubst.init in_bindings
      in
      let start_states =
        match lhs_states with
        | [] -> failwith "wand lhs is False!"
        | first :: rest ->
            let rest_pack_states =
              List.map
                (fun lhs_state ->
                  {
                    lhs_state;
                    current_state = copy_astate astate;
                    subst = SVal.SESubst.copy subst;
                  })
                rest
            in
            let first_pack_state =
              { lhs_state = first; current_state = astate; subst }
            in
            first_pack_state :: rest_pack_states
      in
      L.verbose (fun m ->
          m "About to start consuming rhs of wand. Currently %d search states"
            (List.length start_states));
      let final_states =
        List.fold_left
          (fun acc state ->
            let* acc = acc in
            let+ case = package_mp rhs_mp state in
            case @ acc)
          (Ok []) start_states
      in
      let* states = final_states in
      let all_res =
        List.map
          (fun state ->
            if astate_sure_is_nonempty state.lhs_state then (
              (* Note that failing here is optional. Packaging the wand anyway is correct,
                 though it would almost certainly lead to a verification error.
                 If this behaviour ever comes an issue, it could be deactivate through a feature flag. *)
              L.normal (fun m ->
                  m
                    "Error: AN LHS STATE WAS NOT ENTIRELY DEPLETED, THE MAGIC \
                     WAND IS NON INTERESTING");
              Error [ StateErr.EOther non_empty_message ])
            else Ok state.current_state)
          states
      in
      Result_utils.all all_res
  end

  let package_wand t wand =
    match Wand_packaging.package_wand t wand with
    | Ok [] -> failwith "WAND VANISHED???"
    | r -> r
end
