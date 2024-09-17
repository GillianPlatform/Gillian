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

  val unfold :
    ?additional_bindings:unfold_info_t ->
    t ->
    string ->
    Expr.t list ->
    (SVal.SESubst.t * t, err_t) Res_list.t

  val rec_unfold :
    ?fuel:int -> t -> string -> Expr.t list -> (t, err_t) Res_list.t

  val unfold_all : t -> string -> (t, err_t) Res_list.t
  val try_recovering : t -> Expr.t Recovery_tactic.t -> (t list, string) result

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

  type state_t = State.t [@@deriving yojson]
  type abs_t = string * Expr.t list
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type err_t = State.err_t [@@deriving yojson, show]

  type t = {
    state : State.t;
    preds : Preds.t;
    wands : Wands.t;
    pred_defs : MP.preds_tbl_t;
    variants : variants_t;
  }

  type post_res = (Flag.t * Asrt.t list) option
  type s_state = t * SVal.SESubst.t * MP.t
  type search_state = s_state list * err_t list
  type search_state' = (s_state * int * bool) list * err_t list
  type unfold_info_t = (string * string) list

  (* This is mostly to do with Gillian legacy.
     We have to handle UX and OX separately, or otherwise
     we'd have to refactor the entirety of the code... *)
  type internal_mp_u_res = (t * SVal.SESubst.t * post_res, err_t) List_res.t

  module Logging = struct
    let pp_variants : (string * Expr.t option) Fmt.t =
      Fmt.pair ~sep:Fmt.comma Fmt.string (Fmt.option Expr.pp)

    let pp_astate fmt astate =
      let { state; preds; wands; variants; _ } = astate in
      Fmt.pf fmt "%a@\nPREDS:@\n%a@\nWANDS:@\n%a@\nVARIANTS:@\n%a@\n" State.pp
        state Preds.pp preds Wands.pp wands
        (Fmt.hashtbl ~sep:Fmt.semi pp_variants)
        variants

    let pp_astate_by_need (pvars : SS.t) (lvars : SS.t) (locs : SS.t) fmt astate
        =
      let { state; preds; wands; variants; _ } = astate in
      Fmt.pf fmt "%a@\n@\nPREDS:@\n%a@\nWANDS:@\n%a@\nVARIANTS:@\n%a@\n"
        (State.pp_by_need pvars lvars locs)
        state Preds.pp preds Wands.pp wands
        (Fmt.hashtbl ~sep:Fmt.semi pp_variants)
        variants

    module AstateRec = struct
      type t' = t

      type t = {
        state : state_t;
        preds : Preds.t;
        wands : Wands.t;
        variants : variants_t;
      }
      [@@deriving yojson]

      let from ({ state; preds; wands; variants; _ } : t') =
        { state; preds; variants; wands }

      let pp_custom pp_astate fmt { state; preds; variants; wands } =
        pp_astate fmt
          { state; preds; variants; wands; pred_defs = Hashtbl.create 0 }

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

    module MatchCaseReport = struct
      type t = { astate : AstateRec.t; subst : SVal.SESubst.t; mp : MP.t }
      [@@deriving yojson]

      let to_loggable = L.Loggable.make L.dummy_pp of_yojson to_yojson

      let log report =
        L.Specific.normal (to_loggable report)
          L.Logging_constants.Content_type.match_case
    end

    module MatchResultReport = struct
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

    let structure_match_case_reports
        parent_ids_ref
        target_case_depth
        is_new_case
        astate
        subst
        mp =
      let actual_target_depth =
        if is_new_case then target_case_depth - 1 else target_case_depth
      in
      let case_depth = List.length !parent_ids_ref in
      (* assert (actual_target_depth <= case_depth + 1); *)
      for _ = case_depth downto actual_target_depth + 1 do
        match !parent_ids_ref with
        | [] -> raise (Failure "Mismatched case depth and parent_id list!")
        | parent_id :: rest ->
            L.Parent.release (Some parent_id);
            parent_ids_ref := rest
      done;
      if is_new_case then
        let new_parent_id =
          MatchCaseReport.log { astate = AstateRec.from astate; subst; mp }
        in
        match new_parent_id with
        | Some new_parent_id ->
            L.Parent.set new_parent_id;
            parent_ids_ref := new_parent_id :: !parent_ids_ref;
            target_case_depth
        | None -> target_case_depth
      else target_case_depth
  end

  open Logging

  let clear_resource (astate : t) =
    let { state; preds; wands = _; pred_defs; variants } = astate in
    let state = State.clear_resource state in
    let preds_list = Preds.to_list preds in
    List.iter
      (fun (name, vs) ->
        let pred_def = Hashtbl.find pred_defs name in
        if not pred_def.pred.pred_pure then
          let _ =
            Preds.pop preds (fun (name', vs') -> name' = name && vs' = vs)
          in
          ())
      preds_list;
    { state; preds; wands = Wands.init []; pred_defs; variants }

  type cons_pure_result = Success of state_t | Abort of Formula.t | Vanish

  let cons_pure (state : state_t) (f : Formula.t) : cons_pure_result =
    if !Config.under_approximation then
      match State.assume_a ~matching:true state [ f ] with
      | Some state -> Success state
      | None -> Vanish
    else if State.assert_a state [ f ] then Success state
    else Abort f

  let update_store (astate : t) (x : string) (v : Expr.t) : t =
    let store = State.get_store astate.state in
    let () = SStore.put store x v in
    let state' = State.set_store astate.state store in
    { astate with state = state' }

  let simplify_astate ?(save = false) ?(matching = false) (astate : t) :
      SVal.SESubst.t * t list =
    let { state; preds; wands; pred_defs; variants } = astate in
    let subst, states =
      State.simplify ~save ~kill_new_lvars:false ~matching state
    in
    Preds.substitution_in_place subst preds;
    Wands.substitution_in_place subst wands;
    match states with
    | [] -> failwith "Impossible: state substitution returned []"
    | [ state ] -> (subst, [ { astate with state } ])
    | states ->
        ( subst,
          List.map
            (fun state ->
              {
                state;
                preds = Preds.copy preds;
                pred_defs;
                wands = Wands.copy wands;
                variants = Hashtbl.copy variants;
              })
            states )

  let copy_astate (astate : t) : t =
    {
      state = State.copy astate.state;
      preds = Preds.copy astate.preds;
      wands = Wands.copy astate.wands;
      pred_defs = astate.pred_defs;
      variants = Hashtbl.copy astate.variants;
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
    let { state; preds; pred_defs; _ } = astate in

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
    let { state; preds; pred_defs; _ } = astate in
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

  let rec produce_assertion
      (astate : t)
      (subst : SVal.SESubst.t)
      (a : Asrt.simple) : (t, err_t) Res_list.t =
    let open Res_list.Syntax in
    let { state; preds; pred_defs; variants; wands } = astate in
    let other_state_err msg = [ Error (StateErr.EOther msg) ] in

    L.verbose (fun m ->
        m
          "-------------------------@\n\
           Produce simple assertion: @[<h>%a@]@\n\
           With subst: %a\n\
          \           -------------------------@\n"
          Asrt.pp_simple a SVal.SESubst.pp subst);

    L.verbose (fun m -> m "STATE: %a" pp_astate astate);

    match (a : Asrt.simple) with
    | Emp ->
        L.verbose (fun fmt -> fmt "Emp assertion.");
        [ Ok astate ]
    | CorePred (a_id, ins, outs) ->
        L.verbose (fun fmt -> fmt "Memory producer.");

        let vs = List.map (subst_in_expr subst) (ins @ outs) in
        (* We filter action errors, in theory, production cannot fail, it may only vanish. *)
        State.produce_core_pred a_id state vs
        |> List.map (fun state' ->
               Ok
                 {
                   state = state';
                   preds = Preds.copy preds;
                   wands = Wands.copy wands;
                   pred_defs;
                   variants = Hashtbl.copy variants;
                 })
    | Types les -> (
        L.verbose (fun fmt -> fmt "Types assertion.");
        let state' =
          List.fold_left
            (fun state (le, t) ->
              Option.bind state (fun state ->
                  let v = subst_in_expr subst le in
                  State.assume_t state v t))
            (Some state) les
        in
        match state' with
        | None ->
            other_state_err "Produce Simple Assertion: Cannot produce types"
        | Some _ -> [ Ok { state; preds; wands; pred_defs; variants } ])
    | Pred (pname, les) ->
        L.verbose (fun fmt -> fmt "Predicate assertion.");
        let vs = List.map (subst_in_expr subst) les in
        let pred_def = Hashtbl.find pred_defs pname in
        let++ { state; preds; wands; pred_defs; variants } =
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
                      Formula.subst_expr_for_expr ~to_subst:param ~subst_with:le
                    in
                    List.map subst facts)
                  facts params les
              in
              let facts = Asrt.Pure (Formula.conjunct facts) in
              produce_assertion
                { state; preds; wands; pred_defs; variants }
                subst facts
        in
        let pure = pred_def.pred.pred_pure in
        let preds = Preds.copy preds in
        let wands = Wands.copy wands in
        let state = State.copy state in
        let variants = Hashtbl.copy variants in
        Preds.extend ~pure preds (pname, vs);
        { state; preds; wands; pred_defs; variants }
    | Wand { lhs = lname, largs; rhs = rname, rargs } ->
        if !Config.under_approximation then
          L.fail "Wand assertions are not supported in under-approximation mode";
        L.verbose (fun m -> m "Wand assertion.");
        let largs = List.map (subst_in_expr subst) largs in
        let rargs = List.map (subst_in_expr subst) rargs in
        Wands.extend wands Wands.{ lhs = (lname, largs); rhs = (rname, rargs) };
        Res_list.return astate
    | Pure (Eq (PVar x, le)) | Pure (Eq (le, PVar x)) -> (
        L.verbose (fun fmt -> fmt "Pure assertion.");
        match SVal.SESubst.get subst (PVar x) with
        | Some v_x ->
            let v_le = subst_in_expr subst le in
            let opt_res =
              Option.map
                (fun state ->
                  [ Ok { state; preds; wands; pred_defs; variants } ])
                (State.assume_a ~matching:true
                   ~production:!Config.delay_entailment state
                   [ Eq (v_x, v_le) ])
            in
            Option.value
              ~default:
                (other_state_err
                   "Produce Simple Assertion: Subst does not cover the pure \
                    formula")
              opt_res
        | None ->
            let v = subst_in_expr subst le in
            L.verbose (fun m ->
                m
                  "UNHAPPY. update_store inside produce assertions with prog \
                   variable: %s!!!\n"
                  x);
            Res_list.return (update_store astate x v))
    | Pure f -> (
        L.verbose (fun fmt -> fmt "Pure assertion.");
        let f' = SVal.SESubst.substitute_formula subst ~partial:false f in
        (* let pp_state =
             match !Config.pbn with
             | false -> State.pp
             | true  ->
                 let pvars, lvars, locs = Formula.get_print_info f' in
                 State.pp_by_need pvars lvars locs
           in
           L.
             verbose (fun m ->
                  m "About to assume %a in state:\n%a" Formula.pp f' pp_state state); *)
        (* FIXME: Understand why this causes a bug in Gillian-C *)
        match
          State.assume_a ~matching:true ~production:!Config.delay_entailment
            state [ f' ]
        with
        | None ->
            let msg =
              Fmt.str "Produce Simple Assertion: Cannot assume pure formula %a."
                Formula.pp f'
            in
            other_state_err msg
        | Some state' ->
            Res_list.return
              { state = state'; preds; wands; pred_defs; variants })

  and produce_asrt_list (astate : t) (subst : SVal.SESubst.t) (sas : Asrt.t) :
      (t, err_t) Res_list.t =
    let open Res_list.Syntax in
    let other_state_err msg = Res_list.error_with (StateErr.EOther msg) in
    let () =
      SVal.SESubst.iter subst (fun v value ->
          SVal.SESubst.put subst v (State.simplify_val astate.state value))
    in
    let** { state; preds; wands; pred_defs; variants } =
      List.fold_left
        (fun intermediate_states asrt ->
          let** intermediate_state = intermediate_states in
          try produce_assertion intermediate_state subst asrt
          with e ->
            let admissible =
              State.assume_a ~time:"Produce: final check" ~matching:true
                intermediate_state.state [ True ]
            in
            if !Config.delay_entailment && Option.is_none admissible then (
              L.verbose (fun fmt ->
                  fmt
                    "Production exception due to delayed entailment, survived.");
              other_state_err "Production Exception")
            else raise e)
        (Res_list.return astate) sas
    in
    let state, preds, wands =
      (State.copy state, Preds.copy preds, Wands.copy wands)
    in
    let admissible =
      L.verbose (fun fmt -> fmt "Produce: final check");
      try
        State.assume_a ~time:"Produce: final check" ~matching:true state
          [ True ]
      with _ -> None
    in
    L.verbose (fun fmt -> fmt "Concluded final check");
    match admissible with
    | None -> other_state_err "final state non admissible"
    | Some state -> Res_list.return { state; preds; pred_defs; wands; variants }

  let produce (astate : t) (subst : SVal.SESubst.t) (a : Asrt.t) :
      (t, err_t) Res_list.t =
    L.verbose (fun m ->
        m
          "@[-----------------@\n\
           -----------------@\n\
           Produce assertion: @[%a@]@]" Asrt.pp a);
    let sas = MP.simplify_asrts a in
    produce_asrt_list astate subst sas

  let produce_posts (state : t) (subst : SVal.SESubst.t) (asrts : Asrt.t list) :
      t list =
    let open Syntaxes.List in
    L.verbose (fun m ->
        m
          "@[<v 2>Produce posts: There are %d postconditions to produce. And \
           here they are:@\n\
           %a@]"
          (List.length asrts)
          Fmt.(list ~sep:(any "@\n") Asrt.pp)
          asrts);
    let* asrt = asrts in
    let subst = SVal.SESubst.copy subst in
    let state = copy_astate state in
    produce state subst asrt
    |> List.filter_map (function
         | Error err ->
             L.verbose (fun m -> m "Warning: %a" pp_err_t err);
             None (* Ignoring errors *)
         | Ok state ->
             SVal.SESubst.iter subst (fun e v ->
                 match e with
                 | PVar x -> ignore (update_store state x v)
                 | _ -> ());
             Some state)

  let complete_subst (subst : SVal.SESubst.t) (lab : string * SS.t) : unit =
    let _, existentials = lab in
    SS.iter
      (fun x ->
        let lvar = Expr.LVar x in
        if not (SVal.SESubst.mem subst lvar) then
          SVal.SESubst.put subst lvar lvar)
      existentials

  (** [extend_subts_with_bindings unfold_info pred state subst] takes:
      - A state
      - A substitution
      - A list of pairs of lvar names
      And extends the substitution with the pairs
      [(#y, eval_expr #x)] for each pair [(x, y)] *)
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

  let resource_fail = Res_list.error_with (StateErr.EAsrt ([], True, []))

  (* WARNING: At the moment, unfold behaves over-approximately, it will return only success of only error.
     We only use unfold and fold in OX mode right now, and we don't quite know the meaning of UX fold/unfold. *)
  let rec unfold
      ?(additional_bindings = [])
      (astate : t)
      (pname : string)
      (args : Expr.t list) : (SVal.SESubst.t * t, err_t) Res_list.t =
    let pred = MP.get_pred_def astate.pred_defs pname in
    let params = List.map (fun (x, _) -> Expr.PVar x) pred.pred.pred_params in

    let open Res_list.Syntax in
    let** { state; preds; wands; pred_defs; variants } =
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
          "Combine going to explode. PredName: @[<h>%s@]. Params: @[<h>%a]. \
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
          let state' = State.add_spec_vars state new_spec_vars in
          let astate = { state = state'; preds; wands; pred_defs; variants } in
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
      (t, string) Res_list.t =
    L.verbose (fun m ->
        m "@[<v 2>Starting fold_guarded_with_vals: @[<h>%a@]@\n%a.@\n"
          Fmt.(list ~sep:comma Expr.pp)
          vs pp_astate astate);
    if !Config.manual_proof then Res_list.error_with "Manual proof"
    else
      match select_guarded_predicate_to_fold astate vs with
      | Some (pname, v_args) ->
          L.verbose (fun m -> m "FOUND STH TO FOLD: %s!!!!\n" pname);
          let pred = MP.get_pred_def astate.pred_defs pname in
          let rets =
            fold ~in_matching:true ~match_kind:Fold ~state:(copy_astate astate)
              pred v_args
          in
          Res_list.map_error
            (fun _ -> "fold_guarded_with_vals: Failed to fold")
            rets
      | None ->
          L.verbose (fun m -> m "No predicate found to fold!");
          Res_list.error_with "No predicate found to fold!"

  and unfold_with_vals
      ~(auto_level : [ `High | `Low ])
      (astate : t)
      (vs : Expr.t list) : (SVal.SESubst.t * t) list option =
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
          let only_errors =
            List.filter_map
              (function
                | Error e -> Some e
                | _ -> None)
              rets
          in
          match only_errors with
          | [] ->
              L.verbose (fun m ->
                  m "Unfold complete: %s(@[<h>%a@]): %d" pname
                    Fmt.(list ~sep:comma Expr.pp)
                    v_args (List.length rets));
              let only_successes =
                List.filter_map
                  (function
                    | Ok x -> Some x
                    | _ -> None)
                  rets
              in
              Some only_successes
          | _ :: _ ->
              L.verbose (fun m ->
                  m "Unfolding failed in unfold_with_vals: %a"
                    Fmt.(list ~sep:(any "\n") pp_err_t)
                    only_errors);
              None)
      | None ->
          L.verbose (fun m -> m "NOTHING TO UNFOLD!!!!\n");
          None

  and consume_wand
      ~fold_outs_info
      (astate : t)
      (subst : SVal.SESubst.t)
      (wand : Wands.wand) =
    let open Res_list.Syntax in
    L.verbose (fun m -> m "Matching wand assertion");
    (* We start by building the query *)
    let** query =
      let query_opt =
        Wands.make_query ~pred_defs:astate.pred_defs
          ~subst:(subst_in_expr_opt astate subst)
          wand
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
    match
      Wands.consume_wand ~pred_defs:astate.pred_defs ~semantic_eq astate.wands
        query
    with
    | Some wand -> (
        (* The wand was found *)
        L.verbose (fun m ->
            m "Returning the following wand (before checking outs equality): %a"
              Wands.pp_wand wand);
        let _, wand_outs =
          Wands.wand_ins_outs ~pred_defs:astate.pred_defs wand
        in
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
          match_ins_outs_lists astate.state subst step wand_outs les_outs
        with
        | Success new_state -> Res_list.return { astate with state = new_state }
        | Abort fail_pf ->
            let error =
              StateErr.EAsrt ([], Not fail_pf, [ [ Pure fail_pf ] ])
            in
            Res_list.error_with error
        | Vanish -> Res_list.vanish)
    | None ->
        L.verbose (fun m ->
            m "Could not find any match for the required wand!!!");
        Res_list.error_with (StateErr.EPure False)

  (** Consumes a predicate from the state.
      If the predicate is not "verbatim" in our set of preds,
      and it is not abstract and we are not in manual mode,
      we attempt to fold it. *)
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

    let { state; preds; wands; pred_defs; variants } = astate in
    let pred = MP.get_pred_def pred_defs pname in
    let pred_def = pred.pred in
    let pred_pure = pred_def.pred_pure in
    (* we attempt to consume the pred as-is from our state. *)
    match
      Preds.consume_pred ~maintain:pred_pure preds pname vs
        (Containers.SI.of_list pred_def.pred_ins)
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
            match match_ins_outs_lists state subst step vs les_outs with
            | Success new_state ->
                Res_list.return
                  ({ state = new_state; wands; preds; pred_defs; variants }, vs)
            | Abort fail_pf ->
                let error =
                  StateErr.EAsrt ([], Not fail_pf, [ [ Pure fail_pf ] ])
                in
                Res_list.error_with error
            | Vanish -> Res_list.vanish))
    | None
      when (not !Config.manual_proof)
           && (not pred_def.pred_abstract)
           && not no_auto_fold ->
        (* Recursive Case - Folding required *)
        (* The predicate will be folded (if possible) and then removed from the state.
           Interestingly, if the predicate has a guard, this will produce it but not remove it. *)
        let () =
          L.verbose (fun fmt ->
              fmt "Auto-folding predicate: %s\n" pred.pred.pred_name)
        in
        L.verbose (fun m -> m "Recursive case - attempting to fold.");

        let open Res_list.Syntax in
        let vs_ins = Pred.in_args pred.pred vs in
        let vs_ins = List.map Option.get vs_ins in
        let** folded =
          fold ~in_matching:true ~state:astate ~match_kind:Fold pred vs_ins
        in
        (* Supposedly, we don't need a guard to make sure we're not looping indefinitely:
           if the fold worked, then consume_pred should not take this branch on the next try.
           We should still be keeping an eye on this in case something loops indefinitely. *)
        consume_pred ~no_auto_fold ?fold_outs_info ~in_matching folded pname vs
    | _ ->
        let values = List.filter_map Fun.id vs in
        (* The `False` as second parameter is required for the fixing mechanism to trigger *)
        Res_list.error_with (StateErr.EAsrt (values, False, []))

  and match_ins_outs_lists
      (state : State.t)
      (subst : SVal.SESubst.t)
      (step : MP.step)
      (vos : Expr.t list)
      (eos : Expr.t list) : cons_pure_result =
    let ( let+ ) x f = List.map f x in
    let outs = snd step in
    L.verbose (fun fmt ->
        fmt "Outs: %a"
          Fmt.(
            brackets
              (list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.full_pp))))
          outs);
    L.verbose (fun fmt ->
        fmt "Obtained values: %a" Fmt.(brackets (list ~sep:semi Expr.pp)) vos);
    L.verbose (fun fmt ->
        fmt "Obtained exprs: %a" Fmt.(brackets (list ~sep:semi Expr.pp)) eos);
    (* Substitution of the program variables *)
    let pvar_subst_bindings =
      List.mapi (fun i v -> (Expr.PVar (string_of_int i), v)) vos
    in
    let pvar_subst = SVal.SESubst.init pvar_subst_bindings in
    L.verbose (fun fmt -> fmt "Parameter subst\n%a" SVal.SESubst.pp pvar_subst);
    let outs : MP.outs option =
      try
        Some
          (List.map
             (fun (u, e) ->
               let se = SVal.SESubst.subst_in_expr pvar_subst ~partial:true e in
               (* let se = SVal.SESubst.subst_in_expr subst ~partial:true se in *)
               (u, try Reduction.reduce_lexpr ~matching:true se with _ -> se))
             outs)
      with _ -> None
    in
    match outs with
    | None -> Abort True
    | Some outs -> (
        L.verbose (fun fmt ->
            fmt "Substed outs: %a"
              Fmt.(
                brackets
                  (list ~sep:semi
                     (parens (pair ~sep:comma Expr.pp Expr.full_pp))))
              outs);
        List.iter (fun (u, v) -> SVal.SESubst.put subst u v) outs;
        let eos =
          let+ e = eos in
          match SVal.SESubst.subst_in_expr_opt subst e with
          | None ->
              let msg =
                Fmt.str
                  "INTERNAL ERROR: Not all ins known, I don't know this one: %a"
                  Expr.full_pp e
              in
              L.fail msg
          | Some e -> e
        in

        try
          List.fold_left2
            (fun ac vd od ->
              match ac with
              | Abort _ | Vanish -> ac
              | Success state ->
                  let pf : Formula.t = Eq (vd, od) in
                  cons_pure state pf)
            (Success state) vos eos
        with Invalid_argument _ ->
          Fmt.failwith "Invalid amount of args for the following MP step : %a"
            MP.pp_step step)

  and match_assertion
      ?(no_auto_fold = false)
      (astate : t)
      (subst : SVal.SESubst.t)
      (step : MP.step) : (t, err_t) Res_list.t =
    (* Auxiliary function for actions and predicates, with indexed outs *)
    let { state; wands; preds; pred_defs; variants } = astate in

    let assertion_loggable =
      if L.Mode.enabled () then
        Some
          (let a = fst step in
           (* Get pvars, lvars, locs from the assertion *)
           let a_pvars, a_lvars, a_locs =
             (Asrt.pvars [ a ], Asrt.lvars [ a ], Asrt.locs [ a ])
           in
           let filter_vars = SS.union a_pvars (SS.union a_lvars a_locs) in

           (* From the subst, we take any pair that has any of those and collect
              the pvars, lvars, and alocs, from their values *)
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
             { step; subst; astate = AstateRec.from astate })
      else None
    in

    L.Parent.with_specific assertion_loggable
      L.Logging_constants.Content_type.assertion (fun () ->
        let p, outs = step in
        let open Res_list.Syntax in
        match (p : Asrt.simple) with
        | CorePred (a_id, e_ins, e_outs) -> (
            let vs_ins = List.map (subst_in_expr_opt astate subst) e_ins in
            let failure = List.exists (fun x -> x = None) vs_ins in
            if failure then (
              Fmt.pr "I don't know all ins for %a????" Asrt.pp_simple p;
              if !Config.under_approximation then [] else resource_fail)
            else
              let vs_ins = List.map Option.get vs_ins in
              L.verbose (fun m ->
                  m "Executing consume: %s with ins: @[<h>%a@]" a_id
                    Fmt.(list ~sep:comma Expr.pp)
                    vs_ins);
              let** state'', vs_outs =
                State.consume_core_pred a_id state vs_ins
              in
              (* Separate outs into direct matchables and others*)
              match match_ins_outs_lists state'' subst step vs_outs e_outs with
              | Success state''' ->
                  Res_list.return
                    { state = state'''; preds; wands; pred_defs; variants }
              | Abort fail_pf ->
                  let error =
                    StateErr.EAsrt ([], Not fail_pf, [ [ Pure fail_pf ] ])
                  in
                  Res_list.error_with error
              | Vanish -> Res_list.vanish)
        | Pred (pname, les) ->
            L.verbose (fun m -> m "Matching predicate assertion");
            (* Perform substitution in all predicate parameters *)
            L.verbose (fun fmt ->
                fmt "ARGS: %a" Fmt.(list ~sep:comma Expr.pp) les);
            L.verbose (fun fmt -> fmt "SUBST:\n%a" SVal.SESubst.pp subst);
            let vs = List.map (subst_in_expr_opt astate subst) les in
            (* Get the ins of the predicate *)
            let pred = MP.get_pred_def pred_defs pname in
            let pred_def = pred.pred in
            let vs_ins = Pred.in_args pred_def vs in
            let les_outs = Pred.out_args pred_def les in
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
              let () =
                match consume_pred_res with
                | [] ->
                    let msg = "CONSUME_PRED VANISHED! MEDOOOOOO!!!!!" in
                    L.verbose ~severity:Warning (fun m -> m "%s" msg);
                    if not !Config.under_approximation then
                      Fmt.failwith "%s\n@?" msg
                    else ()
                | _ -> ()
              in
              let++ astate', _ = consume_pred_res in
              astate'
        | Wand { lhs; rhs } ->
            if !Config.under_approximation then L.fail "Wand in under-approx";
            let les_outs =
              let pred = (MP.get_pred_def pred_defs (fst rhs)).pred in
              Pred.out_args pred (snd rhs)
            in
            let fold_outs_info = (subst, step, les_outs) in
            consume_wand ~fold_outs_info astate subst { lhs; rhs }
        (* Conjunction should not be here *)
        | Pure (Formula.And _) ->
            raise (Failure "Match assertion: And: should have been reduced")
        (* Other pure assertions *)
        | Pure f -> (
            let discharges =
              List.fold_left
                (fun discharges (u, out) ->
                  let open Syntaxes.Result in
                  let* discharges = discharges in
                  (* Perform the substitution in the out *)
                  let* out =
                    SVal.SESubst.subst_in_expr_opt subst out
                    |> Result_utils.of_option ~none:()
                  in
                  (* And add to e-subst *)
                  match SVal.SESubst.get subst u with
                  | None ->
                      SVal.SESubst.put subst u out;
                      Ok discharges
                  | Some out' when Expr.equal out out' -> Ok discharges
                  | Some out' ->
                      let new_discharge = Formula.Eq (out, out') in
                      Ok (new_discharge :: discharges))
                (Ok []) outs
            in
            let discharges =
              match discharges with
              | Error () ->
                  Fmt.failwith
                    "INTERNAL ERROR: Matching failure: do not know all ins for \
                     %a"
                    Formula.pp f
              | Ok discharges -> discharges
            in
            (* To match a pure formula we must know all ins *)
            let opf = SVal.SESubst.substitute_in_formula_opt subst f in
            match opf with
            | None ->
                Fmt.failwith "Matching failure: do not know all ins for %a"
                  Formula.pp f
            | Some pf -> (
                let discharges_pf =
                  List.fold_left Formula.Infix.( #&& ) True discharges
                in
                let discharges_pf =
                  Reduction.reduce_formula ~matching:true discharges_pf
                in
                let to_asrt = Formula.Infix.( #&& ) pf discharges_pf in
                match cons_pure state to_asrt with
                | Success new_state ->
                    Res_list.return
                      { state = new_state; preds; wands; pred_defs; variants }
                | Vanish -> Res_list.vanish
                | Abort _ ->
                    let vs = State.unfolding_vals state [ pf ] in
                    let error = StateErr.EAsrt (vs, Not pf, [ [ Pure pf ] ]) in
                    Res_list.error_with error))
        | Types les -> (
            let corrections =
              List.fold_left
                (fun (ac : Formula.t list) (le, t) ->
                  let v_le = (subst_in_expr_opt astate subst) le in
                  let v_le : Expr.t =
                    match v_le with
                    | Some v_le -> v_le
                    | None -> raise (Failure "DEATH. match assertion Types")
                  in
                  match State.get_type state v_le with
                  | Some t' -> if not (Type.equal t t') then False :: ac else ac
                  | None -> Eq (UnOp (TypeOf, v_le), Lit (Type t)) :: ac)
                [] les
            in

            match corrections with
            | [] -> Res_list.return astate
            | _ ->
                if !Config.under_approximation then
                  (* In under-approx we try to assume the types hold*)
                  match State.assume_a ~matching:true state corrections with
                  | None -> Res_list.vanish
                  | Some state' ->
                      Res_list.return
                        { state = state'; wands; preds; pred_defs; variants }
                else
                  let les, _ = List.split les in
                  let les =
                    List.filter_map (subst_in_expr_opt astate subst) les
                  in
                  let conjunct = Formula.conjunct corrections in
                  let error =
                    StateErr.EAsrt (les, Not conjunct, [ [ Pure conjunct ] ])
                  in
                  Res_list.error_with error)
        (* LTrue, LFalse, LEmp *)
        | _ -> raise (Failure "Illegal Assertion in Matching Plan"))

  and match_assertion_safely ?(no_auto_fold = false) state subst step =
    try match_assertion ~no_auto_fold state subst step
    with err -> (
      L.verbose (fun m ->
          m
            "WARNING: UNCAUGHT EXCEPTION IN MATCH ASSERTION: %s@\n\
             Here's the backtrace: %s" (Printexc.to_string err)
            (Printexc.get_backtrace ()));
      if !Config.under_approximation then
        let () =
          L.verbose (fun m -> m "UX mode: vanishing despite exception!")
        in
        Res_list.vanish
      else
        match fst step with
        | Pure pf ->
            let { state = bstate; _ } = state in
            let vs = State.unfolding_vals bstate [ pf ] in
            Res_list.error_with (StateErr.EAsrt (vs, Not pf, [ [ Pure pf ] ]))
        | _ ->
            let other_error =
              StateErr.EOther
                (Fmt.str "Uncaught exception while matching assertions %a"
                   Asrt.pp_simple (fst step))
            in
            Res_list.error_with other_error)

  and match_mp' (parent_ids : L.Report_id.t list ref) (s_states : search_state')
      : internal_mp_u_res =
    let s_states, errs_so_far = s_states in
    L.verbose (fun m ->
        m "Match MP: There are %d states left to consider."
          (List.length s_states));
    let explore_next_states = match_mp' parent_ids in
    let ux = !Config.under_approximation in
    match s_states with
    | [] ->
        (* There are no more states to explore: in OX, it means we failed to match, in UX, it means there are not valid path we know about, we vanish. *)
        if ux then List_res.vanish else Error errs_so_far
    | ((astate, subst, mp), target_case_depth, is_new_case)
      :: rest_search_states -> (
        let case_depth =
          structure_match_case_reports parent_ids target_case_depth is_new_case
            astate subst mp
        in
        match mp with
        | LabelStep (label, rest_mp) ->
            L.verbose (fun m ->
                m
                  "Reached LabelStep, about to complete substitution with \
                   vars: %a"
                  Fmt.(Dump.iter SS.iter nop string)
                  (snd label));
            complete_subst subst label;
            let current_state = ((astate, subst, rest_mp), case_depth, false) in
            explore_next_states
              (current_state :: rest_search_states, errs_so_far)
        | Choice (left_mp, right_mp) ->
            L.verbose (fun m ->
                m "Reached a choice with 2 MPs: about to branch");
            let astate_copy = copy_astate astate in
            let subst_copy = SVal.SESubst.copy subst in
            let left_state = ((astate, subst, left_mp), case_depth + 1, true) in
            let right_state =
              ((astate_copy, subst_copy, right_mp), case_depth + 1, true)
            in
            explore_next_states
              (left_state :: right_state :: rest_search_states, errs_so_far)
        | Finished posts ->
            (* We're done with matching of this case.
               In OX, we may stop, as we proved implication.
               In UX, we explore more as to extend coverage. *)
            let remaining_states =
              List.map
                (fun ((astate, subst, mp), _, _) ->
                  MatchCaseReport.{ astate = AstateRec.from astate; subst; mp })
                rest_search_states
            in
            ignore
            @@ MatchResultReport.log
                 (Success
                    {
                      remaining_states;
                      astate = AstateRec.from astate;
                      subst;
                      posts;
                    });
            if ux then
              let other_paths =
                match explore_next_states (rest_search_states, errs_so_far) with
                | Error _ -> failwith "match_mp' failed in UX!"
                | Ok other_paths -> other_paths
              in
              Ok ((astate, subst, posts) :: other_paths)
            else List_res.return (astate, subst, posts)
        | ConsumeStep (step, rest_mp) -> (
            let successes, errors =
              match_assertion_safely astate subst step
              |> List.partition_map (function
                   | Ok x -> Left x
                   | Error x -> Right x)
            in
            match (!Config.under_approximation, successes, errors) with
            (* We start by handling the crash cases that should never happen *)
            | true, _, _ :: _ ->
                L.fail "ERROR: IMPOSSIBLE! MATCHING ERRORS IN UX MODE!!!!"
            | false, [], [] ->
                L.fail "OX MATCHING VANISHED??? MEDOOOOOOOO!!!!!!!!!"
            | false, _ :: _ :: _, [] -> L.fail "DEATH. OX MATCHING BRANCHED"
            | true, [], _ ->
                (* Vanished in UX *)
                explore_next_states (rest_search_states, errs_so_far)
            | false, _, _ :: _ ->
                (* Matching failed in OX. We try the next case *)
                ignore
                @@ MatchResultReport.log
                     (Failure
                        {
                          astate = AstateRec.from astate;
                          cur_step = Some step;
                          subst;
                          errors;
                        });
                explore_next_states (rest_search_states, errors @ errs_so_far)
            | _, [ state ], [] ->
                explore_next_states
                  ( ((state, subst, rest_mp), case_depth, false)
                    :: rest_search_states,
                    errs_so_far )
            | true, first :: rem, [] ->
                let rem =
                  List.map
                    (fun state ->
                      ( (state, SVal.SESubst.copy subst, rest_mp),
                        case_depth,
                        false ))
                    rem
                in
                explore_next_states
                  ( ((first, subst, rest_mp), case_depth, false) :: rem,
                    errs_so_far )))

  and match_mp (s_states : search_state) : internal_mp_u_res =
    let parent_ids = ref [] in
    let s_states =
      let states, errs = s_states in
      let states = states |> List.map (fun state -> (state, 0, false)) in
      (states, errs)
    in
    let res = match_mp' parent_ids s_states in
    List.iter (fun parent_id -> L.Parent.release (Some parent_id)) !parent_ids;
    res

  and match_
      ?(in_matching = false)
      (astate : t)
      (subst : SVal.SESubst.t)
      (mp : MP.t)
      (match_kind : match_kind) :
      (t * SVal.SESubst.t * post_res, err_t) Res_list.t =
    let astate_i = copy_astate astate in
    let subst_i = SVal.SESubst.copy subst in
    let can_fix errs = List.exists State.can_fix errs in

    let rec handle_ret ~try_recover ret =
      match ret with
      | Ok successes ->
          L.verbose (fun fmt -> fmt "Matcher.match_: Success (possibly empty)");
          Res_list.just_oks successes
      | Error errs
        when try_recover && !Config.unfolding
             && Exec_mode.is_verification_exec !Config.current_exec_mode
             && (not in_matching) && can_fix errs -> (
          L.verbose (fun fmt -> fmt "Matcher.match_: Failure");
          if !Config.under_approximation then
            L.fail "MATCHING ABORTED IN UX MODE???";
          let { state; _ } = astate_i in
          let tactics = State.get_recovery_tactic state errs in
          L.verbose (fun m ->
              m
                "Match. Unable to match. About to attempt the following \
                 recovery tactic:\n\
                 %a"
                (Recovery_tactic.pp Expr.pp)
                tactics);
          match try_recovering astate_i tactics with
          | Error msg ->
              L.normal (fun m -> m "Match. Recovery tactic failed: %s" msg);
              Res_list.just_errors errs
          | Ok sp -> (
              L.verbose (fun m ->
                  m "Unfolding successful: %d results" (List.length sp));
              let open Syntaxes.List in
              let* astate = sp in
              match unfold_concrete_preds astate with
              | None ->
                  let error =
                    StateErr.EOther "Unfolding concrete value failed???"
                  in
                  Res_list.error_with error
              | Some (_, astate) ->
                  (* let subst'' = compose_substs (Subst.to_list subst_i) subst (Subst.init []) in *)
                  let subst'' = SVal.SESubst.copy subst_i in
                  let new_ret = match_mp ([ (astate, subst'', mp) ], []) in
                  (* We already tried recovering once and it failed, we stop here *)
                  handle_ret ~try_recover:false new_ret))
      | Error errors ->
          L.verbose (fun fmt -> fmt "Matcher.match: Failure");
          Res_list.just_errors errors
    in
    MatchReport.as_parent
      { astate = AstateRec.from astate; subst; mp; match_kind }
      (fun () ->
        let ret = match_mp ([ (astate, subst, mp) ], []) in
        handle_ret ~try_recover:true ret)

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
    let () =
      match match_result with
      | [] ->
          Fmt.(
            failwith "@[<h>HORROR: fold vanished for %s(%a) with bindings: %a@]"
              pred_name (Dump.list Expr.pp) args
              (Dump.list @@ Dump.pair Expr.pp Expr.pp)
              additional_bindings)
      | _ -> ()
    in
    let open Res_list.Syntax in
    let** astate', subst', _ = match_result in
    let { preds = preds'; _ } = astate' in
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
    let { preds; pred_defs; _ } = astate in

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
      (t list, string) result =
    let open Syntaxes.Result in
    if !Config.under_approximation then
      L.fail "Recovery tactics not handled in UX mode";
    L.verbose (fun m -> m "Attempting to recover");
    let- fold_error =
      match tactic.try_fold with
      | Some fold_values -> (
          let res = fold_guarded_with_vals astate fold_values in
          let errors =
            List.filter_map
              (function
                | Error e -> Some e
                | _ -> None)
              res
          in
          match errors with
          | [] ->
              let successes =
                List.filter_map
                  (function
                    | Ok x -> Some x
                    | _ -> None)
                  res
              in
              Ok successes
          | _ ->
              let error_string = Fmt.str "%a" Fmt.(Dump.list string) errors in
              Error error_string)
      | None ->
          L.verbose (fun m -> m "No fold recovery tactic");
          Error "None"
    in
    let- unfold_error =
      (* This matches the legacy behaviour *)
      let unfold_values = Option.value ~default:[] tactic.try_unfold in
      match unfold_with_vals ~auto_level:`High astate unfold_values with
      | None -> Error "Automatic unfold failed"
      | Some next_states ->
          Ok (List.map (fun (_, astate) -> astate) next_states)
    in
    Fmt.error "try_fold: %s\ntry_unfold: %s" fold_error unfold_error

  let rec rec_unfold
      ?(fuel = 10)
      (astate : t)
      (pname : string)
      (args : Expr.t list) : (t, err_t) Res_list.t =
    if fuel = 0 then failwith "RECURSIVE UNFOLD: OUT OF FUEL"
    else
      let open Res_list.Syntax in
      let** _, astate = unfold astate pname args in
      let { preds; _ } = astate in
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
        [ [ Asrt.Pred (pred.pred_name, largs) ] ]
      else
        let unfolded_pred =
          Hashtbl.find_opt LogicPreprocessing.unfolded_preds pred.pred_name
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
      (* We are in OX mode, matching must not branch. If it does, something is very wrong.
         Mainly because the substitution is performed in place.
         This function simplifies the return type of match-assertion: it returns a single outcome if it's a success. *)
      let res = match_assertion_safely ~no_auto_fold:true astate subst step in
      let successes, errors = Res_list.split res in
      match (successes, errors) with
      | [ x ], [] -> Ok x
      | [], errs -> Error errs
      | _ ->
          Fmt.failwith
            "Impossible: match-assertion branched in OX mode: %d successes and \
             %d errors"
            (List.length successes) (List.length errors)

    let make_pred_ins_table pred_tbl =
      let tbl = Hashtbl.create (Hashtbl.length pred_tbl) in
      Hashtbl.iter
        (fun pname pred -> Hashtbl.add tbl pname pred.MP.pred.pred_ins)
        pred_tbl;
      tbl

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
      | (Pred (name, args), _), _ ->
          let MP.{ pred; def_mp; _ } = MP.get_pred_def astate.pred_defs name in
          let* () =
            if pred.pred_abstract || Option.is_some pred.pred_guard then None
            else Some ()
          in
          let in_params =
            Pred.in_params pred |> List.map (fun x -> Expr.PVar x)
          in
          let in_args =
            Pred.in_args pred args
            |> List.map (SVal.SESubst.subst_in_expr_opt subst)
            |> List.map Option.get
          in
          let init_subst =
            List.combine in_params in_args |> SVal.SESubst.init
          in
          let out_params = Pred.out_params pred in
          let out_args = Pred.out_args pred args in
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
                let open Formula.Infix in
                Asrt.Pure
                  (Expr.PVar old_out) #== (subst_in_expr pvar_subst new_out))
              out_params new_outs_learn
          in
          let atoms = List.rev_append new_cps learning_equalities in
          let mp =
            let steps =
              MP.s_init_atoms
                ~preds:(make_pred_ins_table astate.pred_defs)
                kb atoms
              |> Result.get_ok
            in
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
          let equality = Formula.Eq (vd, od) in
          if
            State.assert_a state.lhs_state.state [ equality ]
            || State.assert_a state.current_state.state [ equality ]
          then Ok acc
          else
            Error
              [
                StateErr.EAsrt
                  ([], Formula.Infix.fnot equality, [ [ Pure equality ] ]);
              ])
        (Ok state) obtained expected

    let rec package_case_step
        { lhs_state; current_state; subst }
        (step : MP.step) : (package_state list, err_t list) Result.t =
      let open Syntaxes.Result in
      L.verbose (fun m ->
          m "Wand about to consume RHS step: %a" Asrt.pp_simple (fst step));
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
      let lhs_states =
        make_lhs_states
          ~empty_state:(clear_resource (copy_astate astate))
          ~pred_defs:astate.pred_defs wand.lhs
      in
      let rpred = MP.get_pred_def astate.pred_defs (fst wand.rhs) in
      (* let lpred = MP.get_pred_def astate.pred_defs (fst wand.lhs) in *)
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
