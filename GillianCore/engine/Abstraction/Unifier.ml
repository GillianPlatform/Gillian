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

  type unfold_info_t = (string * string) list

  val produce_assertion : t -> st -> Asrt.t -> (t, err_t) Res_list.t
  val produce : t -> st -> Asrt.t -> (t, err_t) Res_list.t
  val produce_posts : t -> st -> Asrt.t list -> t list

  val unfold :
    ?additional_bindings:unfold_info_t ->
    t ->
    string ->
    vt list ->
    (st * t, err_t) Res_list.t

  val rec_unfold : ?fuel:int -> t -> string -> vt list -> (t, err_t) Res_list.t
  val unfold_all : t -> string -> (t, err_t) Res_list.t
  val try_recovering : t -> vt Recovery_tactic.t -> (t list, string) result
  val unfold_with_vals : t -> vt list -> (st * t) list option
  val unfold_concrete_preds : t -> (st option * t) option

  val unify_assertion :
    ?is_post:bool -> t -> st -> UP.step -> (t, err_t) Res_list.t

  val unify :
    ?is_post:bool ->
    ?in_unification:bool ->
    t ->
    st ->
    UP.t ->
    unify_kind ->
    (t * st * post_res, err_t) Res_list.t

  val fold :
    ?is_post:bool ->
    ?in_unification:bool ->
    ?additional_bindings:(Expr.t * vt) list ->
    unify_kind:unify_kind ->
    state:t ->
    UP.pred ->
    vt list ->
    (t, err_t) Res_list.t

  val consume_pred :
    ?is_post:bool ->
    ?in_unification:bool ->
    t ->
    string ->
    vt option list ->
    (st * UP.step * UP.outs * Expr.t list) option ->
    (t * vt list, err_t) Res_list.t
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
     and type err_t = State.err_t = struct
  open Literal
  open Containers
  module L = Logging
  module Val = Val
  module ESubst = ESubst

  type vt = Val.t
  type st = ESubst.t [@@deriving yojson]
  type state_t = State.t [@@deriving yojson]
  type preds_t = Preds.t [@@deriving yojson]
  type abs_t = string * vt list
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type err_t = State.err_t [@@deriving yojson, show]
  type t = state_t * preds_t * UP.preds_tbl_t * variants_t
  type post_res = (Flag.t * Asrt.t list) option
  type s_state = t * st * UP.t
  type search_state = s_state list * err_t list
  type search_state' = (s_state * int * bool) list * err_t list
  type unfold_info_t = (string * string) list

  (* This is mostly to do with Gillian legacy.
     We have to handle UX and OX separately, or otherwise
     we'd have to refactor the entirety of the code... *)
  type internal_up_u_res =
    | USuccess of (t * st * post_res) list
    | UAbort of err_t list
    | UVanish

  module Logging = struct
    let pp_variants : (string * Expr.t option) Fmt.t =
      Fmt.pair ~sep:Fmt.comma Fmt.string (Fmt.option Expr.pp)

    let pp_astate fmt astate =
      let state, preds, _, variants = astate in
      Fmt.pf fmt "%a@\nPREDS:@\n%a@\nVARIANTS:@\n%a@\n" State.pp state Preds.pp
        preds
        (Fmt.hashtbl ~sep:Fmt.semi pp_variants)
        variants

    let pp_astate_by_need (pvars : SS.t) (lvars : SS.t) (locs : SS.t) fmt astate
        =
      let state, preds, _, variants = astate in
      Fmt.pf fmt "%a@\n@\nPREDS:@\n%a@\nVARIANTS:@\n%a@\n"
        (State.pp_by_need pvars lvars locs)
        state Preds.pp preds
        (Fmt.hashtbl ~sep:Fmt.semi pp_variants)
        variants

    module AstateRec = struct
      type t = { state : state_t; preds : preds_t; variants : variants_t }
      [@@deriving yojson]

      let from (state, preds, _, variants) = { state; preds; variants }

      let pp_custom pp_astate fmt { state; preds; variants } =
        pp_astate fmt (state, preds, (), variants)

      let pp = pp_custom pp_astate
    end

    module AssertionReport = struct
      type t = { step : UP.step; subst : ESubst.t; astate : AstateRec.t }
      [@@deriving yojson]

      let pp_custom pp_astate pp_subst fmt { step; subst; astate } =
        Fmt.pf fmt
          "Unify assertion: @[<h>%a@]@\nSubst:@\n%a@\n@[<v 2>STATE:@\n%a@]"
          UP.step_pp step pp_subst subst
          (AstateRec.pp_custom pp_astate)
          astate

      let to_loggable pp_astate pp_subst =
        L.Loggable.make (pp_custom pp_astate pp_subst) of_yojson to_yojson
    end

    module UnifyReport = struct
      type t = {
        astate : AstateRec.t;
        subst : ESubst.t;
        up : UP.t;
        unify_kind : unify_kind;
      }
      [@@deriving yojson]

      let pp fmt _ = Fmt.pf fmt "Unifier.unify: about to unify UP."
      let to_loggable = L.Loggable.make pp of_yojson to_yojson

      let as_parent report f =
        L.Parent.with_specific
          (Some (to_loggable report))
          L.Logging_constants.Content_type.unify f
    end

    module UnifyCaseReport = struct
      type t = { astate : AstateRec.t; subst : st; up : UP.t }
      [@@deriving yojson]

      let to_loggable = L.Loggable.make L.dummy_pp of_yojson to_yojson

      let log report =
        L.Specific.normal (to_loggable report)
          L.Logging_constants.Content_type.unify_case
    end

    module UnifyResultReport = struct
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

      let pp fmt report =
        match report with
        | Success data ->
            Fmt.pf fmt
              "Unifier.unify_up: Unification successful: %d states left"
              (1 + List.length data.remaining_states)
        | Failure { cur_step; subst; astate; errors } ->
            Fmt.pf fmt
              "@[<v 2>WARNING: Unify Assertion Failed: @[<h>%a@] with subst @\n\
               %a in state @\n\
               %a with errors:@\n\
               %a@]"
              Fmt.(option ~none:(any "no assertion - phantom node") UP.step_pp)
              cur_step ESubst.pp subst AstateRec.pp astate
              Fmt.(list ~sep:(any "@\n") State.pp_err)
              errors

      let to_loggable = L.Loggable.make pp of_yojson to_yojson

      let log report =
        L.Specific.normal (to_loggable report)
          L.Logging_constants.Content_type.unify_result
    end

    let structure_unify_case_reports
        parent_ids_ref
        target_case_depth
        is_new_case
        astate
        subst
        up =
      let actual_target_depth =
        if is_new_case then target_case_depth - 1 else target_case_depth
      in
      let case_depth = List.length !parent_ids_ref in
      assert (actual_target_depth <= case_depth + 1);
      for _ = case_depth downto actual_target_depth + 1 do
        match !parent_ids_ref with
        | [] -> raise (Failure "Mismatched case depth and parent_id list!")
        | parent_id :: rest ->
            L.Parent.release (Some parent_id);
            parent_ids_ref := rest
      done;
      if is_new_case then
        let new_parent_id =
          UnifyCaseReport.log { astate = AstateRec.from astate; subst; up }
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

  type handle_pure_result = Success of state_t | Abort of Formula.t | Vanish

  let handle_pure (state : state_t) (f : Formula.t) : handle_pure_result =
    if !Config.under_approximation then
      (* [sat_check_f] returns the model in case of SAT, None otherwise. *)
      match State.assume_a ~unification:true state [ f ] with
      | Some state -> Success state
      | None -> Vanish
    else if State.assert_a state [ f ] then Success state
    else Abort f

  let update_store (astate : t) (x : string) (v : Val.t) : t =
    let state, preds, pred_defs, variants = astate in
    let store = State.get_store state in
    let _ = Store.put store x v in
    let state' = State.set_store state store in
    (state', preds, pred_defs, variants)

  let simplify_astate ?(save = false) ?(unification = false) (astate : t) :
      st * t list =
    let state, preds, pred_defs, variants = astate in
    let subst, states =
      State.simplify ~save ~kill_new_lvars:false ~unification state
    in
    Preds.substitution_in_place subst preds;
    match states with
    | [] -> failwith "Impossible: state substitution returned []"
    | [ state ] -> (subst, [ (state, preds, pred_defs, variants) ])
    | states ->
        ( subst,
          List.map
            (fun state ->
              (state, Preds.copy preds, pred_defs, Hashtbl.copy variants))
            states )

  let copy_astate (astate : t) : t =
    let state, preds, pred_defs, variants = astate in
    (State.copy state, Preds.copy preds, pred_defs, Hashtbl.copy variants)

  let subst_in_expr_opt (astate : t) (subst : st) (e : Expr.t) : vt option =
    let state, _, _, _ = astate in
    let v =
      Option.fold ~some:Val.from_expr ~none:None
        (ESubst.subst_in_expr_opt subst e)
    in
    Option.map (State.simplify_val state) v

  let subst_in_expr (subst : ESubst.t) (le : Expr.t) : Val.t option =
    Val.from_expr (ESubst.subst_in_expr subst ~partial:false le)

  module Predicate_selection_strategies = struct
    let print_local_info (i : int) (name : string) (args : Val.t list) : unit =
      L.verbose (fun m ->
          m "Strategy %d: Examining %s(@[<h>%a@])" i name
            Fmt.(list ~sep:comma Val.pp)
            args)

    let get_pred_def ~pred_defs (name : string) : Pred.t =
      match Hashtbl.find_opt pred_defs name with
      | Some pred -> pred.UP.pred
      | None -> failwith "ERROR: get_pred_with_vs: Predicate doesn't exist."

    (* Strategy 1: The values that we are looking for are in the in-parameters *)
    let strategy_1
        ~pred_defs
        ~state
        ~values
        ((name, args) : string * Val.t list) : int =
      print_local_info 1 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let one_level_list_expander args =
        List.concat_map
          (fun (x : vt) ->
            match Val.to_expr x with
            | EList ls -> List.map (fun x -> Option.get (Val.from_expr x)) ls
            | _ -> [ x ])
          args
      in
      let in_args = one_level_list_expander (Pred.in_args pred_def args) in

      L.verbose (fun fmt ->
          fmt "Original values: %a"
            Fmt.(brackets (list ~sep:comma Val.pp))
            values);
      let vs = State.get_equal_values state values in
      let vs =
        vs
        @ List.map Option.get
            (List.map Val.from_expr
               (List.concat_map Expr.base_elements (List.map Val.to_expr vs)))
      in
      let vs = List.sort_uniq compare vs in
      L.verbose (fun fmt ->
          fmt "Extended values: %a" Fmt.(brackets (list ~sep:comma Val.pp)) vs);
      let vs_inter = List_utils.intersect vs in_args in
      let es_inter =
        List.fold_left
          (fun ac e -> Expr.Set.add e ac)
          Expr.Set.empty
          (List.map Val.to_expr vs_inter)
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
    let strategy_2 ~pred_defs ((name, args) : string * Val.t list) : int =
      print_local_info 2 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let in_args = Pred.in_args pred_def args in
      let all_literals =
        List.for_all
          (fun (x : Val.t) ->
            match Val.to_expr x with
            | Lit _ -> true
            | _ -> false)
          in_args
      in
      if all_literals then 1 else 0

    (* Strategy 3: The values that we are looking for are in the out-parameters *)
    let strategy_3 ~pred_defs ~values ((name, args) : string * Val.t list) : int
        =
      print_local_info 3 name args;
      let pred_def = get_pred_def ~pred_defs name in
      let out_args = Pred.out_args pred_def args in
      let vs_inter = List_utils.intersect values out_args in
      let es_inter =
        List.fold_left
          (fun ac e -> Expr.Set.add e ac)
          Expr.Set.empty
          (List.map Val.to_expr vs_inter)
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
    let strategy_4 ~state ((name, args) : string * Val.t list) : int =
      print_local_info 4 name args;
      let lvars_state = State.get_spec_vars state in
      let lvars_args =
        List.fold_left SS.union SS.empty
          (List.map (fun x -> Expr.lvars (Val.to_expr x)) args)
      in
      let inter = SS.inter lvars_args lvars_state in
      SS.cardinal inter
  end

  let consume_pred_with_vs (astate : t) (values : Val.t list) : abs_t option =
    let state, preds, pred_defs, _ = astate in

    let wrap_strategy f (name, args) =
      let pred = Predicate_selection_strategies.get_pred_def ~pred_defs name in
      if pred.pred_abstract then 0 else f (name, args)
    in

    let apply_strategies (strategies : (string * Val.t list -> int) list) :
        (string * Val.t list) option =
      List.find_map (Preds.strategic_choice ~consume:true preds) strategies
    in
    let open Predicate_selection_strategies in
    let strategies =
      [
        strategy_1 ~state ~values ~pred_defs;
        strategy_2 ~pred_defs;
        strategy_3 ~pred_defs ~values;
        strategy_4 ~state;
      ]
    in
    let strategies = List.map wrap_strategy strategies in
    apply_strategies strategies

  let select_guarded_predicate_to_fold (astate : t) (values : Val.t list) :
      abs_t option =
    let state, preds, pred_defs, _ = astate in
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

  let rec produce_assertion (astate : t) (subst : ESubst.t) (a : Asrt.t) :
      (t, err_t) Res_list.t =
    let open Res_list.Syntax in
    let state, preds, pred_defs, variants = astate in
    let other_state_err msg = [ Error (StateErr.EOther msg) ] in

    L.verbose (fun m ->
        m
          "-------------------------@\n\
           Produce simple assertion: @[<h>%a@]@\n\
           With subst: %a\n\
          \           -------------------------@\n"
          Asrt.pp a ESubst.pp subst);

    L.verbose (fun m -> m "STATE: %a" pp_astate astate);

    match a with
    | GA (a_id, ins, outs) ->
        L.verbose (fun fmt -> fmt "Memory action.");
        let setter = State.ga_to_setter a_id in

        let vs = List.map (subst_in_expr subst) (ins @ outs) in
        let failure = List.exists Option.is_none vs in
        if failure then
          failwith
            "Produce Simple Assertion: Subst does not cover the action ins"
        else
          let vs = List.map Option.get vs in
          (* We filter action errors, in theory, production cannot fail, it may only vanish. *)
          State.execute_action ~unification:true setter state vs
          |> List.filter_map (function
               | Ok (state', _) ->
                   Some
                     (Ok
                        ( state',
                          Preds.copy preds,
                          pred_defs,
                          Hashtbl.copy variants ))
               | Error _ -> None)
    | Types les -> (
        L.verbose (fun fmt -> fmt "Types assertion.");
        let state' =
          List.fold_left
            (fun state (le, t) ->
              match state with
              | None -> None
              | Some state -> (
                  let v = subst_in_expr subst le in
                  match v with
                  | None -> None
                  | Some v -> State.assume_t state v t))
            (Some state) les
        in
        match state' with
        | None ->
            other_state_err "Produce Simple Assertion: Cannot produce types"
        | Some _ -> [ Ok (state, preds, pred_defs, variants) ])
    | Pred (pname, les) ->
        L.verbose (fun fmt -> fmt "Predicate assertion.");
        let vs = List.map (subst_in_expr subst) les in
        let failure = List.exists (fun x -> x = None) vs in
        if failure then
          other_state_err
            "Produce Simple Assertion: Subst does not cover the pred ins"
        else
          let vs = List.map Option.get vs in
          let pred_def = Hashtbl.find pred_defs pname in
          let++ state, preds, pred_defs, variants =
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
                        Formula.subst_expr_for_expr ~to_subst:param
                          ~subst_with:le
                      in
                      List.map subst facts)
                    facts params les
                in
                let facts = Asrt.Pure (Formula.conjunct facts) in
                produce_assertion
                  (state, preds, pred_defs, variants)
                  subst facts
          in
          let pure = pred_def.pred.pred_pure in
          let preds = Preds.copy preds in
          let state = State.copy state in
          let variants = Hashtbl.copy variants in
          Preds.extend ~pure preds (pname, vs);
          (state, preds, pred_defs, variants)
    | Pure (Eq (PVar x, le)) | Pure (Eq (le, PVar x)) ->
        L.verbose (fun fmt -> fmt "Pure assertion.");
        if ESubst.mem subst (PVar x) then
          let v_x = ESubst.get subst (PVar x) in
          let v_le = subst_in_expr subst le in
          let opt_res =
            match (v_x, v_le) with
            | Some v_x, Some v_le ->
                Option.map
                  (fun state -> [ Ok (state, preds, pred_defs, variants) ])
                  (State.assume_a ~unification:true
                     ~production:!Config.delay_entailment state
                     [ Eq (Val.to_expr v_x, Val.to_expr v_le) ])
            | _ -> None
          in
          match opt_res with
          | Some r -> r
          | None ->
              other_state_err
                "Produce Simple Assertion: Subst does not cover the pure \
                 formula"
        else
          let++ v =
            match subst_in_expr subst le with
            | Some r -> Res_list.return r
            | None ->
                other_state_err
                  "Produce Simple Assertion: Subst does not cover the pure \
                   formula"
          in
          L.(
            verbose (fun m ->
                m
                  "UNHAPPY. update_store inside produce assertions with prog \
                   variable: %s!!!\n"
                  x));
          update_store astate x v
    | Pure f -> (
        L.verbose (fun fmt -> fmt "Pure assertion.");
        let f' = ESubst.substitute_formula subst ~partial:false f in
        (* let pp_state =
             match !Config.pbn with
             | false -> State.pp
             | true  ->
                 let pvars, lvars, locs = Formula.get_print_info f' in
                 State.pp_by_need pvars lvars locs
           in
           L.(
             verbose (fun m ->
                  m "About to assume %a in state:\n%a" Formula.pp f' pp_state state)); *)
        (* FIXME: Understand why this causes a bug in Gillian-C *)
        match
          State.assume_a ~unification:true ~production:!Config.delay_entailment
            state [ f' ]
        with
        | None ->
            let msg =
              Fmt.str "Produce Simple Assertion: Cannot assume pure formula %a."
                Formula.pp f'
            in
            other_state_err msg
        | Some state' -> Res_list.return (state', preds, pred_defs, variants))
    | _ -> L.fail "Produce simple assertion: unsupported assertion"

  and produce_asrt_list (astate : t) (subst : ESubst.t) (sas : Asrt.t list) :
      (t, err_t) Res_list.t =
    let open Res_list.Syntax in
    let state, _, _, _ = astate in
    let other_state_err msg = Res_list.error_with (StateErr.EOther msg) in
    let () =
      ESubst.iter subst (fun v value ->
          ESubst.put subst v (State.simplify_val state value))
    in
    let** state, preds, preds_tbl, variants =
      List.fold_left
        (fun intermediate_states asrt ->
          let** intermediate_state = intermediate_states in
          try produce_assertion intermediate_state subst asrt
          with e ->
            let state, _, _, _ = intermediate_state in
            let admissible =
              State.assume_a ~time:"Produce: final check" ~unification:true
                state [ True ]
            in
            if !Config.delay_entailment && Option.is_none admissible then (
              L.verbose (fun fmt ->
                  fmt
                    "Production exception due to delayed entailment, survived.");
              other_state_err "Production Exception")
            else raise e)
        (Res_list.return astate) sas
    in
    let state, preds = (State.copy state, Preds.copy preds) in
    let admissible =
      L.verbose (fun fmt -> fmt "Produce: final check");
      try
        State.assume_a ~time:"Produce: final check" ~unification:true state
          [ True ]
      with _ -> None
    in
    L.verbose (fun fmt -> fmt "Concluded final check");
    match admissible with
    | None -> other_state_err "final state non admissible"
    | Some state -> Res_list.return (state, preds, preds_tbl, variants)

  let produce (astate : t) (subst : ESubst.t) (a : Asrt.t) :
      (t, err_t) Res_list.t =
    L.(
      verbose (fun m ->
          m
            "@[-----------------@\n\
             -----------------@\n\
             Produce assertion: @[%a@]@]" Asrt.pp a));
    let sas = UP.collect_simple_asrts a in
    produce_asrt_list astate subst sas

  let produce_posts (state : t) (subst : ESubst.t) (asrts : Asrt.t list) :
      t list =
    let open Syntaxes.List in
    L.(
      verbose (fun m ->
          m
            "@[<v 2>Produce posts: There are %d postconditions to produce. And \
             here they are:@\n\
             %a@]"
            (List.length asrts)
            Fmt.(list ~sep:(any "@\n") Asrt.pp)
            asrts));
    let* asrt = asrts in
    let subst = ESubst.copy subst in
    let state = copy_astate state in
    produce state subst asrt
    |> List.filter_map (function
         | Error err ->
             L.verbose (fun m -> m "Warning: %a" pp_err_t err);
             None (* Ignoring errors *)
         | Ok state ->
             ESubst.iter subst (fun e v ->
                 match e with
                 | PVar x -> ignore (update_store state x v)
                 | _ -> ());
             Some state)
  (* List.fold_left
     (fun acc asrt ->
       let subst = ESubst.copy subst in
       let state = copy_astate state in
       match produce state subst asrt with
       | Error errs ->
           L.verbose (fun m -> m "Warning: %a" (pp_list pp_err_t) errs);
           acc
       | Ok states ->
           List.iter
             (fun state ->
               ESubst.iter subst (fun e v ->
                   match e with
                   | PVar x -> ignore (update_store state x v)
                   | _ -> ()))
             states;
           states @ acc)
     [] asrts *)

  let complete_subst (subst : ESubst.t) (lab : (string * SS.t) option) : bool =
    match lab with
    | None -> true
    | Some (_, existentials) ->
        List.fold_left
          (fun ac x ->
            let lvar_x = Expr.LVar x in
            if not (ESubst.mem subst lvar_x) then (
              let v_x = Val.from_expr lvar_x in
              match v_x with
              | None -> false
              | Some v_x ->
                  ESubst.put subst lvar_x v_x;
                  true)
            else ac)
          true (SS.elements existentials)

  (** [extend_subts_with_bindings unfold_info pred state subst] takes:
      - A state
      - A substitution
      - A list of pairs of lvar names
      And extends the substitution with the pairs
      [(#y, eval_expr #x)] for each pair [(x, y)] *)
  let extend_subst_with_bindings
      (state : State.t)
      (subst : ESubst.t)
      (bindings : (string * string) list) : unit =
    let bindings =
      List.map
        (fun (x, y) -> (Expr.LVar y, State.eval_expr state (Expr.LVar x)))
        bindings
    in
    ESubst.extend subst bindings;
    L.(
      verbose (fun m ->
          m "@[<v 2>Using unfold info, obtained subst:@\n%a@]@\n" ESubst.pp
            subst))

  (* WARNING: At the moment, unfold behaves over-approximately, it will return only success of only error.
     We only use unfold and fold in OX mode right now, and we don't quite know the meaning of UX fold/unfold. *)
  let rec unfold
      ?(additional_bindings = [])
      (astate : t)
      (pname : string)
      (args : Val.t list) : (ESubst.t * t, err_t) Res_list.t =
    let _, _, pred_defs, _ = astate in
    let pred = UP.get_pred_def pred_defs pname in
    let params = List.map (fun (x, _) -> Expr.PVar x) pred.pred.pred_params in

    let open Res_list.Syntax in
    let** state, preds, pred_defs, variants =
      match pred.pred.pred_guard with
      | None -> Res_list.return astate
      | Some _ ->
          let in_params = Pred.in_params pred.pred in
          let in_params = List.map (fun x -> Expr.PVar x) in_params in
          let in_args = Pred.in_args pred.pred args in
          let subst = ESubst.init (List.combine in_params in_args) in
          let++ s, _, _ =
            unify ~is_post:false ~in_unification:true astate subst
              (Option.get pred.guard_up) PredicateGuard
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
          Fmt.(list ~sep:comma Val.pp)
          args);
    let subst_i = ESubst.init (List_utils.right_combine params args) in

    L.(
      verbose (fun m ->
          m "unfold with unfold_info with additional bindings@\n%a@\n"
            Fmt.(Dump.list (pair string string))
            additional_bindings));

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
          L.(
            verbose (fun m ->
                m "Going to produce %d definitions with subst@\n%a"
                  (List.length (first_def :: rest_defs))
                  ESubst.pp subst_i));
          let state' = State.add_spec_vars state new_spec_vars in
          let astate = (state', preds, pred_defs, variants) in
          let rest_results =
            let* def = rest_defs in
            produce (copy_astate astate) (ESubst.copy subst_i) def
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
              let subst, states = simplify_astate ~unification:true state in
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
                  pp_astate astate ESubst.pp subst))
          rets);
    rets

  and fold_guarded_with_vals (astate : t) (vs : Val.t list) :
      (t, string) Res_list.t =
    L.(
      verbose (fun m ->
          m "@[<v 2>Starting fold_guarded_with_vals: @[<h>%a@]@\n%a.@\n"
            Fmt.(list ~sep:comma Val.pp)
            vs pp_astate astate));
    if !Config.manual_proof then Res_list.error_with "Manual proof"
    else
      match select_guarded_predicate_to_fold astate vs with
      | Some (pname, v_args) ->
          L.(verbose (fun m -> m "FOUND STH TO FOLD: %s!!!!\n" pname));
          let _, _, pred_defs, _ = astate in
          let pred = UP.get_pred_def pred_defs pname in
          let rets =
            fold ~in_unification:true ~unify_kind:Fold
              ~state:(copy_astate astate) pred v_args
          in
          Res_list.map_error
            (fun _ -> "fold_guarded_with_vals: Failed to fold")
            rets
      | None ->
          L.(verbose (fun m -> m "No predicate found to fold!"));
          Res_list.error_with "No predicate found to fold!"

  and unfold_with_vals (astate : t) (vs : Val.t list) :
      (ESubst.t * t) list option =
    L.(
      verbose (fun m ->
          m "@[<v 2>Starting unfold_with_vals: @[<h>%a@]@\n%a.@\n"
            Fmt.(list ~sep:comma Val.pp)
            vs pp_astate astate));

    if !Config.manual_proof then None
    else
      match consume_pred_with_vs astate vs with
      | Some (pname, v_args) -> (
          L.(verbose (fun m -> m "FOUND STH TO UNFOLD: %s!!!!\n" pname));
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
              L.(
                verbose (fun m ->
                    m "Unfold complete: %s(@[<h>%a@]): %d" pname
                      Fmt.(list ~sep:comma Val.pp)
                      v_args (List.length rets)));
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
          L.(verbose (fun m -> m "NOTHING TO UNFOLD!!!!\n"));
          None

  (** Consumes a predicate from the state.
      If the predicate is not "verbatim" in our set of preds,
      and it is not abstract and we are not in manual mode,
      we attempt to fold it. *)
  and consume_pred
      ?(is_post = false)
      ?(in_unification = false)
      (astate : t)
      (pname : string)
      (vs : vt option list)
      (fold_outs_info : (st * UP.step * UP.outs * Expr.t list) option) :
      (t * vt list, err_t) Res_list.t =
    L.(
      tmi (fun m ->
          m "Unifier.consume_pred %s. args: @[<h>%a@]" pname
            Fmt.(list ~sep:comma (Dump.option Val.pp))
            vs));

    let state, preds, pred_defs, variants = astate in
    let pred = UP.get_pred_def pred_defs pname in
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
        L.(
          verbose (fun m ->
              m "Returning the following vs: @[<h>%a@]"
                Fmt.(list ~sep:comma Val.pp)
                vs));
        let vs = Pred.out_args pred_def vs in
        match fold_outs_info with
        | None -> Res_list.return (astate, vs)
        | Some (subst, step, outs, les_outs) -> (
            L.(
              verbose (fun m ->
                  m
                    "learned the outs of a predicate. going to unify \
                     (@[<h>%a@]) against (@[<h>%a@])!!!@\n"
                    Fmt.(list ~sep:comma Val.pp)
                    vs
                    Fmt.(list ~sep:comma Expr.pp)
                    les_outs));
            match unify_ins_outs_lists state subst step outs vs les_outs with
            | Success new_state ->
                Res_list.return ((new_state, preds, pred_defs, variants), vs)
            | Abort fail_pf ->
                let error =
                  StateErr.EAsrt ([], Not fail_pf, [ [ Pure fail_pf ] ])
                in
                Res_list.error_with error
            | Vanish -> Res_list.vanish))
    | None when (not !Config.manual_proof) && not pred_def.pred_abstract ->
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
          fold ~is_post ~in_unification ~state:astate ~unify_kind:Fold pred
            vs_ins
        in
        (* Supposedly, we don't need a guard to make sure we're not looping indefinitely:
           if the fold worked, then consume_pred should not take this branch on the next try.
           We should still be keeping an eye on this in case something loops indefinitely. *)
        consume_pred ~is_post ~in_unification folded pname vs fold_outs_info
    | _ -> Res_list.error_with (StateErr.EPure False)

  and unify_ins_outs_lists
      (state : State.t)
      (subst : st)
      (step : UP.step)
      (outs : UP.outs)
      (vos : Val.t list)
      (eos : Expr.t list) : handle_pure_result =
    let ( let+ ) x f = List.map f x in
    L.verbose (fun fmt ->
        fmt "Outs: %a"
          Fmt.(
            brackets
              (list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.full_pp))))
          outs);
    L.verbose (fun fmt ->
        fmt "Obtained values: %a" Fmt.(brackets (list ~sep:semi Val.pp)) vos);
    L.verbose (fun fmt ->
        fmt "Obtained exprs: %a" Fmt.(brackets (list ~sep:semi Expr.pp)) eos);
    (* Substitution of the program variables *)
    let pvar_subst_bindings =
      List.mapi (fun i v -> (Expr.PVar (string_of_int i), v)) vos
    in
    let pvar_subst = ESubst.init pvar_subst_bindings in
    L.verbose (fun fmt -> fmt "Parameter subst\n%a" ESubst.pp pvar_subst);
    let outs : UP.outs option =
      try
        Some
          (List.map
             (fun (u, e) ->
               let se = ESubst.subst_in_expr pvar_subst ~partial:true e in
               (* let se = ESubst.subst_in_expr subst ~partial:true se in *)
               ( u,
                 try Reduction.reduce_lexpr ~unification:true se with _ -> se ))
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
        let outs =
          let+ u, e = outs in
          let v =
            match Val.from_expr e with
            | None ->
                let msg =
                  Fmt.str
                    "INTERNAL ERROR: Not all expressions convertible to \
                     values: %a"
                    Expr.full_pp e
                in
                L.fail msg
            | Some e -> e
          in
          (u, v)
        in
        List.iter (fun (u, v) -> ESubst.put subst u v) outs;
        let eos =
          let+ e = eos in
          let subst_e =
            match ESubst.subst_in_expr_opt subst e with
            | None ->
                let msg =
                  Fmt.str
                    "INTERNAL ERROR: Not all ins known, I don't know this one: \
                     %a"
                    Expr.full_pp e
                in
                L.fail msg
            | Some e -> e
          in
          match Val.from_expr subst_e with
          | None ->
              let msg =
                Fmt.str
                  "INTERNAL ERROR: Not all expressions convertible to values: \
                   %a"
                  Expr.full_pp subst_e
              in
              L.fail msg
          | Some v -> v
        in
        try
          List.fold_left2
            (fun ac vd od ->
              match ac with
              | Abort _ | Vanish -> ac
              | Success state ->
                  let pf : Formula.t = Eq (Val.to_expr vd, Val.to_expr od) in
                  handle_pure state pf)
            (Success state) vos eos
        with Invalid_argument _ ->
          Fmt.failwith "Invalid amount of args for the following UP step : %a"
            UP.step_pp step)

  and unify_assertion
      ?(is_post = false)
      (astate : t)
      (subst : ESubst.t)
      (step : UP.step) : (t, err_t) Res_list.t =
    (* Auxiliary function for actions and predicates, with indexed outs *)
    let state, preds, pred_defs, variants = astate in

    let make_resource_fail () =
      Res_list.error_with (StateErr.EAsrt ([], True, []))
    in

    let assertion_loggable =
      if L.Mode.enabled () then
        Some
          (let a = fst step in
           (* Get pvars, lvars, locs from the assertion *)
           let a_pvars, a_lvars, a_locs =
             (Asrt.pvars a, Asrt.lvars a, Asrt.locs a)
           in
           let filter_vars = SS.union a_pvars (SS.union a_lvars a_locs) in

           (* From the subst, we take any pair that has any of those and collect
              the pvars, lvars, and alocs, from their values *)
           let s_pvars, s_lvars, s_locs =
             ESubst.fold subst
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
                   ( SS.union s_pvars (Expr.pvars (Val.to_expr v)),
                     SS.union s_lvars (Expr.lvars (Val.to_expr v)),
                     SS.union s_locs (Expr.locs (Val.to_expr v)) )
                 else (s_pvars, s_lvars, s_locs))
               (SS.empty, SS.empty, SS.empty)
           in

           let subst_pp =
             match !Config.pbn with
             | false -> ESubst.pp
             | true ->
                 ESubst.pp_by_need (SS.union a_pvars (SS.union a_lvars a_locs))
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
        match (p : Asrt.t) with
        | GA (a_id, e_ins, e_outs) -> (
            let getter = State.ga_to_getter a_id in
            let remover = State.ga_to_deleter a_id in
            let vs_ins = List.map (subst_in_expr_opt astate subst) e_ins in
            let failure = List.exists (fun x -> x = None) vs_ins in
            if failure then (
              Fmt.pr "I don't know all ins for %a????" Asrt.pp p;
              if !Config.under_approximation then [] else make_resource_fail ())
            else
              let vs_ins = List.map Option.get vs_ins in
              L.(
                verbose (fun m ->
                    m "Executing action: %s with ins: @[<h>%a@]" getter
                      Fmt.(list ~sep:comma Val.pp)
                      vs_ins));
              let** state', vs' = State.execute_action getter state vs_ins in
              let vs_ins', vs_outs =
                List_utils.split_at vs' (List.length vs_ins)
              in
              let** state'', _ = State.execute_action remover state' vs_ins' in
              (* Separate outs into direct unifiables and others*)
              match
                unify_ins_outs_lists state'' subst step outs vs_outs e_outs
              with
              | Success state''' ->
                  Res_list.return (state''', preds, pred_defs, variants)
              | Abort fail_pf ->
                  let error =
                    StateErr.EAsrt ([], Not fail_pf, [ [ Pure fail_pf ] ])
                  in
                  Res_list.error_with error
              | Vanish -> Res_list.vanish)
        | Pred (pname, les) ->
            L.verbose (fun m -> m "Unifying predicate assertion");
            (* Perform substitution in all predicate parameters *)
            L.verbose (fun fmt ->
                fmt "ARGS: %a" Fmt.(list ~sep:comma Expr.pp) les);
            L.verbose (fun fmt -> fmt "SUBST:\n%a" ESubst.pp subst);
            let vs = List.map (subst_in_expr_opt astate subst) les in
            (* Get the ins of the predicate *)
            let pred = UP.get_pred_def pred_defs pname in
            let pred_def = pred.pred in
            let vs_ins = Pred.in_args pred_def vs in
            let les_outs = Pred.out_args pred_def les in
            (* All of which must have survived substitution *)
            let failure = List.exists (fun x -> x = None) vs_ins in
            if failure then (
              L.verbose (fun m -> m "Cannot unify: not all in-parameters known");
              make_resource_fail ())
            else
              let vs_ins = List.map Option.get vs_ins in
              L.verbose (fun m ->
                  m "Looking for ins: %a"
                    Fmt.(brackets (list ~sep:comma Val.pp))
                    vs_ins);
              let consume_pred_res =
                consume_pred ~is_post ~in_unification:true astate pname vs
                  (Some (subst, step, outs, les_outs))
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
        (* Conjunction should not be here *)
        | Pure (Formula.And _) ->
            raise (Failure "Unify assertion: And: should have been reduced")
        (* Other pure assertions *)
        | Pure f -> (
            let discharges =
              List.fold_left
                (fun discharges (u, out) ->
                  let open Syntaxes.Result in
                  let* discharges = discharges in
                  (* Perform the substitution in the out *)
                  let* out =
                    ESubst.subst_in_expr_opt subst out
                    |> Result_utils.of_option ~none:()
                  in
                  (* Convert obtained out to value *)
                  let* out =
                    Val.from_expr out |> Result_utils.of_option ~none:()
                  in
                  (* And add to e-subst *)
                  match ESubst.get subst u with
                  | None ->
                      ESubst.put subst u out;
                      Ok discharges
                  | Some out' when Val.equal out out' -> Ok discharges
                  | Some out' ->
                      let new_discharge =
                        Formula.Eq (Val.to_expr out, Val.to_expr out')
                      in
                      Ok (new_discharge :: discharges))
                (Ok []) outs
            in
            let discharges =
              match discharges with
              | Error () ->
                  Fmt.failwith
                    "INTERNAL ERROR: Unification failure: do not know all ins \
                     for %a"
                    Formula.pp f
              | Ok discharges -> discharges
            in
            (* To unify a pure formula we must know all ins *)
            let opf = ESubst.substitute_in_formula_opt subst f in
            match opf with
            | None ->
                raise
                  (Failure
                     (Format.asprintf
                        "Unification failure: do not know all ins for %a"
                        Formula.pp f))
            | Some pf -> (
                let discharges_pf =
                  List.fold_left Formula.Infix.( #&& ) True discharges
                in
                let discharges_pf =
                  Reduction.reduce_formula ~unification:true discharges_pf
                in
                let to_asrt = Formula.Infix.( #&& ) pf discharges_pf in
                match handle_pure state to_asrt with
                | Success new_state ->
                    Res_list.return (new_state, preds, pred_defs, variants)
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
                  let v_le : vt =
                    match v_le with
                    | Some v_le -> v_le
                    | None -> raise (Failure "DEATH. unify assertion Types")
                  in
                  match State.get_type state v_le with
                  | Some t' -> if not (Type.equal t t') then False :: ac else ac
                  | None ->
                      Eq (UnOp (TypeOf, Val.to_expr v_le), Lit (Type t)) :: ac)
                [] les
            in

            match corrections with
            | [] -> Res_list.return astate
            | _ ->
                if !Config.under_approximation then
                  (* In under-approx we try to assume the types hold*)
                  match State.assume_a ~unification:true state corrections with
                  | None -> Res_list.vanish
                  | Some state' ->
                      Res_list.return (state', preds, pred_defs, variants)
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
        (* LTrue, LFalse, LEmp, LStar *)
        | _ -> raise (Failure "Illegal Assertion in Unification Plan"))

  and unify_up'
      ~is_post
      (parent_ids : L.Report_id.t list ref)
      (s_states : search_state') : internal_up_u_res =
    let s_states, errs_so_far = s_states in
    L.(
      verbose (fun m ->
          m "Unify UP: There are %d states left to consider."
            (List.length s_states)));
    let explore_next_states = unify_up' ~is_post parent_ids in
    let ux = !Config.under_approximation in
    match s_states with
    | [] ->
        (* There are no more states to explore: in OX, it means we failed to unify, in UX, it means there are not valid path we know about, we vanish. *)
        if ux then UVanish else UAbort errs_so_far
    | ((state, subst, up), target_case_depth, is_new_case) :: rest -> (
        let case_depth =
          structure_unify_case_reports parent_ids target_case_depth is_new_case
            state subst up
        in
        let cur_step : UP.step option = UP.head up in
        let ret =
          match cur_step with
          | None -> Res_list.return state
          | Some cur_step -> (
              try unify_assertion ~is_post state subst cur_step
              with err -> (
                L.verbose (fun fmt ->
                    fmt
                      "WARNING: UNCAUGHT EXCEPTION IN UNIFY-ASSERTION : %s@\n\
                       Here's the backtrace: %s" (Printexc.to_string err)
                      (Printexc.get_backtrace ()));
                if ux then
                  let () = L.verbose (fun m -> m "UX mode: vanishing!") in
                  Res_list.vanish
                else
                  let a, _ = cur_step in
                  match a with
                  | Pure pf ->
                      let bstate, _, _, _ = state in
                      let vs = State.unfolding_vals bstate [ pf ] in
                      Res_list.error_with
                        (StateErr.EAsrt (vs, Not pf, [ [ Pure pf ] ]))
                  | _ ->
                      let other_error =
                        StateErr.EOther
                          (Fmt.str
                             "Uncaught exception while unifying assertions %a"
                             Asrt.pp a)
                      in
                      Res_list.error_with other_error))
        in
        (* In theory, errors can only exist in OX mode. If there is a unification error in UX mode, something is terribly wrong.
           So now, we're going to do something different depending on UX or OX mode.
           In OX mode:
              - In case of success, we continue unifying on the same path. If we're at the end of the path, we successfully stop
              - If there is a least one error, it means unification of that path failed. We try the next path (recursive call) *)
        let successes, errors =
          List.partition_map
            (function
              | Ok x -> Left x
              | Error x -> Right x)
            ret
        in
        match (ux, successes, errors) with
        | true, _, _ :: _ ->
            L.fail "ERROR: IMPOSSIBLE! UNIFICATION ERRORS IN UX MODE!!!!"
        | false, _, _ :: _ ->
            UnifyResultReport.log
              (Failure
                 { astate = AstateRec.from state; cur_step; subst; errors })
            |> ignore;
            explore_next_states (rest, errors @ errs_so_far)
        | false, [], [] ->
            L.fail "OX UNIFICATION SUCCEEDED WITH NOTHING! MEDOOOOOO!!!!!"
        | false, _ :: _ :: _, [] -> L.fail "DEATH. OX UNIFICATION BRANCHED"
        | ux, successes, [] -> (
            match UP.next up with
            | None ->
                let posts = UP.posts up in
                List.iter
                  (fun state' ->
                    UnifyResultReport.log
                      (Success
                         {
                           remaining_states =
                             List.map
                               (fun ((astate, subst, up), _, _) :
                                    UnifyResultReport.remaining_state ->
                                 { astate = AstateRec.from astate; subst; up })
                               rest;
                           astate = AstateRec.from state';
                           subst;
                           posts;
                         })
                    |> ignore)
                  successes;
                let these_successes =
                  List.map (fun state -> (state, subst, posts)) successes
                in
                if ux then
                  let other_paths = explore_next_states (rest, errs_so_far) in
                  match other_paths with
                  | UAbort _ | UVanish -> USuccess these_successes
                  | USuccess other_paths ->
                      USuccess (these_successes @ other_paths)
                else USuccess these_successes
            | Some [ (up, lab) ] ->
                let continue_states =
                  if complete_subst subst lab then
                    List.map
                      (fun state' -> ((state', subst, up), case_depth, false))
                      successes
                  else []
                in
                explore_next_states (continue_states @ rest, errs_so_far)
            | Some (_ :: _ as ups) ->
                let open Syntaxes.List in
                let next_states =
                  let* up, lab = ups in
                  if complete_subst subst lab then
                    let+ state' = successes in
                    let new_state = copy_astate state' in
                    let new_subst = ESubst.copy subst in
                    ((new_state, new_subst, up), case_depth + 1, true)
                  else []
                in
                explore_next_states (next_states @ rest, errs_so_far)
            | Some [] -> L.fail "ERROR: unify_up: empty unification plan"))

  and unify_up ~is_post (s_states : search_state) : internal_up_u_res =
    let () =
      L.verbose (fun fmt -> fmt "Unify UP: is-post: %a" Fmt.bool is_post)
    in
    let parent_ids = ref [] in
    let s_states =
      let states, errs = s_states in
      let states = states |> List.map (fun state -> (state, 0, false)) in
      (states, errs)
    in
    let res = unify_up' ~is_post parent_ids s_states in
    List.iter (fun parent_id -> L.Parent.release (Some parent_id)) !parent_ids;
    res

  and unify
      ?(is_post = false)
      ?(in_unification = false)
      (astate : t)
      (subst : ESubst.t)
      (up : UP.t)
      (unify_kind : unify_kind) : (t * st * post_res, err_t) Res_list.t =
    let astate_i = copy_astate astate in
    let subst_i = ESubst.copy subst in
    let can_fix errs = List.exists State.can_fix errs in

    let rec handle_ret ~try_recover ret =
      match ret with
      | UVanish ->
          L.verbose (fun m -> m "Unifier.unify: Vanish");
          Res_list.vanish
      | USuccess successes ->
          L.verbose (fun fmt -> fmt "Unifier.unify: Success");
          Res_list.just_oks successes
      | UAbort errs
        when try_recover && !Config.unfolding
             && Exec_mode.is_verification_exec !Config.current_exec_mode
             && (not in_unification) && can_fix errs -> (
          L.verbose (fun fmt -> fmt "Unifier.unify: Failure");
          if !Config.under_approximation then
            L.fail "UNIFICATION ABORTED IN UX MODE???";
          let state, _, _, _ = astate_i in
          let tactics = State.get_recovery_tactic state errs in
          L.(
            verbose (fun m ->
                m
                  "Unify. Unable to unify. About to attempt the following \
                   recovery tactic:\n\
                   %a"
                  (Recovery_tactic.pp Val.pp)
                  tactics));
          match try_recovering astate_i tactics with
          | Error msg ->
              L.normal (fun m -> m "Unify. Recovery tactic failed: %s" msg);
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
                  let subst'' = ESubst.copy subst_i in
                  let new_ret =
                    unify_up ~is_post ([ (astate, subst'', up) ], [])
                  in
                  (* We already tried recovering once and it failed, we stop here *)
                  handle_ret ~try_recover:false new_ret))
      | UAbort errors ->
          L.verbose (fun fmt -> fmt "Unifier.unify: Failure");
          Res_list.just_errors errors
    in
    UnifyReport.as_parent
      { astate = AstateRec.from astate; subst; up; unify_kind }
      (fun () ->
        let ret = unify_up ~is_post ([ (astate, subst, up) ], []) in
        handle_ret ~try_recover:true ret)

  and fold
      ?(is_post = false)
      ?(in_unification = false)
      ?(additional_bindings = [])
      ~unify_kind
      ~(state : t)
      (pred : UP.pred)
      (args : vt list) : (t, err_t) Res_list.t =
    let pred_name = pred.pred.pred_name in
    L.verbose (fun fmt -> fmt "Folding predicate: %s\n" pred_name);
    if pred.pred.pred_abstract then
      Fmt.failwith "Impossible: Folding abstract predicate %s" pred_name;
    L.verbose (fun m -> m "Predicate unification plan: %a" UP.pp pred.def_up);
    let params = List.map (fun (x, _) -> x) pred.pred.pred_params in
    let param_bindings =
      if List.compare_lengths params args = 0 then List.combine params args
      else
        try List.combine (Pred.in_params pred.pred) args
        with Invalid_argument _ ->
          Fmt.failwith "invalid number of parameter while folding: %s%a"
            pred.pred.pred_name
            Fmt.(parens @@ hbox @@ list ~sep:comma Val.pp)
            args
    in
    let param_bindings =
      List.map (fun (x, v) -> (Expr.PVar x, v)) param_bindings
    in
    let subst = ESubst.init (additional_bindings @ param_bindings) in
    let unify_result =
      unify ~is_post ~in_unification state subst pred.def_up unify_kind
    in
    let () =
      match unify_result with
      | [] ->
          Fmt.(
            failwith "@[<h>HORROR: fold vanished for %s(%a) with bindings: %a@]"
              pred_name (Dump.list Val.pp) args
              (Dump.list @@ Dump.pair Expr.pp Val.pp)
              additional_bindings)
      | _ -> ()
    in
    let open Res_list.Syntax in
    let** astate', subst', _ = unify_result in
    let _, preds', _, _ = astate' in
    let arg_vs =
      if List.compare_lengths params args = 0 then args
      else
        let out_params = Pred.out_params pred.pred in
        let vs_outs =
          List.map
            (fun x ->
              match ESubst.get subst' (PVar x) with
              | Some v_x -> v_x
              | None ->
                  failwith "DEATH. Didnt learn all the outs while folding.")
            out_params
        in
        L.(
          verbose (fun m ->
              m "Out parameters : @[<h>%a@]"
                Fmt.(list ~sep:comma Val.pp)
                vs_outs));
        Pred.combine_ins_outs pred.pred args vs_outs
    in
    (* We extend the list of predicates with our newly folded predicate. *)
    Preds.extend ~pure:pred.pred.pred_pure preds' (pred_name, arg_vs);
    (* If the predicate has a guard, we also produce it in our state,
       otherwise we return the just current state *)
    match pred.pred.pred_guard with
    | None -> Res_list.return astate'
    | Some guard -> produce astate' subst' guard

  and unfold_concrete_preds (astate : t) : (st option * t) option =
    let _, preds, pred_defs, _ = astate in

    let is_unfoldable_lit lit =
      match lit with
      | Loc _ | LList _ -> false
      | _ -> true
    in

    let should_unfold (pname, vs) =
      (* Find a predicate with only concrete args
         and without a guard. *)
      let pred = UP.get_pred_def pred_defs pname in
      Option.is_none pred.pred.pred_guard
      && Pred.in_args pred.pred vs
         |> List.for_all (fun in_arg ->
                match Val.to_literal in_arg with
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
            L.(
              verbose (fun m ->
                  m "unfold_concrete_preds WORKED. Unfolded: %s(@[<h>%a])" name
                    Fmt.(list ~sep:comma Val.pp)
                    vs));
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

  and try_recovering (astate : t) (tactic : vt Recovery_tactic.t) :
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
      match unfold_with_vals astate unfold_values with
      | None -> Error "Automatic unfold failed"
      | Some next_states ->
          Ok (List.map (fun (_, astate) -> astate) next_states)
    in
    Fmt.error "try_fold: %s\ntry_unfold: %s" fold_error unfold_error

  let rec rec_unfold
      ?(fuel = 10)
      (astate : t)
      (pname : string)
      (args : Val.t list) : (t, err_t) Res_list.t =
    if fuel = 0 then failwith "RECURSIVE UNFOLD: OUT OF FUEL"
    else
      let open Res_list.Syntax in
      let** _, astate = unfold astate pname args in
      let _, preds, _, _ = astate in
      match Preds.remove_by_name preds pname with
      | Some (pname, vs) -> rec_unfold ~fuel:(fuel - 1) astate pname vs
      | None -> Res_list.return astate

  let unfold_all (astate : t) (pname : string) : (t, err_t) Res_list.t =
    let _, preds, _, _ = astate in
    match Preds.remove_by_name preds pname with
    | None -> Res_list.return astate
    | Some (pname, vs) -> rec_unfold astate pname vs
end
