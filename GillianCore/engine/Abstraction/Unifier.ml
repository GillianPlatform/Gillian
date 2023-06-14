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

  type gp_ret = (t * vt list, err_t) List_res.t
  type u_res = UWTF | USucc of t | UFail of err_t list
  type unfold_info_t = (string * string) list

  val produce_assertion : t -> st -> Asrt.t -> (t list, err_t list) result
  val produce : t -> st -> Asrt.t -> (t list, err_t list) result
  val produce_posts : t -> st -> Asrt.t list -> t list

  val unfold :
    ?additional_bindings:unfold_info_t ->
    t ->
    string ->
    vt list ->
    (st * t, err_t) List_res.t

  val rec_unfold : ?fuel:int -> t -> string -> vt list -> (t, err_t) List_res.t
  val unfold_all : t -> string -> (t, err_t) List_res.t
  val try_recovering : t -> vt Recovery_tactic.t -> (t list, string) result
  val unfold_with_vals : t -> vt list -> (st * t) list option
  val unfold_concrete_preds : t -> (st option * t) option
  val unify_assertion : ?is_post:bool -> t -> st -> UP.step -> u_res

  val unify :
    ?is_post:bool ->
    ?in_unification:bool ->
    t ->
    st ->
    UP.t ->
    unify_kind ->
    up_u_res

  val fold :
    ?is_post:bool ->
    ?in_unification:bool ->
    ?additional_bindings:(Expr.t * vt) list ->
    unify_kind:unify_kind ->
    state:t ->
    UP.pred ->
    vt list ->
    (t, err_t) List_res.t

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
  type gp_ret = (t * vt list, err_t) List_res.t
  type u_res = UWTF | USucc of t | UFail of err_t list
  type up_u_res = (t * st * post_res, err_t) List_res.t

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
      (t list, err_t list) result =
    let open Syntaxes.Result in
    let state, preds, pred_defs, variants = astate in
    let other_state_err msg = Error [ StateErr.EOther msg ] in

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
                     (state', Preds.copy preds, pred_defs, Hashtbl.copy variants)
               | Error _ -> None)
          |> Result.ok
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
        | Some _ -> Ok [ (state, preds, pred_defs, variants) ])
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
          let+ (ostate : t list) =
            match pred_def.pred.pred_facts with
            | [] -> Ok [ astate ]
            | facts ->
                (* let t = Sys.time () in *)
                let params, _ = List.split pred_def.pred.pred_params in
                let params = List.map (fun x -> Expr.PVar x) params in
                let facts =
                  List.fold_left
                    (fun facts (param, le) ->
                      List.map
                        (fun fact ->
                          Formula.subst_expr_for_expr ~to_subst:param
                            ~subst_with:le fact)
                        facts)
                    facts (List.combine params les)
                in
                let facts = Asrt.Pure (Formula.conjunct facts) in
                let result =
                  produce_assertion
                    (state, preds, pred_defs, variants)
                    subst facts
                in
                (* Utils.Statistics.update_statistics "Produce facts"
                   (Sys.time () -. t); *)
                result
          in
          let pure = pred_def.pred.pred_pure in
          (* FIXME: We could copy only when more than one result, less expensive *)
          List.map
            (fun (state, preds, pred_defs, variants) ->
              let preds = Preds.copy preds in
              let state = State.copy state in
              let variants = Hashtbl.copy variants in
              Preds.extend ~pure preds (pname, vs);
              (state, preds, pred_defs, variants))
            ostate
    | Pure (Eq (PVar x, le)) | Pure (Eq (le, PVar x)) ->
        L.verbose (fun fmt -> fmt "Pure assertion.");
        if ESubst.mem subst (PVar x) then
          let v_x = ESubst.get subst (PVar x) in
          let v_le = subst_in_expr subst le in
          let opt_res =
            match (v_x, v_le) with
            | Some v_x, Some v_le ->
                Option.map
                  (fun state -> [ (state, preds, pred_defs, variants) ])
                  (State.assume_a ~unification:true
                     ~production:!Config.delay_entailment state
                     [ Eq (Val.to_expr v_x, Val.to_expr v_le) ])
            | _ -> None
          in
          match opt_res with
          | Some r -> Ok r
          | None ->
              other_state_err
                "Produce Simple Assertion: Subst does not cover the pure \
                 formula"
        else
          let+ v =
            match subst_in_expr subst le with
            | Some r -> Ok r
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
          [ update_store astate x v ]
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
        | Some state' -> Ok [ (state', preds, pred_defs, variants) ])
    | _ -> L.fail "Produce simple assertion: unsupported assertion"

  and produce_asrt_list (astate : t) (subst : ESubst.t) (sas : Asrt.t list) :
      (t list, err_t list) result =
    let open Syntaxes.Result in
    let state, _, _, _ = astate in
    let other_state_err msg = Error [ StateErr.EOther msg ] in
    let _ =
      ESubst.iter subst (fun v value ->
          ESubst.put subst v (State.simplify_val state value))
    in
    let collect l =
      List.fold_left
        (fun acc res ->
          match (acc, res) with
          | Ok old, Ok new_ -> Ok (new_ @ old)
          | Error old, Error new_ -> Error (old @ new_)
          | Error error, Ok _ | Ok _, Error error -> Error error)
        (Ok []) l
    in
    let rec loop (loop_state : Asrt.t list * t list) :
        (t list, err_t list) result =
      match loop_state with
      | [], astates -> Ok astates
      | a :: rest_as, astates ->
          let on_all_states =
            List.map
              (fun astate ->
                let astate = copy_astate astate in
                try
                  match produce_assertion astate subst a with
                  | Ok astates' -> loop (rest_as, astates')
                  | Error e -> Error e
                with e ->
                  let state, _, _, _ = astate in
                  let admissible =
                    State.assume_a ~time:"Produce: final check"
                      ~unification:true state [ True ]
                  in
                  if !Config.delay_entailment && admissible = None then (
                    L.verbose (fun fmt ->
                        fmt
                          "Production exception due to delayed entailment, \
                           survived.");
                    other_state_err "Production Exception")
                  else raise e)
              astates
          in
          collect on_all_states
    in

    let* astates = loop (sas, [ astate ]) in
    List.map
      (fun (state, preds, preds_tbl, variants) ->
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
        | Some state -> Ok [ (state, preds, preds_tbl, variants) ])
      astates
    |> collect

  let produce (astate : t) (subst : ESubst.t) (a : Asrt.t) :
      (t list, err_t list) result =
    L.(
      verbose (fun m ->
          m
            "@[-----------------@\n\
             -----------------@\n\
             Produce assertion: @[%a@]" Asrt.pp a));
    let sas = UP.collect_simple_asrts a in
    produce_asrt_list astate subst sas

  let produce_posts (state : t) (subst : ESubst.t) (asrts : Asrt.t list) :
      t list =
    L.(
      verbose (fun m ->
          m
            "@[<v 2>Produce posts: There are %d postconditions to produce. And \
             here they are:@\n\
             %a@]"
            (List.length asrts)
            Fmt.(list ~sep:(any "@\n") Asrt.pp)
            asrts));
    List.fold_left
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
      [] asrts

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

  let rec unfold
      ?(additional_bindings = [])
      (astate : t)
      (pname : string)
      (args : Val.t list) : (ESubst.t * t, err_t) List_res.t =
    let _, _, pred_defs, _ = astate in
    let pred = UP.get_pred_def pred_defs pname in
    let params = List.map (fun (x, _) -> Expr.PVar x) pred.pred.pred_params in

    let open List_res.Syntax in
    let* state, preds, pred_defs, variants =
      match pred.pred.pred_guard with
      | None -> Ok [ astate ]
      | Some _ ->
          let in_params = Pred.in_params pred.pred in
          let in_params = List.map (fun x -> Expr.PVar x) in_params in
          let in_args = Pred.in_args pred.pred args in
          let subst = ESubst.init (List.combine in_params in_args) in
          let+ s, _, _ =
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
    let rets =
      match definitions with
      | [] ->
          Fmt.failwith "Cannot Unfold Predicate %s with No Definitions"
            pred.pred.pred_name
      | first_def :: rest_defs ->
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
            List.map
              (fun def ->
                produce (copy_astate astate) (ESubst.copy subst_i) def)
              rest_defs
          in
          let first_results = produce astate subst_i first_def in
          List.fold_left
            (fun acc res ->
              match res with
              | Error errs ->
                  (* If a production fails, it means this branch is not
                     possible, we log and ignore. *)
                  List.iter
                    (fun err ->
                      L.verbose (fun m -> m "Warning: %a" pp_err_t err))
                    errs;
                  acc
              | Ok astates ->
                  let open Syntaxes.List in
                  let new_states =
                    let* state = astates in
                    let subst, states =
                      simplify_astate ~unification:true state
                    in
                    let+ state = states in
                    (subst, state)
                  in
                  new_states @ acc)
            []
            (first_results :: rest_results)
    in

    L.verbose (fun m ->
        m "Results of unfolding %s(@[<h>%a@]):@\n@[%a@]" pname
          Fmt.(list ~sep:comma Expr.pp)
          params
          Fmt.(
            iter_bindings ~sep:(any "@\n ") List.iteri
              (fun f' (i, (subst, astate)) ->
                Fmt.pf f' "Result %d@\nSTATE:@\n  @[%a@]@\nSUBST:@[<h>%a@]@\n" i
                  pp_astate astate ESubst.pp subst))
          rets);
    Ok rets

  and fold_guarded_with_vals (astate : t) (vs : Val.t list) :
      (t list, string) result =
    L.(
      verbose (fun m ->
          m "@[<v 2>Starting fold_guarded_with_vals: @[<h>%a@]@\n%a.@\n"
            Fmt.(list ~sep:comma Val.pp)
            vs pp_astate astate));
    if !Config.manual_proof then Error "Manual proof"
    else
      match select_guarded_predicate_to_fold astate vs with
      | Some (pname, v_args) -> (
          L.(verbose (fun m -> m "FOUND STH TO FOLD: %s!!!!\n" pname));
          let _, _, pred_defs, _ = astate in
          let pred = UP.get_pred_def pred_defs pname in
          let rets =
            fold ~in_unification:true ~unify_kind:Fold
              ~state:(copy_astate astate) pred v_args
          in
          match rets with
          | Ok rets -> Ok rets
          | Error _ -> Error "fold_guareded_with_vals: Failed to fold")
      | None ->
          L.(verbose (fun m -> m "No predicate found to fold!"));
          Error "No predicate found to fold!"

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
          match rets with
          | Ok rets ->
              L.(
                verbose (fun m ->
                    m "Unfold complete: %s(@[<h>%a@]): %d" pname
                      Fmt.(list ~sep:comma Val.pp)
                      v_args (List.length rets)));
              List.iteri
                (fun i (subst, astate) ->
                  L.(
                    verbose (fun m ->
                        m "Result of UNFOLD %d:@\n  @[%a]@\nSubst:@\n  @[%a]@\n"
                          i pp_astate astate ESubst.pp subst)))
                rets;
              Some rets
          | Error errs ->
              L.verbose (fun m ->
                  m "Unfolding failed in unfold_with_vals: %a"
                    Fmt.(list ~sep:(any "\n") pp_err_t)
                    errs);
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
      (fold_outs_info : (st * UP.step * UP.outs * Expr.t list) option) : gp_ret
      =
    L.(
      tmi (fun m ->
          m "Unifier.consume_pred %s. args: @[<h>%a@]" pname
            Fmt.(list ~sep:comma (Dump.option Val.pp))
            vs));

    let state, preds, pred_defs, _ = astate in
    let pred = UP.get_pred_def pred_defs pname in
    let pred_def = pred.pred in
    let pred_pure = pred_def.pred_pure in
    let return = List_res.return in
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
        | None -> return (astate, vs)
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
            | Ok () -> return (astate, vs)
            | Error fail_pf ->
                Error [ EAsrt ([], Not fail_pf, [ [ Pure fail_pf ] ]) ]))
    | None when (not !Config.manual_proof) && not pred_def.pred_abstract ->
        (* Recursive Case - Folding required *)
        (* The predicate will be folded (if possible) and then removed from the state.
           Interestingly, if the predicate has a guard, this will produce it but not remove it. *)
        let () =
          L.verbose (fun fmt ->
              fmt "Auto-folding predicate: %s\n" pred.pred.pred_name)
        in
        L.verbose (fun m -> m "Recursive case - attempting to fold.");

        let open List_res.Syntax in
        let vs_ins = Pred.in_args pred.pred vs in
        let vs_ins = List.map Option.get vs_ins in
        let* folded =
          fold ~is_post ~in_unification ~state:astate ~unify_kind:Fold pred
            vs_ins
        in
        (* Supposedly, we don't need a guard to make sure we're not looping indefinitely:
           if the fold worked, then consume_pred should not take this branch on the next try.
           We should still be keeping an eye on this in case something loops indefinitely. *)
        consume_pred ~is_post ~in_unification folded pname vs fold_outs_info
    | _ -> Error [ StateErr.EPure False ]

  and unify_ins_outs_lists
      (state : State.t)
      (subst : st)
      (step : UP.step)
      (outs : UP.outs)
      (vos : Val.t list)
      (eos : Expr.t list) =
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
    | None -> Error True
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
              | Error _ -> ac
              | Ok () ->
                  let pf : Formula.t = Eq (Val.to_expr vd, Val.to_expr od) in
                  let success = State.assert_a state [ pf ] in
                  if success then Ok () else Error pf)
            (Ok ()) vos eos
        with Invalid_argument _ ->
          Fmt.failwith "Invalid amount of args for the following UP step : %a"
            UP.step_pp step)

  and unify_assertion
      ?(is_post = false)
      (astate : t)
      (subst : ESubst.t)
      (step : UP.step) : u_res =
    (* Auxiliary function for actions and predicates, with indexed outs *)
    let state, preds, pred_defs, variants = astate in

    let make_resource_fail () = UFail [ EAsrt ([], True, []) ] in

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

        match (p : Asrt.t) with
        | GA (a_id, e_ins, e_outs) -> (
            let getter = State.ga_to_getter a_id in
            let vs_ins = List.map (subst_in_expr_opt astate subst) e_ins in
            let failure = List.exists (fun x -> x = None) vs_ins in
            if failure then make_resource_fail ()
            else
              let vs_ins = List.map Option.get vs_ins in
              L.(
                verbose (fun m ->
                    m "Executing action: %s with ins: @[<h>%a@]" getter
                      Fmt.(list ~sep:comma Val.pp)
                      vs_ins));
              match State.execute_action getter state vs_ins with
              | [ Ok (state', vs') ] -> (
                  (* L.(
                     verbose (fun m ->
                         m "@[<v 2>Got state:@\n%a@] and values @[<h>%a@]" State.pp
                           state'
                           Fmt.(list ~sep:comma Val.pp)
                           vs')); *)
                  let vs_ins', vs_outs =
                    List_utils.split_at vs' (List.length vs_ins)
                  in
                  let remover = State.ga_to_deleter a_id in
                  match State.execute_action remover state' vs_ins' with
                  | [ Ok (state'', _) ] -> (
                      (* Separate outs into direct unifiables and others*)
                      match
                        unify_ins_outs_lists state'' subst step outs vs_outs
                          e_outs
                      with
                      | Ok () -> USucc (state'', preds, pred_defs, variants)
                      | Error fail_pf ->
                          UFail
                            [ EAsrt ([], Not fail_pf, [ [ Pure fail_pf ] ]) ])
                  | results -> (
                      let only_errors =
                        List.filter_map
                          (function
                            | Error e -> Some e
                            | Ok _ -> None)
                          results
                      in
                      match only_errors with
                      | [] ->
                          raise
                            (Exceptions.Unsupported
                               "unify_assertion: action remover returns \
                                multiple results")
                      | errs -> UFail errs))
              | results -> (
                  let only_errors =
                    List.filter_map
                      (function
                        | Error e -> Some e
                        | Ok _ -> None)
                      results
                  in
                  match only_errors with
                  | [] ->
                      raise
                        (Exceptions.Unsupported
                           "unify_assertion: action getter returns multiple \
                            results")
                  | errs -> UFail errs))
        | Pred (pname, les) -> (
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
              match
                consume_pred ~is_post ~in_unification:true astate pname vs
                  (Some (subst, step, outs, les_outs))
              with
              | Ok [] ->
                  L.verbose (fun m ->
                      m "SUCCEEDED WITH NOTHING! MEDOOOOOO!!!!!");
                  UWTF
              | Ok [ (astate', _) ] -> USucc astate'
              | Ok _ ->
                  raise (Failure "DEATH. BRANCHING GETPRED INSIDE UNIFICATION.")
              | Error errs ->
                  L.verbose (fun m -> m "Failed to unify against predicate.");
                  UFail errs)
        (* Conjunction should not be here *)
        | Pure (Formula.And _) ->
            raise (Failure "Unify assertion: And: should have been reduced")
        (* Other pure assertions *)
        | Pure f -> (
            let success, discharges =
              List.fold_left
                (fun (success, discharges) (u, out) ->
                  (* We know how to create the out *)
                  if not success then (false, discharges)
                  else
                    (* Perform the substitution in the out *)
                    match ESubst.subst_in_expr_opt subst out with
                    | None -> (false, discharges)
                    | Some out -> (
                        (* Convert obtained out to value *)
                        match Val.from_expr out with
                        | None -> (false, discharges)
                        | Some out -> (
                            (* And add to e-subst *)
                            match ESubst.get subst u with
                            | None ->
                                ESubst.put subst u out;
                                (true, discharges)
                            | Some out' when Val.equal out out' ->
                                (true, discharges)
                            | Some out' ->
                                ( true,
                                  Formula.Eq (Val.to_expr out, Val.to_expr out')
                                  :: discharges ))))
                (true, []) outs
            in
            match success with
            | false ->
                raise
                  (Failure
                     (Format.asprintf
                        "INTERNAL ERROR: Unification failure: do not know all \
                         ins for %a"
                        Formula.pp f))
            | true -> (
                (* To unify a pure formula we must know all ins *)
                let opf = ESubst.substitute_in_formula_opt subst f in
                match opf with
                | None ->
                    raise
                      (Failure
                         (Format.asprintf
                            "Unification failure: do not know all ins for %a"
                            Formula.pp f))
                | Some pf ->
                    let discharges_pf =
                      List.fold_left
                        (fun ac x ->
                          match ac with
                          | Formula.True -> x
                          | _ -> And (ac, x))
                        True discharges
                    in
                    let discharges_pf =
                      Reduction.reduce_formula ~unification:true discharges_pf
                    in
                    if State.assert_a state [ And (pf, discharges_pf) ] then
                      USucc astate
                    else
                      let vs = State.unfolding_vals state [ pf ] in
                      UFail [ EAsrt (vs, Not pf, [ [ Pure pf ] ]) ]))
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
                  | Some t' -> if t <> t' then False :: ac else ac
                  | None ->
                      Eq (UnOp (TypeOf, Val.to_expr v_le), Lit (Type t)) :: ac)
                [] les
            in

            match corrections with
            | [] -> USucc astate
            | _ ->
                let les, _ = List.split les in
                let les = List.map (subst_in_expr_opt astate subst) les in
                UFail
                  [
                    EAsrt
                      ( List.map Option.get
                          (List.filter (fun x -> x <> None) les),
                        Not (Formula.conjunct corrections),
                        [ [ Pure (Formula.conjunct corrections) ] ] );
                  ])
        (* LTrue, LFalse, LEmp, LStar *)
        | _ -> raise (Failure "Illegal Assertion in Unification Plan"))

  and unify_up'
      ~is_post
      (parent_ids : L.Report_id.t list ref)
      (s_states : search_state') : up_u_res =
    let s_states, errs_so_far = s_states in
    L.(
      verbose (fun m ->
          m "Unify UP: There are %d states left to consider."
            (List.length s_states)));
    let f = unify_up' ~is_post parent_ids in
    match s_states with
    | [] -> Error errs_so_far
    | ((state, subst, up), target_case_depth, is_new_case) :: rest -> (
        let case_depth =
          structure_unify_case_reports parent_ids target_case_depth is_new_case
            state subst up
        in
        let cur_step : UP.step option = UP.head up in
        let ret =
          try
            Option.fold
              ~some:(unify_assertion ~is_post state subst)
              ~none:(USucc state) cur_step
          with err -> (
            L.verbose (fun fmt ->
                fmt
                  "WARNING: UNCAUGHT EXCEPTION IN UNIFY-ASSERTION : %s@\n\
                   Here's the backtrace: %s" (Printexc.to_string err)
                  (Printexc.get_backtrace ()));
            let a = fst (Option.get cur_step) in
            match a with
            | Pure pf ->
                let bstate, _, _, _ = state in
                let vs = State.unfolding_vals bstate [ pf ] in
                UFail [ EAsrt (vs, Not pf, [ [ Pure pf ] ]) ]
            | _ -> UFail [])
        in
        match ret with
        | UWTF -> L.fail "Impossible, WTF?"
        | USucc state' -> (
            match UP.next up with
            | None ->
                let posts = UP.posts up in
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
                |> ignore;
                List_res.return (state', subst, posts)
            | Some [ (up, lab) ] ->
                if complete_subst subst lab then
                  f
                    ( ((state', subst, up), case_depth, false) :: rest,
                      errs_so_far )
                else f (rest, errs_so_far)
            | Some ((up, lab) :: ups') ->
                let next_states =
                  List.map
                    (fun (up, lab) ->
                      let new_subst = ESubst.copy subst in
                      let new_state = copy_astate state' in
                      if complete_subst new_subst lab then
                        Some (new_state, new_subst, up)
                      else None)
                    ups'
                in
                let next_states = List_utils.get_list_somes next_states in
                let next_states =
                  if complete_subst subst lab then
                    (state', subst, up) :: next_states
                  else next_states
                in
                let next_states =
                  next_states
                  |> List.map (fun state -> (state, case_depth + 1, true))
                in
                f (next_states @ rest, errs_so_far)
            | Some [] -> L.fail "ERROR: unify_up: empty unification plan")
        | UFail errors ->
            UnifyResultReport.log
              (Failure
                 { astate = AstateRec.from state; cur_step; subst; errors })
            |> ignore;
            f (rest, errors @ errs_so_far))

  and unify_up ~is_post (s_states : search_state) : up_u_res =
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
      (unify_kind : unify_kind) : up_u_res =
    let astate_i = copy_astate astate in
    let subst_i = ESubst.copy subst in
    let can_fix errs = List.exists State.can_fix errs in
    UnifyReport.as_parent
      { astate = AstateRec.from astate; subst; up; unify_kind }
      (fun () ->
        let ret = unify_up ~is_post ([ (astate, subst, up) ], []) in
        match ret with
        | Ok _ ->
            L.verbose (fun fmt -> fmt "Unifier.unify: Success");
            ret
        | Error errs
          when !Config.unfolding && can_fix errs && not in_unification -> (
            L.verbose (fun fmt -> fmt "Unifier.unify: Failure");
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
                Error errs
            | Ok sp -> (
                L.verbose (fun m ->
                    m "Unfolding successful: %d results" (List.length sp));
                let open List_res.Syntax in
                let* astate = Ok sp in
                match unfold_concrete_preds astate with
                | None -> Error []
                | Some (_, astate) ->
                    (* let subst'' = compose_substs (Subst.to_list subst_i) subst (Subst.init []) in *)
                    let subst'' = ESubst.copy subst_i in
                    unify_up ~is_post ([ (astate, subst'', up) ], [])))
        | Error _ ->
            L.verbose (fun fmt -> fmt "Unifier.unify: Failure");
            ret)

  and fold
      ?(is_post = false)
      ?(in_unification = false)
      ?(additional_bindings = [])
      ~unify_kind
      ~(state : t)
      (pred : UP.pred)
      (args : vt list) : (t, err_t) List_res.t =
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
      | Ok [] ->
          Fmt.(
            failwith "@[<h>HORROR: fold vanished for %s(%a) with bindings: %a@]"
              pred_name (Dump.list Val.pp) args
              (Dump.list @@ Dump.pair Expr.pp Val.pp)
              additional_bindings)
      | _ -> ()
    in
    let open List_res.Syntax in
    let* astate', subst', _ = unify_result in
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
    | None -> List_res.return astate'
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
        | Ok [] -> None
        | Ok [ (subst, astate'') ] ->
            L.(
              verbose (fun m ->
                  m "unfold_concrete_preds WORKED. Unfolded: %s(@[<h>%a])" name
                    Fmt.(list ~sep:comma Val.pp)
                    vs));
            Some (Some subst, astate'')
        | Ok _ ->
            failwith
              "Impossible: pred with concrete ins unfolded to multiple states."
        | Error errs ->
            Fmt.failwith
              "Impossible: pred with concrete ins and no guard failed to \
               unfold with errors: %a"
              Fmt.(Dump.list pp_err_t)
              errs)
    | None -> Some (None, astate)

  and try_recovering (astate : t) (tactic : vt Recovery_tactic.t) :
      (t list, string) result =
    let open Syntaxes.Result in
    L.verbose (fun m -> m "Attempting to recover");
    let- fold_error =
      match tactic.try_fold with
      | Some fold_values -> fold_guarded_with_vals astate fold_values
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
      (args : Val.t list) : (t, err_t) List_res.t =
    if fuel = 0 then failwith "RECURSIVE UNFOLD: OUT OF FUEL"
    else
      let open List_res.Syntax in
      let* _, astate = unfold astate pname args in
      let _, preds, _, _ = astate in
      match Preds.remove_by_name preds pname with
      | Some (pname, vs) -> rec_unfold ~fuel:(fuel - 1) astate pname vs
      | None -> List_res.return astate

  let unfold_all (astate : t) (pname : string) : (t, err_t) List_res.t =
    let _, preds, _, _ = astate in
    match Preds.remove_by_name preds pname with
    | None -> List_res.return astate
    | Some (pname, vs) -> rec_unfold astate pname vs
end
