open Containers
open Literal
module L = Logging

module Make (State : SState.S) = struct
  type heap_t = State.heap_t
  type state_t = State.t
  type st = SVal.SESubst.t
  type vt = Expr.t [@@deriving show, yojson]
  type t = SS.t * state_t * state_t
  type store_t = SStore.t
  type err_t = State.err_t [@@deriving yojson, show]
  type fix_t = State.fix_t
  type m_err_t = State.m_err_t
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type init_data = State.init_data

  exception Internal_State_Error of err_t list * t

  type action_ret = (t * Expr.t list, err_t) result list

  let make ~(procs : SS.t) ~(state : State.t) ~(init_data : State.init_data) : t
      =
    (procs, state, State.init init_data)

  let eval_expr (bi_state : t) (e : Expr.t) =
    let _, state, _ = bi_state in
    try State.eval_expr state e
    with State.Internal_State_Error (errs, _) ->
      raise (Internal_State_Error (errs, bi_state))

  let get_store (bi_state : t) : SStore.t =
    let _, state, _ = bi_state in
    State.get_store state

  let set_store (bi_state : t) (store : SStore.t) : t =
    let procs, state, af_state = bi_state in
    let state' = State.set_store state store in
    (procs, state', af_state)

  let assume ?(unfold = false) (bi_state : t) (v : Expr.t) : t list =
    let procs, state, state_af = bi_state in
    let new_states = State.assume ~unfold state v in
    match new_states with
    | [] -> []
    | first_state :: rest ->
        (* Slight optim, we don't copy the anti_frame if we have only one state. *)
        let rest =
          List.map (fun state' -> (procs, state', State.copy state_af)) rest
        in
        (procs, first_state, state_af) :: rest

  let assume_a
      ?(matching = false)
      ?(production = false)
      ?time:_
      (bi_state : t)
      (fs : Formula.t list) : t option =
    let procs, state, state_af = bi_state in
    match State.assume_a ~matching ~production state fs with
    | Some state -> Some (procs, state, state_af)
    | None -> None

  let assume_t (bi_state : t) (v : Expr.t) (t : Type.t) : t option =
    let procs, state, state_af = bi_state in
    match State.assume_t state v t with
    | Some state -> Some (procs, state, state_af)
    | None -> None

  let sat_check (bi_state : t) (v : Expr.t) : bool =
    let _, state, _ = bi_state in
    State.sat_check state v

  let sat_check_f (bi_state : t) (fs : Formula.t list) : SVal.SESubst.t option =
    let _, state, _ = bi_state in
    State.sat_check_f state fs

  let assert_a (bi_state : t) (fs : Formula.t list) : bool =
    let _, state, _ = bi_state in
    State.assert_a state fs

  let equals (bi_state : t) (v1 : Expr.t) (v2 : Expr.t) : bool =
    let _, state, _ = bi_state in
    State.equals state v1 v2

  let get_type (bi_state : t) (v : Expr.t) : Type.t option =
    let _, state, _ = bi_state in
    State.get_type state v

  let copy (bi_state : t) : t =
    let procs, state, state_af = bi_state in
    (procs, State.copy state, State.copy state_af)

  let simplify
      ?(save = false)
      ?(kill_new_lvars : bool option)
      ?matching:_
      (bi_state : t) : SVal.SESubst.t * t list =
    let kill_new_lvars = Option.value ~default:true kill_new_lvars in
    let procs, state, state_af = bi_state in
    let subst, states = State.simplify ~save ~kill_new_lvars state in

    let states =
      List.concat_map
        (fun state ->
          let subst_af = SVal.SESubst.copy subst in
          let svars = State.get_spec_vars state in
          SVal.SESubst.filter_in_place subst_af (fun x x_v ->
              match x with
              | LVar x -> if SS.mem x svars then None else Some x_v
              | _ -> Some x_v);
          List.map
            (fun state_af -> (procs, state, state_af))
            (State.substitution_in_place subst_af state_af))
        states
    in

    (subst, states)

  let simplify_val (bi_state : t) (v : Expr.t) : Expr.t =
    let _, state, _ = bi_state in
    State.simplify_val state v

  let pp fmt bi_state =
    let procs, state, state_af = bi_state in
    Fmt.pf fmt "PROCS:@\n@[<h>%a@]@\nMAIN STATE:@\n%a@\nANTI FRAME:@\n%a@\n"
      Fmt.(iter ~sep:comma SS.iter string)
      procs State.pp state State.pp state_af

  (* TODO: By-need formatter *)
  let pp_by_need _ _ _ fmt state = pp fmt state

  let add_spec_vars (bi_state : t) (vs : Var.Set.t) : t =
    let procs, state, state_af = bi_state in
    let state' = State.add_spec_vars state vs in
    (procs, state', state_af)

  let get_spec_vars (bi_state : t) : Var.Set.t =
    let _, state, _ = bi_state in
    State.get_spec_vars state

  let get_lvars (bi_state : t) : Var.Set.t =
    let _, state, _ = bi_state in
    State.get_lvars state

  let to_assertions ?(to_keep : SS.t option) (bi_state : t) : Asrt.t list =
    let _, state, _ = bi_state in
    State.to_assertions ?to_keep state

  let evaluate_slcmd (prog : 'a MP.prog) (lcmd : SLCmd.t) (bi_state : t) :
      (t, err_t) Res_list.t =
    let open Res_list.Syntax in
    let procs, state, state_af = bi_state in
    let++ state' = State.evaluate_slcmd prog lcmd state in
    (procs, state', state_af)

  let match_invariant _ _ _ _ _ =
    raise (Failure "ERROR: match_invariant called for bi-abductive execution")

  let frame_on _ _ _ =
    raise (Failure "ERROR: framing called for bi-abductive execution")

  let unfolding_vals (bi_state : t) (fs : Formula.t list) : Expr.t list =
    let _, state, _ = bi_state in
    State.unfolding_vals state fs

  let substitution_in_place ?subst_all:_ (_ : SVal.SESubst.t) (_ : t) =
    raise (Failure "substitution_in_place inside BI STATE")

  let fresh_loc ?loc:_ (_ : t) : Expr.t =
    raise (Failure "fresh_loc inside BI STATE")

  let clean_up ?keep:_ (bi_state : t) : unit =
    let _, state, _ = bi_state in
    State.clean_up state

  let get_components (bi_state : t) : State.t * State.t =
    let _, state, state_af = bi_state in
    (state, state_af)

  let subst_in_val (subst : SVal.SESubst.t) (v : Expr.t) : Expr.t =
    SVal.SESubst.subst_in_expr subst ~partial:true v

  let compose_substs (subst1 : SVal.SESubst.t) (subst2 : SVal.SESubst.t) :
      SVal.SESubst.t =
    let bindings =
      SVal.SESubst.fold subst1
        (fun x v bindings -> (x, subst_in_val subst2 v) :: bindings)
        []
    in
    SVal.SESubst.init bindings

  type post_res = (Flag.t * Asrt.t list) option

  let match_
      (_ : SS.t)
      (state : State.t)
      (state_af : State.t)
      (subst : SVal.SESubst.t)
      (mp : MP.t) : (state_t * state_t * SVal.SESubst.t * post_res) list =
    if not !Config.under_approximation then (
      L.print_to_all "Running bi-abduction without under-approximation?\n";
      exit 1);
    let open Syntaxes.List in
    let rec search next_state =
      let state, state_af, subst, mp = next_state in
      match mp with
      | MP.ConsumeStep (step, rest_up) -> (
          let matching_results = State.match_assertion state subst step in
          let should_copy =
            match matching_results with
            | _ :: _ :: _ -> true
            | _ -> false
          in
          let* matching_result = matching_results in
          match matching_result with
          | Ok state' ->
              if should_copy then
                search
                  ( State.copy state',
                    State.copy state_af,
                    SVal.SESubst.copy subst,
                    rest_up )
              else search (state', state_af, subst, rest_up)
          | Error err ->
              L.verbose (fun m ->
                  m
                    "@[<v 2>WARNING: Match Assertion Failed: %a with error: \
                     %a. CUR SUBST:@\n\
                     %a@]@\n"
                    Asrt.pp (fst step) State.pp_err err SVal.SESubst.pp subst);
              if not (State.can_fix err) then (
                L.verbose (fun m -> m "CANNOT FIX!");
                [])
              else (
                L.verbose (fun m -> m "May be able to fix!!!");
                let* fixes = State.get_fixes state err in
                (* TODO: a better implementation here might be to say that apply_fix returns a list of fixed states, possibly empty *)
                let state' = State.copy state in
                let state_af' = State.copy state_af in
                let* state' = State.apply_fixes state' fixes in
                let* state_af' = State.apply_fixes state_af' fixes in
                L.verbose (fun m -> m "BEFORE THE SIMPLIFICATION!!!");
                let new_subst, states = State.simplify state' in
                let state' =
                  match states with
                  | [ x ] -> x
                  | _ ->
                      L.fail
                        "Expected exactly one state after simplifying fixed \
                         state"
                in
                L.verbose (fun m ->
                    m "@[<v 2>SIMPLIFICATION SUBST:@\n%a@]" SVal.SESubst.pp
                      new_subst);
                let subst' = compose_substs subst new_subst in
                L.(
                  verbose (fun m ->
                      m "@[<v 2>AF BEFORE SIMPLIFICATION:@\n%a@]@\n" State.pp
                        state_af'));
                let svars = State.get_spec_vars state' in
                SVal.SESubst.filter_in_place new_subst (fun x x_v ->
                    match x with
                    | LVar x -> if SS.mem x svars then None else Some x_v
                    | _ -> Some x_v);
                let subst_afs =
                  State.substitution_in_place new_subst state_af'
                in
                let state_af' =
                  match subst_afs with
                  | [ x ] -> x
                  | _ ->
                      L.fail "Subst in place is not allowed to branch on AF!!!!"
                in
                L.(
                  verbose (fun m ->
                      m "@[<v 2>AF AFTER SIMPLIFICATION:@\n%a@]\n" State.pp
                        state_af'));
                search (state', state_af', subst', mp)))
      | Choice (left, right) ->
          let state_copy = State.copy state in
          let state_af_copy = State.copy state_af in
          let subst_copy = SVal.SESubst.copy subst in
          let left = search (state, state_af, subst, left) in
          let right = search (state_copy, state_af_copy, subst_copy, right) in
          left @ right
      | Finished post ->
          L.verbose (fun m -> m "ONE SPEC IS DONE!!!@\n");
          [ (state, state_af, subst, post) ]
      | LabelStep _ -> L.fail "DEATH: LABEL STEP IN BI-ABDUCTION"
    in
    search (state, state_af, subst, mp)

  let update_store (state : State.t) (x : string option) (v : Expr.t) : State.t
      =
    match x with
    | None -> state
    | Some x ->
        let store = State.get_store state in
        let _ = SStore.put store x v in
        let state' = State.set_store state store in
        state'

  let run_spec
      (spec : MP.spec)
      (bi_state : t)
      (x : string)
      (args : Expr.t list)
      (_ : (string * (string * Expr.t) list) option) : (t * Flag.t) list =
    (* let start_time = time() in *)
    L.(
      verbose (fun m ->
          m "INSIDE RUN spec of %s with the following MP:@\n%a@\n"
            spec.data.spec_name MP.pp spec.mp));
    (* FIXME: CARE *)
    let subst_i, states = simplify bi_state in
    assert (List.length states = 1);
    let bi_state = List.hd states in
    let args = List.map (subst_in_val subst_i) args in
    L.(
      verbose (fun m ->
          m "ARGS: @[<h>%a@]. SUBST:@\n%a"
            Fmt.(list ~sep:comma Expr.pp)
            args SVal.SESubst.pp subst_i));

    let procs, state, state_af = bi_state in

    let old_store = State.get_store state in

    let new_store = SStore.init (List.combine spec.data.spec_params args) in
    let state' = State.set_store state new_store in
    let store_bindings = SStore.bindings new_store in
    let store_bindings =
      List.map (fun (x, v) -> (Expr.PVar x, v)) store_bindings
    in
    let subst = SVal.SESubst.init store_bindings in

    L.(
      verbose (fun m ->
          m
            "@[<v 2>About to use the spec of %s with the following MP inside \
             BI-ABDUCTION:@\n\
             %a@]@\n"
            spec.data.spec_name MP.pp spec.mp));
    let ret_states = match_ procs state' state_af subst spec.mp in
    L.(
      verbose (fun m ->
          m "Concluding matching With %d results" (List.length ret_states)));
    let open Syntaxes.List in
    let* frame_state, state_af, subst, posts = ret_states in
    let fl, posts =
      match posts with
      | Some (fl, posts) -> (fl, posts)
      | _ ->
          let msg =
            Printf.sprintf
              "SYNTAX ERROR: Spec of %s does not have a postcondition"
              spec.data.spec_name
          in
          L.normal (fun m -> m "%s" msg);
          raise (Failure msg)
    in

    L.(
      verbose (fun m ->
          m
            "@[<v 2>SUBST:@\n\
             %a@]\n\
             @[<v 2>FRAME STATE:@\n\
             %a@]@\n\
             @[<v 2>ANTI FRAME:@\n\
             %a@]@\n"
            SVal.SESubst.pp subst State.pp frame_state State.pp state_af));
    let+ final_state = State.produce_posts frame_state subst posts in
    let state_af' : State.t = State.copy state_af in
    let final_store : SStore.t = State.get_store final_state in
    let v_ret : Expr.t option = SStore.get final_store Names.return_variable in
    let final_state' : State.t =
      State.set_store final_state (SStore.copy old_store)
    in
    let v_ret : Expr.t = Option.value ~default:(Lit Undefined) v_ret in
    let final_state' : State.t = update_store final_state' (Some x) v_ret in
    (* FIXME: NOT WORKING DUE TO SIMPLIFICATION TYPE CHANGING *)
    let _ = State.simplify ~matching:true final_state' in
    let bi_state : t = (procs, final_state', state_af') in

    L.(
      verbose (fun m ->
          m
            "@[<v 2>At the end of matching with AF:@\n\
             %a@]@\n\
             @[<v 2>AND STATE:@\n\
             %a@]@\n"
            State.pp state_af' State.pp final_state'));

    (bi_state, fl)

  let run_spec
      (spec : MP.spec)
      (bi_state : t)
      (x : string)
      (args : Expr.t list)
      (_ : (string * (string * Expr.t) list) option) =
    Res_list.just_oks (run_spec spec bi_state x args None)

  let produce_posts (_ : t) (_ : SVal.SESubst.t) (_ : Asrt.t list) : t list =
    raise (Failure "produce_posts from bi_state.")

  let produce (_ : t) (_ : SVal.SESubst.t) (_ : Asrt.t) : (t, err_t) Res_list.t
      =
    raise (Failure "produce_posts from bi_state.")

  let match_assertion (_ : t) (_ : SVal.SESubst.t) (_ : MP.step) :
      (t, err_t) Res_list.t =
    raise (Failure "Match assertion from bi_state.")

  let update_subst (_ : t) (_ : SVal.SESubst.t) : unit = ()

  (* to throw errors: *)

  let get_fixes (_ : t) (_ : err_t) : fix_t list list =
    raise (Failure "get_fixes not implemented in MakeBiState")

  let apply_fixes (_ : t) (_ : fix_t list) : t list =
    raise (Failure "apply_fixes not implemented in MakeBiState")

  let get_recovery_tactic (_ : t) (_ : err_t list) =
    raise (Failure "get_recovery_tactic not implemented in MakeBiState")

  let try_recovering _ _ : (t list, string) result =
    Error "try_recovering not supported in bi-abduction yet"

  (** new functions *)

  let mem_constraints (bi_state : t) : Formula.t list =
    let _, state, _ = bi_state in
    State.mem_constraints state

  let is_overlapping_asrt (a : string) : bool = State.is_overlapping_asrt a
  let pp_err = State.pp_err
  let pp_fix = State.pp_fix
  let get_failing_constraint = State.get_failing_constraint
  let can_fix = State.can_fix

  let rec execute_action (action : string) (astate : t) (args : Expr.t list) :
      action_ret =
    let open Syntaxes.List in
    let procs, state, state_af = astate in
    let* ret = State.execute_action action state args in
    match ret with
    | Ok (state', outs) -> [ Ok ((procs, state', state_af), outs) ]
    | Error err when not (State.can_fix err) -> [ Error err ]
    | Error err -> (
        match State.get_fixes state err with
        | [] -> [] (* No fix, we stop *)
        | fixes ->
            let* fix = fixes in
            let state' = State.copy state in
            let state_af' = State.copy state_af in
            let* state' = State.apply_fixes state' fix in
            let* state_af' = State.apply_fixes state_af' fix in
            execute_action action (procs, state', state_af') args)

  let get_equal_values bi_state =
    let _, state, _ = bi_state in
    State.get_equal_values state

  let get_heap bi_state =
    let _, state, _ = bi_state in
    State.get_heap state

  let of_yojson _ =
    failwith
      "Please implement of_yojson to enable logging this type to a database"

  let to_yojson _ =
    failwith
      "Please implement to_yojson to enable logging this type to a database"
end
