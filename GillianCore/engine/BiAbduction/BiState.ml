open Containers
open Literal
module L = Logging

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : SState.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t) =
struct
  type vt = Val.t [@@deriving yojson, show]
  type st = ESubst.t
  type store_t = Store.t
  type heap_t = State.heap_t
  type state_t = State.t
  type t = SS.t * state_t * state_t
  type err_t = State.err_t [@@deriving yojson, show]
  type fix_t = State.fix_t
  type m_err_t = State.m_err_t
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type init_data = State.init_data

  exception Internal_State_Error of err_t list * t

  type action_ret = (t * vt list, err_t) result list
  type u_res = UWTF | USucc of t | UFail of err_t list

  let get_pred_defs (bi_state : t) : UP.preds_tbl_t option =
    let _, state, _ = bi_state in
    State.get_pred_defs state

  let make ~(procs : SS.t) ~(state : State.t) ~(init_data : State.init_data) : t
      =
    (procs, state, State.init init_data)

  let eval_expr (bi_state : t) (e : Expr.t) =
    let _, state, _ = bi_state in
    try State.eval_expr state e
    with State.Internal_State_Error (errs, _) ->
      raise (Internal_State_Error (errs, bi_state))

  let get_store (bi_state : t) : store_t =
    let _, state, _ = bi_state in
    State.get_store state

  let set_store (bi_state : t) (store : store_t) : t =
    let procs, state, af_state = bi_state in
    let state' = State.set_store state store in
    (procs, state', af_state)

  let to_loc (bi_state : t) (v : vt) : (t * vt) option =
    let procs, state, af_state = bi_state in
    match State.to_loc state v with
    | Some (state', loc) -> Some ((procs, state', af_state), loc)
    | None ->
        (* BIABDUCTION TODO: CREATE A NEW OBJECT *)
        None

  let assume ?(unfold = false) (bi_state : t) (v : vt) : t list =
    let procs, state, state_af = bi_state in
    let v_not = Val.from_expr (Expr.UnOp (UNot, Val.to_expr v)) in
    let bi_abduce =
      Option.fold ~some:(fun v -> State.sat_check state v) ~none:true v_not
    in
    List.map
      (fun state' ->
        if bi_abduce then
          match State.assume ~unfold state_af v with
          | [ state_af' ] -> (procs, state', state_af')
          | _ -> raise (Failure "DEATH. ASSUME BI-ABDUCTION")
        else (procs, state', state_af))
      (State.assume ~unfold state v)

  let assume_a
      ?(unification = false)
      ?(production = false)
      ?time:_
      (bi_state : t)
      (fs : Formula.t list) : t option =
    let procs, state, state_af = bi_state in
    match State.assume_a ~unification ~production state fs with
    | Some state -> Some (procs, state, state_af)
    | None -> None

  let assume_t (bi_state : t) (v : vt) (t : Type.t) : t option =
    let procs, state, state_af = bi_state in
    match State.assume_t state v t with
    | Some state -> Some (procs, state, state_af)
    | None -> None

  let sat_check (bi_state : t) (v : vt) : bool =
    let _, state, _ = bi_state in
    State.sat_check state v

  let sat_check_f (bi_state : t) (fs : Formula.t list) : ESubst.t option =
    let _, state, _ = bi_state in
    State.sat_check_f state fs

  let assert_a (bi_state : t) (fs : Formula.t list) : bool =
    let _, state, _ = bi_state in
    State.assert_a state fs

  let equals (bi_state : t) (v1 : vt) (v2 : vt) : bool =
    let _, state, _ = bi_state in
    State.equals state v1 v2

  let get_type (bi_state : t) (v : vt) : Type.t option =
    let _, state, _ = bi_state in
    State.get_type state v

  let copy (bi_state : t) : t =
    let procs, state, state_af = bi_state in
    (procs, State.copy state, State.copy state_af)

  let simplify
      ?(save = false)
      ?(kill_new_lvars : bool option)
      ?unification:_
      (bi_state : t) : ESubst.t * t list =
    let kill_new_lvars = Option.value ~default:true kill_new_lvars in
    let procs, state, state_af = bi_state in
    let subst, states = State.simplify ~save ~kill_new_lvars state in

    let states =
      List.concat_map
        (fun state ->
          let subst_af = ESubst.copy subst in
          let svars = State.get_spec_vars state in
          ESubst.filter_in_place subst_af (fun x x_v ->
              match x with
              | LVar x -> if SS.mem x svars then None else Some x_v
              | _ -> Some x_v);
          List.map
            (fun state_af -> (procs, state, state_af))
            (State.substitution_in_place subst_af state_af))
        states
    in

    (subst, states)

  let simplify_val (bi_state : t) (v : vt) : vt =
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

  let evaluate_slcmd (prog : 'a UP.prog) (lcmd : SLCmd.t) (bi_state : t) :
      (t list, err_t list) result =
    let procs, state, state_af = bi_state in
    Result.bind (State.evaluate_slcmd prog lcmd state) (fun x ->
        Ok (List.map (fun state' -> (procs, state', state_af)) x))

  let unify_invariant _ _ _ _ _ =
    raise (Failure "ERROR: unify_invariant called for bi-abductive execution")

  let frame_on _ _ _ =
    raise (Failure "ERROR: framing called for bi-abductive execution")

  let unfolding_vals (bi_state : t) (fs : Formula.t list) : vt list =
    let _, state, _ = bi_state in
    State.unfolding_vals state fs

  let substitution_in_place ?subst_all:_ (_ : st) (_ : t) =
    raise (Failure "substitution_in_place inside BI STATE")

  let fresh_val (_ : t) : vt = raise (Failure "fresh_val inside BI STATE")

  let fresh_loc ?loc:_ (_ : t) : vt =
    raise (Failure "fresh_loc inside BI STATE")

  let clean_up ?keep:_ (bi_state : t) : unit =
    let _, state, _ = bi_state in
    State.clean_up state

  let get_components (bi_state : t) : State.t * State.t =
    let _, state, state_af = bi_state in
    (state, state_af)

  let subst_in_val (subst : st) (v : vt) : vt =
    match
      Val.from_expr (ESubst.subst_in_expr subst ~partial:true (Val.to_expr v))
    with
    | Some v -> v
    | None -> v

  let compose_substs (subst1 : st) (subst2 : st) : st =
    let bindings =
      ESubst.fold subst1
        (fun x v bindings -> (x, subst_in_val subst2 v) :: bindings)
        []
    in
    ESubst.init bindings

  type post_res = (Flag.t * Asrt.t list) option

  let unify
      (_ : SS.t)
      (state : State.t)
      (state_af : State.t)
      (subst : ESubst.t)
      (up : UP.t) : (state_t * state_t * st * post_res) list =
    let search_state = ([ (state, state_af, subst, up) ], []) in

    let rec search search_state =
      match search_state with
      | [], rets -> rets
      | (state, state_af, subst, up) :: rest, rets -> (
          let cur_step : UP.step option = UP.head up in
          let ret : State.u_res =
            Option.fold
              ~some:(State.unify_assertion state subst)
              ~none:(State.USucc state) cur_step
          in
          match ret with
          | UWTF -> search (rest, rets)
          | USucc state' -> (
              match UP.next up with
              | None ->
                  L.verbose (fun m -> m "ONE SPEC IS DONE!!!@\n");
                  search (rest, (state', state_af, subst, UP.posts up) :: rets)
              | Some [ (up, _) ] ->
                  search ((state', state_af, subst, up) :: rest, rets)
              | Some ((up, _) :: ups') ->
                  let next_states =
                    (state', state_af, subst, up)
                    :: List.map
                         (fun (up, _) ->
                           ( State.copy state',
                             State.copy state_af,
                             ESubst.copy subst,
                             up ))
                         ups'
                  in
                  search (next_states @ rest, rets)
              | Some [] -> search (rest, rets))
          | UFail errs ->
              let cur_asrt = Option.map fst cur_step in
              L.verbose (fun m ->
                  m
                    "@[<v 2>WARNING: Unify Assertion Failed: %a with errors: \
                     %a. CUR SUBST:@\n\
                     %a@]@\n"
                    Fmt.(
                      option ~none:(any "no assertion - phantom node") Asrt.pp)
                    cur_asrt
                    Fmt.(list ~sep:(any "@\n") State.pp_err)
                    errs ESubst.pp subst);

              if List.exists State.can_fix errs then (
                L.(verbose (fun m -> m "CAN FIX!!!"));
                L.verbose (fun m ->
                    m "@[<v 2>My state is:@\n%a@]" State.pp state);
                let ffixes =
                  List.filter_map
                    (fun err ->
                      match State.get_fixes state err with
                      | [] -> None
                      | fixes -> Some fixes)
                    errs
                in
                L.verbose (fun m ->
                    m "Fixes:\n%a\n"
                      (Fmt.Dump.list @@ Fmt.Dump.list @@ State.pp_fix)
                      ffixes);
                let fixed_states =
                  List.map
                    (fun new_fixes ->
                      let state' = State.copy state in
                      let state_af' = State.copy state_af in
                      let state', _ = State.apply_fixes state' new_fixes in
                      let state_af', _ =
                        State.apply_fixes state_af' new_fixes
                      in
                      Option.map
                        (fun state' ->
                          let state_af' = Option.get state_af' in
                          L.(
                            verbose (fun m -> m "BEFORE THE SIMPLIFICATION!!!"));
                          (* FIXME: THIS NEEDS CARE *)
                          let new_subst, states = State.simplify state' in
                          assert (List.length states = 1);
                          let state' = List.hd states in
                          L.verbose (fun m ->
                              m "@[<v 2>SIMPLIFICATION SUBST:@\n%a@]" ESubst.pp
                                new_subst);
                          let subst' = compose_substs subst new_subst in
                          L.(
                            verbose (fun m ->
                                m "@[<v 2>AF BEFORE SIMPLIFICATION:@\n%a@]@\n"
                                  State.pp state_af'));
                          let svars = State.get_spec_vars state' in
                          ESubst.filter_in_place new_subst (fun x x_v ->
                              match x with
                              | LVar x ->
                                  if SS.mem x svars then None else Some x_v
                              | _ -> Some x_v);

                          (* TODO: THIS SUBST IN PLACE MUST NOT BRANCH *)
                          let subst_in_place =
                            State.substitution_in_place new_subst state_af'
                          in
                          assert (List.length subst_in_place = 1);

                          L.(
                            verbose (fun m ->
                                m "@[<v 2>AF AFTER SIMPLIFICATION:@\n%a@]\n"
                                  State.pp state_af'));
                          (state', state_af', subst'))
                        state')
                    ffixes
                in
                let fixed_states = List_utils.get_list_somes fixed_states in
                let next_search_states =
                  List.map
                    (fun (state, state_af, subst) ->
                      (state, state_af, subst, up))
                    fixed_states
                in

                (if next_search_states = [] then
                 L.(verbose (fun m -> m "TRIED TO FIX BUT CANNOT FIX!!!")));

                search (next_search_states @ rest, rets))
              else (
                L.(verbose (fun m -> m "CANNOT FIX!!!"));
                search (rest, rets)))
    in
    search search_state

  let update_store (state : State.t) (x : string option) (v : Val.t) : State.t =
    match x with
    | None -> state
    | Some x ->
        let store = State.get_store state in
        let _ = Store.put store x v in
        let state' = State.set_store state store in
        state'

  let run_spec
      (spec : UP.spec)
      (bi_state : t)
      (x : string)
      (args : vt list)
      (_ : (string * (string * vt) list) option) :
      ((t * Flag.t) list, err_t list) result =
    (* let start_time = time() in *)
    L.(
      verbose (fun m ->
          m "INSIDE RUN spec of %s with the following UP:@\n%a@\n"
            spec.data.spec_name UP.pp spec.up));
    (* FIXME: CARE *)
    let subst_i, states = simplify bi_state in
    assert (List.length states = 1);
    let bi_state = List.hd states in
    let args = List.map (subst_in_val subst_i) args in
    L.(
      verbose (fun m ->
          m "ARGS: @[<h>%a@]. SUBST:@\n%a"
            Fmt.(list ~sep:comma Val.pp)
            args ESubst.pp subst_i));

    let procs, state, state_af = bi_state in

    let old_store = State.get_store state in

    let new_store = Store.init (List.combine spec.data.spec_params args) in
    let state' = State.set_store state new_store in
    let store_bindings = Store.bindings new_store in
    let store_bindings =
      List.map (fun (x, v) -> (Expr.PVar x, v)) store_bindings
    in
    let subst = ESubst.init store_bindings in

    L.(
      verbose (fun m ->
          m
            "@[<v 2>About to use the spec of %s with the following UP inside \
             BI-ABDUCTION:@\n\
             %a@]@\n"
            spec.data.spec_name UP.pp spec.up));
    let ret_states = unify procs state' state_af subst spec.up in
    L.(
      verbose (fun m ->
          m "Concluding unification With %d results" (List.length ret_states)));

    let result =
      List.concat
        (List.map
           (fun (frame_state, state_af, subst, posts) ->
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
                     ESubst.pp subst State.pp frame_state State.pp state_af));

             let sp : State.t list =
               State.produce_posts frame_state subst posts
             in
             List.map
               (fun (final_state : State.t) ->
                 let state_af' : State.t = State.copy state_af in
                 let final_store : Store.t = State.get_store final_state in
                 let v_ret : vt option =
                   Store.get final_store Names.return_variable
                 in
                 let final_state' : State.t =
                   State.set_store final_state (Store.copy old_store)
                 in
                 let v_ret : Val.t =
                   Option.value
                     ~default:(Option.get (Val.from_expr (Lit Undefined)))
                     v_ret
                 in
                 let final_state' : State.t =
                   update_store final_state' (Some x) v_ret
                 in
                 (* FIXME: NOT WORKING DUE TO SIMPLIFICATION TYPE CHANGING *)
                 let _ = State.simplify ~unification:true final_state' in
                 let bi_state : t = (procs, final_state', state_af') in

                 L.(
                   verbose (fun m ->
                       m
                         "@[<v 2>At the end of unification with AF:@\n\
                          %a@]@\n\
                          @[<v 2>AND STATE:@\n\
                          %a@]@\n"
                         State.pp state_af' State.pp final_state'));

                 (bi_state, fl))
               sp)
           ret_states)
    in
    (* update_statistics "run_spec" (time() -. start_time); *)
    Ok result

  let produce_posts (_ : t) (_ : st) (_ : Asrt.t list) : t list =
    raise (Failure "produce_posts from bi_state.")

  let produce (_ : t) (_ : st) (_ : Asrt.t) : (t list, err_t list) result =
    raise (Failure "produce_posts from bi_state.")

  let unify_assertion (_ : t) (_ : st) (_ : UP.step) : u_res =
    raise (Failure "Unify assertion from bi_state.")

  let update_subst (_ : t) (_ : st) : unit = ()

  (* to throw errors: *)

  let get_fixes (_ : t) (_ : err_t) : fix_t list =
    raise (Failure "get_fixes not implemented in MakeBiState")

  let apply_fixes (_ : t) (_ : fix_t list) : t option * Asrt.t list =
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
  let ga_to_setter (a : string) : string = State.ga_to_setter a
  let ga_to_getter (a : string) : string = State.ga_to_getter a
  let ga_to_deleter (a : string) : string = State.ga_to_deleter a

  let rec execute_action
      ?(unification = false)
      (action : string)
      (astate : t)
      (args : vt list) : action_ret =
    let open Syntaxes.List in
    let procs, state, state_af = astate in
    let* ret = State.execute_action ~unification action state args in
    match ret with
    | Ok (st, outs) -> [ Ok ((procs, st, state_af), outs) ]
    | Error err when not (State.can_fix err) -> [ Error err ]
    | Error err -> (
        match State.get_fixes state err with
        | [] -> [] (* No fix, we stop *)
        | fix -> (
            let state' = State.copy state in
            let state_af' = State.copy state_af in
            let state', _ = State.apply_fixes state' fix in
            let state_af', _ = State.apply_fixes state_af' fix in
            match (state', state_af') with
            | Some state', Some state_af' ->
                execute_action action (procs, state', state_af') args
            | _ -> (* If we fail to fix, we give up *) []))

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
