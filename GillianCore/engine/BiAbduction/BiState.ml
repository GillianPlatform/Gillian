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
  module Preds = Preds.Make (Val) (ESubst)
  module AState = PState.Make (Val) (ESubst) (Store) (State) (Preds)

  type vt = Val.t [@@deriving yojson, show]
  type st = ESubst.t
  type store_t = Store.t
  type heap_t = State.heap_t
  type state_t = State.t
  type t = SS.t * state_t * state_t
  type err_t = State.err_t [@@deriving yojson]
  type fix_t = State.fix_t
  type m_err_t = State.m_err_t
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]

  exception Internal_State_Error of err_t list * t

  type action_ret = ASucc of (t * vt list) list | AFail of err_t list
  type u_res = UWTF | USucc of t | UFail of err_t list

  let merge_action_results (rets : action_ret list) : action_ret =
    let ret_succs, ret_fails =
      List.partition
        (fun ret ->
          match ret with
          | ASucc _ -> true
          | _ -> false)
        rets
    in
    if ret_fails <> [] then
      let errs =
        List.map
          (fun ret ->
            match ret with
            | AFail errs -> errs
            | _ -> [])
          ret_fails
      in
      AFail (List.concat errs)
    else
      let rets =
        List.map
          (fun ret ->
            match ret with
            | ASucc rets -> rets
            | _ -> [])
          ret_succs
      in
      ASucc (List.concat rets)

  let init ?(preds : UP.preds_tbl_t option) ?(variants : variants_t option) () :
      t =
    let _ = variants in
    (SS.empty, State.init ?preds (), State.init ?preds ())

  let get_pred_defs (bi_state : t) : UP.preds_tbl_t option =
    let _, state, _ = bi_state in
    State.get_pred_defs state

  let initialise
      (procs : SS.t)
      (state : State.t)
      (pred_defs : UP.preds_tbl_t option) : t =
    (procs, state, State.init ?preds:pred_defs ())

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

  let evaluate_slcmd (prog : UP.prog) (lcmd : SLCmd.t) (bi_state : t) :
      (t list, string) result =
    let procs, state, state_af = bi_state in
    Result.bind (State.evaluate_slcmd prog lcmd state) (fun x ->
        Ok (List.map (fun state' -> (procs, state', state_af)) x))

  let unify_invariant _ _ _ _ _ =
    raise (Failure "ERROR: unify_invariant called for bi-abductive execution")

  let clear_resource _ =
    raise (Failure "ERROR: clear_resource called for bi-abdutive execution")

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

              if State.can_fix errs then (
                L.(verbose (fun m -> m "CAN FIX!!!"));
                L.verbose (fun m ->
                    m "@[<v 2>My state is:@\n%a@]" State.pp state);
                let ffixes = State.get_fixes state errs in
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
                          assert (subst_in_place = []);

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
      (_ : (string * (string * vt) list) option) : (t * Flag.t) list =
    (* let start_time = time() in *)
    L.(
      verbose (fun m ->
          m "INSIDE RUN spec of %s with the following UP:@\n%a@\n"
            spec.spec.spec_name UP.pp spec.up));
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

    let new_store = Store.init (List.combine spec.spec.spec_params args) in
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
            spec.spec.spec_name UP.pp spec.up));
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
                       spec.spec.spec_name
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
    result

  let produce_posts (_ : t) (_ : st) (_ : Asrt.t list) : t list =
    raise (Failure "produce_posts from bi_state.")

  let produce (_ : t) (_ : st) (_ : Asrt.t) : (t list, string) result =
    raise (Failure "produce_posts from bi_state.")

  let unify_assertion (_ : t) (_ : st) (_ : UP.step) : u_res =
    raise (Failure "Unify assertion from bi_state.")

  let update_subst (_ : t) (_ : st) : unit = ()

  (* to throw errors: *)

  let get_fixes ?simple_fix:(_ = true) (_ : t) (_ : err_t list) :
      fix_t list list =
    raise (Failure "get_fixes not implemented in MakeBiState")

  let apply_fixes (_ : t) (_ : fix_t list) : t option * Asrt.t list =
    raise (Failure "apply_fixes not implemented in MakeBiState")

  let get_recovery_vals (_ : t) (_ : err_t list) : vt list =
    raise (Failure "get_recovery_vals not implemented in MakeBiState")

  let automatic_unfold _ _ : (t list, string) result =
    Error "Automatic unfold not supported in bi-abduction yet"

  let struct_init
      ?preds:_
      ?variants:_
      (_ : Store.t)
      (_ : PFS.t)
      (_ : TypEnv.t)
      (_ : SS.t) : t =
    raise (Failure "struct_init not implemented in MakeBiState")

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
    let procs, state, state_af = astate in
    match State.execute_action ~unification action state args with
    | State.ASucc rets ->
        let rets' =
          List.map (fun (st, outs) -> ((procs, st, state_af), outs)) rets
        in
        ASucc rets'
    | State.AFail errs when State.can_fix errs ->
        L.(
          verbose (fun m ->
              m "BState: EA: %d errors and they can be fixed."
                (List.length errs)));
        let rets =
          List.map
            (fun fix ->
              let state' = State.copy state in
              let state_af' = State.copy state_af in
              let state', _ = State.apply_fixes state' fix in
              let state_af', _ = State.apply_fixes state_af' fix in
              match (state', state_af') with
              | Some state', Some state_af' ->
                  Some (execute_action action (procs, state', state_af') args)
              | _ -> None)
            (State.get_fixes ~simple_fix:false state errs)
        in
        let rets = List_utils.get_list_somes rets in
        merge_action_results rets
    | State.AFail errs -> AFail errs

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
