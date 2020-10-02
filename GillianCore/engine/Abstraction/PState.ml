(**
    Interface for GIL Predicate States.
    They are considered to be mutable.
*)
module type S = sig
  include State.S

  type state_t

  type preds_t

  type abs_t = string * vt list

  val initialise : state_t -> preds_t -> UP.preds_tbl_t option -> t

  (** Get preds of given symbolic state *)
  val get_preds : t -> preds_t

  (** Set preds of given symbolic state *)
  val set_preds : t -> preds_t -> t

  val unify : t -> st -> UP.t -> bool

  val add_pred_defs : UP.preds_tbl_t -> t -> t

  val deabstract : t -> state_t * bool

  val get_all_preds : ?keep:bool -> (abs_t -> bool) -> t -> abs_t list

  val set_pred : t -> abs_t -> unit

  val automatic_unfold : t -> vt list -> (t list, string) result
end

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (Preds : Preds.S with type vt = Val.t and type st = ESubst.t) :
  S
    with type vt = Val.t
     and type st = ESubst.t
     and type store_t = Store.t
     and type state_t = State.t
     and type preds_t = Preds.t = struct
  open Containers
  open Literal
  module L = Logging

  type t = State.t * Preds.t * UP.preds_tbl_t

  type vt = Val.t

  type st = ESubst.t

  type store_t = Store.t

  type abs_t = Preds.abs_t

  type state_t = State.t

  type preds_t = Preds.t

  type err_t = State.err_t

  type fix_t = State.fix_t

  type m_err_t = State.m_err_t

  exception Internal_State_Error of err_t list * t

  exception Preprocessing_Error of UP.up_err_t list

  module Unifier = Unifier.Make (Val) (ESubst) (Store) (State) (Preds)

  type action_ret = ASucc of (t * vt list) list | AFail of err_t list

  type u_res = UWTF | USucc of t | UFail of err_t list

  let init (pred_defs : UP.preds_tbl_t option) : t =
    let empty_pred_defs : UP.preds_tbl_t = UP.init_pred_defs () in
    let new_pred_defs : UP.preds_tbl_t =
      Option.value ~default:empty_pred_defs pred_defs
    in
    (State.init pred_defs, Preds.init [], new_pred_defs)

  let struct_init
      (pred_defs : UP.preds_tbl_t option)
      (store : store_t)
      (pfs : PFS.t)
      (gamma : TypEnv.t)
      (svars : SS.t) : t =
    let empty_pred_defs : UP.preds_tbl_t = UP.init_pred_defs () in
    let new_pred_defs : UP.preds_tbl_t =
      Option.value ~default:empty_pred_defs pred_defs
    in
    let state = State.struct_init pred_defs store pfs gamma svars in
    (state, Preds.init [], new_pred_defs)

  let initialise
      (state : State.t) (preds : Preds.t) (pred_defs : UP.preds_tbl_t option) :
      t =
    let pred_defs = Option.value ~default:(UP.init_pred_defs ()) pred_defs in
    (state, preds, pred_defs)

  let get_pred_defs (state : t) : UP.preds_tbl_t option =
    let _, _, pred_defs = state in
    Some pred_defs

  let eval_expr (astate : t) (e : Expr.t) =
    let state, _, _ = astate in
    try State.eval_expr state e
    with State.Internal_State_Error (errs, s) ->
      raise (Internal_State_Error (errs, astate))

  let get_store (astate : t) : Store.t =
    let state, _, _ = astate in
    State.get_store state

  let set_store (astate : t) (store : Store.t) : t =
    let state, preds, pred_defs = astate in
    let state' = State.set_store state store in
    (state', preds, pred_defs)

  let get_preds (astate : t) : Preds.t =
    let _, preds, _ = astate in
    preds

  let set_preds (astate : t) (preds : Preds.t) : t =
    let state, _, pred_defs = astate in
    (state, preds, pred_defs)

  let to_loc (astate : t) (v : Val.t) : (t * Val.t) option =
    let state, preds, pred_defs = astate in
    match State.to_loc state v with
    | Some (state', loc) -> Some ((state', preds, pred_defs), loc)
    | None               -> None

  let assume ?(unfold = false) (astate : t) (v : Val.t) : t list =
    let state, preds, pred_defs = astate in
    List.concat
      (List.map
         (fun state ->
           let astate' = (state, preds, pred_defs) in
           match (!Config.unfolding && unfold, Val.to_expr v) with
           | _, Lit (Bool true) -> [ astate' ]
           | false, _           -> [ astate' ]
           | true, _            ->
               let unfold_vals = Expr.base_elements (Val.to_expr v) in
               let unfold_vals = List.map Val.from_expr unfold_vals in
               let unfold_vals =
                 List.map Option.get
                   (List.filter (fun x -> x <> None) unfold_vals)
               in
               let next_states, worked =
                 Unifier.unfold_with_vals astate' unfold_vals
               in
               if not worked then [ astate' ]
               else List.map (fun (_, astate) -> astate) next_states)
         (State.assume ~unfold state v))

  let assume_a
      ?(unification = false)
      ?(production = false)
      (astate : t)
      (fs : Formula.t list) : t option =
    let state, preds, pred_defs = astate in
    match State.assume_a ~unification ~production state fs with
    | Some state -> Some (state, preds, pred_defs)
    | None       -> None

  let assume_t (astate : t) (v : Val.t) (t : Type.t) : t option =
    let state, preds, pred_defs = astate in
    match State.assume_t state v t with
    | Some state -> Some (state, preds, pred_defs)
    | None       -> None

  let sat_check (astate : t) (v : Val.t) : bool =
    let state, _, _ = astate in
    State.sat_check state v

  let sat_check_f (astate : t) (fs : Formula.t list) : ESubst.t option =
    let state, _, _ = astate in
    State.sat_check_f state fs

  let assert_a (astate : t) (fs : Formula.t list) : bool =
    let state, _, _ = astate in
    State.assert_a state fs

  let equals (astate : t) (v1 : Val.t) (v2 : Val.t) : bool =
    let state, _, _ = astate in
    State.equals state v1 v2

  let get_type (astate : t) (v : Val.t) : Type.t option =
    let state, _, _ = astate in
    State.get_type state v

  let copy (astate : t) : t =
    let state, preds, pred_defs = astate in
    (State.copy state, Preds.copy preds, pred_defs)

  let simplify
      ?(save = false)
      ?(kill_new_lvars : bool option)
      ?(unification = false)
      (astate : t) : ESubst.t =
    let state, preds, _ = astate in
    let subst = State.simplify ~save ?kill_new_lvars ~unification state in
    Preds.substitution_in_place subst preds;
    subst

  let simplify_val (astate : t) (v : Val.t) : Val.t =
    let state, _, _ = astate in
    State.simplify_val state v

  let pp fmt (astate : t) : unit =
    let state, preds, _ = astate in
    Fmt.pf fmt "%a@\n@[<v 2>PREDICATES:@\n%a@]@\n" State.pp state Preds.pp preds

  let add_spec_vars (astate : t) (vs : Var.Set.t) : t =
    let state, preds, pred_defs = astate in
    let state' = State.add_spec_vars state vs in
    (state', preds, pred_defs)

  let get_spec_vars (astate : t) : Var.Set.t =
    let state, _, _ = astate in
    State.get_spec_vars state

  let get_lvars (astate : t) : Var.Set.t =
    let state, _, _ = astate in
    State.get_lvars state

  let to_assertions ?(to_keep : SS.t option) (astate : t) : Asrt.t list =
    let state, preds, _ = astate in
    let s_asrts = State.to_assertions ?to_keep state in
    let p_asrts = Preds.to_assertions preds in
    List.sort Asrt.compare (p_asrts @ s_asrts)

  let substitution_in_place (subst : st) (astate : t) : unit =
    let state, preds, _ = astate in
    State.substitution_in_place subst state;
    Preds.substitution_in_place subst preds

  let update_store (state : t) (x : string option) (v : Val.t) : t =
    match x with
    | None   -> state
    | Some x ->
        let store = get_store state in
        let _ = Store.put store x v in
        let state' = set_store state store in
        state'

  (* FIXME: This needs to change -> we need to return a unification ret type, so we can
      compose with bi-abduction at the spec level *)
  let run_spec_aux
      ?(existential_bindings : (string * vt) list option)
      (name : string)
      (params : string list)
      (up : UP.t)
      (astate : t)
      (x : string option)
      (args : vt list) : ((t * Flag.t) list, Unifier.err_t list) result =
    L.verbose (fun m ->
        m "INSIDE RUN spec of %s with the following UP:@\n%a@\n" name UP.pp up);

    let old_store = get_store astate in
    let new_store = Store.init (List.combine params args) in
    let astate' = set_store astate new_store in
    let existential_bindings = Option.value ~default:[] existential_bindings in
    let existential_bindings =
      List.map (fun (x, v) -> (Expr.LVar x, v)) existential_bindings
    in
    let store_bindings = Store.bindings new_store in
    let store_bindings =
      List.map (fun (x, v) -> (Expr.PVar x, v)) store_bindings
    in
    let subst = ESubst.init (existential_bindings @ store_bindings) in

    L.verbose (fun m ->
        m "About to use the spec of %s with the following UP:@\n%a@\n" name
          UP.pp up);

    match Unifier.unify astate' subst up with
    | UPUSucc rets ->
        Ok
          ((* Successful Unification *)
           List.concat
             (List.map
                (fun (frame_state, subst, posts) ->
                  let fl, posts =
                    match posts with
                    | Some (fl, posts) -> (fl, posts)
                    | _                ->
                        let msg =
                          Printf.sprintf
                            "SYNTAX ERROR: Spec of %s does not have a \
                             postcondition"
                            name
                        in
                        L.normal (fun m -> m "%s" msg);
                        raise (Failure msg)
                  in

                  let sp = Unifier.produce_posts frame_state subst posts in

                  List.map
                    (fun final_state ->
                      let final_store = get_store final_state in
                      let v_ret = Store.get final_store Names.return_variable in
                      let final_state =
                        set_store final_state (Store.copy old_store)
                      in
                      let v_ret =
                        Option.value
                          ~default:(Option.get (Val.from_expr (Lit Undefined)))
                          v_ret
                      in
                      let final_state = update_store final_state x v_ret in
                      let _ = simplify ~unification:true final_state in
                      let subst, final_state =
                        Option.get (Unifier.unfold_concrete_preds final_state)
                      in
                      (final_state, fl))
                    sp)
                rets))
    | UPUFail errs ->
        let msg =
          Fmt.str
            "WARNING: Failed to unify against the precondition of procedure %s"
            name
        in
        L.normal (fun m -> m "%s" msg);
        Error errs

  let fresh_subst (xs : SS.t) : ESubst.t =
    let xs = SS.elements xs in
    let bindings =
      List.map
        (fun x ->
          (Expr.LVar x, Option.get (Val.from_expr (LVar (LVar.alloc ())))))
        xs
    in
    ESubst.init bindings

  let make_id_subst (a : Asrt.t) : ESubst.t =
    let lvars = Asrt.lvars a in
    let alocs = Asrt.alocs a in
    let lvars_subst =
      List.map
        (fun x -> (Expr.LVar x, Option.get (Val.from_expr (LVar x))))
        (SS.elements lvars)
    in
    let alocs_subst =
      List.map
        (fun x -> (Expr.ALoc x, Option.get (Val.from_expr (ALoc x))))
        (SS.elements alocs)
    in
    let subst_lst = lvars_subst @ alocs_subst in
    ESubst.init subst_lst

  let unify_invariant
      (prog : UP.prog)
      (revisited : bool)
      (astate : t)
      (a : Asrt.t)
      (binders : string list) : t * t =
    let state, preds, pred_defs = astate in
    let store = State.get_store state in
    let pvars_store = Store.domain store in
    let pvars_a = Asrt.pvars a in
    let pvars_diff = SS.diff pvars_a pvars_store in
    L.verbose (fun m -> m "%s" (String.concat ", " (SS.elements pvars_diff)));
    match pvars_diff = SS.empty with
    | false ->
        let pvars_errs : err_t list =
          List.map (fun pvar -> (EVar pvar : err_t)) (SS.elements pvars_diff)
        in
        raise (Internal_State_Error (pvars_errs, astate))
    | true  -> (
        (* TODO: I would like to not forget the store, but let's see... *)
        let store_subst = Store.to_ssubst store in
        let a = SVal.SESubst.substitute_asrt store_subst true a in
        let state_lvars = State.get_lvars state in
        let known_lvars =
          SS.elements
            (SS.diff (SS.inter state_lvars (Asrt.lvars a)) (SS.of_list binders))
        in
        let known_lvars = List.map (fun x -> Expr.LVar x) known_lvars in
        let asrt_alocs =
          List.map (fun x -> Expr.ALoc x) (SS.elements (Asrt.alocs a))
        in
        let known_unifiables = Expr.Set.of_list (known_lvars @ asrt_alocs) in
        let pred_ins =
          Hashtbl.fold
            (fun name (pred : Pred.t) pred_ins ->
              Hashtbl.add pred_ins name pred.pred_ins;
              pred_ins)
            prog.prog.preds
            (Hashtbl.create Config.medium_tbl_size)
        in
        let up =
          UP.init known_unifiables Expr.Set.empty pred_ins [ (a, (None, None)) ]
        in
        (* This will not do anything in the original pass,
           but will do precisely what is needed in the re-establishment *)
        let vars_to_forget = SS.inter state_lvars (SS.of_list binders) in
        if vars_to_forget <> SS.empty then (
          let oblivion_subst = fresh_subst vars_to_forget in
          L.verbose (fun m ->
              m "Forget @[%a@] with subst: %a"
                Fmt.(iter ~sep:comma SS.iter string)
                vars_to_forget ESubst.pp oblivion_subst);
          substitution_in_place oblivion_subst astate;
          L.verbose (fun m ->
              m "State after substitution:@\n@[%a@]\n" pp astate) )
        else ();
        match up with
        | Error asrts -> raise (Preprocessing_Error [ UPAssert (a, asrts) ])
        | Ok up       -> (
            let bindings =
              List.map
                (fun (e : Expr.t) ->
                  let id =
                    match e with
                    | LVar x | ALoc x -> e
                    | _               ->
                        raise
                          (Failure
                             "Impossible: unifiable not an lvar or an aloc")
                  in
                  (id, Option.get (Val.from_expr e)))
                (Expr.Set.elements known_unifiables)
            in
            let old_astate = copy astate in
            let subst = ESubst.init bindings in
            let u_ret = Unifier.unify astate subst up in
            match u_ret with
            | UPUSucc [ (new_state, subst', _) ] -> (
                (* Successful Unification *)
                (* TODO: understand if the frame state should have the subst produced *)
                let frame_state = copy new_state in

                let lbinders = List.map (fun x -> Expr.LVar x) binders in
                let new_bindings =
                  List.map (fun e -> (e, ESubst.get subst' e)) lbinders
                in
                let success =
                  List.for_all (fun (x, x_v) -> x_v <> None) new_bindings
                in
                if not success then
                  raise (Failure "Assert failed - binders not captured")
                else
                  let new_bindings =
                    List.map (fun (x, x_v) -> (x, Option.get x_v)) new_bindings
                  in
                  let bindings =
                    List.filter
                      (fun (e, v) ->
                        (not (List.mem e lbinders)) && e <> Val.to_expr v)
                      (ESubst.to_list subst')
                  in
                  L.verbose (fun fmt ->
                      fmt "Additional bindings: %a"
                        Fmt.(
                          brackets
                            (list ~sep:semi
                               (parens (pair ~sep:comma Expr.pp Val.pp))))
                        bindings);
                  let bindings =
                    match revisited with
                    | false -> bindings
                    | true  -> new_bindings @ bindings
                  in
                  let bindings =
                    List.map
                      (fun (e, e_v) -> Asrt.Pure (Eq (e, Val.to_expr e_v)))
                      bindings
                  in
                  let a_bindings = Asrt.star bindings in
                  let subst_bindings = make_id_subst a_bindings in
                  let full_subst = make_id_subst a in
                  let _ = ESubst.merge_left full_subst subst_bindings in
                  let a_substed =
                    ESubst.substitute_asrt subst_bindings true a
                  in
                  let a_produce = Asrt.star [ a_bindings; a_substed ] in
                  (* Create empty state *)
                  let empty_state =
                    (State.init None, Preds.init [], pred_defs)
                  in
                  let empty_state = set_store empty_state store in
                  match Unifier.produce empty_state full_subst a_produce with
                  | Some new_astate ->
                      let new_state, new_preds, pred_defs = new_astate in
                      let new_state' =
                        State.add_spec_vars new_state (SS.of_list binders)
                      in
                      let _ = State.simplify ~kill_new_lvars:false new_state' in
                      (frame_state, (new_state', new_preds, pred_defs))
                  | _               ->
                      let msg =
                        Fmt.str
                          "Assert failed with argument %a. unable to produce \
                           variable bindings."
                          Asrt.pp a
                      in
                      L.fail msg )
            | UPUSucc _ ->
                raise
                  (Exceptions.Unsupported
                     "sep_assert: unification resulted in multiple returns")
            | UPUFail errs ->
                let fail_pfs : Formula.t list =
                  List.map State.get_failing_constraint errs
                in
                let fail_pfs =
                  List.filter (fun (f : Formula.t) -> f <> True) fail_pfs
                in

                let failing_model = State.sat_check_f state fail_pfs in
                let () =
                  Fmt.pr
                    "Assert failed with argument @[<h>%a@]. Unification failed.@\n\
                     @[<v 2>Errors:@\n\
                     %a.@]@\n\
                     @[<v 2>Failing Model:@\n\
                     %a@]@\n"
                    Asrt.pp a
                    Fmt.(list ~sep:(any "@\n") State.pp_err)
                    errs
                    Fmt.(option ~none:(any "CANNOT CREATE MODEL") ESubst.pp)
                    failing_model
                in
                raise (Internal_State_Error (errs, old_astate)) ) )

  (**
    Evaluation of logic commands

    @param prog GIL program
    @param lcmd Logic command to be evaluated
    @param state Current state
    @param preds Current predicate set
    @return List of states/predicate sets resulting from the evaluation
  *)
  let rec evaluate_slcmd (prog : UP.prog) (lcmd : SLCmd.t) (astate : t) :
      (t list, string) result =
    let state, preds, pred_defs = astate in

    let eval_expr e =
      try State.eval_expr state e
      with State.Internal_State_Error (errs, s) ->
        raise (Internal_State_Error (errs, astate))
    in

    match lcmd with
    | Fold (pname, les, folding_info) -> (
        let pred = UP.get_pred_def prog.preds pname in
        match pred.pred.pred_abstract with
        | true  ->
            raise
              (Failure
                 (Format.asprintf "Impossible: Unfold of abstract predicate %s"
                    pname))
        | false -> (
            let vs = List.map eval_expr les in
            let params = List.map (fun (x, _) -> x) pred.pred.pred_params in
            let i_bindings =
              Option.fold
                ~some:(fun (def, bindings) ->
                  List.map (fun (x, e) -> (Expr.LVar x, eval_expr e)) bindings)
                ~none:[] folding_info
            in
            let param_bindings =
              if List.length params = List.length vs then List.combine params vs
              else List.combine (Pred.in_params pred.pred) vs
            in
            let param_bindings =
              List.map (fun (x, v) -> (Expr.PVar x, v)) param_bindings
            in
            let subst = ESubst.init (i_bindings @ param_bindings) in
            match Unifier.unify astate subst pred.up with
            | UPUSucc [ (astate', subst', _) ] ->
                let _, preds', _ = astate' in
                let arg_vs =
                  if List.length params = List.length vs then vs
                  else
                    let out_params = Pred.out_params pred.pred in
                    let vs_outs =
                      List.map
                        (fun x ->
                          let v_x = ESubst.get subst' (PVar x) in
                          match v_x with
                          | Some v_x -> v_x
                          | None     ->
                              raise (Failure "DEATH. evaluate_slcmd. fold"))
                        out_params
                    in
                    Pred.combine_ins_outs pred.pred vs vs_outs
                in
                Preds.extend ~pure:pred.pred.pred_pure preds' (pname, arg_vs);
                Ok [ astate' ]
            | _ ->
                let msg =
                  Fmt.str
                    "@[<h>IMPOSSIBLE FOLD for %s(%a) with folding_info: %a@]"
                    pname
                    Fmt.(list ~sep:comma Val.pp)
                    vs SLCmd.pp_folding_info folding_info
                in
                raise (Failure msg) ) )
    | Unfold (pname, les, unfold_info, b) -> (
        let pred = UP.get_pred_def prog.preds pname in
        match pred.pred.pred_abstract with
        | true  ->
            raise
              (Failure
                 (Format.asprintf "Impossible: Unfold of abstract predicate %s"
                    pname))
        | false -> (
            let vs = List.map eval_expr les in
            let vs_ins = Pred.in_args pred.pred vs in
            let vs = List.map (fun x -> Some x) vs in
            (* FIXME: make sure correct number of params *)
            match Unifier.get_pred astate pname vs with
            | GPSucc [ (_, vs') ] ->
                L.verbose (fun m ->
                    m "@[<h>Returned values: %a@]"
                      Fmt.(list ~sep:comma Val.pp)
                      vs');
                let vs = Pred.combine_ins_outs pred.pred vs_ins vs' in
                L.verbose (fun m ->
                    m "@[<h>LCMD Unfold about to happen with rec %b info: %a@]"
                      b SLCmd.pp_unfold_info unfold_info);
                if b then Ok [ Unifier.rec_unfold astate pname vs ]
                else (
                  L.verbose (fun m ->
                      m "@[<h>Values: %a@]" Fmt.(list ~sep:comma Val.pp) vs);
                  Ok
                    (List.map
                       (fun (_, state) -> state)
                       (Unifier.unfold astate pname vs unfold_info)) )
            | _                   -> raise (Failure "IMPOSSIBLE UNFOLD") ) )
    | GUnfold pname -> Ok [ Unifier.unfold_all astate pname ]
    | SepAssert (a, binders) -> (
        let store = State.get_store state in
        let pvars_store = Store.domain store in
        let pvars_a = Asrt.pvars a in
        let pvars_diff = SS.diff pvars_a pvars_store in
        L.verbose (fun m ->
            m "%s" (String.concat ", " (SS.elements pvars_diff)));
        match pvars_diff = SS.empty with
        | false ->
            let pvars_errs : err_t list =
              List.map
                (fun pvar -> (EVar pvar : err_t))
                (SS.elements pvars_diff)
            in
            raise (Internal_State_Error (pvars_errs, astate))
        | true  -> (
            let store_subst = Store.to_ssubst store in
            let a = SVal.SESubst.substitute_asrt store_subst true a in
            (* let known_vars   = SS.diff (SS.filter is_spec_var_name (Asrt.lvars a)) (SS.of_list binders) in *)
            let state_lvars = State.get_lvars state in
            let known_lvars =
              SS.elements
                (SS.diff
                   (SS.inter state_lvars (Asrt.lvars a))
                   (SS.of_list binders))
            in
            let known_lvars = List.map (fun x -> Expr.LVar x) known_lvars in
            let asrt_alocs =
              List.map (fun x -> Expr.ALoc x) (SS.elements (Asrt.alocs a))
            in
            let known_unifiables =
              Expr.Set.of_list (known_lvars @ asrt_alocs)
            in

            let pred_ins =
              Hashtbl.fold
                (fun name (pred : Pred.t) pred_ins ->
                  Hashtbl.add pred_ins name pred.pred_ins;
                  pred_ins)
                prog.prog.preds
                (Hashtbl.create Config.medium_tbl_size)
            in

            let up =
              UP.init known_unifiables Expr.Set.empty pred_ins
                [ (a, (None, None)) ]
            in
            let vars_to_forget = SS.inter state_lvars (SS.of_list binders) in
            if vars_to_forget <> SS.empty then (
              let oblivion_subst = fresh_subst vars_to_forget in
              L.verbose (fun m ->
                  m "Forget @[%a@] with subst: %a"
                    Fmt.(iter ~sep:comma SS.iter string)
                    vars_to_forget ESubst.pp oblivion_subst);
              substitution_in_place oblivion_subst astate;
              L.verbose (fun m ->
                  m "State after substitution:@\n@[%a@]\n" pp astate) )
            else ();
            match up with
            | Error asrts -> raise (Preprocessing_Error [ UPAssert (a, asrts) ])
            | Ok up       -> (
                let bindings =
                  List.map
                    (fun (e : Expr.t) ->
                      let id =
                        match e with
                        | LVar x | ALoc x -> e
                        | _               ->
                            raise
                              (Failure
                                 "Impossible: unifiable not an lvar or an aloc")
                      in
                      (id, Option.get (Val.from_expr e)))
                    (Expr.Set.elements known_unifiables)
                in
                let old_astate = copy astate in
                let subst = ESubst.init bindings in
                let u_ret = Unifier.unify astate subst up in
                match u_ret with
                | UPUSucc [ (new_state, subst', _) ] -> (
                    (* Successful Unification *)
                    let lbinders = List.map (fun x -> Expr.LVar x) binders in
                    let new_bindings =
                      List.map (fun e -> (e, ESubst.get subst' e)) lbinders
                    in
                    let success =
                      List.for_all (fun (x, x_v) -> x_v <> None) new_bindings
                    in
                    if not success then
                      raise (Failure "Assert failed - binders not captured")
                    else
                      let additional_bindings =
                        let eq_temp e vl =
                          let vl_e = Val.to_expr vl in
                          Expr.equal e vl_e
                        in
                        List.filter
                          (fun (e, v) ->
                            (not (List.mem e lbinders)) && not (eq_temp e v))
                          (ESubst.to_list subst')
                      in
                      let new_bindings =
                        List.concat
                          [
                            new_bindings;
                            List.map
                              (fun (x, y) -> (x, Some y))
                              additional_bindings;
                          ]
                      in
                      let new_bindings =
                        List.map
                          (fun (e, e_v) ->
                            Asrt.Pure (Eq (e, Val.to_expr (Option.get e_v))))
                          new_bindings
                      in
                      let a_new_bindings = Asrt.star new_bindings in
                      let subst_bindings = make_id_subst a_new_bindings in
                      let full_subst = make_id_subst a in
                      let _ = ESubst.merge_left full_subst subst_bindings in
                      let a_substed =
                        ESubst.substitute_asrt subst_bindings true a
                      in
                      let a_produce = Asrt.star [ a_new_bindings; a_substed ] in
                      match Unifier.produce new_state full_subst a_produce with
                      | Some new_astate ->
                          let new_state, new_preds, pred_defs = new_astate in
                          let new_state' =
                            State.add_spec_vars new_state (SS.of_list binders)
                          in
                          let _ =
                            State.simplify ~kill_new_lvars:false new_state'
                          in
                          Ok [ (new_state', new_preds, pred_defs) ]
                      | _               ->
                          let msg =
                            Fmt.str
                              "Assert failed with argument %a. unable to \
                               produce variable bindings."
                              Asrt.pp a
                          in
                          Error msg )
                | UPUSucc _ ->
                    raise
                      (Exceptions.Unsupported
                         "sep_assert: unification resulted in multiple returns")
                | UPUFail errs ->
                    let fail_pfs : Formula.t list =
                      List.map State.get_failing_constraint errs
                    in
                    let fail_pfs =
                      List.filter (fun (f : Formula.t) -> f <> True) fail_pfs
                    in

                    let failing_model = State.sat_check_f state fail_pfs in
                    let () =
                      Fmt.pr
                        "Assert failed with argument @[<h>%a@]. Unification \
                         failed.@\n\
                         @[<v 2>Errors:@\n\
                         %a.@]@\n\
                         @[<v 2>Failing Model:@\n\
                         %a@]@\n"
                        Asrt.pp a
                        Fmt.(list ~sep:(any "@\n") State.pp_err)
                        errs
                        Fmt.(option ~none:(any "CANNOT CREATE MODEL") ESubst.pp)
                        failing_model
                    in
                    raise (Internal_State_Error (errs, old_astate)) ) ) )
    | ApplyLem (lname, args, binders) -> (
        let lemma = UP.get_lemma prog lname in
        match lemma with
        | Error _  ->
            raise (Failure (Printf.sprintf "Lemma %s does not exist" lname))
        | Ok lemma -> (
            let v_args : vt list = List.map eval_expr args in
            (* Printf.printf "apply lemma. binders: %s. existentials: %s\n\n"
               (String.concat ", " binders) (String.concat ", " lemma.lemma.existentials); *)
            let existential_bindings =
              List.map2
                (fun x y -> (x, Option.get (Val.from_expr (Expr.LVar y))))
                lemma.lemma.lemma_existentials binders
            in
            let rets =
              run_spec_aux ~existential_bindings lname lemma.lemma.lemma_params
                lemma.up astate None v_args
            in
            match rets with
            | Ok rets    ->
                Ok
                  (List.map
                     (fun (astate, _) ->
                       let astate =
                         add_spec_vars astate (Var.Set.of_list binders)
                       in
                       let _ = simplify ~unification:true astate in
                       astate)
                     rets)
            | Error errs ->
                Error
                  (Format.asprintf "Cannot apply lemma %s in state\n%a" lname pp
                     astate) ) )
    | Invariant (a, binders) ->
        raise
          (Failure "Invariant must be treated by the unify_invariant function")

  let run_spec
      (spec : UP.spec)
      (astate : t)
      (x : string)
      (args : vt list)
      (subst : (string * (string * vt) list) option) : (t * Flag.t) list =
    let results =
      match subst with
      | None                ->
          run_spec_aux spec.spec.spec_name spec.spec.spec_params spec.up astate
            (Some x) args
      | Some (_, subst_lst) ->
          run_spec_aux ~existential_bindings:subst_lst spec.spec.spec_name
            spec.spec.spec_params spec.up astate (Some x) args
    in
    match results with
    | Ok results -> results
    | Error msg  ->
        L.normal (fun fmt ->
            fmt "WARNING: Unable to use specification of function %s"
              spec.spec.spec_name);
        []

  let unify (astate : t) (subst : st) (up : UP.t) : bool =
    let result =
      match Unifier.unify astate subst up with
      | Unifier.UPUSucc _ -> true
      | _                 -> false
    in
    L.verbose (fun fmt -> fmt "PSTATE.unify: Success: %b" result);
    result

  let unfolding_vals (astate : t) (fs : Formula.t list) : vt list =
    let state, _, _ = astate in
    State.unfolding_vals state fs

  let add_pred_defs (pred_defs : UP.preds_tbl_t) (astate : t) : t =
    let state, preds, _ = astate in
    (state, preds, pred_defs)

  let fresh_val (astate : t) : vt =
    let state, _, _ = astate in
    State.fresh_val state

  let fresh_loc ?(loc : vt option) (astate : t) : vt =
    let state, _, _ = astate in
    State.fresh_loc ?loc state

  let deabstract (astate : t) : state_t * bool =
    let state, preds, _ = astate in
    (state, Preds.is_empty preds)

  let clean_up (astate : t) : unit =
    let state, _, _ = astate in
    State.clean_up state

  let produce (astate : t) (subst : st) (a : Asrt.t) : t option =
    Unifier.produce astate subst a

  let unify_assertion (astate : t) (subst : st) (step : UP.step) : u_res =
    match Unifier.unify_assertion astate subst step with
    | UWTF          -> UWTF
    | USucc astate' -> USucc astate'
    | UFail errs    -> UFail errs

  let produce_posts (astate : t) (subst : st) (asrts : Asrt.t list) : t list =
    Unifier.produce_posts astate subst asrts

  let get_all_preds ?(keep : bool option) (sel : abs_t -> bool) (astate : t) :
      abs_t list =
    let _, preds, _ = astate in
    Preds.get_all ~maintain:(Option.value ~default:true keep) sel preds

  let set_pred (astate : t) (pred : abs_t) : unit =
    let _, preds, preds_table = astate in
    let pred_name, _ = pred in
    let pure = (Hashtbl.find preds_table pred_name).pred.pred_pure in
    Preds.extend ~pure preds pred

  let update_subst (astate : t) (subst : st) : unit =
    let state, _, _ = astate in
    State.update_subst state subst

  let ga_to_setter (a : string) : string = State.ga_to_setter a

  let ga_to_getter (a : string) : string = State.ga_to_getter a

  let ga_to_deleter (a : string) : string = State.ga_to_deleter a

  let execute_action (action : string) (astate : t) (args : vt list) :
      action_ret =
    let state, preds, pred_defs = astate in
    match State.execute_action action state args with
    | State.ASucc rets ->
        let rets' =
          List.map (fun (st, outs) -> ((st, preds, pred_defs), outs)) rets
        in
        ASucc rets'
    | State.AFail errs -> AFail errs

  let mem_constraints (astate : t) : Formula.t list =
    let state, _, _ = astate in
    State.mem_constraints state

  let merge_ins (action : string) (l_ins : vt list) (o_ins : vt list) : vt list
      =
    State.merge_ins action l_ins o_ins

  let split_ins (action : string) (ins : vt list) : vt list * vt list =
    State.split_ins action ins

  let is_overlapping_asrt (a : string) : bool = State.is_overlapping_asrt a

  let pp_err = State.pp_err

  let pp_fix = State.pp_fix

  let get_recovery_vals = State.get_recovery_vals

  let automatic_unfold (astate : t) (rvs : vt list) : (t list, string) result =
    let next_states, worked = Unifier.unfold_with_vals astate rvs in
    if not worked then Error "Automatic unfold failed"
    else Ok (List.map (fun (_, astate) -> astate) next_states)

  let get_failing_constraint = State.get_failing_constraint

  let can_fix = State.can_fix

  let get_fixes ?simple_fix:(sf = true) (state : t) (errs : err_t list) :
      fix_t list list =
    L.verbose (fun m -> m "AState: get_fixes");
    let st, preds, pht = state in
    State.get_fixes ~simple_fix:sf st errs

  let apply_fixes (state : t) (fixes : fix_t list) : t option * Asrt.t list =
    L.verbose (fun m -> m "AState: apply_fixes");
    let st, preds, pht = state in
    let ost, asrts = State.apply_fixes st fixes in
    match ost with
    | None    -> (None, [])
    | Some st ->
        let state = (st, preds, pht) in
        let ost =
          List.fold_left
            (fun os ga ->
              match os with
              | Some os ->
                  let subst = make_id_subst ga in
                  produce os subst ga
              | None    -> None)
            (Some state) asrts
        in
        (ost, [])

  let get_equal_values astate =
    let state, _, _ = astate in
    State.get_equal_values state
end
