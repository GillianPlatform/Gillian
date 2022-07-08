(**
    Interface for GIL Predicate States.
    They are considered to be mutable.
*)
module type S = sig
  include SState.S

  type state_t
  type preds_t
  type abs_t = string * vt list

  val initialise :
    state_t -> preds_t -> UP.preds_tbl_t option -> variants_t -> t

  val expose : t -> state_t * preds_t * UP.preds_tbl_t * variants_t

  (** Get preds of given symbolic state *)
  val get_preds : t -> preds_t

  (** Set preds of given symbolic state *)
  val set_preds : t -> preds_t -> t

  val set_variants : t -> variants_t -> t
  val unify : t -> st -> UP.t -> Unifier.unify_kind -> bool
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
    (State : SState.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (Preds : Preds.S with type vt = Val.t and type st = ESubst.t) :
  S
    with type vt = Val.t
     and type st = ESubst.t
     and type store_t = Store.t
     and type state_t = State.t
     and type preds_t = Preds.t
     and type heap_t = State.heap_t
     and type m_err_t = State.m_err_t = struct
  open Containers
  open Literal
  module L = Logging

  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]
  type t = State.t * Preds.t * UP.preds_tbl_t * variants_t
  type vt = Val.t [@@deriving yojson, show]
  type st = ESubst.t
  type store_t = Store.t
  type heap_t = State.heap_t
  type abs_t = Preds.abs_t
  type state_t = State.t
  type preds_t = Preds.t
  type err_t = State.err_t [@@deriving yojson]
  type fix_t = State.fix_t
  type m_err_t = State.m_err_t

  exception Internal_State_Error of err_t list * t
  exception Preprocessing_Error of UP.up_err_t list

  let () =
    Printexc.register_printer (function
      | Preprocessing_Error upu ->
          Some
            (Fmt.str "Preprocessing Error: %a"
               (Fmt.Dump.list UP.pp_up_err_t)
               upu)
      | _ -> None)

  module SUnifier = Unifier.Make (Val) (ESubst) (Store) (State) (Preds)

  type action_ret = ASucc of (t * vt list) list | AFail of err_t list
  type u_res = UWTF | USucc of t | UFail of err_t list

  let init ?(preds : UP.preds_tbl_t option) ?(variants : variants_t option) () =
    let empty_pred_defs : UP.preds_tbl_t = UP.init_pred_defs () in
    let empty_variants : variants_t = Hashtbl.create 1 in
    let new_pred_defs : UP.preds_tbl_t =
      Option.value ~default:empty_pred_defs preds
    in
    let new_variants : variants_t =
      Option.value ~default:empty_variants variants
    in
    (State.init (), Preds.init [], new_pred_defs, new_variants)

  let struct_init
      ?(preds : UP.preds_tbl_t option)
      ?(variants : variants_t option)
      (store : store_t)
      (pfs : PFS.t)
      (gamma : TypEnv.t)
      (svars : SS.t) : t =
    let empty_pred_defs : UP.preds_tbl_t = UP.init_pred_defs () in
    let empty_variants : variants_t = Hashtbl.create 1 in
    let new_pred_defs : UP.preds_tbl_t =
      Option.value ~default:empty_pred_defs preds
    in
    let new_variants : variants_t =
      Option.value ~default:empty_variants variants
    in
    let state = State.struct_init store pfs gamma svars in
    (state, Preds.init [], new_pred_defs, new_variants)

  let initialise
      (state : State.t)
      (preds : Preds.t)
      (pred_defs : UP.preds_tbl_t option)
      (variants : variants_t) : t =
    let pred_defs = Option.value ~default:(UP.init_pred_defs ()) pred_defs in
    (state, preds, pred_defs, variants)

  let expose state = state

  let simplify
      ?(save = false)
      ?(kill_new_lvars : bool option)
      ?(unification = false)
      (astate : t) : ESubst.t * t list =
    let state, preds, pred_defs, variants = astate in
    let subst, states =
      State.simplify ~save ?kill_new_lvars ~unification state
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

  let get_pred_defs (state : t) : UP.preds_tbl_t option =
    let _, _, pred_defs, _ = state in
    Some pred_defs

  let eval_expr (astate : t) (e : Expr.t) =
    let state, _, _, _ = astate in
    try State.eval_expr state e
    with State.Internal_State_Error (errs, _) ->
      raise (Internal_State_Error (errs, astate))

  let get_store (astate : t) : Store.t =
    let state, _, _, _ = astate in
    State.get_store state

  let set_store (astate : t) (store : Store.t) : t =
    let state, preds, pred_defs, variants = astate in
    let state' = State.set_store state store in
    (state', preds, pred_defs, variants)

  let get_preds (astate : t) : Preds.t =
    let _, preds, _, _ = astate in
    preds

  let set_preds (astate : t) (preds : Preds.t) : t =
    let state, _, pred_defs, variants = astate in
    (state, preds, pred_defs, variants)

  let set_variants (astate : t) (variants : variants_t) : t =
    let state, preds, pred_defs, _ = astate in
    (state, preds, pred_defs, variants)

  let to_loc (astate : t) (v : Val.t) : (t * Val.t) option =
    let state, preds, pred_defs, variants = astate in
    match State.to_loc state v with
    | Some (state', loc) -> Some ((state', preds, pred_defs, variants), loc)
    | None -> None

  let assume ?(unfold = false) (astate : t) (v : Val.t) : t list =
    let state, preds, pred_defs, variants = astate in
    List.concat
      (List.map
         (fun state ->
           let astate' = (state, preds, pred_defs, variants) in
           match (!Config.unfolding && unfold, Val.to_expr v) with
           | _, Lit (Bool true) -> [ astate' ]
           | false, _ -> [ astate' ]
           | true, _ ->
               let unfold_vals = Expr.base_elements (Val.to_expr v) in
               let unfold_vals = List.map Val.from_expr unfold_vals in
               let unfold_vals =
                 List.map Option.get
                   (List.filter (fun x -> x <> None) unfold_vals)
               in
               let next_states, worked =
                 SUnifier.unfold_with_vals astate' unfold_vals
               in
               if not worked then [ astate' ]
               else
                 List.concat_map
                   (fun (_, astate) ->
                     let _, astates = simplify ~kill_new_lvars:false astate in
                     astates)
                   next_states)
         (State.assume ~unfold state v))

  let assume_a
      ?(unification = false)
      ?(production = false)
      ?(time = "")
      (astate : t)
      (fs : Formula.t list) : t option =
    let state, preds, pred_defs, variants = astate in
    match State.assume_a ~unification ~production ~time state fs with
    | Some state -> Some (state, preds, pred_defs, variants)
    | None -> None

  let assume_t (astate : t) (v : Val.t) (t : Type.t) : t option =
    let state, preds, pred_defs, variants = astate in
    match State.assume_t state v t with
    | Some state -> Some (state, preds, pred_defs, variants)
    | None -> None

  let sat_check (astate : t) (v : Val.t) : bool =
    let state, _, _, _ = astate in
    State.sat_check state v

  let sat_check_f (astate : t) (fs : Formula.t list) : ESubst.t option =
    let state, _, _, _ = astate in
    State.sat_check_f state fs

  let assert_a (astate : t) (fs : Formula.t list) : bool =
    let state, _, _, _ = astate in
    State.assert_a state fs

  let equals (astate : t) (v1 : Val.t) (v2 : Val.t) : bool =
    let state, _, _, _ = astate in
    State.equals state v1 v2

  let get_type (astate : t) (v : Val.t) : Type.t option =
    let state, _, _, _ = astate in
    State.get_type state v

  let copy (astate : t) : t =
    let state, preds, pred_defs, variants = astate in
    (State.copy state, Preds.copy preds, pred_defs, Hashtbl.copy variants)

  let simplify_val (astate : t) (v : Val.t) : Val.t =
    let state, _, _, _ = astate in
    State.simplify_val state v

  let pp_variants : (string * Expr.t option) Fmt.t =
    Fmt.pair ~sep:Fmt.comma Fmt.string (Fmt.option Expr.pp)

  let pp fmt (astate : t) : unit =
    let state, preds, _, variants = astate in
    Fmt.pf fmt "%a@\n@[<v 2>PREDICATES:@\n%a@]@\n@[<v 2>VARIANTS:@\n%a@]@\n"
      State.pp state Preds.pp preds
      (Fmt.hashtbl ~sep:Fmt.semi pp_variants)
      variants

  let pp_by_need pvars lvars locs fmt astate : unit =
    let state, preds, _, _ = astate in
    (* TODO: Pred printing by need *)
    Fmt.pf fmt "%a@\n@[<v 2>PREDICATES:@\n%a@]@\n"
      (State.pp_by_need pvars lvars locs)
      state Preds.pp preds

  let add_spec_vars (astate : t) (vs : Var.Set.t) : t =
    let state, preds, pred_defs, variants = astate in
    let state' = State.add_spec_vars state vs in
    (state', preds, pred_defs, variants)

  let get_spec_vars (astate : t) : Var.Set.t =
    let state, _, _, _ = astate in
    State.get_spec_vars state

  let get_lvars (astate : t) : Var.Set.t =
    let state, preds, _, _ = astate in
    SS.union (State.get_lvars state) (Preds.get_lvars preds)

  let get_lvars_for_exact (astate : t) : Var.Set.t =
    let state, preds, _, _ = astate in
    SS.union (State.get_lvars_for_exact state) (Preds.get_lvars preds)

  let to_assertions ?(to_keep : SS.t option) (astate : t) : Asrt.t list =
    let state, preds, _, _ = astate in
    let s_asrts = State.to_assertions ?to_keep state in
    let p_asrts = Preds.to_assertions preds in
    List.sort Asrt.compare (p_asrts @ s_asrts)

  let substitution_in_place ?(subst_all = false) (subst : st) (astate : t) :
      t list =
    let state, preds, pred_defs, variants = astate in
    Preds.substitution_in_place subst preds;
    let subst_variants = Hashtbl.create 1 in
    let () =
      Hashtbl.iter
        (fun func variant ->
          Hashtbl.add subst_variants func
            (Option.map (ESubst.subst_in_expr subst ~partial:true) variant))
        variants
    in
    List.map
      (fun st -> (st, Preds.copy preds, pred_defs, Hashtbl.copy subst_variants))
      (State.substitution_in_place ~subst_all subst state)

  let update_store (state : t) (x : string option) (v : Val.t) : t =
    match x with
    | None -> state
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
      (args : vt list) : ((t * Flag.t) list, SUnifier.err_t list) result =
    L.verbose (fun m ->
        m "INSIDE RUN spec of %s with the following UP:@\n%a@\n" name UP.pp up);

    let old_store = get_store astate in
    let new_store =
      try Store.init (List.combine params args)
      with Invalid_argument _ ->
        let message =
          Fmt.str
            "Running spec of %s which takes %i parameters with the following \
             %i arguments : %a"
            name (List.length params) (List.length args) (Fmt.Dump.list Val.pp)
            args
        in
        raise (Invalid_argument message)
    in
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

    match SUnifier.unify astate' subst up FunctionCall with
    | UPUSucc rets ->
        let acceptable =
          match !Config.Verification.exact with
          | false -> true
          | true ->
              let check_variant (state : t) (subst : st) =
                let bstate, _, _, variants = state in
                let variant = Hashtbl.find_opt variants name in
                match variant with
                | None -> true
                | Some None ->
                    (* FIXME: This should be an error *)
                    failwith
                      ("Error: " ^ name
                     ^ ": Recursive call in exact verification without a \
                        variant")
                | Some (Some variant) -> (
                    let new_variant = ESubst.subst_in_expr_opt subst variant in
                    let () =
                      L.verbose (fun fmt ->
                          fmt "New variant: %a" (Fmt.Dump.option Expr.pp)
                            new_variant)
                    in
                    match new_variant with
                    | None ->
                        failwith
                          ("ERROR: " ^ name
                         ^ ": recursive call variant substitution failure")
                    | Some new_variant -> (
                        let variant_check =
                          Expr.UnOp
                            (UnOp.UNot, BinOp (new_variant, ILessThan, variant))
                        in
                        let variant_check = Val.from_expr variant_check in
                        match variant_check with
                        | None ->
                            failwith
                              ("ERROR: " ^ name
                             ^ ": variant could not be converted to a value")
                        | Some variant_check ->
                            not (State.sat_check bstate variant_check)))
              in
              List.for_all
                (fun (state, subst, _) -> check_variant state subst)
                rets
        in
        if acceptable then
          Ok
            ((* Successful Unification *)
             let ( let++ ) x f = List.concat_map f x in
             let++ frame_state, subst, posts = rets in
             let fl, posts =
               match posts with
               | Some (fl, posts) -> (fl, posts)
               | _ ->
                   let msg =
                     Printf.sprintf
                       "SYNTAX ERROR: Spec of %s does not have a postcondition"
                       name
                   in
                   L.normal (fun m -> m "%s" msg);
                   raise (Failure msg)
             in
             (* OK FOR DELAY ENTAILMENT *)
             let++ final_state =
               SUnifier.produce_posts frame_state subst posts
             in
             let final_store = get_store final_state in
             let v_ret = Store.get final_store Names.return_variable in
             let final_state = set_store final_state (Store.copy old_store) in
             let v_ret =
               Option.value
                 ~default:(Option.get (Val.from_expr (Lit Undefined)))
                 v_ret
             in
             let final_state = update_store final_state x v_ret in
             let _, final_states = simplify ~unification:true final_state in
             List.map
               (fun final_state ->
                 ( snd (Option.get (SUnifier.unfold_concrete_preds final_state)),
                   fl ))
               final_states)
        else
          (* FIXME: Cannot pass an appropriate error for the moment *)
          let err = [] in
          Error err
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

  let clear_resource (astate : t) =
    let state, preds, pred_defs, variants = astate in
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
    (state, preds, pred_defs, variants)

  let unify_invariant
      (prog : UP.prog)
      (revisited : bool)
      (astate : t)
      (a : Asrt.t)
      (binders : string list) : (t * t) list =
    let state, _, _, _ = astate in
    let store = State.get_store state in
    let pvars_store = Store.domain store in
    let pvars_a = Asrt.pvars a in
    let pvars_diff = SS.diff pvars_a pvars_store in
    L.verbose (fun m -> m "%s" (String.concat ", " (SS.elements pvars_diff)));
    match pvars_diff = SS.empty with
    | false ->
        let pvars_errs : err_t list =
          List.map (fun pvar : err_t -> EVar pvar) (SS.elements pvars_diff)
        in
        raise (Internal_State_Error (pvars_errs, astate))
    | true -> (
        let lvar_binders, pvar_binders =
          List.partition Names.is_lvar_name binders
        in
        let known_pvars = List.map Expr.from_var_name (SS.elements pvars_a) in
        let state_lvars = State.get_lvars state in
        let known_lvars =
          SS.elements
            (SS.diff
               (SS.inter state_lvars (Asrt.lvars a))
               (SS.of_list lvar_binders))
        in
        let known_lvars = List.map (fun x -> Expr.LVar x) known_lvars in
        let asrt_alocs =
          List.map (fun x -> Expr.ALoc x) (SS.elements (Asrt.alocs a))
        in
        let known_unifiables =
          Expr.Set.of_list (known_pvars @ known_lvars @ asrt_alocs)
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
          (* FIXME: UNDERSTAND IF THE OX SHOULD BE [] *)
          UP.init known_unifiables Expr.Set.empty pred_ins
            [ (a, (None, None, [])) ]
        in
        (* This will not do anything in the original pass,
           but will do precisely what is needed in the re-establishment *)
        let vars_to_forget = SS.inter state_lvars (SS.of_list lvar_binders) in
        if vars_to_forget <> SS.empty then (
          let oblivion_subst = fresh_subst vars_to_forget in
          L.verbose (fun m ->
              m "Forget @[%a@] with subst: %a"
                Fmt.(iter ~sep:comma SS.iter string)
                vars_to_forget ESubst.pp oblivion_subst);

          (* TODO: THIS SUBST IN PLACE MUST NOT BRANCH *)
          let subst_in_place =
            substitution_in_place ~subst_all:true oblivion_subst astate
          in
          assert (List.length subst_in_place = 1);
          let astate = List.hd subst_in_place in

          L.verbose (fun m ->
              m "State after substitution:@\n@[%a@]\n" pp astate))
        else ();
        match up with
        | Error asrts -> raise (Preprocessing_Error [ UPAssert (a, asrts) ])
        | Ok up -> (
            let bindings =
              List.map
                (fun (e : Expr.t) ->
                  let binding =
                    match e with
                    | PVar x -> Store.get (State.get_store state) x
                    | LVar _ | ALoc _ -> Val.from_expr e
                    | _ ->
                        raise
                          (Failure
                             "Impossible: unifiable not a pvar or an lvar or \
                              an aloc")
                  in
                  (e, Option.get binding))
                (Expr.Set.elements known_unifiables)
            in
            let old_astate = copy astate in
            let subst = ESubst.init bindings in
            let u_ret = SUnifier.unify astate subst up Invariant in
            match u_ret with
            | UPUSucc states ->
                List.concat_map
                  (fun (new_state, subst', _) ->
                    (* Successful Unification *)
                    (* TODO: Should the frame state have the subst produced? *)
                    let frame_state = copy new_state in
                    let frame_state = set_store frame_state (Store.init []) in

                    let lbinders =
                      List.map (fun x -> Expr.LVar x) lvar_binders
                    in
                    let new_bindings =
                      List.map (fun e -> (e, ESubst.get subst' e)) lbinders
                    in
                    let success =
                      List.for_all (fun (_, x_v) -> x_v <> None) new_bindings
                    in
                    if not success then
                      raise
                        (Failure "UNIFY INVARIANT FAILURE: binders not captured")
                    else
                      let new_bindings =
                        List.map
                          (fun (x, x_v) -> (x, Option.get x_v))
                          new_bindings
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
                      let known_pvars =
                        SS.elements (SS.diff pvars_a (SS.of_list pvar_binders))
                      in
                      let bindings =
                        match revisited with
                        | false -> bindings
                        | true -> new_bindings @ bindings
                      in
                      let bindings =
                        List.filter
                          (fun (x, _) ->
                            match x with
                            | Expr.PVar x when List.mem x pvar_binders -> false
                            | UnOp (LstLen, _) -> false
                            | _ -> true)
                          bindings
                      in
                      let bindings =
                        List.map
                          (fun (e, e_v) -> Asrt.Pure (Eq (e, Val.to_expr e_v)))
                          bindings
                      in
                      let a_bindings = Asrt.star bindings in
                      let subst_bindings = make_id_subst a_bindings in
                      let pvar_subst_list_known =
                        List.map
                          (fun x ->
                            ( Expr.PVar x,
                              Option.get (Store.get (State.get_store state) x)
                            ))
                          known_pvars
                      in
                      let pvar_subst_list_bound =
                        List.map
                          (fun x -> (Expr.PVar x, Expr.LVar (LVar.alloc ())))
                          pvar_binders
                      in
                      let full_subst = make_id_subst a in
                      let pvar_subst_list_bound =
                        List.map
                          (fun (x, y) -> (x, Option.get (Val.from_expr y)))
                          pvar_subst_list_bound
                      in
                      let pvar_subst_list =
                        pvar_subst_list_known @ pvar_subst_list_bound
                      in
                      let pvar_subst = ESubst.init pvar_subst_list in
                      let _ = ESubst.merge_left full_subst subst_bindings in
                      let _ = ESubst.merge_left full_subst pvar_subst in
                      L.verbose (fun fmt -> fmt "Invariant v1: %a" Asrt.pp a);
                      let a_substed =
                        Reduction.reduce_assertion
                          (ESubst.substitute_asrt subst_bindings ~partial:true a)
                      in
                      L.verbose (fun fmt ->
                          fmt "Invariant v2: %a" Asrt.pp a_substed);
                      let a_produce =
                        Reduction.reduce_assertion
                          (Asrt.star [ a_bindings; a_substed ])
                      in
                      L.verbose (fun fmt ->
                          fmt "Invariant v3: %a" Asrt.pp a_produce);
                      (* Create empty state *)
                      let invariant_state : t = clear_resource new_state in
                      let () =
                        List.iter
                          (fun (x, v) ->
                            let x =
                              match x with
                              | Expr.PVar x -> x
                              | _ -> failwith "Impossible"
                            in
                            Store.put store x v)
                          pvar_subst_list
                      in
                      let invariant_state = set_store invariant_state store in
                      match
                        SUnifier.produce invariant_state full_subst a_produce
                      with
                      (* FIXME: Should allow several new states ? *)
                      | Ok ([ _new_astate ] as new_astates) ->
                          List.concat_map
                            (fun (new_state, new_preds, pred_defs, variants) ->
                              let new_state' =
                                State.add_spec_vars new_state
                                  (SS.of_list lvar_binders)
                              in
                              let invariant_state =
                                (new_state', new_preds, pred_defs, variants)
                              in
                              let _, invariant_states =
                                simplify ~kill_new_lvars:true invariant_state
                              in
                              List.map
                                (fun invariant_state ->
                                  (copy frame_state, invariant_state))
                                invariant_states)
                            new_astates
                      | Ok _ ->
                          let msg =
                            Fmt.str
                              "UNIFY INVARIANT FAILURE: several states \
                               produced. Supposedly the code is in place to \
                               enable the multi_state behaviour, but it hasn't \
                               been tested yet. Just remove that case and make \
                               the above case general"
                          in
                          L.fail msg
                      | Error e ->
                          let msg =
                            Fmt.str
                              "UNIFY INVARIANT FAILURE: %a\n\
                               unable to produce variable bindings: %s." Asrt.pp
                              a e
                          in
                          L.fail msg)
                  states
            | UPUFail errs ->
                let fail_pfs : Formula.t list =
                  List.map State.get_failing_constraint errs
                in
                let fail_pfs =
                  List.filter (fun (f : Formula.t) -> f <> True) fail_pfs
                in

                let failing_model = State.sat_check_f state fail_pfs in
                let () =
                  L.print_to_all
                    (Format.asprintf
                       "UNIFY INVARIANT FAILURE: with argument @[<h>%a@]. \
                        Unification failed.@\n\
                        @[<v 2>Errors:@\n\
                        %a.@]@\n\
                        @[<v 2>Failing Model:@\n\
                        %a@]@\n"
                       Asrt.pp a
                       Fmt.(list ~sep:(any "@\n") State.pp_err)
                       errs
                       Fmt.(option ~none:(any "CANNOT CREATE MODEL") ESubst.pp)
                       failing_model)
                in
                raise (Internal_State_Error (errs, old_astate))))

  let frame_on (astate : t) (iframes : (string * t) list) (ids : string list) :
      t list =
    let rec fl2 f acc la lb =
      match (la, lb) with
      | [], _ | _, [] -> acc
      | a :: r, b :: q -> fl2 f (f acc a b) r q
    in
    fl2
      (fun astates (id, frame) id' ->
        let ( let++ ) x f = List.concat_map f x in
        if id <> id' then L.fail "Framing: Malformed loop identifiers."
        else
          let++ astate = astates in
          let++ astate =
            let frame_asrt = Asrt.star (to_assertions frame) in
            let full_subst = make_id_subst frame_asrt in
            match SUnifier.produce astate full_subst frame_asrt with
            | Error msg ->
                L.fail
                  (Format.asprintf
                     "Unable to produce frame for loop %s, because of :\n%s" id
                     msg)
            | Ok l -> l
          in
          (* TODO: FIX THIS FOR MULTI-STATES *)
          let _, states = simplify ~kill_new_lvars:true astate in
          states)
      [ astate ] iframes ids

  (**
    Evaluation of logic commands

    @param prog GIL program
    @param lcmd Logic command to be evaluated
    @param state Current state
    @param preds Current predicate set
    @return List of states/predicate sets resulting from the evaluation
  *)
  let evaluate_slcmd (prog : UP.prog) (lcmd : SLCmd.t) (astate : t) :
      (t list, string) result =
    let state, _, _, _ = astate in

    let eval_expr e =
      try State.eval_expr state e
      with State.Internal_State_Error (errs, _) ->
        raise (Internal_State_Error (errs, astate))
    in

    let result =
      match lcmd with
      | SymbExec -> failwith "Impossible: Untreated SymbExec"
      | Fold (pname, les, folding_info) -> (
          let () = L.verbose (fun fmt -> fmt "Folding predicate: %s\n" pname) in
          let pred = UP.get_pred_def prog.preds pname in
          match pred.pred.pred_abstract with
          | true ->
              raise
                (Failure
                   (Format.asprintf "Impossible: Fold of abstract predicate %s"
                      pname))
          | false -> (
              let vs = List.map eval_expr les in
              let params = List.map (fun (x, _) -> x) pred.pred.pred_params in
              let i_bindings =
                Option.fold
                  ~some:(fun (_, bindings) ->
                    List.map (fun (x, e) -> (Expr.LVar x, eval_expr e)) bindings)
                  ~none:[] folding_info
              in
              let param_bindings =
                if List.length params = List.length vs then
                  List.combine params vs
                else List.combine (Pred.in_params pred.pred) vs
              in
              let param_bindings =
                List.map (fun (x, v) -> (Expr.PVar x, v)) param_bindings
              in
              let subst = ESubst.init (i_bindings @ param_bindings) in
              match SUnifier.unify astate subst pred.up LogicCommand with
              | UPUSucc [ (astate', subst', _) ] ->
                  let _, preds', _, _ = astate' in
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
                            | None ->
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
                  raise (Failure msg)))
      | Unfold (pname, les, unfold_info, b) -> (
          let pred = UP.get_pred_def prog.preds pname in
          match pred.pred.pred_abstract with
          | true ->
              raise
                (Failure
                   (Format.asprintf
                      "Impossible: Unfold of abstract predicate %s" pname))
          | false -> (
              let vs = List.map eval_expr les in
              let vs_ins = Pred.in_args pred.pred vs in
              let vs = List.map (fun x -> Some x) vs in
              (* FIXME: make sure correct number of params *)
              match SUnifier.get_pred astate pname vs None with
              | GPSucc [ (_, vs') ] ->
                  L.verbose (fun m ->
                      m "@[<h>Returned values: %a@]"
                        Fmt.(list ~sep:comma Val.pp)
                        vs');
                  let vs = Pred.combine_ins_outs pred.pred vs_ins vs' in
                  L.verbose (fun m ->
                      m
                        "@[<h>LCMD Unfold about to happen with rec %b info: \
                         %a@]"
                        b SLCmd.pp_unfold_info unfold_info);
                  if b then Ok (SUnifier.rec_unfold astate pname vs)
                  else (
                    L.verbose (fun m ->
                        m "@[<h>Values: %a@]" Fmt.(list ~sep:comma Val.pp) vs);
                    Ok
                      (List.concat_map
                         (fun (_, state) ->
                           let _, states =
                             simplify ~kill_new_lvars:true ~unification:true
                               state
                           in
                           states)
                         (SUnifier.unfold astate pname vs unfold_info)))
              | _ -> Fmt.failwith "IMPOSSIBLE UNFOLD: %a" SLCmd.pp lcmd))
      | GUnfold pname ->
          let astates = SUnifier.unfold_all astate pname in
          let astates =
            List.concat_map
              (fun astate -> snd (simplify ~kill_new_lvars:true astate))
              astates
          in
          Ok astates
      | SepAssert (a, binders) -> (
          if not (List.for_all Names.is_lvar_name binders) then
            failwith "Binding of pure variables in *-assert.";
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
                  (fun pvar : err_t -> EVar pvar)
                  (SS.elements pvars_diff)
              in
              raise (Internal_State_Error (pvars_errs, astate))
          | true -> (
              let store_subst = Store.to_ssubst store in
              let a =
                SVal.SESubst.substitute_asrt store_subst ~partial:true a
              in
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
                  [ (a, (None, None, [])) ]
              in
              let vars_to_forget = SS.inter state_lvars (SS.of_list binders) in
              if vars_to_forget <> SS.empty then (
                let oblivion_subst = fresh_subst vars_to_forget in
                L.verbose (fun m ->
                    m "Forget @[%a@] with subst: %a"
                      Fmt.(iter ~sep:comma SS.iter string)
                      vars_to_forget ESubst.pp oblivion_subst);

                (* TODO: THIS SUBST IN PLACE MUST NOT BRANCH *)
                let subst_in_place =
                  substitution_in_place oblivion_subst astate
                in
                assert (List.length subst_in_place = 1);
                let astate = List.hd subst_in_place in

                L.verbose (fun m ->
                    m "State after substitution:@\n@[%a@]\n" pp astate))
              else ();
              match up with
              | Error asrts ->
                  raise (Preprocessing_Error [ UPAssert (a, asrts) ])
              | Ok up -> (
                  let bindings =
                    List.map
                      (fun (e : Expr.t) ->
                        let id =
                          match e with
                          | LVar _ | ALoc _ -> e
                          | _ ->
                              raise
                                (Failure
                                   "Impossible: unifiable not an lvar or an \
                                    aloc")
                        in
                        (id, Option.get (Val.from_expr e)))
                      (Expr.Set.elements known_unifiables)
                  in
                  let old_astate = copy astate in
                  let subst = ESubst.init bindings in
                  let u_ret = SUnifier.unify astate subst up LogicCommand in
                  match u_ret with
                  | UPUSucc [ (new_state, subst', _) ] -> (
                      (* Successful Unification *)
                      let lbinders = List.map (fun x -> Expr.LVar x) binders in
                      let new_bindings =
                        List.map (fun e -> (e, ESubst.get subst' e)) lbinders
                      in
                      let success =
                        List.for_all (fun (_, x_v) -> x_v <> None) new_bindings
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
                          ESubst.substitute_asrt subst_bindings ~partial:true a
                        in
                        let a_produce =
                          Asrt.star [ a_new_bindings; a_substed ]
                        in
                        match
                          SUnifier.produce new_state full_subst a_produce
                        with
                        | Ok new_astates ->
                            List.concat_map
                              (fun new_astate ->
                                let new_state, new_preds, pred_defs, variants =
                                  new_astate
                                in
                                let new_state' =
                                  State.add_spec_vars new_state
                                    (SS.of_list binders)
                                in
                                let subst, new_states =
                                  State.simplify ~kill_new_lvars:true new_state'
                                in
                                let () =
                                  Preds.substitution_in_place subst new_preds
                                in
                                List.map
                                  (fun new_state ->
                                    ( new_state,
                                      Preds.copy new_preds,
                                      pred_defs,
                                      variants ))
                                  new_states)
                              new_astates
                            |> Result.ok
                        | _ ->
                            let msg =
                              Fmt.str
                                "Assert failed with argument %a. unable to \
                                 produce variable bindings."
                                Asrt.pp a
                            in
                            Error msg)
                  | UPUSucc _ ->
                      raise
                        (Exceptions.Unsupported
                           "sep_assert: unification resulted in multiple \
                            returns")
                  | UPUFail errs ->
                      let fail_pfs : Formula.t list =
                        List.map State.get_failing_constraint errs
                      in
                      let fail_pfs =
                        List.filter (fun (f : Formula.t) -> f <> True) fail_pfs
                      in

                      let failing_model = State.sat_check_f state fail_pfs in
                      let msg =
                        Fmt.str
                          "Assert failed with argument @[<h>%a@]. Unification \
                           failed.@\n\
                           @[<v 2>Errors:@\n\
                           %a.@]@\n\
                           @[<v 2>Failing Model:@\n\
                           %a@]@\n"
                          Asrt.pp a
                          Fmt.(list ~sep:(any "@\n") State.pp_err)
                          errs
                          Fmt.(
                            option ~none:(any "CANNOT CREATE MODEL") ESubst.pp)
                          failing_model
                      in
                      print_string msg;
                      L.normal (fun fmt -> fmt "%s" msg);
                      raise (Internal_State_Error (errs, old_astate)))))
      | ApplyLem (lname, args, binders) -> (
          if not (List.for_all Names.is_lvar_name binders) then
            failwith "Binding of pure variables in lemma application.";
          let lemma = UP.get_lemma prog lname in
          match lemma with
          | Error _ ->
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
                run_spec_aux ~existential_bindings lname
                  lemma.lemma.lemma_params lemma.up astate None v_args
              in
              match rets with
              | Ok rets ->
                  Ok
                    (List.concat_map
                       (fun (astate, _) ->
                         let astate =
                           add_spec_vars astate (Var.Set.of_list binders)
                         in
                         let _, astates = simplify ~unification:true astate in
                         astates)
                       rets)
              | Error _ ->
                  Error
                    (Format.asprintf "Cannot apply lemma %s in state\n%a" lname
                       pp astate)))
      | Invariant _ ->
          raise
            (Failure "Invariant must be treated by the unify_invariant function")
    in
    match result with
    | Ok results ->
        Ok (List.concat_map (fun result -> snd (simplify result)) results)
    | _ -> result

  let run_spec
      (spec : UP.spec)
      (astate : t)
      (x : string)
      (args : vt list)
      (subst : (string * (string * vt) list) option) : (t * Flag.t) list =
    let results =
      match subst with
      | None ->
          run_spec_aux spec.spec.spec_name spec.spec.spec_params spec.up astate
            (Some x) args
      | Some (_, subst_lst) ->
          run_spec_aux ~existential_bindings:subst_lst spec.spec.spec_name
            spec.spec.spec_params spec.up astate (Some x) args
    in
    match results with
    | Ok results -> results
    | Error _ ->
        L.normal (fun fmt ->
            fmt "WARNING: Unable to use specification of function %s"
              spec.spec.spec_name);
        []

  let unify
      (astate : t)
      (subst : st)
      (up : UP.t)
      (unify_type : Unifier.unify_kind) : bool =
    let result =
      let is_post = unify_type = Unifier.Postcondition in
      match SUnifier.unify ~is_post astate subst up unify_type with
      | SUnifier.UPUSucc _ -> true
      | _ -> false
    in
    L.verbose (fun fmt -> fmt "PSTATE.unify: Success: %b" result);
    result

  let unfolding_vals (astate : t) (fs : Formula.t list) : vt list =
    let state, _, _, _ = astate in
    State.unfolding_vals state fs

  let add_pred_defs (pred_defs : UP.preds_tbl_t) (astate : t) : t =
    let state, preds, _, variants = astate in
    (state, preds, pred_defs, variants)

  let fresh_val (astate : t) : vt =
    let state, _, _, _ = astate in
    State.fresh_val state

  let fresh_loc ?(loc : vt option) (astate : t) : vt =
    let state, _, _, _ = astate in
    State.fresh_loc ?loc state

  let deabstract (astate : t) : state_t * bool =
    let state, preds, _, _ = astate in
    (state, Preds.is_empty preds)

  let clean_up ?keep:_ (astate : t) : unit =
    let state, _, _, _ = astate in
    State.clean_up state

  let produce (astate : t) (subst : st) (a : Asrt.t) : (t list, string) result =
    SUnifier.produce astate subst a

  let unify_assertion (astate : t) (subst : st) (step : UP.step) : u_res =
    match SUnifier.unify_assertion astate subst None step with
    | UWTF -> UWTF
    | USucc astate' -> USucc astate'
    | UFail errs -> UFail errs

  let produce_posts (astate : t) (subst : st) (asrts : Asrt.t list) : t list =
    SUnifier.produce_posts astate subst asrts

  let get_all_preds ?(keep : bool option) (sel : abs_t -> bool) (astate : t) :
      abs_t list =
    let _, preds, _, _ = astate in
    Preds.get_all ~maintain:(Option.value ~default:true keep) sel preds

  let set_pred (astate : t) (pred : abs_t) : unit =
    let _, preds, preds_table, _ = astate in
    let pred_name, _ = pred in
    let pure = (Hashtbl.find preds_table pred_name).pred.pred_pure in
    Preds.extend ~pure preds pred

  let update_subst (astate : t) (subst : st) : unit =
    let state, _, _, _ = astate in
    State.update_subst state subst

  let ga_to_setter (a : string) : string = State.ga_to_setter a
  let ga_to_getter (a : string) : string = State.ga_to_getter a
  let ga_to_deleter (a : string) : string = State.ga_to_deleter a

  let execute_action
      ?(unification = false)
      (action : string)
      (astate : t)
      (args : vt list) : action_ret =
    let state, preds, pred_defs, variants = astate in
    match State.execute_action ~unification action state args with
    | State.ASucc rets ->
        let rets' =
          List.map
            (fun (st, outs) ->
              ((st, Preds.copy preds, pred_defs, variants), outs))
            rets
        in
        ASucc rets'
    | State.AFail errs -> AFail errs

  let mem_constraints (astate : t) : Formula.t list =
    let state, _, _, _ = astate in
    State.mem_constraints state

  let is_overlapping_asrt (a : string) : bool = State.is_overlapping_asrt a
  let pp_err = State.pp_err
  let pp_fix = State.pp_fix

  let get_recovery_vals astate vs =
    let state, _, _, _ = astate in
    State.get_recovery_vals state vs

  let automatic_unfold (astate : t) (rvs : vt list) : (t list, string) result =
    let next_states, worked = SUnifier.unfold_with_vals astate rvs in
    if not worked then Error "Automatic unfold failed"
    else Ok (List.map (fun (_, astate) -> astate) next_states)

  let get_failing_constraint = State.get_failing_constraint
  let can_fix = State.can_fix

  let get_fixes ?simple_fix:(sf = true) (state : t) (errs : err_t list) :
      fix_t list list =
    L.verbose (fun m -> m "AState: get_fixes");
    let st, _, _, _ = state in
    State.get_fixes ~simple_fix:sf st errs

  let apply_fixes (state : t) (fixes : fix_t list) : t option * Asrt.t list =
    L.verbose (fun m -> m "AState: apply_fixes");
    let st, preds, pht, variants = state in
    let ost, asrts = State.apply_fixes st fixes in
    match ost with
    | None -> (None, [])
    | Some st ->
        let state = (st, preds, pht, variants) in
        let ost =
          List.fold_left
            (fun os ga ->
              match os with
              | Some os -> (
                  let subst = make_id_subst ga in
                  match produce os subst ga with
                  | Ok [ x ] -> Some x
                  | Ok _ -> failwith "multiple productions in bi-abduction"
                  | Error _ -> None)
              | None -> None)
            (Some state) asrts
        in
        (ost, [])

  let get_equal_values astate =
    let state, _, _, _ = astate in
    State.get_equal_values state

  let get_heap pstate =
    let state, _, _, _ = pstate in
    State.get_heap state

  let get_typ_env pstate =
    let state, _, _, _ = pstate in
    State.get_typ_env state

  let get_pfs pstate =
    let state, _, _, _ = pstate in
    State.get_pfs state

  let hides ~is_post:_ ~used_unifiables:_ ~exprs_to_hide:_ _ =
    failwith "Check for hidden variables only available from symbolic states."

  let of_yojson (yojson : Yojson.Safe.t) : (t, string) result =
    (* TODO: Deserialize other components of pstate *)
    match yojson with
    | `Assoc
        [
          ("state", state_yojson);
          ("preds", preds_yojson);
          ("variants", variants_yojson);
        ] ->
        Result.bind (State.of_yojson state_yojson) (fun state ->
            Result.bind (Preds.of_yojson preds_yojson) (fun (preds : Preds.t) ->
                variants_t_of_yojson variants_yojson
                |> Result.map (fun variants ->
                       (state, preds, UP.init_pred_defs (), variants))))
    | _ -> Error "Cannot parse yojson into PState"

  let to_yojson pstate =
    (* TODO: Serialize other components of pstate *)
    let state, preds, _, variants = pstate in
    `Assoc
      [
        ("state", State.to_yojson state);
        ("preds", Preds.to_yojson preds);
        ("variants", variants_t_to_yojson variants);
      ]
end
