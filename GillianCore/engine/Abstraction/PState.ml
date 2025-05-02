(**
    Interface for GIL Predicate States.
    They are considered to be mutable.
*)
module type S = sig
  include SState.S

  type state_t
  type abs_t = string * vt list

  val make_p :
    preds:MP.preds_tbl_t ->
    init_data:init_data ->
    store:store_t ->
    pfs:PFS.t ->
    gamma:Type_env.t ->
    spec_vars:SS.t ->
    unit ->
    t

  val init_with_pred_table : MP.preds_tbl_t -> init_data -> t

  (** Get preds of given symbolic state *)
  val get_preds : t -> Preds.t

  (** Set preds of given symbolic state *)
  val set_preds : t -> Preds.t -> t

  (** Set preds of given symbolic state *)
  val set_wands : t -> Wands.t -> t

  val set_variants : t -> variants_t -> t
  val matches : t -> st -> MP.t -> Matcher.match_kind -> bool
  val add_pred_defs : MP.preds_tbl_t -> t -> t
  val get_all_preds : ?keep:bool -> (abs_t -> bool) -> t -> abs_t list
  val set_pred : t -> abs_t -> unit
  val try_recovering : t -> vt Recovery_tactic.t -> (t list, string) result
end

module Make (State : SState.S) :
  S
    with type state_t = State.t
     and type heap_t = State.heap_t
     and type m_err_t = State.m_err_t
     and type init_data = State.init_data = struct
  open Containers
  open Literal
  module L = Logging
  module SMatcher = Matcher.Make (State)

  type init_data = State.init_data
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]

  type t = SMatcher.t = {
    state : State.t;
    preds : Preds.t;
    wands : Wands.t;
    pred_defs : MP.preds_tbl_t;
    variants : variants_t;
  }

  type vt = Expr.t [@@deriving yojson, show]
  type st = SVal.SESubst.t
  type store_t = SStore.t
  type heap_t = State.heap_t
  type abs_t = Preds.abs_t
  type state_t = State.t
  type err_t = State.err_t [@@deriving yojson, show]
  type m_err_t = State.m_err_t [@@deriving yojson]

  exception Internal_State_Error of err_t list * t
  exception Preprocessing_Error of MP.err list

  let () =
    Printexc.register_printer (function
      | Preprocessing_Error mp_errs ->
          Some
            (Fmt.str "Preprocessing Error: %a" (Fmt.Dump.list MP.pp_err) mp_errs)
      | _ -> None)

  type action_ret = (t * vt list, err_t) Res_list.t

  let init_with_pred_table pred_defs init_data =
    let empty_variants : variants_t = Hashtbl.create 1 in
    {
      state = State.init init_data;
      preds = Preds.init [];
      wands = Wands.init [];
      pred_defs;
      variants = empty_variants;
    }

  let init init_data =
    let empty_pred_defs : MP.preds_tbl_t = MP.init_pred_defs () in
    let empty_variants : variants_t = Hashtbl.create 1 in
    {
      state = State.init init_data;
      preds = Preds.init [];
      wands = Wands.init [];
      pred_defs = empty_pred_defs;
      variants = empty_variants;
    }

  let get_init_data astate = State.get_init_data astate.state
  let sure_is_nonempty t = State.sure_is_nonempty t.state

  let copy_with_state (astate : t) (state : state_t) =
    {
      state;
      preds = Preds.copy astate.preds;
      wands = Wands.copy astate.wands;
      pred_defs = astate.pred_defs;
      variants = Hashtbl.copy astate.variants;
    }

  let make_p
      ~(preds : MP.preds_tbl_t)
      ~(init_data : init_data)
      ~(store : store_t)
      ~(pfs : PFS.t)
      ~(gamma : Type_env.t)
      ~(spec_vars : SS.t)
      () : t =
    let state = State.make_s ~init_data ~store ~pfs ~gamma ~spec_vars in
    let variants = Hashtbl.create 1 in
    {
      state;
      preds = Preds.init [];
      wands = Wands.init [];
      pred_defs = preds;
      variants;
    }

  let make_s ~init_data:_ ~store:_ ~pfs:_ ~gamma:_ ~spec_vars:_ : t =
    failwith "Calling make_s on a PState"

  let simplify
      ?(save = false)
      ?(kill_new_lvars : bool option)
      ?(matching = false)
      (astate : t) : SVal.SESubst.t * t list =
    let subst, states =
      State.simplify ~save ?kill_new_lvars ~matching astate.state
    in
    Preds.substitution_in_place subst astate.preds;
    Wands.substitution_in_place subst astate.wands;
    match states with
    | [] -> failwith "Impossible: state substitution returned []"
    | [ state ] -> (subst, [ { astate with state } ])
    | states -> (subst, List.map (copy_with_state astate) states)

  let eval_expr (astate : t) (e : Expr.t) =
    try State.eval_expr astate.state e
    with State.Internal_State_Error (errs, _) ->
      raise (Internal_State_Error (errs, astate))

  let get_store (astate : t) : SStore.t = State.get_store astate.state

  let set_store (astate : t) (store : SStore.t) : t =
    { astate with state = State.set_store astate.state store }

  let get_preds (astate : t) : Preds.t = astate.preds
  let set_preds (astate : t) (preds : Preds.t) : t = { astate with preds }
  let set_wands astate wands = { astate with wands }

  let set_variants (astate : t) (variants : variants_t) : t =
    { astate with variants }

  let assume ?(unfold = false) (astate : t) (v : Expr.t) : t list =
    let open Syntaxes.List in
    let* state = State.assume ~unfold astate.state v in
    let astate' = { astate with state } in
    match (!Config.unfolding && unfold, v) with
    | _, Lit (Bool true) -> [ astate' ]
    | false, _ -> [ astate' ]
    | true, _ -> (
        let unfold_vals = Expr.base_elements v in
        match
          SMatcher.unfold_with_vals ~auto_level:`Low astate' unfold_vals
        with
        | None -> [ astate' ]
        | Some next_states ->
            let* _, astate = next_states in
            let _, astates = simplify ~kill_new_lvars:false astate in
            astates)

  let assume_a
      ?(matching = false)
      ?(production = false)
      ?(time = "")
      (astate : t)
      (fs : Expr.t list) : t option =
    match State.assume_a ~matching ~production ~time astate.state fs with
    | Some state -> Some { astate with state }
    | None -> None

  let assume_t (astate : t) (v : Expr.t) (t : Type.t) : t option =
    State.assume_t astate.state v t
    |> Option.map (fun state -> { astate with state })

  let sat_check (astate : t) (v : Expr.t) : bool =
    State.sat_check astate.state v

  let sat_check_f (astate : t) (fs : Expr.t list) : SVal.SESubst.t option =
    State.sat_check_f astate.state fs

  let assert_a (astate : t) (fs : Expr.t list) : bool =
    State.assert_a astate.state fs

  let equals (astate : t) (v1 : Expr.t) (v2 : Expr.t) : bool =
    State.equals astate.state v1 v2

  let get_type (astate : t) (v : Expr.t) : Type.t option =
    State.get_type astate.state v

  let copy (astate : t) : t =
    let { state; preds; wands; pred_defs; variants } = astate in
    {
      state = State.copy state;
      preds = Preds.copy preds;
      wands = Wands.copy wands;
      pred_defs;
      variants = Hashtbl.copy variants;
    }

  let simplify_val (astate : t) (v : Expr.t) : Expr.t =
    State.simplify_val astate.state v

  let pp_variants : (string * Expr.t option) Fmt.t =
    Fmt.pair ~sep:Fmt.comma Fmt.string (Fmt.option Expr.pp)

  let pp fmt (astate : t) : unit =
    let { state; preds; wands; variants; _ } = astate in
    Fmt.pf fmt
      "%a@\n\
       @[<v 2>PREDICATES:@\n\
       %a@]@\n\
       @[<v 2>WANDS:@\n\
       %a@]@\n\
       @[<v 2>VARIANTS:@\n\
       %a@]@\n"
      State.pp state Preds.pp preds Wands.pp wands
      (Fmt.hashtbl ~sep:Fmt.semi pp_variants)
      variants

  let pp_by_need pvars lvars locs fmt astate : unit =
    let { state; preds; wands; _ } = astate in
    (* TODO: Pred printing by need *)
    Fmt.pf fmt "%a@\n@[<v 2>PREDICATES:@\n%a@]@\n@[<v 2>WANDS:@\n%a@]@\n"
      (State.pp_by_need pvars lvars locs)
      state Preds.pp preds Wands.pp wands

  let add_spec_vars (astate : t) (vs : Var.Set.t) : t =
    let state = State.add_spec_vars astate.state vs in
    { astate with state }

  let get_spec_vars (astate : t) : Var.Set.t = State.get_spec_vars astate.state

  let get_lvars (astate : t) : Var.Set.t =
    let { state; preds; wands; _ } = astate in
    State.get_lvars state
    |> SS.union (Preds.get_lvars preds)
    |> SS.union (Wands.get_lvars wands)

  let to_assertions ?(to_keep : SS.t option) (astate : t) : Asrt.t =
    let { state; preds; wands; _ } = astate in
    let s_asrts = State.to_assertions ?to_keep state in
    let p_asrts = Preds.to_assertions preds in
    let w_asrts = Wands.to_assertions wands in
    List.sort Asrt.compare (p_asrts @ s_asrts @ w_asrts)

  let substitution_in_place ?(subst_all = false) (subst : st) (astate : t) :
      t list =
    let { state; preds; wands; pred_defs; variants } = astate in
    Preds.substitution_in_place subst preds;
    Wands.substitution_in_place subst wands;
    let subst_variants = Hashtbl.create 1 in
    let () =
      Hashtbl.iter
        (fun func variant ->
          Hashtbl.add subst_variants func
            (Option.map
               (SVal.SESubst.subst_in_expr subst ~partial:true)
               variant))
        variants
    in
    List.map
      (fun state ->
        {
          state;
          preds = Preds.copy preds;
          wands = Wands.copy wands;
          variants = Hashtbl.copy subst_variants;
          pred_defs;
        })
      (State.substitution_in_place ~subst_all subst state)

  let update_store (state : t) (x : string option) (v : Expr.t) : t =
    match x with
    | None -> state
    | Some x ->
        let store = get_store state in
        let _ = SStore.put store x v in
        let state' = set_store state store in
        state'

  (* FIXME: This needs to change -> we need to return a matching ret type, so we can
      compose with bi-abduction at the spec level *)
  let run_spec_aux
      ?(existential_bindings : (string * vt) list option)
      (name : string)
      (params : string list)
      (mp : MP.t)
      (astate : t)
      (x : string option)
      (args : vt list) : (t * Flag.t, SMatcher.err_t) Res_list.t =
    L.verbose (fun m ->
        m "INSIDE RUN spec of %s with the following MP:@\n%a@\n" name MP.pp mp);
    let old_store = get_store astate in
    let new_store =
      try SStore.init (List.combine params args)
      with Invalid_argument _ ->
        let message =
          Fmt.str
            "Running spec of %s which takes %i parameters with the following \
             %i arguments : %a"
            name (List.length params) (List.length args) (Fmt.Dump.list Expr.pp)
            args
        in
        raise (Invalid_argument message)
    in
    let astate' = set_store astate new_store in
    let existential_bindings = Option.value ~default:[] existential_bindings in
    let existential_bindings =
      List.map (fun (x, v) -> (Expr.LVar x, v)) existential_bindings
    in
    let store_bindings = SStore.bindings new_store in
    let store_bindings =
      List.map (fun (x, v) -> (Expr.PVar x, v)) store_bindings
    in
    let subst = SVal.SESubst.init (existential_bindings @ store_bindings) in

    L.verbose (fun m ->
        m "About to use the spec of %s with the following MP:@\n%a@\n" name
          MP.pp mp);

    let open Res_list.Syntax in
    let open Syntaxes.List in
    let res = SMatcher.match_ astate' subst mp (FunctionCall name) in
    if List.find_opt Result.is_error res |> Option.is_some then
      L.normal (fun m ->
          m "WARNING: Failed to match against the precondition of procedure %s"
            name);
    let** frame_state, subst, posts = res in
    let fl, posts =
      match posts with
      | Some (fl, posts) -> (fl, posts)
      | None ->
          let msg =
            Printf.sprintf
              "SYNTAX ERROR: Spec of %s does not have a postcondition" name
          in
          L.fail msg
    in
    (* OK FOR DELAY ENTAILMENT *)
    let* final_state = SMatcher.produce_posts frame_state subst posts in
    let final_store = get_store final_state in
    let v_ret = SStore.get final_store Names.return_variable in
    let final_state = set_store final_state (SStore.copy old_store) in
    let v_ret = Option.value ~default:(Lit Undefined) v_ret in
    let final_state = update_store final_state x v_ret in
    let _, final_states = simplify ~matching:true final_state in
    let+ final_state = final_states in
    match SMatcher.unfold_concrete_preds final_state with
    | Some (_, with_unfolded_concrete) -> Ok (with_unfolded_concrete, fl)
    | None -> raise (Internal_State_Error ([], final_state))

  let fresh_subst (xs : SS.t) : SVal.SESubst.t =
    let xs = SS.elements xs in
    let bindings =
      List.map (fun x -> (Expr.LVar x, Expr.LVar (LVar.alloc ()))) xs
    in
    SVal.SESubst.init bindings

  let make_id_subst (a : Asrt.t) : SVal.SESubst.t =
    let lvars = Asrt.lvars a in
    let alocs = Asrt.alocs a in
    let lvars_subst =
      List.map (fun x -> (Expr.LVar x, Expr.LVar x)) (SS.elements lvars)
    in
    let alocs_subst =
      List.map (fun x -> (Expr.ALoc x, Expr.ALoc x)) (SS.elements alocs)
    in
    let subst_lst = lvars_subst @ alocs_subst in
    SVal.SESubst.init subst_lst

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

  let consume ~(prog : 'a MP.prog) astate (a : Asrt.t) binders =
    if not (List.for_all Names.is_lvar_name binders) then
      failwith "Binding of pure variables in *-assert.";
    let store = State.get_store astate.state in
    let pvars_store = SStore.domain store in
    let pvars_a = Asrt.pvars a in
    let pvars_diff = SS.diff pvars_a pvars_store in
    (if not (SS.is_empty pvars_diff) then
       let pvars_errs : err_t list =
         List.map (fun pvar : err_t -> EVar pvar) (SS.elements pvars_diff)
       in
       raise (Internal_State_Error (pvars_errs, astate)));
    let store_subst = SStore.to_ssubst store in
    let a = SVal.SESubst.substitute_asrt store_subst ~partial:true a in
    (* let known_vars   = SS.diff (SS.filter is_spec_var_name (Asrt.lvars a)) (SS.of_list binders) in *)
    let state_lvars = State.get_lvars astate.state in
    let known_lvars =
      SS.elements
        (SS.diff (SS.inter state_lvars (Asrt.lvars a)) (SS.of_list binders))
    in
    let known_lvars = List.map (fun x -> Expr.LVar x) known_lvars in
    let asrt_alocs =
      List.map (fun x -> Expr.ALoc x) (SS.elements (Asrt.alocs a))
    in
    let known_matchables = Expr.Set.of_list (known_lvars @ asrt_alocs) in

    let pred_ins =
      Hashtbl.fold
        (fun name (pred : Pred.t) pred_ins ->
          Hashtbl.add pred_ins name pred.pred_ins;
          pred_ins)
        prog.prog.preds
        (Hashtbl.create Config.medium_tbl_size)
    in

    let mp =
      MP.init known_matchables Expr.Set.empty pred_ins [ (a, (None, None)) ]
    in
    let vars_to_forget = SS.inter state_lvars (SS.of_list binders) in
    if not (SS.is_empty vars_to_forget) then (
      let oblivion_subst = fresh_subst vars_to_forget in
      L.verbose (fun m ->
          m "Forget @[%a@] with subst: %a"
            Fmt.(iter ~sep:comma SS.iter string)
            vars_to_forget SVal.SESubst.pp oblivion_subst);

      (* TODO: THIS SUBST IN PLACE MUST NOT BRANCH *)
      let subst_in_place = substitution_in_place oblivion_subst astate in
      assert (List.length subst_in_place = 1);
      let astate = List.hd subst_in_place in

      L.verbose (fun m -> m "State after substitution:@\n@[%a@]\n" pp astate));
    let mp =
      match mp with
      | Error asrts -> raise (Preprocessing_Error [ MPAssert (a, asrts) ])
      | Ok mp -> mp
    in
    let bindings =
      List.map
        (fun (e : Expr.t) ->
          let id =
            match e with
            | LVar _ | ALoc _ -> e
            | _ ->
                raise (Failure "Impossible: matchable not an lvar or an aloc")
          in
          (id, e))
        (Expr.Set.elements known_matchables)
    in
    (* let old_astate = copy astate in *)
    let subst = SVal.SESubst.init bindings in
    let open Syntaxes.List in
    let* matching_result = SMatcher.match_ astate subst mp LogicCommand in
    match matching_result with
    | Ok (new_state, subst', _) ->
        (* Successful matching *)
        let lbinders = List.map (fun x -> Expr.LVar x) binders in
        let new_bindings =
          List.map (fun e -> (e, SVal.SESubst.get subst' e)) lbinders
        in
        let success = List.for_all (fun (_, x_v) -> x_v <> None) new_bindings in
        if not success then
          raise (Failure "Assert failed - binders not captured");
        let additional_bindings =
          List.filter
            (fun (e, v) -> (not (List.mem e lbinders)) && not (Expr.equal e v))
            (SVal.SESubst.to_list subst')
        in
        let new_bindings =
          List.map (fun (x, y) -> (x, Option.get y)) new_bindings
          @ additional_bindings
        in
        let new_bindings =
          List.map
            (fun (e, e_v) -> Asrt.Pure (BinOp (e, Equal, e_v)))
            new_bindings
        in
        let full_subst = make_id_subst a in
        let a_produce = new_bindings in
        let open Res_list.Syntax in
        let result =
          let** new_astate = SMatcher.produce new_state full_subst a_produce in
          let new_state' =
            State.add_spec_vars new_astate.state (SS.of_list binders)
          in
          let subst, new_states =
            State.simplify ~kill_new_lvars:true new_state'
          in
          let () = Preds.substitution_in_place subst new_astate.preds in
          let () = Wands.substitution_in_place subst new_astate.wands in
          let+ new_state = new_states in
          Ok (copy_with_state new_astate new_state)
        in
        Res_list.map_error
          (fun _ ->
            let msg =
              Fmt.str
                "Assert failed with argument %a. unable to produce variable \
                 bindings."
                Asrt.pp a
            in
            StateErr.EOther msg)
          result
    | Error err ->
        let fail_pfs : Expr.t = State.get_failing_constraint err in

        let failing_model = State.sat_check_f astate.state [ fail_pfs ] in
        let msg =
          Fmt.str
            "Assert failed with argument @[<h>%a@]. matching failed.@\n\
             @[<v 2>Errors:@\n\
             %a.@]@\n\
             @[<v 2>Failing Model:@\n\
             %a@]@\n"
            Asrt.pp a State.pp_err err
            Fmt.(option ~none:(any "CANNOT CREATE MODEL") SVal.SESubst.pp)
            failing_model
        in
        L.print_to_all msg;
        Res_list.error_with (StateErr.EPure fail_pfs)

  let produce astate a =
    let store = State.get_store astate.state in
    let pvars_store = SStore.domain store in
    let pvars_a = Asrt.pvars a in
    let pvars_diff = SS.diff pvars_a pvars_store in
    (if not (SS.is_empty pvars_diff) then
       let pvars_errs : err_t list =
         List.map (fun pvar : err_t -> EVar pvar) (SS.elements pvars_diff)
       in
       raise (Internal_State_Error (pvars_errs, astate)));
    let store_subst = SStore.to_ssubst store in
    let a = SVal.SESubst.substitute_asrt store_subst ~partial:true a in
    let open Syntaxes.List in
    let open Res_list.Syntax in
    let full_subst = make_id_subst a in
    let** new_astate = SMatcher.produce astate full_subst a in
    let subst, new_states =
      State.simplify ~kill_new_lvars:true new_astate.state
    in
    let () = Preds.substitution_in_place subst new_astate.preds in
    let () = Wands.substitution_in_place subst new_astate.wands in
    let+ new_state = new_states in
    Ok (copy_with_state new_astate new_state)

  let match_invariant
      (prog : 'a MP.prog)
      (revisited : bool)
      (astate : t)
      (a : Asrt.t)
      (binders : string list) : (t * t, err_t) Res_list.t =
    let store = State.get_store astate.state in
    let pvars_store = SStore.domain store in
    let pvars_a = Asrt.pvars a in
    let pvars_diff = SS.diff pvars_a pvars_store in
    L.verbose (fun m -> m "%s" (String.concat ", " (SS.elements pvars_diff)));
    (if not (SS.is_empty pvars_diff) then
       let pvars_errs : err_t list =
         List.map (fun pvar : err_t -> EVar pvar) (SS.elements pvars_diff)
       in
       raise (Internal_State_Error (pvars_errs, astate)));
    let lvar_binders, pvar_binders =
      List.partition Names.is_lvar_name binders
    in
    let known_pvars = List.map Expr.from_var_name (SS.elements pvars_a) in
    let state_lvars = State.get_lvars astate.state in
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
    let known_matchables =
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
    let mp =
      (* FIXME: UNDERSTAND IF THE OX SHOULD BE [] *)
      MP.init known_matchables Expr.Set.empty pred_ins [ (a, (None, None)) ]
    in
    (* This will not do anything in the original pass,
       but will do precisely what is needed in the re-establishment *)
    let vars_to_forget = SS.inter state_lvars (SS.of_list lvar_binders) in
    if vars_to_forget <> SS.empty then (
      let oblivion_subst = fresh_subst vars_to_forget in
      L.verbose (fun m ->
          m "Forget @[%a@] with subst: %a"
            Fmt.(iter ~sep:comma SS.iter string)
            vars_to_forget SVal.SESubst.pp oblivion_subst);

      (* TODO: THIS SUBST IN PLACE MUST NOT BRANCH *)
      let subst_in_place =
        substitution_in_place ~subst_all:true oblivion_subst astate
      in
      assert (List.length subst_in_place = 1);
      let astate = List.hd subst_in_place in

      L.verbose (fun m -> m "State after substitution:@\n@[%a@]\n" pp astate))
    else ();
    let mp =
      match mp with
      | Error asrts -> raise (Preprocessing_Error [ MPAssert (a, asrts) ])
      | Ok mp -> mp
    in
    let bindings =
      List.map
        (fun (e : Expr.t) ->
          let binding =
            match e with
            | PVar x -> SStore.get (State.get_store astate.state) x
            | LVar _ | ALoc _ -> Some e
            | _ ->
                raise
                  (Failure
                     "Impossible: matchable not a pvar or an lvar or an aloc")
          in
          (e, Option.get binding))
        (Expr.Set.elements known_matchables)
    in
    let subst = SVal.SESubst.init bindings in
    let open Res_list.Syntax in
    let open Syntaxes.List in
    let** new_state, subst', _ =
      let+ result = SMatcher.match_ astate subst mp Invariant in
      match result with
      | Ok state -> Ok state
      | Error err ->
          let fail_pfs : Expr.t = State.get_failing_constraint err in
          let failing_model = State.sat_check_f astate.state [ fail_pfs ] in
          let () =
            L.print_to_all
              (Format.asprintf
                 "MATCH INVARIANT FAILURE: with argument @[<h>%a@]. matching \
                  failed.@\n\
                  @[<v 2>Errors:@\n\
                  %a.@]@\n\
                  @[<v 2>Failing Model:@\n\
                  %a@]@\n"
                 Asrt.pp a State.pp_err err
                 Fmt.(option ~none:(any "CANNOT CREATE MODEL") SVal.SESubst.pp)
                 failing_model)
          in
          Error (StateErr.EPure fail_pfs)
    in
    (* Successful matching *)
    (* TODO: Should the frame state have the subst produced? *)
    let frame_state = copy new_state in
    let frame_state = set_store frame_state (SStore.init []) in

    let lbinders = List.map (fun x -> Expr.LVar x) lvar_binders in
    let new_bindings =
      List.map (fun e -> (e, SVal.SESubst.get subst' e)) lbinders
    in
    let success = List.for_all (fun (_, x_v) -> x_v <> None) new_bindings in
    if not success then
      raise (Failure "MATCH INVARIANT FAILURE: binders not captured")
    else
      let new_bindings =
        List.map (fun (x, x_v) -> (x, Option.get x_v)) new_bindings
      in
      let bindings =
        List.filter
          (fun (e, v) -> (not (List.mem e lbinders)) && not (Expr.equal e v))
          (SVal.SESubst.to_list subst')
      in
      L.verbose (fun fmt ->
          fmt "Additional bindings: %a"
            Fmt.(
              brackets
                (list ~sep:semi (parens (pair ~sep:comma Expr.pp Expr.pp))))
            bindings);
      let known_pvars =
        SS.elements (SS.diff pvars_a (SS.of_list pvar_binders))
      in
      let bindings =
        (if revisited then new_bindings @ bindings else bindings)
        |> List.filter (fun (x, _) ->
               match x with
               | Expr.PVar x when List.mem x pvar_binders -> false
               | UnOp (LstLen, _) -> false
               | _ -> true)
        |> List.map (fun (e, e_v) -> Asrt.Pure (BinOp (e, Equal, e_v)))
      in
      let subst_bindings = make_id_subst bindings in
      let pvar_subst_list_known =
        List.map
          (fun x ->
            ( Expr.PVar x,
              Option.get (SStore.get (State.get_store astate.state) x) ))
          known_pvars
      in
      let pvar_subst_list_bound =
        List.map
          (fun x -> (Expr.PVar x, Expr.LVar (LVar.alloc ())))
          pvar_binders
      in
      let full_subst = make_id_subst a in
      let pvar_subst_list = pvar_subst_list_known @ pvar_subst_list_bound in
      let pvar_subst = SVal.SESubst.init pvar_subst_list in
      let _ = SVal.SESubst.merge_left full_subst subst_bindings in
      let _ = SVal.SESubst.merge_left full_subst pvar_subst in
      L.verbose (fun fmt -> fmt "Invariant v1: %a" Asrt.pp a);
      let a_substed =
        Reduction.reduce_assertion
          (SVal.SESubst.substitute_asrt subst_bindings ~partial:true a)
      in
      L.verbose (fun fmt -> fmt "Invariant v2: %a" Asrt.pp a_substed);
      let a_produce = Reduction.reduce_assertion (bindings @ a_substed) in
      L.verbose (fun fmt -> fmt "Invariant v3: %a" Asrt.pp a_produce);
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
            SStore.put store x v)
          pvar_subst_list
      in
      let invariant_state = set_store invariant_state store in
      let* res = SMatcher.produce invariant_state full_subst a_produce in
      match res with
      | Ok new_astate ->
          let new_state' =
            State.add_spec_vars new_astate.state (SS.of_list lvar_binders)
          in
          let invariant_state = { new_astate with state = new_state' } in
          let _, invariant_states =
            simplify ~kill_new_lvars:true invariant_state
          in
          let+ invariant_state = invariant_states in
          Ok (copy frame_state, invariant_state)
      | Error e ->
          let msg =
            Fmt.str
              "MATCH INVARIANT FAILURE: %a\n\
               unable to produce variable bindings: %a." Asrt.pp a pp_err_t e
          in
          L.print_to_all msg;
          Res_list.error_with e

  let frame_on (astate : t) (iframes : (string * t) list) (ids : string list) :
      (t, err_t) Res_list.t =
    let rec get_relevant_frames iframes ids =
      match (iframes, ids) with
      | [], _ | _, [] -> []
      | (id, frame) :: ar, id' :: br ->
          if String.equal id id' then (id, frame) :: get_relevant_frames ar br
          else L.fail "Framing: Malformed loop identifiers."
    in
    let open Syntaxes.List in
    let open Res_list.Syntax in
    let frames = get_relevant_frames iframes ids in
    List.fold_left
      (fun astates (id, frame) ->
        let** astate = astates in
        let** astate =
          let frame_asrt = to_assertions frame in
          let full_subst = make_id_subst frame_asrt in
          let+ produced = SMatcher.produce astate full_subst frame_asrt in
          match produced with
          | Error err ->
              L.print_to_all
                (Fmt.str "Unable to produce frame for loop %s, because of :\n%a"
                   id pp_err_t err);
              Error err
          | Ok succ -> Ok succ
        in
        let _, states = simplify ~kill_new_lvars:true astate in
        List.map Result.ok states)
      (Res_list.return astate) frames

  (**
    Evaluation of logic commands

    @param prog GIL program
    @param lcmd Logic command to be evaluated
    @param state Current state
    @param preds Current predicate set
    @return List of states/predicate sets resulting from the evaluation
  *)
  let evaluate_slcmd (prog : 'a MP.prog) (lcmd : SLCmd.t) (astate : t) :
      (t, err_t) Res_list.t =
    let eval_expr e =
      try State.eval_expr astate.state e
      with State.Internal_State_Error (errs, _) ->
        raise (Internal_State_Error (errs, astate))
    in
    let open Res_list.Syntax in
    let** resulting_astate =
      match lcmd with
      | SymbExec -> failwith "Impossible: Untreated SymbExec"
      | Fold (pname, les, fold_info) ->
          let vs = List.map eval_expr les in
          let pred = MP.get_pred_def prog.preds pname in
          let additional_bindings =
            Option.fold
              ~some:(fun (_, bindings) ->
                List.map (fun (x, e) -> (Expr.LVar x, eval_expr e)) bindings)
              ~none:[] fold_info
          in
          SMatcher.fold ~additional_bindings ~match_kind:LogicCommand
            ~state:astate pred vs
      | Unfold (pname, les, additional_bindings, b) ->
          (* Unfoldig predicate with name [pname] and arguments [les].
             [additional_bindings] is the set set of additional bindings that may be learned when unfolding,
             and [b] says if the predicate should be unfolded entirely (up to 10 times, otherwise failure) *)
          (* 1) We retrieve the definition of the predicate to unfold and make sure
             it is not abstract and hence can be unfolded. *)
          let pred = MP.get_pred_def prog.preds pname in
          if pred.pred.pred_abstract then
            Fmt.failwith "Impossible: Unfold of abstract predicate %s" pname;
          (* 2) We evaluate the arguments, filter to keep only the in-parameters
             (which are sufficient to trigger the unfold) *)
          let vs = List.map eval_expr les in
          let vs_ins = Pred.in_args pred.pred vs in
          let vs = List.map Option.some vs in
          (* FIXME: make sure correct number of params *)
          (* 3) We consume the predicate from the state. *)
          let cons_res = SMatcher.consume_pred astate pname vs in
          let () =
            match (cons_res, !Config.under_approximation) with
            | [], false ->
                Fmt.failwith
                  "HORROR - unfold vanished while consuming folded predicate: \
                   %a"
                  SLCmd.pp lcmd
            | _ -> ()
          in
          let** astate, vs' = cons_res in
          L.verbose (fun m ->
              m "@[<h>Returned values: %a@]" Fmt.(list ~sep:comma Expr.pp) vs');
          let vs = Pred.combine_ins_outs pred.pred vs_ins vs' in
          L.verbose (fun m ->
              m "@[<h>LCMD Unfold about to happen with rec %b info: %a@]" b
                SLCmd.pp_unfold_info additional_bindings);
          if b then SMatcher.rec_unfold astate pname vs
          else (
            L.verbose (fun m ->
                m "@[<h>Values: %a@]" Fmt.(list ~sep:comma Expr.pp) vs);
            let** _, state =
              SMatcher.unfold ?additional_bindings astate pname vs
            in
            let _, states =
              simplify ~kill_new_lvars:true ~matching:true state
            in
            Res_list.just_oks states)
      | Package { lhs; rhs } ->
          let++ astate =
            let res = SMatcher.package_wand astate { lhs; rhs } in
            L.verbose (fun m ->
                m "wand package returned %a"
                  (List_res.pp ~ok:pp ~err:pp_err_t)
                  res);
            Res_list.of_list_res res
          in
          Wands.extend astate.wands { lhs; rhs };
          astate
      | GUnfold pname ->
          let** astate = SMatcher.unfold_all astate pname in
          let _, astates = simplify ~kill_new_lvars:true astate in
          Res_list.just_oks astates
      | SepAssert (a, binders) -> (
          if not (List.for_all Names.is_lvar_name binders) then
            failwith "Binding of pure variables in *-assert.";
          let store = State.get_store astate.state in
          let pvars_store = SStore.domain store in
          let pvars_a = Asrt.pvars a in
          let pvars_diff = SS.diff pvars_a pvars_store in
          L.verbose (fun m ->
              m "%s" (String.concat ", " (SS.elements pvars_diff)));
          (if not (SS.is_empty pvars_diff) then
             let pvars_errs : err_t list =
               List.map (fun pvar : err_t -> EVar pvar) (SS.elements pvars_diff)
             in
             raise (Internal_State_Error (pvars_errs, astate)));
          let store_subst = SStore.to_ssubst store in
          let a = SVal.SESubst.substitute_asrt store_subst ~partial:true a in
          (* let known_vars   = SS.diff (SS.filter is_spec_var_name (Asrt.lvars a)) (SS.of_list binders) in *)
          let state_lvars = State.get_lvars astate.state in
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
          let known_matchables = Expr.Set.of_list (known_lvars @ asrt_alocs) in

          let pred_ins =
            Hashtbl.fold
              (fun name (pred : Pred.t) pred_ins ->
                Hashtbl.add pred_ins name pred.pred_ins;
                pred_ins)
              prog.prog.preds
              (Hashtbl.create Config.medium_tbl_size)
          in

          let mp =
            MP.init known_matchables Expr.Set.empty pred_ins
              [ (a, (None, None)) ]
          in
          let vars_to_forget = SS.inter state_lvars (SS.of_list binders) in
          if not (SS.is_empty vars_to_forget) then (
            let oblivion_subst = fresh_subst vars_to_forget in
            L.verbose (fun m ->
                m "Forget @[%a@] with subst: %a"
                  Fmt.(iter ~sep:comma SS.iter string)
                  vars_to_forget SVal.SESubst.pp oblivion_subst);

            (* TODO: THIS SUBST IN PLACE MUST NOT BRANCH *)
            let subst_in_place = substitution_in_place oblivion_subst astate in
            assert (List.length subst_in_place = 1);
            let astate = List.hd subst_in_place in

            L.verbose (fun m ->
                m "State after substitution:@\n@[%a@]\n" pp astate));
          let mp =
            match mp with
            | Error asrts -> raise (Preprocessing_Error [ MPAssert (a, asrts) ])
            | Ok mp -> mp
          in
          let bindings =
            List.map
              (fun (e : Expr.t) ->
                let id =
                  match e with
                  | LVar _ | ALoc _ -> e
                  | _ ->
                      raise
                        (Failure "Impossible: matchable not an lvar or an aloc")
                in
                (id, e))
              (Expr.Set.elements known_matchables)
          in
          (* let old_astate = copy astate in *)
          let subst = SVal.SESubst.init bindings in
          let open Syntaxes.List in
          let* matching_result = SMatcher.match_ astate subst mp LogicCommand in
          match matching_result with
          | Ok (new_state, subst', _) ->
              (* Successful matching *)
              let lbinders = List.map (fun x -> Expr.LVar x) binders in
              let new_bindings =
                List.map (fun e -> (e, SVal.SESubst.get subst' e)) lbinders
              in
              let success =
                List.for_all (fun (_, x_v) -> x_v <> None) new_bindings
              in
              if not success then
                raise (Failure "Assert failed - binders not captured");
              let additional_bindings =
                List.filter
                  (fun (e, v) ->
                    (not (List.mem e lbinders)) && not (Expr.equal e v))
                  (SVal.SESubst.to_list subst')
              in
              let new_bindings =
                List.map (fun (x, y) -> (x, Option.get y)) new_bindings
                @ additional_bindings
              in
              let new_bindings =
                List.map
                  (fun (e, e_v) -> Asrt.Pure (BinOp (e, Equal, e_v)))
                  new_bindings
              in
              let a_new_bindings = new_bindings in
              let subst_bindings = make_id_subst a_new_bindings in
              let full_subst = make_id_subst a in
              let _ = SVal.SESubst.merge_left full_subst subst_bindings in
              let a_substed =
                SVal.SESubst.substitute_asrt subst_bindings ~partial:true a
              in
              let a_produce = a_new_bindings @ a_substed in
              let result =
                let** new_astate =
                  SMatcher.produce new_state full_subst a_produce
                in
                let new_state' =
                  State.add_spec_vars new_astate.state (SS.of_list binders)
                in
                let subst, new_states =
                  State.simplify ~kill_new_lvars:true new_state'
                in
                let () = Preds.substitution_in_place subst new_astate.preds in
                let () = Wands.substitution_in_place subst new_astate.wands in
                let+ new_state = new_states in

                Ok (copy_with_state new_astate new_state)
              in
              Res_list.map_error
                (fun _ ->
                  let msg =
                    Fmt.str
                      "Assert failed with argument %a. unable to produce \
                       variable bindings."
                      Asrt.pp a
                  in
                  StateErr.EOther msg)
                result
          | Error err ->
              let fail_pfs : Expr.t = State.get_failing_constraint err in

              let failing_model = State.sat_check_f astate.state [ fail_pfs ] in
              let msg =
                Fmt.str
                  "Assert failed with argument @[<h>%a@]. matching failed.@\n\
                   @[<v 2>Errors:@\n\
                   %a.@]@\n\
                   @[<v 2>Failing Model:@\n\
                   %a@]@\n"
                  Asrt.pp a State.pp_err err
                  Fmt.(option ~none:(any "CANNOT CREATE MODEL") SVal.SESubst.pp)
                  failing_model
              in
              L.print_to_all msg;
              Res_list.error_with (StateErr.EPure fail_pfs))
      | Consume (asrt, binders) -> consume ~prog astate asrt binders
      | Produce asrt -> produce astate asrt
      | ApplyLem (lname, args, binders) ->
          if not (List.for_all Names.is_lvar_name binders) then
            failwith "Binding of pure variables in lemma application.";
          let lemma =
            match MP.get_lemma prog lname with
            | Error _ -> Fmt.failwith "Lemma %s does not exist" lname
            | Ok lemma -> lemma
          in
          let v_args : vt list = List.map eval_expr args in
          (* Printf.printf "apply lemma. binders: %s. existentials: %s\n\n"
             (String.concat ", " binders) (String.concat ", " lemma.lemma.existentials); *)
          let existential_bindings =
            List.map2
              (fun x y -> (x, Expr.LVar y))
              lemma.data.lemma_existentials binders
          in
          let** astate, _ =
            run_spec_aux ~existential_bindings lname lemma.data.lemma_params
              lemma.mp astate None v_args
          in
          let astate = add_spec_vars astate (Var.Set.of_list binders) in
          let _, astates = simplify ~matching:true astate in
          Res_list.just_oks astates
      | Invariant _ ->
          raise
            (Failure "Invariant must be treated by the match_invariant function")
    in
    let _, astates = simplify resulting_astate in
    Res_list.just_oks astates

  let run_spec
      (spec : MP.spec)
      (astate : t)
      (x : string)
      (args : vt list)
      (subst : (string * (string * vt) list) option) :
      (t * Flag.t, err_t) Res_list.t =
    match subst with
    | None ->
        run_spec_aux spec.data.spec_name spec.data.spec_params spec.mp astate
          (Some x) args
    | Some (_, subst_lst) ->
        run_spec_aux ~existential_bindings:subst_lst spec.data.spec_name
          spec.data.spec_params spec.mp astate (Some x) args

  let matches
      (astate : t)
      (subst : st)
      (mp : MP.t)
      (match_type : Matcher.match_kind) : bool =
    if !Config.under_approximation then
      failwith
        "WE CAN'T CHECK IF SOMETHING FULLY MATCHES IN UNDER-APPROXIMATION MODE";
    let matching_results = SMatcher.match_ astate subst mp match_type in
    let success = List.for_all Result.is_ok matching_results in
    L.verbose (fun fmt -> fmt "PSTATE.matches: Success: %b" success);
    success

  let unfolding_vals (astate : t) (fs : Expr.t list) : vt list =
    State.unfolding_vals astate.state fs

  let add_pred_defs (pred_defs : MP.preds_tbl_t) (astate : t) : t =
    { astate with pred_defs }

  let fresh_loc ?(loc : vt option) (astate : t) : vt =
    State.fresh_loc ?loc astate.state

  let clean_up ?keep:_ (astate : t) : unit = State.clean_up astate.state

  let produce (astate : t) (subst : st) (a : Asrt.t) : (t, err_t) Res_list.t =
    SMatcher.produce astate subst a

  let match_assertion (astate : t) (subst : st) (step : MP.step) =
    SMatcher.match_assertion astate subst step

  let produce_posts (astate : t) (subst : st) (asrts : Asrt.t list) : t list =
    SMatcher.produce_posts astate subst asrts

  let get_all_preds ?(keep : bool option) (sel : abs_t -> bool) (astate : t) :
      abs_t list =
    Preds.get_all ~maintain:(Option.value ~default:true keep) sel astate.preds

  let set_pred (astate : t) (pred : abs_t) : unit =
    let { preds; pred_defs; _ } = astate in
    let pred_name, _ = pred in
    let pure = (Hashtbl.find pred_defs pred_name).pred.pred_pure in
    Preds.extend ~pure preds pred

  let update_subst (astate : t) (subst : st) : unit =
    State.update_subst astate.state subst

  let execute_action (action : string) (astate : t) (args : vt list) :
      action_ret =
    let open Syntaxes.List in
    let+ result = State.execute_action action astate.state args in
    match result with
    | Ok (state, outs) -> Ok (copy_with_state astate state, outs)
    | Error err -> Error err

  let consume_core_pred core_pred astate in_args =
    let open Syntaxes.List in
    let+ result = State.consume_core_pred core_pred astate.state in_args in
    match result with
    | Ok (state, outs) -> Ok (copy_with_state astate state, outs)
    | Error err -> Error err

  let produce_core_pred core_pred astate args =
    let open Syntaxes.List in
    let+ state = State.produce_core_pred core_pred astate.state args in
    copy_with_state astate state

  let split_core_pred_further astate core_pred ins err =
    State.split_core_pred_further astate.state core_pred ins err

  let mem_constraints (astate : t) : Expr.t list =
    State.mem_constraints astate.state

  let is_overlapping_asrt (a : string) : bool = State.is_overlapping_asrt a
  let pp_err = State.pp_err
  let get_recovery_tactic astate vs = State.get_recovery_tactic astate.state vs

  let try_recovering (astate : t) (tactic : vt Recovery_tactic.t) :
      (t list, string) result =
    SMatcher.try_recovering astate tactic

  let get_failing_constraint = State.get_failing_constraint
  let can_fix = State.can_fix

  let get_fixes (errs : err_t) =
    L.verbose (fun m -> m "AState: get_fixes");
    State.get_fixes errs

  let get_equal_values astate = State.get_equal_values astate.state
  let get_heap astate = State.get_heap astate.state
  let get_typ_env astate = State.get_typ_env astate.state
  let get_pfs astate = State.get_pfs astate.state

  let of_yojson (yojson : Yojson.Safe.t) : (t, string) result =
    (* TODO: Deserialize other components of pstate *)
    let open Syntaxes.Result in
    let rec aux = function
      | Some state, Some preds, Some variants, Some wands, [] ->
          Ok { state; preds; pred_defs = MP.init_pred_defs (); variants; wands }
      | None, preds, variants, wands, ("state", state_yojson) :: rest ->
          let* state = State.of_yojson state_yojson in
          aux (Some state, preds, variants, wands, rest)
      | state, None, variants, wands, ("preds", preds_yojson) :: rest ->
          let* preds = Preds.of_yojson preds_yojson in
          aux (state, Some preds, variants, wands, rest)
      | state, preds, None, wands, ("variants", variants_yojson) :: rest ->
          let* variants = variants_t_of_yojson variants_yojson in
          aux (state, preds, Some variants, wands, rest)
      | state, preds, variants, None, ("wands", variants_yojson) :: rest ->
          let* wands = Wands.of_yojson variants_yojson in
          aux (state, preds, variants, Some wands, rest)
      | _ -> Error "Cannot parse yojson into PState"
    in
    match yojson with
    | `Assoc sections -> aux (None, None, None, None, sections)
    | _ -> Error "Cannot parse yojson into PState"

  let to_yojson pstate =
    (* TODO: Serialize other components of pstate *)
    let { state; preds; wands; variants; _ } = pstate in
    `Assoc
      [
        ("state", State.to_yojson state);
        ("preds", Preds.to_yojson preds);
        ("wands", Wands.to_yojson wands);
        ("variants", variants_t_to_yojson variants);
      ]
end
