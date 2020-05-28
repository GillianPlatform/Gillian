module type S = sig
  type vt

  type st

  type err_t

  type state_t

  type preds_t

  type t = state_t * preds_t * UP.preds_tbl_t

  type post_res = (Flag.t * Asrt.t list) option

  type search_state = (t * st * UP.t) list * err_t list

  type up_u_res = UPUSucc of (t * st * post_res) list | UPUFail of err_t list

  type gp_ret = GPSucc of (t * vt list) list | GPFail of err_t list

  type u_res = UWTF | USucc of t | UFail of err_t list

  type unfold_info_t = string * (string * Expr.t) list

  val produce_assertion : t -> st -> Asrt.t -> t option

  val produce : t -> st -> Asrt.t -> t option

  val produce_posts : t -> st -> Asrt.t list -> t list

  val unfold : t -> string -> vt list -> unfold_info_t option -> (st * t) list

  val rec_unfold : t -> string -> vt list -> t

  val unfold_all : t -> string -> t

  val unfold_with_vals : t -> vt list -> (st * t) list * bool

  val unfold_concrete_preds : t -> (st option * t) option

  val unify_assertion : t -> st -> Asrt.t -> u_res

  val unify_up : search_state -> up_u_res

  val unify : ?in_unification:bool -> t -> st -> UP.t -> up_u_res

  val get_pred : ?in_unification:bool -> t -> string -> vt list -> gp_ret
end

module Make
    (Val : Val.S)
    (Subst : Subst.S with type vt = Val.t and type t = Val.st)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = Subst.t
                and type store_t = Store.t)
    (Preds : Preds.S with type vt = Val.t and type st = Subst.t) :
  S
    with type vt = Val.t
     and type st = Subst.t
     and type state_t = State.t
     and type preds_t = Preds.t
     and type err_t = State.err_t = struct
  open Literal
  open Containers
  module L = Logging
  module SSubst = SVal.SSubst

  type vt = Val.t

  type st = Subst.t

  type state_t = State.t

  type preds_t = Preds.t

  type abs_t = string * vt list

  type err_t = State.err_t

  type t = state_t * preds_t * UP.preds_tbl_t

  type post_res = (Flag.t * Asrt.t list) option

  type search_state = (t * st * UP.t) list * err_t list

  type unfold_info_t = string * (string * Expr.t) list

  type gp_ret = GPSucc of (t * vt list) list | GPFail of err_t list

  type u_res = UWTF | USucc of t | UFail of err_t list

  type up_u_res = UPUSucc of (t * st * post_res) list | UPUFail of err_t list

  let update_store (astate : t) (x : string) (v : Val.t) : t =
    let state, preds, pred_defs = astate in
    let store = State.get_store state in
    let _ = Store.put store x v in
    let state' = State.set_store state store in
    (state', preds, pred_defs)

  let simplify_astate ?(save = false) ?(unification = false) (astate : t) : st =
    let state, preds, pred_defs = astate in
    let subst = State.simplify ~save ~kill_new_lvars:false ~unification state in
    Preds.substitution_in_place subst preds;
    subst

  let pp_astate fmt astate =
    let state, preds, _ = astate in
    Fmt.pf fmt "%a@\nPREDS:@\n%a@\n" State.pp state Preds.pp preds

  let copy_astate (astate : t) : t =
    let state, preds, pred_defs = astate in
    (State.copy state, Preds.copy preds, pred_defs)

  let subst_in_expr_opt (astate : t) (subst : st) (e : Expr.t) : vt option =
    let state, _, _ = astate in
    let v =
      Option.fold ~some:Val.from_expr ~none:None
        (Subst.subst_in_expr_opt subst e)
    in
    Option.map (State.simplify_val state) v

  let subst_in_expr (subst : Subst.t) (le : Expr.t) : Val.t option =
    Val.from_expr (Subst.subst_in_expr subst false le)

  let compose_substs
      (subst_lst : (string * vt) list) (subst1 : st) (subst2 : st) : st =
    let subst = Subst.init [] in

    let aux (v : vt) : vt =
      let e1 = Subst.subst_in_expr subst1 true (Val.to_expr v) in
      let e2 = Subst.subst_in_expr subst2 true e1 in
      match Val.from_expr e2 with
      | None    -> v
      | Some v2 -> v2
    in

    List.iter (fun (x, v) -> Subst.put subst x (aux v)) subst_lst;
    subst

  let get_pred_with_vs (astate : t) (vs : Val.t list) : abs_t option =
    let state, preds, pred_defs = astate in

    let print_local_info (i : int) (name : string) (args : Val.t list) : unit =
      L.verbose (fun m ->
          m "Strategy %d: Examining %s(@[<h>%a@])" i name
            Fmt.(list ~sep:comma Val.pp)
            args)
    in

    let get_pred_def (name : string) : Pred.t =
      try
        let pred = Hashtbl.find pred_defs name in
        pred.pred
      with _ ->
        raise (Failure "ERROR: get_pred_with_vs: Predicate doesn't exist.")
    in

    let apply_strategies (strategies : (string * Val.t list -> bool) list) :
        (string * Val.t list) option =
      List.fold_left
        (fun ac strategy -> if ac <> None then ac else Preds.pop preds strategy)
        None strategies
    in

    (* Strategy 1: The values that we are looking for are in the in-parameters *)
    let strategy_1 ((name, args) : string * Val.t list) : bool =
      print_local_info 1 name args;
      let pred_def = get_pred_def name in
      let in_args = Pred.in_args pred_def args in
      let vs_inter = List_utils.list_inter vs in_args in
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
            | _     -> true)
          es_inter
      in
      L.verbose (fun m ->
          m "get_pred_with_vs. Strategy 1. Intersection: %s"
            (String.concat ", "
               (List.map (Fmt.to_to_string Expr.pp)
                  (Expr.Set.elements es_inter))));
      es_inter <> Expr.Set.empty
    in

    (* Strategy 2: Predicate has all literals as in-parameters *)
    let strategy_2 ((name, args) : string * Val.t list) : bool =
      print_local_info 2 name args;
      let pred_def = get_pred_def name in
      let in_args = Pred.in_args pred_def args in
      let all_literals =
        List.map
          (fun (x : Expr.t) ->
            match x with
            | Lit _ -> true
            | _     -> false)
          (List.map Val.to_expr in_args)
      in
      List.for_all (fun x -> x = true) all_literals
    in

    (* Strategy 3: The values that we are looking for are in the out-parameters *)
    let strategy_3 ((name, args) : string * Val.t list) : bool =
      print_local_info 3 name args;
      let pred_def = get_pred_def name in
      let out_args = Pred.out_args pred_def args in
      let vs_inter = List_utils.list_inter vs out_args in
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
            | _     -> true)
          es_inter
      in
      L.verbose (fun m ->
          m "get_pred_with_vs. Strategy 3. Intersection: %s"
            (String.concat ", "
               (List.map (Fmt.to_to_string Expr.pp)
                  (Expr.Set.elements es_inter))));
      es_inter <> Expr.Set.empty
    in

    (* Strategy 3: Predicate has non-literal parameters in pure formulae *)
    let strategy_4 ((name, args) : string * Val.t list) : bool =
      print_local_info 4 name args;
      let lvars_state = State.get_spec_vars state in
      let lvars_args =
        List.fold_left SS.union SS.empty
          (List.map (fun x -> Expr.lvars (Val.to_expr x)) args)
      in
      let inter = SS.inter lvars_args lvars_state in
      inter <> SS.empty
    in
    apply_strategies [ strategy_1; strategy_2; strategy_3; strategy_4 ]

  let produce_assertion (astate : t) (subst : Subst.t) (a : Asrt.t) : t option =
    let state, preds, pred_defs = astate in

    L.verbose (fun m ->
        m
          "------------------------\n\
           Produce simple assertion: %a@\n\
           @[<v 2>Subst:%a@]@\n\
           @[<v 2>STATE:@\n\
           %a@]@\n\
           @[<v 2>PREDS:@\n\
           %a@]\n"
          Asrt.pp a Subst.pp subst State.pp state Preds.pp preds);

    match a with
    | GA (a_id, ins, outs) -> (
        let setter = State.ga_to_setter a_id in
        let vs = List.map (subst_in_expr subst) (ins @ outs) in
        let failure = List.exists (fun x -> x = None) vs in
        if failure then None
        else
          let vs = List.map Option.get vs in
          match State.execute_action setter state vs with
          | ASucc [ (state', _) ] -> Some (state', preds, pred_defs)
          | _                     -> None )
    | Types les ->
        let state' =
          List.fold_left
            (fun state (le, t) ->
              match state with
              | None       -> None
              | Some state -> (
                  let v = subst_in_expr subst le in
                  match v with
                  | None   -> None
                  | Some v -> State.assume_t state v t ))
            (Some state) les
        in
        Option.map (fun state -> (state, preds, pred_defs)) state'
    | Pred (pname, les) ->
        let vs = List.map (subst_in_expr subst) les in
        let failure = List.exists (fun x -> x = None) vs in
        if failure then None
        else
          let vs = List.map Option.get vs in
          Preds.extend preds (pname, vs);
          Some (state, preds, pred_defs)
    | Pure (Eq (PVar x, le)) | Pure (Eq (le, PVar x)) ->
        if Subst.mem subst x then
          let v_x = Subst.get subst x in
          let v_le = subst_in_expr subst le in
          match (v_x, v_le) with
          | Some v_x, Some v_le ->
              Option.fold
                ~some:(fun state -> Some (state, preds, pred_defs))
                ~none:None
                (State.assume_a ~unification:true state
                   [ Eq (Val.to_expr v_x, Val.to_expr v_le) ])
          | _                   -> None
        else
          Option.fold
            ~some:(fun v ->
              L.(
                verbose (fun m ->
                    m
                      "UNHAPPY. update_store inside produce assertions with \
                       prog variable: %s!!!\n"
                      x));
              Some (update_store astate x v))
            ~none:None (subst_in_expr subst le)
    | Pure f -> (
        let sbst_lst = Subst.to_ssubst subst in
        let sbst = SSubst.init sbst_lst in
        let f' = SSubst.substitute_formula sbst false f in
        L.(
          verbose (fun m ->
              m "About to assume %a in state %a" Formula.pp f' State.pp state));
        (* TODO: Understand why this causes a bug in Gillian-C *)
        match
          State.assume_a ~production:!Config.delay_entailment state [ f' ]
        with
        | None        -> None
        | Some state' ->
            SSubst.iter sbst (fun x le ->
                if not (Subst.mem subst x) then
                  match Val.from_expr le with
                  | Some v -> Subst.put subst x v
                  | None   ->
                      L.fail "Produce simple assertion: did not obtain a value.");
            Some (state', preds, pred_defs) )
    | _ -> L.fail "Produce simple assertion: unsupported assertion"

  let produce_asrt_list (astate : t) (subst : Subst.t) (sas : Asrt.t list) :
      t option =
    let state, preds, _ = astate in
    let _ =
      Subst.iter subst (fun v value ->
          Subst.put subst v (State.simplify_val state value))
    in
    let rec loop (loop_state : Asrt.t list * t) : t option =
      match loop_state with
      | [], astate           -> Some astate
      | a :: rest_as, astate -> (
          let ret = produce_assertion astate subst a in
          match ret with
          | Some astate' -> loop (rest_as, astate')
          | None         -> None )
    in

    let ret = loop (sas, astate) in
    match ret with
    | None -> None
    | Some (state, preds, preds_tbl) -> (
        let admissible = State.assume_a ~unification:true state [ True ] in
        match admissible with
        | None       -> None
        | Some state -> Some (state, preds, preds_tbl) )

  let produce (astate : t) (subst : Subst.t) (a : Asrt.t) : t option =
    L.(
      verbose (fun m ->
          m
            "@[-----------------@\n\
             -----------------@\n\
             Produce assertion: @[%a@]" Asrt.pp a));
    let sas = UP.collect_simple_asrts a in
    produce_asrt_list astate subst sas

  let produce_posts (state : t) (subst : Subst.t) (asrts : Asrt.t list) : t list
      =
    L.(
      verbose (fun m ->
          m
            "@[<v 2>Produce posts: There are %d postconditions to produce. And \
             here they are:@\n\
             %a@]"
            (List.length asrts)
            Fmt.(list ~sep:(any "@\n") Asrt.pp)
            asrts));
    let rets =
      List.map
        (fun a ->
          let subst = Subst.copy subst in
          (produce (copy_astate state) subst a, subst))
        asrts
    in
    let rets = List.filter (fun (x, _) -> x <> None) rets in
    let rets =
      List.map
        (fun (x, subst) ->
          let state = Option.get x in
          Subst.iter subst (fun x v ->
              if Names.is_pvar_name x then
                let _ = update_store state x v in
                ());
          state)
        rets
    in
    rets

  let use_unfold_info
      (unfold_info : (string * (string * Expr.t) list) option)
      (pred : Pred.t)
      (state : State.t)
      (subst : Subst.t) : Asrt.t list =
    match unfold_info with
    | None                    -> List.map (fun (_, x) -> x) pred.pred_definitions
    | Some (def_id, bindings) ->
        let defs =
          List.filter
            (fun (os, a) ->
              match os with
              | None              -> false
              | Some (def_id', _) -> def_id' = def_id)
            pred.pred_definitions
        in
        let defs' = List.map (fun (os, a) -> a) defs in
        let bindings =
          List.map (fun (x, e) -> (x, State.eval_expr state e)) bindings
        in
        Subst.extend subst bindings;
        L.(
          verbose (fun m ->
              m "@[<v 2>Using unfold info, obtained subst:@\n%a@\n" Subst.pp
                subst));
        defs'

  let unfold
      (astate : t)
      (pname : string)
      (args : Val.t list)
      (unfold_info : (string * (string * Expr.t) list) option) :
      (Subst.t * t) list =
    let state, preds, pred_defs = astate in
    let pred = UP.get_pred_def pred_defs pname in
    let params = List.map (fun (x, _) -> x) pred.pred.pred_params in
    L.verbose (fun m ->
        m
          "Combine going to explode. PredName: @[<h>%s@]. Params: @[<h>%a]. \
           Args: @[<h>%a@]"
          pname
          Fmt.(list ~sep:comma string)
          params
          Fmt.(list ~sep:comma Val.pp)
          args);
    let subst_i = Subst.init (List_utils.right_combine params args) in

    L.(
      verbose (fun m ->
          m "unfold with unfold_info with:@\n%a@\n" SLCmd.pp_folding_info
            unfold_info));

    let rets =
      match use_unfold_info unfold_info pred.pred state subst_i with
      | []               -> raise
                              (Failure
                                 "Cannot Unfold Predicate with No Definitions")
      | def :: rest_defs ->
          L.(
            verbose (fun m ->
                m "Going to produce %d definitions with subst@\n%a"
                  (List.length (def :: rest_defs))
                  Subst.pp subst_i));
          let results =
            List.map
              (fun def -> produce (copy_astate astate) (Subst.copy subst_i) def)
              rest_defs
          in
          let first_result = produce astate subst_i def in
          let results =
            List.filter (fun x -> x <> None) (first_result :: results)
          in
          let results = List.map Option.get results in
          let results =
            List.map (fun x -> (simplify_astate ~unification:true x, x)) results
          in
          results
    in

    L.verbose (fun m ->
        m "Results of unfolding %s(@[<h>%a@]):@\n@[%a@]" pname
          Fmt.(list ~sep:comma string)
          params
          Fmt.(
            iter_bindings ~sep:(any "@\n ") List.iteri
              (fun f' (i, (subst, astate)) ->
                Fmt.pf f' "Result %d@\nSTATE:@\n  @[%a@]@\nSUBST:@[<h>%a@]@\n" i
                  pp_astate astate Subst.pp subst))
          rets);
    rets

  let rec rec_unfold (astate : t) (pname : string) (args : Val.t list) : t =
    let saved_state = copy_astate astate in

    match unfold astate pname args None with
    | [ (_, astate) ] -> (
        let _, preds, _ = astate in
        let pred_asrts = Preds.find_pabs_by_name preds pname in
        match pred_asrts with
        | [ pred_asrt ] -> (
            match Preds.remove_by_name preds pname with
            | Some (pname, vs) -> rec_unfold astate pname vs
            | None             -> raise (Failure "DEATH. rec_unfold") )
        | _             -> astate )
    | _               -> saved_state

  let unfold_all (astate : t) (pname : string) : t =
    L.verbose (fun m -> m "Starting UNFOLD ALL@\n");
    let rec loop astate =
      let _, preds, _ = astate in
      match Preds.remove_by_name preds pname with
      | None             ->
          L.(
            verbose (fun m ->
                m "Finishing Unfond_all with state:@[%a@]@\n" pp_astate astate));
          astate
      | Some (pname, vs) ->
          let astate = rec_unfold astate pname vs in
          L.verbose (fun m ->
              m "IN UNFOLD ALL - ONE SUCCESSFUL CALL TO REC UNFOLD@\n");
          loop astate
    in
    loop astate

  let unfold_with_vals (astate : t) (vs : Val.t list) :
      (Subst.t * t) list * bool =
    L.(
      verbose (fun m ->
          m "@[<v 2>Starting unfold_with_vals: @[<h>%a@]@\n%a.@\n"
            Fmt.(list ~sep:comma Val.pp)
            vs pp_astate astate));

    if !Config.manual_proof then ([ (Subst.init [], astate) ], false)
    else
      match get_pred_with_vs astate vs with
      | Some (pname, v_args) ->
          L.(verbose (fun m -> m "FOUND STH TO UNFOLD!!!!\n"));
          let rets = unfold (copy_astate astate) pname v_args None in
          L.(
            verbose (fun m ->
                m "Unfold complete: %s(@[<h>%a@])" pname
                  Fmt.(list ~sep:comma Val.pp)
                  v_args));
          List.iteri
            (fun i (subst, astate) ->
              L.(
                verbose (fun m ->
                    m "Result of UNFOLD %d:@\n  @[%a]@\nSubst:@\n  @[%a]@\n" i
                      pp_astate astate Subst.pp subst)))
            rets;
          (rets, true)
      | None                 ->
          L.(verbose (fun m -> m "NOTHING TO UNFOLD!!!!\n"));
          ([ (Subst.init [], astate) ], false)

  let unfold_concrete_preds (astate : t) : (st option * t) option =
    let state, preds, pred_defs = astate in

    let is_unfoldable_lit lit =
      match lit with
      | Loc _ | LList _ -> false
      | _               -> true
    in

    let should_unfold (pname, vs) =
      let pred = UP.get_pred_def pred_defs pname in
      let vs_ins = Pred.in_args pred.pred vs in
      let lit_ins =
        List_utils.get_list_somes (List.map Val.to_literal vs_ins)
      in
      match List.length vs_ins = List.length lit_ins with
      | true  ->
          let flash_lits = List.map is_unfoldable_lit lit_ins in
          List.exists (fun x -> x) flash_lits
      | false -> false
    in

    let pred_to_unfold = Preds.pop preds should_unfold in
    match pred_to_unfold with
    | Some (name, vs) -> (
        let next_states = unfold astate name vs None in
        match next_states with
        | []                    -> None
        | [ (subst, astate'') ] ->
            L.(
              verbose (fun m ->
                  m "unfold_concrete_preds WORKED. Unfolded: %s(@[<h>%a])" name
                    Fmt.(list ~sep:comma Val.pp)
                    vs));
            Some (Some subst, astate'')
        | _                     ->
            raise
              (Failure
                 "Impossible: pred with concrete ins unfolded to multiple \
                  states.") )
    | None            -> Some (None, astate)

  let is_known (subst : Subst.t) (le : Expr.t) : bool =
    let s_le = Subst.subst_in_expr_opt subst le in
    not (s_le = None)

  (**
    Unification of two logical expressions

    @param subst  Substitution
    @param le     Value
    @param le_pat Logical expression

    @return If the unification is possible: set of newly found variables, list of discharges
            and updates the substitution destructively
  *)
  let rec unify_expr_core
      (state : State.t) (subst : Subst.t) (v : Val.t) (le_pat : Expr.t) :
      (SS.t * (Val.t * Expr.t) list) option =
    L.(
      tmi (fun m ->
          m "unify_expr_core: val: %a. le_pat: %a. subst: %a" Val.pp v Expr.pp
            le_pat Subst.pp subst));

    let f = unify_expr_core state subst in

    let eval_expr = State.eval_expr state in

    let f2 v1 le_pat1 v2 le_pat2 =
      Option.fold
        ~some:(fun (new_vars1, discharges1) ->
          Option.fold
            ~some:(fun (new_vars2, discharges2) ->
              Some (new_vars2, discharges1 @ discharges2))
            ~none:None (f v2 le_pat2))
        ~none:None (f v1 le_pat1)
    in

    match le_pat with
    | Lit _ ->
        let le = Val.to_expr v in
        if le = le_pat then Some (SS.empty, [])
        else Some (SS.empty, [ (v, le_pat) ])
    | PVar x | LVar x -> (
        match Subst.get subst x with
        | Some v' -> Some (SS.empty, [ (v, Val.to_expr v') ])
        | None    ->
            let v' = State.simplify_val state v in
            Subst.put subst x v';
            Some (SS.singleton x, []) )
    | ALoc x -> (
        match Subst.get subst x with
        | Some v' -> Some (SS.empty, [ (v, Val.to_expr v') ])
        | None    -> (
            let v' = State.simplify_val state v in
            let tv = State.get_type state v' in
            match tv with
            | Some ObjectType | None ->
                Subst.put subst x v';
                Some (SS.singleton x, [])
            | Some _                 -> None ) )
    | BinOp (le_pat1, FPlus, Lit (Num i)) | BinOp (Lit (Num i), FPlus, le_pat1)
      ->
        let le : Expr.t = Val.to_expr v in
        let le1 : Expr.t = BinOp (le, FMinus, Lit (Num i)) in
        let v1 : Val.t = eval_expr le1 in
        f v1 le_pat1
    | NOp (LstCat, [ x ]) -> f v x
    (* First concatted expr is an EList *)
    | NOp (LstCat, EList les :: le_pat2) ->
        let le : Expr.t = Val.to_expr v in
        let len : Expr.t = Lit (Num (float_of_int (List.length les))) in
        let le1 : Expr.t = LstSub (le, Lit (Num 0.), len) in
        let le2 : Expr.t =
          LstSub (le, len, BinOp (UnOp (LstLen, le), FMinus, len))
        in
        let v1 : Val.t = eval_expr le1 in
        let v2 : Val.t = eval_expr le2 in
        f2 v1 (EList les) v2 (NOp (LstCat, le_pat2))
    (* First concatted expr is known, but is not an EList *)
    | NOp (LstCat, le_pat1 :: le_pat2) when is_known subst le_pat1 ->
        let le : Expr.t = Val.to_expr v in
        let le1 : Expr.t = LstSub (le, Lit (Num 0.), UnOp (LstLen, le_pat1)) in
        let le2 : Expr.t =
          LstSub
            ( le,
              UnOp (LstLen, le_pat1),
              BinOp (UnOp (LstLen, le), FMinus, UnOp (LstLen, le_pat1)) )
        in
        let v1 : Val.t = eval_expr le1 in
        let v2 : Val.t = eval_expr le2 in
        f2 v1 le_pat1 v2 (NOp (LstCat, le_pat2))
    (* TODO: This one needs to be generalised, but not yet
       | BinOp (le_pat1, LstCat, EList le_pat2)
           when ((List.length le_pat2) > 0) && ((list_uvs (le_pat1 :: le_pat2)) <> SS.empty) ->
           (match List.rev le_pat2 with
             | le_pat2_last :: le_pat2_prefix ->
               let le  : Expr.t = Val.to_expr v in
               let le1 : Expr.t = UnOp (Car, UnOp (LstRev, le)) in
               let le2 : Expr.t = UnOp (Cdr, UnOp (LstRev, le)) in
               let v1  : Val.t  = eval_expr le1 in
               let v2  : Val.t  = eval_expr le2 in
               f2 v1 le_pat2_last v2 (BinOp (EList le_pat2_prefix, LstCat, UnOp (LstRev, le_pat1)))
             | _ -> raise (Failure "DEATH. unify_expr_core")) *)

    (* TODO: These two should not be relevant
       | BinOp (EList [], LstCat, le_pat2) -> f v le_pat2
       | BinOp (le_pat1, LstCat, EList []) -> f v le_pat1 *)
    | UnOp (LstRev, le_pat) ->
        let le : Expr.t = Val.to_expr v in
        let le' : Expr.t = UnOp (LstRev, le) in
        let v1 : Val.t = eval_expr le' in
        f v1 le_pat
    | EList (le_pat_hd :: le_pat_tl) ->
        let le : Expr.t = Val.to_expr v in
        let le1 : Expr.t = UnOp (Car, le) in
        let le2 : Expr.t = UnOp (Cdr, le) in
        let v1 : Val.t = eval_expr le1 in
        let v2 : Val.t = eval_expr le2 in
        f2 v1 le_pat_hd v2 (EList le_pat_tl)
    | _ -> Some (SS.empty, [ (v, le_pat) ])

  (* I don't know how much recovery information we can give when the unification of lexprs fails *)
  let unify_lexpr (state : State.t) (subst : Subst.t) (v : Val.t) (le : Expr.t)
      : (unit, (Expr.t * Expr.t) option) result =
    let le = Reduction.reduce_lexpr le in
    L.verbose (fun m -> m "Unify lexpr with v: %a, le: %a" Val.pp v Expr.pp le);

    try
      let ret = unify_expr_core state subst v le in
      L.(
        verbose (fun m ->
            m "Unify lexpr: Entering stage 1 with %a"
              Fmt.(
                option ~none:(any "None") (fun f (vars, les) ->
                    pf f "@[<h>Some { %a }, [ %a ]@]"
                      (iter ~sep:comma SS.iter string)
                      vars
                      (list ~sep:comma (fun f' (a, b) ->
                           pf f' "(%a, %a)" Val.pp a Expr.pp b))
                      les))
              ret));

      match ret with
      (* Error and we have no way of fixing *)
      | None -> Error None
      | Some (_, discharges) ->
          let eqs : (Expr.t * Expr.t) list =
            List.map
              (fun (v1, e2) ->
                (* TODO: experimental *)
                (* let e2 = Subst.subst_in_expr subst true e2 in *)
                L.(
                  verbose (fun m ->
                      m "Unify lexpr: Passed to stage 2: %a %a" Val.pp v1
                        Expr.pp e2));
                let v1' = State.simplify_val state v1 in
                (Val.to_expr v1', e2))
              discharges
          in
          (* Fold over here *)
          let result =
            List.fold_left
              (fun res (e1, e2) ->
                match res with
                | Error x -> Error x
                | Ok ()   -> (
                    match State.assert_a state [ Eq (e1, e2) ] with
                    | true  -> Ok ()
                    | false -> Error (Some (e1, e2)) ))
              (Ok ()) eqs
          in
          result
    with Reduction.ReductionException _ -> Error None

  let unify_lexprs
      (state : State.t) (subst : Subst.t) (lst : (Val.t * Expr.t) list) :
      (unit, (Expr.t * Expr.t) option) result =
    List.fold_left
      (fun res (v, le) ->
        match res with
        | Error x -> Error x
        | Ok ()   -> unify_lexpr state subst v le)
      (Ok ()) lst

  let complete_subst (subst : Subst.t) (lab : (string * SS.t) option) : bool =
    match lab with
    | None                   -> true
    | Some (_, existentials) ->
        List.fold_left
          (fun ac x ->
            if not (Subst.mem subst x) then (
              let new_lvar = Expr.LVar x in
              let v_x = Val.from_expr new_lvar in
              match v_x with
              | None     -> false
              | Some v_x ->
                  Subst.put subst x v_x;
                  true )
            else ac)
          true (SS.elements existentials)

  let rec get_pred
      ?(in_unification : bool option)
      (astate : t)
      (pname : string)
      (vs_ins : vt list) : gp_ret =
    let merge_gp_results (rets : gp_ret list) : gp_ret =
      let ret_succs, ret_fails =
        List.partition
          (fun ret ->
            match ret with
            | GPSucc _ -> true
            | _        -> false)
          rets
      in
      if ret_fails <> [] then
        let errs =
          List.map
            (fun ret ->
              match ret with
              | GPFail errs -> errs
              | _           -> [])
            ret_fails
        in
        GPFail (List.concat errs)
      else
        let rets =
          List.map
            (fun ret ->
              match ret with
              | GPSucc rets -> rets
              | _           -> [])
            ret_succs
        in
        GPSucc (List.concat rets)
    in

    L.(
      tmi (fun m ->
          m "get_pred %s. args: @[<h>%a@]" pname
            Fmt.(list ~sep:comma Val.pp)
            vs_ins));

    let state, preds, pred_defs = astate in
    let pred = UP.get_pred_def pred_defs pname in
    let pred_def = pred.pred in
    let pred_pure = pred.pure in
    match
      Preds.get_pred pred_pure preds pname vs_ins pred_def.pred_ins
        (State.equals state)
    with
    | Some (_, vs) ->
        L.(
          verbose (fun m ->
              m "Returning the following vs: @[<h>%a@]"
                Fmt.(list ~sep:comma Val.pp)
                vs));
        GPSucc [ (astate, Pred.out_args pred_def vs) ]
    | _ when not !Config.manual_proof -> (
        (* Recursive Case - Folding required *)
        L.verbose (fun m -> m "Recursive case - attempting to fold.");
        let up = pred.up in
        let param_ins = Pred.in_params pred.pred in
        let subst = Subst.init (List.combine param_ins vs_ins) in
        match unify ?in_unification astate subst up with
        | UPUSucc rets ->
            let rets =
              List.map
                (fun (astate', subst', _) ->
                  L.verbose (fun m -> m "Recursive fold success.");
                  let out_params = Pred.out_params pred_def in
                  let vs_outs = List.map (Subst.get subst') out_params in
                  L.(
                    verbose (fun m ->
                        m "Out parameters : @[<h>%a@]"
                          Fmt.(
                            list ~sep:comma (option ~none:(any "None") Val.pp))
                          vs_outs));
                  let failure = List.exists (fun x -> x = None) vs_outs in
                  if failure then GPFail [ EAsrt (vs_ins, True, []) ]
                  else
                    let vs_outs = List.map Option.get vs_outs in
                    GPSucc [ (astate', vs_outs) ])
                rets
            in
            merge_gp_results rets
        | UPUFail errs -> GPFail errs )
    | _ -> GPFail [ StateErr.EPure False ]

  and unify_assertion (astate : t) (subst : Subst.t) (p : Asrt.t) : u_res =
    let state, preds, pred_defs = astate in
    let subst_in_expr_opt = subst_in_expr_opt astate subst in

    let make_resource_fail () = UFail [ EAsrt ([], True, []) ] in

    L.verbose (fun m ->
        m
          "Unify assertion: @[<h>%a@]@\n\
           @[<v 2>Subst:@\n\
           %a@]@\n\
           @[<v 2>STATE:@\n\
           %a@]"
          Asrt.pp p Subst.pp subst pp_astate astate);

    match (p : Asrt.t) with
    | GA (a_id, e_ins, e_outs) -> (
        let getter = State.ga_to_getter a_id in
        let vs_ins = List.map subst_in_expr_opt e_ins in
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
          | ASucc [ (state', vs') ] -> (
              L.(
                verbose (fun m ->
                    m "@[<v 2>Got state:@\n%a@] and values @[<h>%a@]" State.pp
                      state'
                      Fmt.(list ~sep:comma Val.pp)
                      vs'));
              let vs_ins', vs_outs =
                List_utils.divide_list_by_index vs' (List.length vs_ins)
              in
              let remover = State.ga_to_deleter a_id in
              match State.execute_action remover state' vs_ins' with
              | ASucc [ (state'', _) ] -> (
                  match
                    unify_lexprs state' subst (List.combine vs_outs e_outs)
                  with
                  | Ok ()                 -> USucc (state'', preds, pred_defs)
                  | Error (Some (e1, e2)) ->
                      UFail
                        [
                          EAsrt
                            ([], Not (Eq (e1, e2)), [ [ Pure (Eq (e1, e2)) ] ]);
                        ]
                  | Error None            -> make_resource_fail () )
              | ASucc _                ->
                  raise
                    (Exceptions.Unsupported
                       "unify_assertion: action getter returns multiple results")
              | AFail errs             -> UFail errs )
          | ASucc _                 ->
              raise
                (Exceptions.Unsupported
                   "unify_assertion: action getter returns multiple results")
          | AFail errs              -> UFail errs )
    | Pure (Formula.And (f1, f2)) -> (
        match unify_assertion astate subst (Asrt.Pure f1) with
        | USucc state -> unify_assertion astate subst (Asrt.Pure f2)
        | res         -> res )
    | Pure (Eq (le1, le2)) when UP.outs_expr le1 <> SS.empty ->
        L.verbose (fun fmt -> fmt "Pure equality with non-empty outs");
        let v2 = subst_in_expr_opt le2 in
        Option.fold
          ~some:(fun v2 ->
            match unify_lexpr state subst v2 le1 with
            | Ok ()                 -> USucc astate
            | Error (Some (e1, e2)) ->
                UFail
                  [
                    EAsrt ([ v2 ], Not (Eq (e1, e2)), [ [ Pure (Eq (e1, e2)) ] ]);
                  ]
            | Error None            -> make_resource_fail ())
          ~none:(make_resource_fail ()) v2
    | Types les ->
        let corrections =
          List.fold_left
            (fun (ac : Formula.t list) (le, t) ->
              let v_le = subst_in_expr_opt le in
              let v_le : vt =
                match v_le with
                | Some v_le -> v_le
                | None      -> raise (Failure "DEATH. unify assertion Types")
              in
              match State.get_type state v_le with
              | Some t' -> if t <> t' then False :: ac else ac
              | None    -> Eq (UnOp (TypeOf, Val.to_expr v_le), Lit (Type t))
                           :: ac)
            [] les
        in

        if corrections = [] then USucc astate
        else
          let les, _ = List.split les in
          let les = List.map subst_in_expr_opt les in
          UFail
            [
              EAsrt
                ( List.map Option.get (List.filter (fun x -> x <> None) les),
                  True,
                  [ [ Pure (Formula.conjunct corrections) ] ] );
            ]
    | Pred (pname, les) -> (
        let pred = UP.get_pred_def pred_defs pname in
        let pred_def = pred.pred in
        let les_ins = Pred.in_args pred_def les in
        let vs_ins = List.map subst_in_expr_opt les_ins in
        let failure = List.exists (fun x -> x = None) vs_ins in

        L.verbose (fun m -> m "Unifying predicate assertion");
        if failure then make_resource_fail ()
        else
          let vs_ins = List.map Option.get vs_ins in
          match get_pred ~in_unification:true astate pname vs_ins with
          | GPSucc [] ->
              L.verbose (fun m -> m "SUCCEEDED WITH NOTHING! MEDOOOOOO!!!!!");
              UWTF
          | GPSucc [ (astate', vs_outs) ] -> (
              let les_outs = Pred.out_args pred_def les in
              L.(
                verbose (fun m ->
                    m
                      "learned the outs of a predicate. going to unify \
                       (@[<h>%a@]) against (@[<h>%a@])!!!@\n"
                      Fmt.(list ~sep:comma Val.pp)
                      vs_outs
                      Fmt.(list ~sep:comma Expr.pp)
                      les_outs));
              let state', _, _ = astate' in
              match
                unify_lexprs state' subst (List.combine vs_outs les_outs)
              with
              | Ok ()                 -> USucc astate'
              | Error (Some (e1, e2)) ->
                  UFail
                    [
                      EAsrt ([], Not (Eq (e1, e2)), [ [ Pure (Eq (e1, e2)) ] ]);
                    ]
              | Error None            -> make_resource_fail () )
          | GPSucc _ ->
              raise (Failure "DEATH. BRANCHING GETPRED INSIDE UNIFICATION.")
          | GPFail errs -> make_resource_fail () )
    | Pure f ->
        L.verbose (fun fmt -> fmt "Pure formula");
        let sbst_lst = Subst.to_ssubst subst in
        let sbst = SSubst.init sbst_lst in
        let f' = SSubst.substitute_formula sbst false f in
        if State.assert_a state [ f' ] then USucc astate
        else
          let vs = State.unfolding_vals state [ f' ] in
          UFail [ EAsrt (vs, Not f, [ [ Pure f' ] ]) ]
    (* LTrue, LFalse, LEmp, LStar*)
    | _ -> raise (Failure "Illegal Assertion in Unification Plan")

  and unify_up (s_states : search_state) : up_u_res =
    let s_states, errs_so_far = s_states in
    L.(
      verbose (fun m ->
          m "Unify UP: There are %d states left to consider."
            (List.length s_states)));
    let f = unify_up in
    match s_states with
    | []                         -> UPUFail errs_so_far
    | (state, subst, up) :: rest -> (
        let cur_asrt : Asrt.t option = UP.head up in
        let ret =
          Option.fold
            ~some:(unify_assertion state subst)
            ~none:(USucc state) cur_asrt
        in
        match ret with
        | UWTF         -> UPUSucc []
        | USucc state' -> (
            match UP.next up with
            | None                     -> UPUSucc
                                            [ (state', subst, UP.posts up) ]
            | Some [ (up, lab) ]       ->
                if complete_subst subst lab then
                  f ((state', subst, up) :: rest, errs_so_far)
                else f (rest, errs_so_far)
            | Some ((up, lab) :: ups') ->
                let next_states =
                  List.map
                    (fun (up, lab) ->
                      let new_subst = Subst.copy subst in
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
                f (next_states @ rest, errs_so_far)
            | Some []                  -> L.fail
                                            "ERROR: unify_up: empty \
                                             unification plan" )
        | UFail errs   ->
            L.(
              verbose (fun m ->
                  m
                    "@[<v 2>WARNING: Unify Assertion Failed: @[<h>%a@] with \
                     errors:@\n\
                     %a@]"
                    Fmt.(
                      option ~none:(any "no assertion - phantom node") Asrt.pp)
                    cur_asrt
                    Fmt.(list ~sep:(any "@\n") State.pp_err)
                    errs));
            f (rest, errs @ errs_so_far) )

  and unify ?(in_unification = false) (astate : t) (subst : Subst.t) (up : UP.t)
      : up_u_res =
    let astate_i = copy_astate astate in
    let subst_i = Subst.copy subst in

    let merge_upu_res (rets : up_u_res list) : up_u_res =
      let ret_succs, ret_fails =
        List.partition
          (fun ret ->
            match ret with
            | UPUSucc _ -> true
            | _         -> false)
          rets
      in
      if ret_fails <> [] then
        let errs =
          List.map
            (fun ret ->
              match ret with
              | UPUFail errs -> errs
              | _            -> [])
            ret_fails
        in
        UPUFail (List.concat errs)
      else
        let rets =
          List.map
            (fun ret ->
              match ret with
              | UPUSucc rets -> rets
              | _            -> [])
            ret_succs
        in
        UPUSucc (List.concat rets)
    in

    let ret = unify_up ([ (astate, subst, up) ], []) in
    match ret with
    | UPUSucc _ -> ret
    | UPUFail errs
      when !Config.unfolding && State.can_fix errs && not in_unification ->
        let vals = State.get_recovery_vals errs in
        L.(
          verbose (fun m ->
              m
                "Unify. Unable to unify. Checking if there are predicates to \
                 unfold. Looking for: @[<h>%a@]"
                Fmt.(list ~sep:comma Val.pp)
                vals));
        let sp, worked = unfold_with_vals astate_i vals in
        if not worked then (
          L.normal (fun m -> m "Unify. No predicates found to unfold.");
          UPUFail errs )
        else (
          L.verbose (fun m -> m "Unfolding successful.");
          let rets =
            List.map
              (fun (subst, astate) ->
                match unfold_concrete_preds astate with
                | None             -> UPUSucc []
                | Some (_, astate) ->
                    (* let subst'' = compose_substs (Subst.to_list subst_i) subst (Subst.init []) in *)
                    let subst'' = Subst.copy subst_i in
                    unify_up ([ (astate, subst'', up) ], []))
              sp
          in
          merge_upu_res rets )
    | UPUFail errs -> ret
end
