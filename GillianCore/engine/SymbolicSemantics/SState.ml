open Literal
open Containers
open Names
open Generators
module L = Logging
module SSubst = SVal.SESubst

module Make (SMemory : SMemory.S) :
  State.S
    with type st = SVal.SESubst.t
     and type vt = SVal.M.t
     and type store_t = SStore.t = struct
  type vt = SVal.M.t

  type st = SVal.M.et

  type heap_t = SMemory.t

  type store_t = SStore.t

  type m_err_t = SMemory.err_t

  type t = heap_t * store_t * PFS.t * TypEnv.t * SS.t

  type m_fix_t = SMemory.c_fix_t

  type fix_t =
    | MFix   of SMemory.c_fix_t
    | FPure  of Formula.t
    | FSVars of SS.t
    | FAsrt  of Asrt.t

  type err_t = (m_err_t, vt) StateErr.err_t

  type action_ret = ASucc of (t * vt list) list | AFail of err_t list

  type u_res = UWTF | USucc of t | UFail of err_t list

  exception Internal_State_Error of err_t list * t

  let lift_merrs (errs : m_err_t list) : err_t list =
    List.map (fun x -> StateErr.EMem x) errs

  let pp fmt state =
    let heap, store, pfs, gamma, svars = state in
    let pp_heap fmt heap =
      if !Config.no_heap then Fmt.string fmt "NO HEAP PRINTED"
      else SMemory.pp fmt heap
    in
    Fmt.pf fmt
      "@[<h>SPEC VARS: %a@]@\n\
       @[<v 2>HEAP:@\n\
       %a@]@\n\
       @[<v 2>STORE:@\n\
       %a@]@\n\
       @\n\
       @[<v 2>PURE FORMULAE:@\n\
       %a@]@\n\
       @\n\
       @[<v 2>TYPING ENVIRONMENT:@\n\
       %a@]"
      (Fmt.iter ~sep:Fmt.comma SS.iter Fmt.string)
      svars pp_heap heap SStore.pp store PFS.pp pfs TypEnv.pp gamma

  let init (pred_defs : UP.preds_tbl_t option) =
    (SMemory.init (), SStore.init [], PFS.init (), TypEnv.init (), SS.empty)

  let struct_init
      (pred_defs : UP.preds_tbl_t option)
      (store : SStore.t)
      (pfs : PFS.t)
      (gamma : TypEnv.t)
      (svars : SS.t) : t =
    (SMemory.init (), store, pfs, gamma, svars)

  let execute_action (action : string) (state : t) (args : vt list) : action_ret
      =
    let heap, store, pfs, gamma, vars = state in
    match SMemory.execute_action action heap pfs gamma args with
    | SMemory.ASucc ret_succs ->
        let result =
          ASucc
            (List.map
               (fun (new_heap, v, new_fofs, new_types) ->
                 let new_store = SStore.copy store in
                 let new_pfs = PFS.copy pfs in
                 let new_gamma = TypEnv.copy gamma in
                 List.iter (fun (x, t) -> TypEnv.update new_gamma x t) new_types;
                 List.iter (fun fof -> PFS.extend new_pfs fof) new_fofs;
                 ((new_heap, new_store, new_pfs, new_gamma, vars), v))
               ret_succs)
        in
        result
    | SMemory.AFail errs      -> AFail (lift_merrs errs)

  let ga_to_setter (a_id : string) = SMemory.ga_to_setter a_id

  let ga_to_getter (a_id : string) = SMemory.ga_to_getter a_id

  let ga_to_deleter (a_id : string) = SMemory.ga_to_deleter a_id

  let get_pred_defs (state : t) : UP.preds_tbl_t option = None

  let is_overlapping_asrt (a : string) : bool = SMemory.is_overlapping_asrt a

  let eval_expr (state : t) (e : Expr.t) : vt =
    let _, store, pfs, gamma, _ = state in
    let rec symb_evaluate_expr ?(no_reduce = false) (expr : Expr.t) : Expr.t =
      let f = symb_evaluate_expr ~no_reduce:true in
      let result : Expr.t =
        match expr with
        | PVar x -> (
            match SStore.get store x with
            | Some v -> v
            | None   -> raise (Internal_State_Error ([ EVar x ], state)) )
        | BinOp (e1, op, e2) -> BinOp (f e1, op, f e2)
        (* Unary operators *)
        | UnOp (op, e) -> UnOp (op, f e)
        (* Lists, sets, n-ary operators *)
        | EList es -> EList (List.map f es)
        | ESet es -> ESet (List.map f es)
        | NOp (op, es) -> NOp (op, List.map f es)
        | LstSub (e1, e2, e3) -> LstSub (f e1, f e2, f e3)
        | _ -> expr
      in
      (* Perform reduction *)
      if no_reduce then result
      else Reduction.reduce_lexpr ~gamma ~reduce_lvars:true ~pfs result
    in
    symb_evaluate_expr e

  let get_store (state : t) : store_t =
    let _, store, _, _, _ = state in
    store

  let set_store (state : t) (store : store_t) : t =
    let heap, _, pfs, gamma, svars = state in
    (heap, store, pfs, gamma, svars)

  let assume ?(unfold = false) (state : t) (v : Expr.t) : t list =
    L.verbose (fun fmt -> fmt "Assuming expression: %a" Expr.pp v);
    let _, _, pfs, gamma, _ = state in
    let result =
      if v = Lit (Bool true) then [ state ]
      else if v = Lit (Bool false) then []
      else
        (* let t = time() in *)
        let v_asrt =
          match
            Formula.lift_logic_expr (Reduction.reduce_lexpr ~pfs ~gamma v)
          with
          | Some (v_asrt, _) -> v_asrt
          | _                -> False
        in
        if v_asrt = False then []
        else (
          PFS.extend pfs v_asrt;
          L.verbose (fun fmt -> fmt "Assumed state: %a" pp state);
          [ state ] )
    in
    result

  let assume_a
      ?(unification = false)
      ?(production = false)
      (state : t)
      (ps : Formula.t list) : t option =
    let _, _, pfs, gamma, _ = state in
    let ps = List.map (Reduction.reduce_formula ~pfs ~gamma) ps in
    let result =
      if
        production
        || FOSolver.check_satisfiability ~unification
             (ps @ PFS.to_list pfs)
             gamma
      then (
        List.iter (PFS.extend pfs) ps;
        Some state )
      else None
    in
    result

  let assume_t (state : t) (v : vt) (t : Type.t) : t option =
    let _, _, pfs, gamma, _ = state in
    match Typing.reverse_type_lexpr true gamma [ (v, t) ] with
    | None        -> None
    | Some gamma' ->
        TypEnv.extend gamma gamma';
        Some state

  let sat_check (state : t) (v : Expr.t) : bool =
    (* let t = time() in *)
    L.verbose (fun m -> m "SState: sat_check: %a" Expr.pp v);
    let _, _, pfs, gamma, _ = state in
    let v = Reduction.reduce_lexpr ~pfs ~gamma v in
    if v = Lit (Bool true) then true
    else if v = Lit (Bool false) then false
    else
      let v_asrt =
        match Formula.lift_logic_expr v with
        | Some (v_asrt, _) -> v_asrt
        | _                -> False
      in
      L.verbose (fun m -> m "SState: lifted assertion: %a" Formula.pp v_asrt);
      let result =
        FOSolver.check_satisfiability (v_asrt :: PFS.to_list pfs) gamma
      in
      L.(verbose (fun m -> m "SState: sat_check done: %b" result));
      (* update_statistics "SAT Check" (time() -. t); *)
      result

  let sat_check_f (state : t) (fs : Formula.t list) : st option =
    let _, store, pfs, gamma, _ = state in
    FOSolver.check_satisfiability_with_model (fs @ PFS.to_list pfs) gamma

  let assert_a (state : t) (ps : Formula.t list) : bool =
    let _, _, pfs, gamma, _ = state in
    FOSolver.check_entailment SS.empty (PFS.to_list pfs) ps gamma

  let equals (state : t) (le1 : vt) (le2 : vt) : bool =
    let _, _, pfs, gamma, _ = state in
    let result = FOSolver.is_equal ~pfs ~gamma le1 le2 in
    result

  let get_type (state : t) (le : vt) : Type.t option =
    let _, _, pfs, gamma, _ = state in
    let le = Reduction.reduce_lexpr ?gamma:(Some gamma) ?pfs:(Some pfs) le in
    let t, _, _ = Typing.type_lexpr gamma le in
    t

  let simplify
      ?(save = false)
      ?(kill_new_lvars = true)
      ?(unification = false)
      (state : t) : st =
    (* let start_time = time() in  *)
    let heap, store, pfs, gamma, svars = state in
    let save_spec_vars = if save then (SS.empty, true) else (svars, false) in
    L.verbose (fun m ->
        m
          "-----------------------------------\n\
           STATE BEFORE SIMPLIFICATIONS:\n\
           %a\n\
           -----------------------------------"
          pp state);
    let subst, _ =
      Simplifications.simplify_pfs_and_gamma ~kill_new_lvars pfs gamma
        ~unification ~save_spec_vars
    in
    let subst =
      SSubst.filter subst (fun x _ ->
          match x with
          | LVar x | PVar x | ALoc x -> not (SS.mem x svars)
          | _                        -> true)
    in
    SMemory.substitution_in_place subst heap;
    SStore.substitution_in_place subst store;
    if not kill_new_lvars then Typing.naively_infer_type_information pfs gamma;
    L.verbose (fun m ->
        m
          "-----------------------------------\n\
           STATE AFTER SIMPLIFICATIONS:@\n\
           @[%a@]@\n\
           @\n\
           @[<v 2>with substitution:@\n\
           @[%a@]@\n\
           -----------------------------------"
          pp state SSubst.pp subst);
    (* update_statistics) "Simplify" (time() -. start_time); *)
    subst

  let simplify_val (state : t) (v : vt) : vt =
    let _, _, pfs, gamma, _ = state in
    Reduction.reduce_lexpr ~gamma ~pfs v

  let to_loc (state : t) (loc : vt) : (t * vt) option =
    let _, _, pfs, gamma, _ = state in
    let loc = Reduction.reduce_lexpr ~gamma ~pfs loc in
    match loc with
    | Lit (Loc loc_name) | ALoc loc_name -> Some (state, loc)
    | LVar x -> (
        match Reduction.resolve_expr_to_location pfs gamma (LVar x) with
        | Some loc_name ->
            if is_aloc_name loc_name then Some (state, ALoc loc_name)
            else Some (state, Lit (Loc loc_name))
        | None          ->
            let new_aloc = ALoc.alloc () in
            let p : Formula.t = Eq (LVar x, ALoc new_aloc) in
            if FOSolver.check_satisfiability (p :: PFS.to_list pfs) gamma then (
              PFS.extend pfs p;
              Some (state, Expr.ALoc new_aloc) )
            else None )
    | _ -> None

  let copy (state : t) : t =
    let heap, store, pfs, gamma, svars = state in
    let result =
      ( SMemory.copy heap,
        SStore.copy store,
        PFS.copy pfs,
        TypEnv.copy gamma,
        svars )
    in
    result

  let add_spec_vars (state : t) (xs : Var.Set.t) : t =
    let heap, store, pfs, gamma, svars = state in
    (heap, store, pfs, gamma, SS.union xs svars)

  let get_spec_vars (state : t) : SS.t =
    let _, _, _, _, svars = state in
    svars

  let get_lvars (state : t) : Var.Set.t =
    let heap, store, pfs, gamma, svars = state in
    List.fold_left SS.union SS.empty
      [
        SMemory.lvars heap;
        SStore.lvars store;
        PFS.lvars pfs;
        TypEnv.lvars gamma;
        svars;
      ]

  let to_assertions ?(to_keep : SS.t option) (state : t) : Asrt.t list =
    let heap, store, pfs, gamma, _ = state in
    let store' =
      Option.fold
        ~some:(fun store_dom -> SStore.projection store (SS.elements store_dom))
        ~none:store to_keep
    in
    let asrts_pfs =
      List.sort Asrt.compare (List.map (fun f -> Asrt.Pure f) (PFS.to_list pfs))
    in
    let asrts_store =
      List.sort Asrt.compare
        (List.map (fun f -> Asrt.Pure f) (SStore.assertions store'))
    in
    if TypEnv.empty gamma then asrts_store @ SMemory.assertions heap @ asrts_pfs
    else
      asrts_store @ SMemory.assertions heap @ asrts_pfs
      @ [ Types (TypEnv.to_list_expr gamma) ]

  let evaluate_slcmd
      ?(revisited_invariant = false)
      (prog : UP.prog)
      (slcmd : SLCmd.t)
      (state : t) : t list =
    raise (Failure "ERROR: evaluate_slcmd called for non-abstract execution")

  let run_spec
      (spec : UP.spec)
      (state : t)
      (x : string)
      (args : vt list)
      (subst : (string * (string * vt) list) option) : (t * Flag.t) list =
    raise (Failure "ERROR: run_spec called for non-abstract execution")

  let unfolding_vals (state : t) (fs : Formula.t list) : vt list =
    let lvars =
      SS.of_list
        (List.concat (List.map (fun f -> SS.elements (Formula.lvars f)) fs))
    in
    let alocs =
      SS.of_list
        (List.concat (List.map (fun f -> SS.elements (Formula.alocs f)) fs))
    in
    let clocs =
      SS.of_list
        (List.concat (List.map (fun f -> SS.elements (Formula.clocs f)) fs))
    in
    let lvars = List.map (fun x -> Expr.LVar x) (SS.elements lvars) in
    let alocs = List.map (fun x -> Expr.ALoc x) (SS.elements alocs) in
    let clocs = List.map (fun x -> Expr.Lit (Loc x)) (SS.elements clocs) in
    clocs @ alocs @ lvars

  let substitution_in_place (subst : st) (state : t) : unit =
    let heap, store, pfs, gamma, svars = state in
    SMemory.substitution_in_place subst heap;
    SStore.substitution_in_place subst store;
    PFS.substitution subst pfs;
    Typing.substitution_in_place subst gamma

  let unify_assertion (state : t) (subst : st) (step : UP.step) : u_res =
    raise (Failure "Unify assertion from non-abstract symbolic state.")

  let produce_posts (state : t) (subst : st) (asrts : Asrt.t list) : t list =
    raise (Failure "produce_posts from non-abstract symbolic state.")

  let produce (state : t) (subst : st) (asrt : Asrt.t) : t option =
    raise (Failure "produce_post from non-abstract symbolic state.")

  let fresh_val (state : t) : vt = LVar (LVar.alloc ())

  let clean_up (state : t) : unit =
    let heap, _, _, _, _ = state in
    SMemory.clean_up heap

  let update_subst (state : t) (subst : st) : unit =
    let _, _, pfs, gamma, _ = state in
    let new_bindings =
      SSubst.fold subst
        (fun x e ac ->
          match e with
          | LVar y -> (
              match TypEnv.get gamma y with
              | Some ObjectType -> (
                  match
                    Reduction.resolve_expr_to_location pfs gamma (LVar y)
                  with
                  | Some loc_name ->
                      if is_aloc_name loc_name then
                        (x, Expr.ALoc loc_name) :: ac
                      else ac
                  | _             -> ac )
              | _               -> ac )
          | _      -> ac)
        []
    in
    List.iter (fun (x, e) -> SSubst.put subst x e) new_bindings

  (* Auxiliary Functions *)
  let get_loc_name (loc : Expr.t) state : string option =
    L.(tmi (fun m -> m "get_loc_name: %s" ((Fmt.to_to_string Expr.pp) loc)));
    let _, _, pfs, gamma, _ = state in
    match loc with
    | Lit (Loc loc) | ALoc loc -> Some loc
    | LVar x                   -> Reduction.resolve_expr_to_location pfs gamma
                                    (LVar x)
    | _                        ->
        L.verbose (fun m -> m "Unsupported location MAKESState: %a" Expr.pp loc);
        raise
          (Internal_State_Error ([ EType (loc, None, Type.ObjectType) ], state))

  let fresh_loc ?(loc : vt option) (state : t) : vt =
    match loc with
    | Some loc -> (
        let loc_name = get_loc_name loc state in
        match loc_name with
        | Some loc_name ->
            if is_aloc_name loc_name then Expr.ALoc loc_name
            else Expr.Lit (Loc loc_name)
        | None          -> ALoc (ALoc.alloc ()) )
    | None     -> ALoc (ALoc.alloc ())

  let mem_constraints (state : t) : Formula.t list =
    let heap, _, _, _, _ = state in
    SMemory.mem_constraints heap

  let split_ins (action : string) (ins : vt list) : vt list * vt list =
    let l_indexes = SMemory.ga_loc_indexes action in
    let ins' : (vt * int) list = List.mapi (fun i x -> (x, i)) ins in
    let l_ins, o_ins =
      List.partition (fun (_, i) -> List.mem i l_indexes) ins'
    in
    let l_ins = List.map (fun (v, _) -> v) l_ins in
    let o_ins = List.map (fun (v, _) -> v) o_ins in
    (l_ins, o_ins)

  let merge_ins (action : string) (l_ins : vt list) (o_ins : vt list) : vt list
      =
    let l_indexes = SMemory.ga_loc_indexes action in

    let rec loop
        (cur_index : int)
        (l_ins : vt list)
        (o_ins : vt list)
        (l_indexes : int list)
        (lst : vt list) : vt list =
      match (l_ins, o_ins, l_indexes) with
      | [], [], [] -> lst
      | lv :: l_ins', o_ins, l_index :: l_indexes' when cur_index = l_index ->
          loop (cur_index + 1) l_ins' o_ins l_indexes' (lv :: lst)
      | l_ins, ov :: o_ins', l_index :: _ when cur_index <> l_index ->
          loop (cur_index + 1) l_ins o_ins' l_indexes (ov :: lst)
      | [], ov :: o_ins', [] -> loop (cur_index + 1) [] o_ins' [] (ov :: lst)
      | lst1, lst2, lst3 ->
          let comma = Fmt.any ", " in
          let msg =
            Fmt.str
              "DEATH inside merge_ins with:@\n\
               l_ins: %a\n\
               o_ins:%a@\n\
               l_indexes:%a@\n"
              (Fmt.list ~sep:comma SVal.M.pp)
              lst1
              (Fmt.list ~sep:comma SVal.M.pp)
              lst2
              (Fmt.list ~sep:comma Fmt.int)
              lst3
          in
          raise (Failure msg)
    in
    let lst = loop 0 l_ins o_ins l_indexes [] in
    List.rev lst

  let pp_fix fmt = function
    | MFix mf   -> SMemory.pp_c_fix fmt mf
    | FPure f   -> Fmt.pf fmt "SFPure(%a)" Formula.pp f
    | FSVars vs ->
        Fmt.pf fmt "SFSVar(@[<h>%a@])"
          (Fmt.iter ~sep:Fmt.comma SS.iter Fmt.string)
          vs
    | FAsrt ga  -> Fmt.pf fmt "SFSVar(@[<h>%a@])" Asrt.pp ga

  let get_recovery_vals (errs : err_t list) : vt list =
    StateErr.get_recovery_vals errs SMemory.get_recovery_vals

  let automatic_unfold _ _ : (t list, string) result =
    Error "Automatic unfold not supported in symbolic execution"

  let pp_err = StateErr.pp_err SMemory.pp_err SVal.M.pp

  let can_fix = StateErr.can_fix

  let get_failing_constraint (err : err_t) : Formula.t =
    StateErr.get_failing_constraint err SMemory.get_failing_constraint

  let normalise_fix (pfs : PFS.t) (gamma : TypEnv.t) (fix : fix_t list) :
      fix_t list option =
    let fixes, pfs', svars, asrts =
      List.fold_right
        (fun fix (mfix, pfs, svars, asrts) ->
          match fix with
          | MFix mfix'    -> (mfix' :: mfix, pfs, svars, asrts)
          | FPure pf'     ->
              (mfix, (if pf' = True then pfs else pf' :: pfs), svars, asrts)
          | FSVars svars' -> (mfix, pfs, SS.union svars' svars, asrts)
          | FAsrt ga      -> (mfix, pfs, svars, ga :: asrts))
        fix ([], [], SS.empty, [])
    in
    (* Check SAT for some notion of checking SAT *)
    let mfixes = List.map (fun fix -> MFix fix) fixes in
    let asrts = List.map (fun fix -> FAsrt fix) asrts in
    let is_sat = FOSolver.check_satisfiability (PFS.to_list pfs @ pfs') gamma in
    match is_sat with
    | true  ->
        let pfixes = List.map (fun pfix -> FPure pfix) pfs' in
        Some
          ( (if svars = SS.empty then [] else [ FSVars svars ])
          @ pfixes @ mfixes @ asrts )
    | false ->
        L.verbose (fun m -> m "Warning: invalid fix.");
        None

  let get_fixes ?simple_fix:(sf = true) (state : t) (errs : err_t list) :
      fix_t list list =
    let pp_fixes fmt fixes =
      Fmt.pf fmt "[[ %a ]]" (Fmt.list ~sep:(Fmt.any ", ") pp_fix) fixes
    in
    L.verbose (fun m -> m "SState: get_fixes");
    let heap, store, pfs, gamma, svars = state in
    let one_step_fixes : fix_t list list list =
      List.map
        (fun (err : err_t) ->
          match err with
          | EMem err            ->
              List.map
                (fun (mfixes, pfixes, svars, asrts) ->
                  List.map (fun l -> MFix l) mfixes
                  @ List.map (fun pf -> FPure pf) pfixes
                  @
                  if svars == SS.empty then []
                  else
                    [ FSVars svars ] @ List.map (fun asrt -> FAsrt asrt) asrts)
                (SMemory.get_fixes ~simple_fix:sf heap pfs gamma err)
          | EPure f             ->
              let result = [ [ FPure f ] ] in
              L.verbose (fun m ->
                  m "@[<v 2>Memory: Fixes found:@\n%a@]"
                    (Fmt.list ~sep:(Fmt.any "@\n") pp_fixes)
                    result);
              result
          | EAsrt (_, _, fixes) ->
              let result =
                List.map
                  (fun (fixes : Asrt.t list) ->
                    List.map
                      (fun (fix : Asrt.t) ->
                        match fix with
                        | Pure fix -> FPure fix
                        | _        ->
                            raise
                              (Exceptions.Impossible
                                 "Non-pure fix for an assertion failure"))
                      fixes)
                  fixes
              in
              L.verbose (fun m ->
                  m "@[<v 2>Memory: Fixes found:@\n%a@]"
                    (Fmt.list ~sep:(Fmt.any "@\n") pp_fixes)
                    result);
              result
          | _                   -> raise
                                     (Failure
                                        "DEATH: get_fixes: error cannot be \
                                         fixed."))
        errs
    in
    (* Cartesian product of the fixes *)
    let product = List_utils.list_product one_step_fixes in
    let candidate_fixes = List.map (fun ll -> List.concat ll) product in
    let normalised_fixes =
      List.map (fun fix -> normalise_fix pfs gamma fix) candidate_fixes
    in
    let result = List_utils.get_list_somes normalised_fixes in
    L.(verbose (fun m -> m "Normalised fixes: %i" (List.length result)));
    L.verbose (fun m ->
        m "%a" (Fmt.list ~sep:(Fmt.any "@\n@\n") pp_fixes) result);
    result

  (**
   @param state The state on which to apply the fixes
   @param fixes A list of fixes to apply

   @return The state resulting from applying the fixes

   [apply_fixes state fixes] applies the fixes [fixes] to the state [state],
   and returns the resulting state, if successful.
   *)
  let apply_fixes (state : t) (fixes : fix_t list) : t option * Asrt.t list =
    L.verbose (fun m -> m "SState: apply_fixes");
    let heap, store, pfs, gamma, svars = state in

    let gas = ref [] in

    let apply_fix (heap : heap_t) (new_vars : SS.t) (fix : fix_t) :
        heap_t * SS.t =
      match fix with
      (* Apply fix in memory - this may change the pfs and gamma *)
      | MFix fix ->
          let heap' = SMemory.apply_fix heap pfs gamma fix in
          (heap', new_vars)
      | FPure f ->
          PFS.extend pfs f;
          (heap, new_vars)
      | FSVars vars -> (heap, SS.union new_vars vars)
      | FAsrt ga ->
          L.verbose (fun m ->
              m
                "Warning: Non-abstract states do not support assertion fixes, \
                 hoping you're actually in an abstract state");
          gas := !gas @ [ ga ];
          (heap, new_vars)
    in

    let rec apply_fixes_rec
        (heap : heap_t) (new_vars : SS.t) (fixes : fix_t list) : heap_t * SS.t =
      match fixes with
      | []          -> (heap, new_vars)
      | fix :: rest ->
          let heap', new_vars' = apply_fix heap new_vars fix in
          apply_fixes_rec heap' new_vars' rest
    in

    let heap', new_vars = apply_fixes_rec heap SS.empty fixes in
    (Some (heap, store, pfs, gamma, SS.union svars new_vars), !gas)
end
