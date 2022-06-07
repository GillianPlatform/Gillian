open Literal
open Names
module L = Logging
module SSubst = SVal.SESubst

module type S = sig
  include State.S

  val get_typ_env : t -> TypEnv.t
  val get_pfs : t -> PFS.t
  val get_lvars_for_exact : t -> Var.Set.t

  val hides :
    is_post:bool ->
    used_unifiables:Expr.Set.t ->
    exprs_to_hide:Expr.t list ->
    t ->
    (unit, Expr.t) result
end

module Make (SMemory : SMemory.S) :
  S
    with type st = SVal.SESubst.t
     and type vt = SVal.M.t
     and type store_t = SStore.t
     and type heap_t = SMemory.t
     and type m_err_t = SMemory.err_t = struct
  type vt = SVal.M.t [@@deriving yojson, show]
  type st = SVal.M.et
  type heap_t = SMemory.t [@@deriving yojson]
  type store_t = SStore.t [@@deriving yojson]
  type m_err_t = SMemory.err_t [@@deriving yojson]
  type t = heap_t * store_t * PFS.t * TypEnv.t * SS.t [@@deriving yojson]
  type variants_t = (string, Expr.t option) Hashtbl.t [@@deriving yojson]

  type fix_t =
    | MFix of SMemory.c_fix_t
    | FPure of Formula.t
    | FSVars of SS.t
    | FAsrt of Asrt.t

  type err_t = (m_err_t, vt) StateErr.err_t [@@deriving yojson]
  type action_ret = ASucc of (t * vt list) list | AFail of err_t list
  type u_res = UWTF | USucc of t | UFail of err_t list

  exception Internal_State_Error of err_t list * t

  module ES = Expr.Set

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
       @[<v 2>STORE:@\n\
       %a@]@\n\
       @\n\
       @[<v 2>MEMORY:@\n\
       %a@]@\n\
       @\n\
       @[<v 2>PURE FORMULAE:@\n\
       %a@]@\n\
       @\n\
       @[<v 2>TYPING ENVIRONMENT:@\n\
       %a@]"
      (Fmt.iter ~sep:Fmt.comma SS.iter Fmt.string)
      svars SStore.pp store pp_heap heap PFS.pp pfs TypEnv.pp gamma

  let pp_by_need pvars cmd_lvars cmd_locs fmt state =
    let memory, store, pfs, gamma, svars = state in

    let rec get_print_info (lvars : SS.t) (locs : SS.t) : SS.t * SS.t =
      (* let pp_str_list = Fmt.(brackets (list ~sep:comma string)) in
         let () =
           L.verbose (fun fmt ->
               fmt "get_print_info:@\nLVars: %a@\nLocs:%a\n" pp_str_list
                 (SS.elements lvars) pp_str_list (SS.elements locs))
         in *)
      (* Get locs from lvars... *)
      let pfs_locs =
        SS.fold
          (fun x ac ->
            match Reduction.resolve_expr_to_location pfs gamma (LVar x) with
            | Some loc -> SS.add loc ac
            | None -> ac)
          lvars SS.empty
      in
      (* ...and add them to the current locs *)
      let new_locs = SS.union locs pfs_locs in
      (* Get relevant lvars and locs from the memory... *)
      let mem_lvars, mem_locs = SMemory.get_print_info new_locs memory in
      (* ...and add them accordingly *)
      let new_lvars = SS.union lvars mem_lvars in
      let new_locs = SS.union new_locs mem_locs in
      (* Learn more from the pfs... *)
      let _, more_lvars, more_locs =
        PFS.get_relevant_info pvars new_lvars new_locs pfs
      in
      (* ...and add that accordingly *)
      let new_lvars = SS.union new_lvars more_lvars in
      let new_locs = SS.union new_locs more_locs in
      (* If nothing has been learned, stop; otherwise, retry *)
      if SS.equal lvars new_lvars && SS.equal locs new_locs then (lvars, locs)
      else get_print_info new_lvars new_locs
    in

    (* Logical variables and locations from the store *)
    let store_lvars, store_locs =
      SS.fold
        (fun pvar ac ->
          match SStore.get store pvar with
          | None -> ac
          | Some e ->
              (SS.union (fst ac) (Expr.lvars e), SS.union (snd ac) (Expr.locs e)))
        pvars (SS.empty, SS.empty)
    in
    (* LVars: commands + store *)
    let lvars = SS.union cmd_lvars store_lvars in
    let locs = SS.union cmd_locs store_locs in
    (* Locations found in the pfs *)
    let lvars, locs = get_print_info lvars locs in
    (* Filter spec vars *)
    let svars = SS.filter (fun x -> SS.mem x lvars) svars in

    (* TODO: Locations for the heap *)
    (* TODO: Logical variables for the pfs and gamma *)
    let pp_memory fmt memory =
      if !Config.no_heap then Fmt.string fmt "NO MEMORY PRINTED"
      else SMemory.pp_by_need locs fmt memory
    in
    Fmt.pf fmt
      "@[<h>SPEC VARS: %a@]@\n\
       @\n\
       @[<v 2>STORE:@\n\
       %a@]@\n\
       @\n\
       @[<v 2>MEMORY:@\n\
       %a@]@\n\
       @\n\
       @[<v 2>PURE FORMULAE:@\n\
       %a@]@\n\
       @\n\
       @[<v 2>TYPING ENVIRONMENT:@\n\
       %a@]"
      (Fmt.iter ~sep:Fmt.comma SS.iter Fmt.string)
      svars (SStore.pp_by_need pvars) store pp_memory memory
      (PFS.pp_by_need (pvars, lvars, locs))
      pfs
      (TypEnv.pp_by_need (List.fold_left SS.union SS.empty [ pvars; lvars ]))
      gamma

  let init ?(preds : UP.preds_tbl_t option) ?(variants : variants_t option) () =
    let _, _ = (preds, variants) in
    (SMemory.init (), SStore.init [], PFS.init (), TypEnv.init (), SS.empty)

  let struct_init
      ?(preds : UP.preds_tbl_t option)
      ?(variants : variants_t option)
      (store : SStore.t)
      (pfs : PFS.t)
      (gamma : TypEnv.t)
      (svars : SS.t) : t =
    let _, _ = (preds, variants) in
    (SMemory.init (), store, pfs, gamma, svars)

  let execute_action
      ?(unification = false)
      (action : string)
      (state : t)
      (args : vt list) : action_ret =
    let heap, store, pfs, gamma, vars = state in
    match SMemory.execute_action ~unification action heap pfs gamma args with
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
    | SMemory.AFail errs -> AFail (lift_merrs errs)

  let ga_to_setter (a_id : string) = SMemory.ga_to_setter a_id
  let ga_to_getter (a_id : string) = SMemory.ga_to_getter a_id
  let ga_to_deleter (a_id : string) = SMemory.ga_to_deleter a_id
  let get_pred_defs (_ : t) : UP.preds_tbl_t option = None
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
            | None -> raise (Internal_State_Error ([ EVar x ], state)))
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

  let assume ?unfold:_ (state : t) (v : Expr.t) : t list =
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
          | _ -> False
        in
        if v_asrt = False then []
        else (
          PFS.extend pfs v_asrt;
          [ state ])
    in
    result

  let assume_a
      ?(unification = false)
      ?(production = false)
      ?(time = "")
      (state : t)
      (ps : Formula.t list) : t option =
    let _, _, pfs, gamma, _ = state in
    let ps =
      List.map
        (Reduction.reduce_formula
           ~time:("SState: assume_a: " ^ time)
           ~pfs ~gamma)
        ps
    in
    let result =
      if
        production
        || FOSolver.check_satisfiability
             ~time:("SState: assume_a: " ^ time)
             ~unification
             (ps @ PFS.to_list pfs)
             gamma
      then (
        List.iter (PFS.extend pfs) ps;
        Some state)
      else (
        Logging.verbose (fun m ->
            m "assume_a: Couldn't assume %a" (Fmt.Dump.list Formula.pp) ps);
        None)
    in
    result

  let assume_t (state : t) (v : vt) (t : Type.t) : t option =
    let _, _, _, gamma, _ = state in
    match Typing.reverse_type_lexpr true gamma [ (v, t) ] with
    | None -> None
    | Some gamma' ->
        TypEnv.extend gamma gamma';
        Some state

  let sat_check (state : t) (v : Expr.t) : bool =
    L.verbose (fun m -> m "SState: sat_check: %a" Expr.pp v);
    let _, _, pfs, gamma, _ = state in
    let v = Reduction.reduce_lexpr ~pfs ~gamma v in
    if v = Lit (Bool true) then true
    else if v = Lit (Bool false) then false
    else
      let v_asrt =
        match Formula.lift_logic_expr v with
        | Some (v_asrt, _) -> v_asrt
        | _ -> False
      in
      let relevant_info = (Expr.pvars v, Expr.lvars v, Expr.locs v) in
      let result =
        FOSolver.check_satisfiability ~relevant_info
          (v_asrt :: PFS.to_list pfs)
          gamma
      in
      L.(verbose (fun m -> m "SState: sat_check done: %b" result));
      result

  let sat_check_f (state : t) (fs : Formula.t list) : st option =
    let _, _, pfs, gamma, _ = state in
    FOSolver.check_satisfiability_with_model (fs @ PFS.to_list pfs) gamma

  let assert_a (state : t) (ps : Formula.t list) : bool =
    let _, _, pfs, gamma, _ = state in
    FOSolver.check_entailment SS.empty pfs ps gamma

  let equals (state : t) (le1 : vt) (le2 : vt) : bool =
    let _, _, pfs, gamma, _ = state in
    let result = FOSolver.is_equal ~pfs ~gamma le1 le2 in
    result

  let get_type (state : t) (le : vt) : Type.t option =
    let _, _, pfs, gamma, _ = state in
    let le = Reduction.reduce_lexpr ~gamma ~pfs le in
    let t, _, _ = Typing.type_lexpr gamma le in
    t

  let simplify
      ?(save = false)
      ?(kill_new_lvars = true)
      ?(unification = false)
      (state : t) : st * t list =
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
          | _ -> true)
    in
    SSubst.iter subst (fun k v ->
        if Expr.is_unifiable v then
          match SSubst.mem subst v with
          | true -> SSubst.put subst k (Option.get (SSubst.get subst v))
          | false -> ());
    Logging.verbose (fun fmt ->
        fmt "Filtered subst, to be applied to memory:\n%a" SSubst.pp subst);
    SStore.substitution_in_place subst store;

    let memories = SMemory.substitution_in_place ~pfs ~gamma subst heap in

    let states =
      match memories with
      | [] -> failwith "Impossible: memory substitution returned []"
      | [ (mem, lpfs, lgamma) ] ->
          let () = Formula.Set.iter (PFS.extend pfs) lpfs in
          let () = List.iter (fun (t, v) -> TypEnv.update gamma t v) lgamma in
          if not kill_new_lvars then
            Typing.naively_infer_type_information pfs gamma;
          [ (mem, store, pfs, gamma, svars) ]
      | multi_mems ->
          List.map
            (fun (mem, lpfs, lgamma) ->
              let bpfs = PFS.copy pfs in
              let bgamma = TypEnv.copy gamma in
              let () = Formula.Set.iter (PFS.extend bpfs) lpfs in
              let () =
                List.iter (fun (t, v) -> TypEnv.update bgamma t v) lgamma
              in
              if not kill_new_lvars then
                Typing.naively_infer_type_information bpfs bgamma;
              (mem, SStore.copy store, bpfs, bgamma, svars))
            multi_mems
    in

    L.verbose (fun m ->
        m "Substitution results in %d results: " (List.length states));
    List.iter
      (fun state ->
        L.verbose (fun m ->
            m
              "-----------------------------------\n\
               STATE AFTER SIMPLIFICATIONS:@\n\
               @[%a@]@\n\
               @\n\
               @[<v 2>with substitution:@\n\
               @[%a@]@\n\
               -----------------------------------"
              pp state SSubst.pp subst))
      states;
    (subst, states)

  let simplify_val (state : t) (v : vt) : vt =
    let _, _, pfs, gamma, _ = state in
    Reduction.reduce_lexpr ~gamma ~pfs v

  let to_loc (state : t) (loc : vt) : (t * vt) option =
    let _, _, pfs, gamma, _ = state in
    let loc = Reduction.reduce_lexpr ~gamma ~pfs loc in
    match loc with
    | Lit (Loc _) | ALoc _ -> Some (state, loc)
    | LVar x -> (
        match Reduction.resolve_expr_to_location pfs gamma (LVar x) with
        | Some loc_name ->
            if is_aloc_name loc_name then Some (state, ALoc loc_name)
            else Some (state, Lit (Loc loc_name))
        | None ->
            let new_aloc = ALoc.alloc () in
            let p : Formula.t = Eq (LVar x, ALoc new_aloc) in
            if FOSolver.check_satisfiability (p :: PFS.to_list pfs) gamma then (
              PFS.extend pfs p;
              Some (state, Expr.ALoc new_aloc))
            else None)
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
    SMemory.lvars heap
    |> SS.union (SStore.lvars store)
    |> SS.union (PFS.lvars pfs)
    |> SS.union (TypEnv.lvars gamma)
    |> SS.union svars

  let get_lvars_for_exact (state : t) : Var.Set.t =
    let heap, store, pfs, _, _ = state in
    SStore.lvars store
    |> SS.union (SMemory.lvars heap)
    |> SS.union (PFS.lvars pfs)

  let get_alocs_for_exact (state : t) : Var.Set.t =
    let heap, store, pfs, _, _ = state in
    SStore.alocs store
    |> SS.union (SMemory.alocs heap)
    |> SS.union (PFS.alocs pfs)

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

  let evaluate_slcmd (_ : UP.prog) (_ : SLCmd.t) (_ : t) :
      (t list, string) result =
    raise (Failure "ERROR: evaluate_slcmd called for non-abstract execution")

  let unify_invariant _ _ _ _ _ =
    raise (Failure "ERROR: unify_invariant called for pure symbolic execution")

  let clear_resource (state : t) : t =
    let _, store, pfs, gamma, svars = state in
    (SMemory.init (), store, pfs, gamma, svars)

  let frame_on _ _ _ =
    raise (Failure "ERROR: framing called for symbolic execution")

  let run_spec
      (_ : UP.spec)
      (_ : t)
      (_ : string)
      (_ : vt list)
      (_ : (string * (string * vt) list) option) : (t * Flag.t) list =
    raise (Failure "ERROR: run_spec called for non-abstract execution")

  let unfolding_vals (_ : t) (fs : Formula.t list) : vt list =
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

  let substitution_in_place ?(subst_all = false) (subst : st) (state : t) :
      t list =
    let heap, store, pfs, gamma, svars = state in
    SStore.substitution_in_place ~subst_all subst store;
    PFS.substitution subst pfs;
    Typing.substitution_in_place subst gamma;
    match SMemory.substitution_in_place ~pfs ~gamma subst heap with
    | [] -> failwith "IMPOSSIBLE: SMemory always returns at least one memory"
    | [ (mem, lpfs, lgamma) ] ->
        let () = Formula.Set.iter (PFS.extend pfs) lpfs in
        let () = List.iter (fun (t, v) -> TypEnv.update gamma t v) lgamma in
        [ (mem, store, pfs, gamma, svars) ]
    | multi_mems ->
        List.map
          (fun (mem, lpfs, lgamma) ->
            let bpfs = PFS.copy pfs in
            let bgamma = TypEnv.copy gamma in
            let () = Formula.Set.iter (PFS.extend bpfs) lpfs in
            let () =
              List.iter (fun (t, v) -> TypEnv.update bgamma t v) lgamma
            in
            (mem, SStore.copy store, bpfs, bgamma, svars))
          multi_mems

  let unify_assertion (_ : t) (_ : st) (_ : UP.step) : u_res =
    raise (Failure "Unify assertion from non-abstract symbolic state.")

  let produce_posts (_ : t) (_ : st) (_ : Asrt.t list) : t list =
    raise (Failure "produce_posts from non-abstract symbolic state.")

  let produce (_ : t) (_ : st) (_ : Asrt.t) : (t list, string) result =
    raise (Failure "produce_post from non-abstract symbolic state.")

  let fresh_val (_ : t) : vt = LVar (LVar.alloc ())

  let clean_up ?(keep = ES.empty) (state : t) : unit =
    let heap, store, _, _, _ = state in
    let keep =
      keep
      |> SS.fold (fun x ac -> ES.add (Expr.ALoc x) ac) (SStore.alocs store)
      |> SS.fold (fun x ac -> ES.add (Expr.LVar x) ac) (SStore.lvars store)
    in
    let forgettables, keep = SMemory.clean_up ~keep heap in
    L.verbose (fun fmt ->
        fmt "Forgettables: %a"
          (Fmt.list ~sep:Fmt.comma Expr.pp)
          (ES.elements forgettables));
    L.verbose (fun fmt ->
        fmt "Keep: %a" (Fmt.list ~sep:Fmt.comma Expr.pp) (ES.elements keep))

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
                  | _ -> ac)
              | _ -> ac)
          | _ -> ac)
        []
    in
    List.iter (fun (x, e) -> SSubst.put subst x e) new_bindings

  (* Auxiliary Functions *)
  let get_loc_name (loc : Expr.t) state : string option =
    L.(tmi (fun m -> m "get_loc_name: %s" ((Fmt.to_to_string Expr.pp) loc)));
    let _, _, pfs, gamma, _ = state in
    match loc with
    | Lit (Loc loc) | ALoc loc -> Some loc
    | LVar x -> Reduction.resolve_expr_to_location pfs gamma (LVar x)
    | _ ->
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
        | None -> ALoc (ALoc.alloc ()))
    | None -> ALoc (ALoc.alloc ())

  let mem_constraints (state : t) : Formula.t list =
    let heap, _, _, _, _ = state in
    SMemory.mem_constraints heap

  let pp_fix fmt = function
    | MFix mf -> SMemory.pp_c_fix fmt mf
    | FPure f -> Fmt.pf fmt "SFPure(%a)" Formula.pp f
    | FSVars vs ->
        Fmt.pf fmt "SFSVar(@[<h>%a@])"
          (Fmt.iter ~sep:Fmt.comma SS.iter Fmt.string)
          vs
    | FAsrt ga -> Fmt.pf fmt "SFSVar(@[<h>%a@])" Asrt.pp ga

  let get_recovery_vals (state : t) (errs : err_t list) : vt list =
    let heap, _, pfs, _, _ = state in
    let vs = StateErr.get_recovery_vals errs (SMemory.get_recovery_vals heap) in
    let svs = ES.of_list vs in
    let extras =
      PFS.fold_left
        (fun exs pf ->
          match pf with
          | Eq (ALoc loc, LVar x)
            when ES.mem (ALoc loc) svs && Names.is_spec_var_name x ->
              Expr.LVar x :: exs
          | Eq (LVar x, ALoc loc)
            when ES.mem (ALoc loc) svs && Names.is_spec_var_name x ->
              Expr.LVar x :: exs
          | _ -> exs)
        [] pfs
    in
    ES.elements (ES.of_list (vs @ extras))

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
          | MFix mfix' -> (mfix' :: mfix, pfs, svars, asrts)
          | FPure pf' ->
              (mfix, (if pf' = True then pfs else pf' :: pfs), svars, asrts)
          | FSVars svars' -> (mfix, pfs, SS.union svars' svars, asrts)
          | FAsrt ga -> (mfix, pfs, svars, ga :: asrts))
        fix ([], [], SS.empty, [])
    in
    (* Check SAT for some notion of checking SAT *)
    let mfixes = List.map (fun fix -> MFix fix) fixes in
    let asrts = List.map (fun fix -> FAsrt fix) asrts in
    let is_sat = FOSolver.check_satisfiability (PFS.to_list pfs @ pfs') gamma in
    match is_sat with
    | true ->
        let pfixes = List.map (fun pfix -> FPure pfix) pfs' in
        Some
          ((if svars = SS.empty then [] else [ FSVars svars ])
          @ pfixes @ mfixes @ asrts)
    | false ->
        L.verbose (fun m -> m "Warning: invalid fix.");
        None

  let get_fixes ?simple_fix:(sf = true) (state : t) (errs : err_t list) :
      fix_t list list =
    let pp_fixes fmt fixes =
      Fmt.pf fmt "[[ %a ]]" (Fmt.list ~sep:(Fmt.any ", ") pp_fix) fixes
    in
    L.verbose (fun m -> m "SState: get_fixes");
    let heap, _, pfs, gamma, _ = state in
    let one_step_fixes : fix_t list list list =
      List.map
        (fun (err : err_t) ->
          match err with
          | EMem err ->
              List.map
                (fun (mfixes, pfixes, svars, asrts) ->
                  List.map (fun l -> MFix l) mfixes
                  @ List.map (fun pf -> FPure pf) pfixes
                  @
                  if svars == SS.empty then []
                  else
                    [ FSVars svars ] @ List.map (fun asrt -> FAsrt asrt) asrts)
                (SMemory.get_fixes ~simple_fix:sf heap pfs gamma err)
          | EPure f ->
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
                        | _ ->
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
          | _ -> raise (Failure "DEATH: get_fixes: error cannot be fixed."))
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
        (heap : heap_t)
        (new_vars : SS.t)
        (fixes : fix_t list) : heap_t * SS.t =
      match fixes with
      | [] -> (heap, new_vars)
      | fix :: rest ->
          let heap', new_vars' = apply_fix heap new_vars fix in
          apply_fixes_rec heap' new_vars' rest
    in
    (* FIXME: this unused heap' variable is suspicious *)
    let _heap', new_vars = apply_fixes_rec heap SS.empty fixes in
    (Some (heap, store, pfs, gamma, SS.union svars new_vars), !gas)

  let get_equal_values state les =
    let _, _, pfs, _, _ = state in
    les @ List.concat_map (Reduction.get_equal_expressions pfs) les

  let get_heap state =
    let heap, _, _, _, _ = state in
    heap

  let get_typ_env state =
    let _, _, _, typ_env, _ = state in
    typ_env

  let get_pfs state =
    let _, _, pfs, _, _ = state in
    pfs

  let hides
      ~is_post
      ~(used_unifiables : ES.t)
      ~(exprs_to_hide : vt list)
      (state : t) =
    let elist_pp = Fmt.list ~sep:Fmt.comma Expr.pp in
    let () =
      L.verbose (fun fmt ->
          fmt
            "\nEXACT: HIDES: In-post: %a\nUsed: %a\nTo hide: %a\n\nSTATE:\n%a\n"
            Fmt.bool is_post elist_pp
            (ES.elements used_unifiables)
            elist_pp exprs_to_hide pp state)
    in
    let heap, store, pfs, gamma, _ = copy state in
    let store = if is_post then SStore.init [] else store in
    let state = (heap, store, pfs, gamma, SS.empty) in
    let subst, states = simplify ~kill_new_lvars:true state in
    List.fold_left
      (fun outcome state ->
        let heap, _, pfs, _, _ = state in
        PFS.clean_up pfs;
        let used_unifiables =
          ES.map (SSubst.subst_in_expr ~partial:true subst) used_unifiables
        in
        let exprs_to_hide =
          List.map (SSubst.subst_in_expr ~partial:true subst) exprs_to_hide
        in
        let () =
          L.verbose (fun fmt ->
              fmt "EXACT: Hides after subst: %a" elist_pp exprs_to_hide)
        in
        match outcome with
        | Error _ -> outcome
        | Ok () -> (
            let _, keep = SMemory.clean_up ~keep:used_unifiables heap in
            let used_unifiables = ES.union used_unifiables keep in
            let () =
              L.verbose (fun fmt ->
                  fmt "STATE after memory clean-up:\n%a\n" pp state)
            in
            let () =
              L.verbose (fun fmt ->
                  fmt "Used unifiables: %a" elist_pp
                    (Expr.Set.elements used_unifiables))
            in
            let hides_lvars =
              List.fold_left SS.union SS.empty
                (List.map Expr.lvars exprs_to_hide)
            in
            let hides_alocs =
              List.fold_left SS.union SS.empty
                (List.map Expr.alocs exprs_to_hide)
            in
            let state_lvars = get_lvars_for_exact state in
            let state_alocs = get_alocs_for_exact state in
            let inter =
              SS.inter
                (SS.union hides_lvars hides_alocs)
                (SS.union state_lvars state_alocs)
            in
            match SS.is_empty inter with
            | true -> Ok ()
            | false ->
                let non_hidable_expr = SS.min_elt inter in
                let non_hidable_expr =
                  match Names.is_lvar_name non_hidable_expr with
                  | true -> Expr.LVar non_hidable_expr
                  | false -> ALoc non_hidable_expr
                in
                let () =
                  L.verbose (fun fmt ->
                      fmt "EXACT: ERROR: hidden expression in state: %a" Expr.pp
                        non_hidable_expr)
                in
                Error non_hidable_expr))
      (Ok ()) states
end
