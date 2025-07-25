open Literal
open Names
module L = Logging
module SSubst = SVal.SESubst

module type S = sig
  include
    State.S
      with type vt = Expr.t
       and type st = SVal.SESubst.t
       and type store_t = SStore.t

  val make_s :
    init_data:init_data ->
    store:store_t ->
    pfs:PFS.t ->
    gamma:Type_env.t ->
    spec_vars:SS.t ->
    t

  val init : init_data -> t
  val get_init_data : t -> init_data
  val clear_resource : t -> t
  val get_typ_env : t -> Type_env.t
  val get_pfs : t -> PFS.t
  val sure_is_nonempty : t -> bool
  val consume_core_pred : string -> t -> vt list -> action_ret
  val produce_core_pred : string -> t -> vt list -> t list

  (** See {!val:SMemory.S.split_further} *)
  val split_core_pred_further :
    t -> string -> vt list -> err_t -> (vt list list * vt list) option
end

module Make (SMemory : SMemory.S) :
  S
    with type heap_t = SMemory.t
     and type m_err_t = SMemory.err_t
     and type init_data = SMemory.init_data = struct
  type vt = SVal.M.t [@@deriving yojson, show]
  type st = SVal.M.et
  type heap_t = SMemory.t [@@deriving yojson]
  type store_t = SStore.t [@@deriving yojson]
  type m_err_t = SMemory.err_t [@@deriving yojson, show]

  type t = {
    heap : heap_t;
    store : store_t;
    pfs : PFS.t;
    gamma : Type_env.t;
    spec_vars : SS.t;
  }
  [@@deriving yojson]

  type init_data = SMemory.init_data
  type err_t = (m_err_t, vt) StateErr.t [@@deriving yojson, show]
  type action_ret = (t * vt list, err_t) result list

  exception Internal_State_Error of err_t list * t

  module ES = Expr.Set

  let pp fmt state =
    let { heap; store; pfs; gamma; spec_vars } = state in
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
      spec_vars SStore.pp store pp_heap heap PFS.pp pfs Type_env.pp gamma

  let sure_is_nonempty { heap; _ } = SMemory.sure_is_nonempty heap

  let pp_by_need pvars cmd_lvars cmd_locs fmt state =
    let { heap = memory; store; pfs; gamma; spec_vars } = state in

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
    let spec_vars = SS.filter (fun x -> SS.mem x lvars) spec_vars in

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
      spec_vars (SStore.pp_by_need pvars) store pp_memory memory
      (PFS.pp_by_need (pvars, lvars, locs))
      pfs
      (Type_env.pp_by_need (List.fold_left SS.union SS.empty [ pvars; lvars ]))
      gamma

  let init init_data =
    {
      heap = SMemory.init init_data;
      store = SStore.init [];
      pfs = PFS.init ();
      gamma = Type_env.init ();
      spec_vars = SS.empty;
    }

  let make_s
      ~(init_data : init_data)
      ~(store : SStore.t)
      ~(pfs : PFS.t)
      ~(gamma : Type_env.t)
      ~(spec_vars : SS.t) : t =
    { heap = SMemory.init init_data; store; pfs; gamma; spec_vars }

  let execute_action (action : string) (state : t) (args : vt list) : action_ret
      =
    let open Syntaxes.List in
    let { heap; store; pfs; gamma; spec_vars } = state in
    let pc = Gpc.make ~matching:false ~pfs ~gamma () in
    let+ Gbranch.{ value; pc } = SMemory.execute_action action heap pc args in
    match value with
    | Ok (new_heap, vs) ->
        let store = SStore.copy store in
        let new_state =
          { heap = new_heap; store; pfs = pc.pfs; gamma = pc.gamma; spec_vars }
        in
        Ok (new_state, vs)
    | Error err -> Error (StateErr.EMem err)

  let consume_core_pred core_pred state in_args =
    let open Syntaxes.List in
    let { heap; store; pfs; gamma; spec_vars } = state in
    let pc = Gpc.make ~matching:true ~pfs ~gamma () in
    let+ Gbranch.{ value; pc } = SMemory.consume core_pred heap pc in_args in
    match value with
    | Ok (new_heap, vs) ->
        let store = SStore.copy store in
        let new_state =
          { heap = new_heap; store; pfs = pc.pfs; gamma = pc.gamma; spec_vars }
        in
        Ok (new_state, vs)
    | Error err -> Error (StateErr.EMem err)

  let split_core_pred_further state core_pred ins err =
    let { heap; _ } = state in
    match err with
    | StateErr.EMem err -> SMemory.split_further heap core_pred ins err
    | _ -> None

  let produce_core_pred core_pred state args =
    let open Syntaxes.List in
    let { heap; store; pfs; gamma; spec_vars } = state in
    (* matching false is suspicious here *)
    let pc = Gpc.make ~matching:false ~pfs ~gamma () in
    let+ Gbranch.{ value = new_heap; pc } =
      SMemory.produce core_pred heap pc args
    in
    { heap = new_heap; store; pfs = pc.pfs; gamma = pc.gamma; spec_vars }

  let is_overlapping_asrt (a : string) : bool = SMemory.is_overlapping_asrt a

  let eval_expr (state : t) (e : Expr.t) : vt =
    let { store; pfs; gamma; _ } = state in
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
        (* Exists. We can just evaluate pvars because they cannot be quantified *)
        | Exists (bt, e) -> Exists (bt, f e)
        | ForAll (bt, e) -> ForAll (bt, f e)
        | Lit _ | LVar _ | ALoc _ -> expr
      in
      (* Perform reduction *)
      if no_reduce then result
      else
        try Reduction.reduce_lexpr ~gamma ~reduce_lvars:true ~pfs result
        with Reduction.ReductionException (expr, msg) ->
          let msg = Fmt.str "Couldn't reduce %a - %s" Expr.pp expr msg in
          raise (Internal_State_Error ([ StateErr.EOther msg ], state))
    in
    symb_evaluate_expr e

  let get_store ({ store; _ } : t) : store_t = store
  let set_store (state : t) (store : store_t) : t = { state with store }

  let assume ?unfold:_ (state : t) (v : Expr.t) : t list =
    L.verbose (fun fmt -> fmt "Assuming expression: %a" Expr.pp v);
    let { pfs; gamma; _ } = state in
    match v with
    | Lit (Bool true) -> [ state ]
    | Lit (Bool false) -> []
    | _ ->
        (* let t = time() in *)
        let red = Reduction.reduce_lexpr ~pfs ~gamma v in
        if not @@ Expr.is_boolean_expr red then []
        else if red = Lit (Bool false) then []
        else (
          PFS.extend pfs red;
          [ state ])

  let assume_a
      ?(matching = false)
      ?(production = false)
      ?(time = "")
      (state : t)
      (ps : Expr.t list) : t option =
    let { pfs; gamma; _ } = state in
    try
      let ps = List.map (Reduction.reduce_lexpr ~pfs ~gamma) ps in
      let result =
        if
          production
          || FOSolver.check_satisfiability
               ~time:("SState: assume_a: " ^ time)
               ~matching
               (ps @ PFS.to_list pfs)
               gamma
        then (
          List.iter (PFS.extend pfs) ps;
          Some state)
        else (
          Logging.verbose (fun m ->
              m "assume_a: Couldn't assume %a" (Fmt.Dump.list Expr.pp) ps);
          None)
      in
      result
    with Reduction.ReductionException (e, msg) ->
      Logging.verbose (fun m ->
          m "assume_a: Couldn't assume due to an error reducing %a - %s\nps: %a"
            Expr.pp e msg (Fmt.Dump.list Expr.pp) ps);
      None

  let assume_t ({ gamma; _ } as state : t) (v : vt) (t : Type.t) : t option =
    match Typing.reverse_type_lexpr true gamma [ (v, t) ] with
    | None -> None
    | Some gamma' ->
        Type_env.extend gamma gamma';
        Some state

  let sat_check ({ pfs; gamma; _ } : t) (v : Expr.t) : bool =
    L.verbose (fun m -> m "SState: sat_check: %a" Expr.pp v);
    let v = Reduction.reduce_lexpr ~pfs ~gamma v in
    if v = Lit (Bool true) then true
    else if v = Lit (Bool false) then false
    else if not @@ Expr.is_boolean_expr v then false
    else
      let relevant_info = (Expr.pvars v, Expr.lvars v, Expr.locs v) in
      let result =
        FOSolver.check_satisfiability ~relevant_info (v :: PFS.to_list pfs)
          gamma
      in
      L.(verbose (fun m -> m "SState: sat_check done: %b" result));
      result

  let sat_check_f ({ pfs; gamma; _ } : t) (fs : Expr.t list) : st option =
    FOSolver.check_satisfiability_with_model (fs @ PFS.to_list pfs) gamma

  let assert_a ({ pfs; gamma; _ } : t) (ps : Expr.t list) : bool =
    FOSolver.check_entailment SS.empty pfs ps gamma

  let equals ({ pfs; gamma; _ } : t) (le1 : vt) (le2 : vt) : bool =
    let result = FOSolver.is_equal ~pfs ~gamma le1 le2 in
    result

  let get_type ({ pfs; gamma; _ } : t) (le : vt) : Type.t option =
    let le = Reduction.reduce_lexpr ~gamma ~pfs le in
    let t, _ = Typing.type_lexpr gamma le in
    t

  let simplify
      ?(save = false)
      ?(kill_new_lvars = true)
      ?(matching = false)
      (state : t) : st * t list =
    let { heap; store; pfs; gamma; spec_vars } = state in
    let save_spec_vars =
      if save then (SS.empty, true) else (spec_vars, false)
    in
    L.verbose (fun m ->
        m
          "-----------------------------------\n\
           STATE BEFORE SIMPLIFICATIONS:\n\
           %a\n\
           -----------------------------------"
          pp state);
    let subst, _ =
      Simplifications.simplify_pfs_and_gamma ~kill_new_lvars pfs gamma ~matching
        ~save_spec_vars
    in
    let subst =
      SSubst.filter subst (fun x _ ->
          match x with
          | LVar x | PVar x | ALoc x -> not (SS.mem x spec_vars)
          | _ -> true)
    in
    (* Sometimes, [simplify_pfs_and_gamma] leaves abstract locations on the
       rhs of the subst that should be gone, according to itself.
       We filter that. *)
    let subst = SSubst.to_list subst in
    let loc_subst =
      subst
      |> List.filter (fun (x, _) ->
             match x with
             | Expr.ALoc _ | Lit (Loc _) -> true
             | _ -> false)
      |> SSubst.init
    in
    let subst =
      List.map
        (fun (x, y) -> (x, SSubst.subst_in_expr loc_subst ~partial:true y))
        subst
      |> SSubst.init
    in
    let subst = SSubst.filter subst (fun x y -> not (Expr.equal x y)) in
    if SSubst.is_empty subst then (
      Logging.verbose (fun fmt ->
          fmt "No simplifications were made, state unchanged.");
      (subst, [ state ]))
    else (
      Logging.verbose (fun fmt ->
          fmt "Filtered and fixed subst, to be applied to memory:\n%a" SSubst.pp
            subst);
      SStore.substitution_in_place subst store;

      let memories = SMemory.substitution_in_place ~pfs ~gamma subst heap in

      let states =
        match memories with
        | [] -> failwith "Impossible: memory substitution returned []"
        | [ (mem, lpfs, lgamma) ] ->
            let () = Expr.Set.iter (PFS.extend pfs) lpfs in
            let () =
              List.iter (fun (t, v) -> Type_env.update gamma t v) lgamma
            in
            if not kill_new_lvars then
              Typing.naively_infer_type_information pfs gamma;
            [ { heap = mem; store; pfs; gamma; spec_vars } ]
        | multi_mems ->
            List.map
              (fun (mem, lpfs, lgamma) ->
                let bpfs = PFS.copy pfs in
                let bgamma = Type_env.copy gamma in
                let () = Expr.Set.iter (PFS.extend bpfs) lpfs in
                let () =
                  List.iter (fun (t, v) -> Type_env.update bgamma t v) lgamma
                in
                if not kill_new_lvars then
                  Typing.naively_infer_type_information bpfs bgamma;
                {
                  heap = mem;
                  store = SStore.copy store;
                  pfs = bpfs;
                  gamma = bgamma;
                  spec_vars;
                })
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
      (subst, states))

  let simplify_val ({ pfs; gamma; _ } : t) (v : vt) : vt =
    Reduction.reduce_lexpr ~gamma ~pfs v

  let copy (state : t) : t =
    let { heap; store; pfs; gamma; spec_vars } = state in
    let result =
      {
        heap = SMemory.copy heap;
        store = SStore.copy store;
        pfs = PFS.copy pfs;
        gamma = Type_env.copy gamma;
        spec_vars;
      }
    in
    result

  let add_spec_vars (state : t) (xs : Var.Set.t) : t =
    let spec_vars = SS.union xs state.spec_vars in
    { state with spec_vars }

  let get_spec_vars ({ spec_vars; _ } : t) : SS.t = spec_vars

  let get_lvars (state : t) : Var.Set.t =
    let { heap; store; pfs; gamma; spec_vars } = state in
    SMemory.lvars heap
    |> SS.union (SStore.lvars store)
    |> SS.union (PFS.lvars pfs)
    |> SS.union (Type_env.lvars gamma)
    |> SS.union spec_vars

  let to_assertions ?(to_keep : SS.t option) (state : t) : Asrt.t =
    let { heap; store; pfs; gamma; _ } = state in
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
    if Type_env.empty gamma then
      asrts_store @ SMemory.assertions heap @ asrts_pfs
    else
      asrts_store @ SMemory.assertions heap @ asrts_pfs
      @ [ Types (Type_env.to_list_expr gamma) ]

  let evaluate_slcmd (_ : 'a MP.prog) (_ : SLCmd.t) (_ : t) :
      (t, err_t) Res_list.t =
    raise (Failure "ERROR: evaluate_slcmd called for non-abstract execution")

  let match_invariant _ _ _ _ _ =
    raise (Failure "ERROR: match_invariant called for pure symbolic execution")

  let clear_resource (state : t) : t =
    let heap = SMemory.clear state.heap in
    { state with heap }

  let get_init_data { heap; _ } = SMemory.get_init_data heap

  let frame_on _ _ _ =
    raise (Failure "ERROR: framing called for symbolic execution")

  let run_spec
      (_ : MP.spec)
      (_ : t)
      (_ : string)
      (_ : vt list)
      (_ : (string * (string * vt) list) option) =
    raise (Failure "ERROR: run_spec called for non-abstract execution")

  let unfolding_vals (_ : t) (fs : Expr.t list) : vt list =
    let map to_str to_expr =
      List.map to_str fs
      |> List.fold_left SS.union SS.empty
      |> SS.elements |> List.map to_expr
    in
    let lvars = map Expr.lvars (fun x -> Expr.LVar x) in
    let alocs = map Expr.alocs (fun x -> Expr.ALoc x) in
    let clocs = map Expr.clocs (fun x -> Expr.Lit (Loc x)) in
    clocs @ alocs @ lvars

  let substitution_in_place ?(subst_all = false) (subst : st) (state : t) :
      t list =
    let subst = SSubst.filter subst (fun x y -> not (Expr.equal x y)) in
    if SSubst.is_empty subst then [ state ]
    else
      let { heap; store; pfs; gamma; spec_vars } = state in
      SStore.substitution_in_place ~subst_all subst store;
      PFS.substitution subst pfs;
      Typing.substitution_in_place subst gamma;
      match SMemory.substitution_in_place ~pfs ~gamma subst heap with
      | [] -> failwith "IMPOSSIBLE: SMemory always returns at least one memory"
      | [ (mem, lpfs, lgamma) ] ->
          let () = Expr.Set.iter (PFS.extend pfs) lpfs in
          let () = List.iter (fun (t, v) -> Type_env.update gamma t v) lgamma in
          [ { heap = mem; store; pfs; gamma; spec_vars } ]
      | multi_mems ->
          List.map
            (fun (mem, lpfs, lgamma) ->
              let bpfs = PFS.copy pfs in
              let bgamma = Type_env.copy gamma in
              let () = Expr.Set.iter (PFS.extend bpfs) lpfs in
              let () =
                List.iter (fun (t, v) -> Type_env.update bgamma t v) lgamma
              in
              {
                heap = mem;
                store = SStore.copy store;
                pfs = bpfs;
                gamma = bgamma;
                spec_vars;
              })
            multi_mems

  let match_assertion (_ : t) (_ : st) (_ : MP.step) : (t, err_t) Res_list.t =
    raise (Failure "Match assertion from non-abstract symbolic state.")

  let produce_posts (_ : t) (_ : st) (_ : Asrt.t list) : t list =
    raise (Failure "produce_posts from non-abstract symbolic state.")

  let produce (_ : t) (_ : st) (_ : Asrt.t) : (t, err_t) Res_list.t =
    raise (Failure "produce_post from non-abstract symbolic state.")

  let clean_up ?(keep = ES.empty) (state : t) : unit =
    let { heap; store; _ } = state in
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
    let { pfs; gamma; _ } = state in
    let new_bindings =
      SSubst.fold subst
        (fun x e ac ->
          match e with
          | LVar y -> (
              match Type_env.get gamma y with
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
    let { pfs; gamma; _ } = state in
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

  let mem_constraints ({ heap; _ } : t) : Expr.t list =
    SMemory.mem_constraints heap

  let get_recovery_tactic (state : t) (errs : err_t list) : vt Recovery_tactic.t
      =
    let { heap; pfs; _ } = state in
    let memory_tactic =
      StateErr.get_recovery_tactic errs (SMemory.get_recovery_tactic heap)
    in
    if Recovery_tactic.is_none memory_tactic then memory_tactic
    else
      PFS.fold_left
        (fun (acc : vt Recovery_tactic.t) -> function
          | BinOp ((ALoc _ as loc), Equal, LVar x)
          | BinOp (LVar x, Equal, (ALoc _ as loc)) ->
              if Names.is_spec_var_name x then
                let try_fold =
                  Option.map
                    (fun l -> if List.mem loc l then Expr.LVar x :: l else l)
                    acc.try_fold
                in
                let try_unfold =
                  Option.map
                    (fun l -> if List.mem loc l then Expr.LVar x :: l else l)
                    acc.try_unfold
                in
                { try_fold; try_unfold }
              else acc
          | _ -> acc)
        memory_tactic pfs

  let try_recovering _ _ : (t list, string) result =
    Error "try_recovering not supported in symbolic execution"

  let pp_err = StateErr.pp_err SMemory.pp_err SVal.M.pp
  let can_fix = StateErr.can_fix SMemory.can_fix

  let get_failing_constraint (err : err_t) : Expr.t =
    StateErr.get_failing_constraint err SMemory.get_failing_constraint

  (* get_fixes returns a list of possible fixes.
     Each "fix" is actually a list of assertions, each of which have to be applied to the same state *)
  let get_fixes (err : err_t) : Asrt.t list =
    let pp_fix fmt fix = Fmt.pf fmt "[[ %a ]]" Asrt.pp fix in
    let one_step_fixes : Asrt.t list =
      match err with
      | EMem err -> SMemory.get_fixes err
      | EPure f ->
          let result = [ [ Asrt.Pure f ] ] in
          L.verbose (fun m ->
              m "@[<v 2>Memory: Fixes found:@\n%a@]"
                (Fmt.list ~sep:(Fmt.any "@\n") pp_fix)
                result);
          result
      | EAsrt (_, _, fixes) ->
          let result =
            (List.map
               (List.map (function
                 | Asrt.Pure _ as fix -> fix
                 | _ ->
                     raise
                       (Exceptions.Impossible
                          "Non-pure fix for an assertion failure"))))
              fixes
          in
          L.verbose (fun m ->
              m "@[<v 2>Memory: Fixes found:@\n%a@]"
                (Fmt.list ~sep:(Fmt.any "@\n") pp_fix)
                result);
          result
      | _ -> raise (Failure "DEATH: get_fixes: error cannot be fixed.")
    in

    L.tmi (fun m ->
        m "All fixes before normalisation: %a"
          Fmt.Dump.(list Asrt.pp)
          one_step_fixes);
    List.map
      (fun fixes ->
        let pure, unpure = List.partition Asrt.is_pure_asrt fixes in
        pure @ unpure)
      one_step_fixes

  let get_equal_values state les =
    let { pfs; _ } = state in
    les @ List.concat_map (Reduction.get_equal_expressions pfs) les

  let get_heap state =
    let { heap; _ } = state in
    heap

  let get_typ_env state =
    let { gamma; _ } = state in
    gamma

  let get_pfs state =
    let { pfs; _ } = state in
    pfs
end
