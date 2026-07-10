open Literal
open Names
module L = Logging
module SSubst = SVal.SESubst

exception Preprocessing_Error of MP.err list

let () =
  Printexc.register_printer (function
    | Preprocessing_Error mp_errs ->
        Some
          (Fmt.str "Preprocessing Error: %a" (Fmt.Dump.list MP.pp_err) mp_errs)
    | _ -> None)

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

  val make_s_from_heap :
    heap:heap_t ->
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

  let make_s_from_heap
      ~(heap : heap_t)
      ~(store : store_t)
      ~(pfs : PFS.t)
      ~(gamma : Type_env.t)
      ~(spec_vars : SS.t) : t =
    { heap; store; pfs; gamma; spec_vars }

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

      let memories = SMemory.substitution ~pfs ~gamma subst heap in

      let states =
        match memories with
        | [] ->
            L.normal (fun m -> m "Memory substitution vanished");
            []
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
        heap;
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

  let clear_resource (state : t) : t =
    let heap = SMemory.clear state.heap in
    { state with heap }

  let get_init_data { heap; _ } = SMemory.get_init_data heap

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
      match SMemory.substitution ~pfs ~gamma subst heap with
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
          (Internal_State_Error
             ( [ EOther (Fmt.str "Couldn't get location of %a" Expr.pp loc) ],
               state ))

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
      | EAsrt (_, pf) ->
          let pf = Reduction.reduce_lexpr pf in
          let fix =
            match pf with
            | Expr.Lit (Bool _) -> []
            | _ -> [ [ Asrt.Pure pf ] ]
          in
          L.verbose (fun m ->
              m "@[<v 2>Memory: Fixes found:@\n%a@]"
                (Fmt.list ~sep:(Fmt.any "@\n") pp_fix)
                fix);
          fix
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

  (* -------------------------------------------------------------------- *)
  (* Verification: matching, SL commands, and spec application.

     Everything below runs through the matching engine. [Matcher.Make]'s
     parameter is the self-contained [Matcher.MatchableState] signature
     (rather than [SState.S]) precisely so that this instantiation is legal
     here without a dependency cycle. When executed over a plain symbolic
     memory these operations are inert: nothing in pure symbolic execution
     issues SL commands or applies specs. *)
  (* -------------------------------------------------------------------- *)

  module SMatcher = Matcher.Make (struct
    type nonrec t = t

    let to_yojson = to_yojson
    let of_yojson = of_yojson

    type nonrec m_err_t = m_err_t
    type nonrec err_t = err_t

    let err_t_to_yojson = err_t_to_yojson
    let err_t_of_yojson = err_t_of_yojson
    let pp_err_t = pp_err_t
    let show_err_t = show_err_t
    let pp = pp
    let pp_by_need = pp_by_need
    let pp_err = pp_err
    let copy = copy
    let get_store = get_store
    let set_store = set_store
    let simplify = simplify
    let simplify_val = simplify_val
    let assume_a = assume_a
    let assume_t = assume_t
    let assert_a = assert_a
    let get_type = get_type
    let unfolding_vals = unfolding_vals
    let can_fix = can_fix
    let get_recovery_tactic = get_recovery_tactic
    let execute_action = execute_action
    let consume_core_pred = consume_core_pred
    let produce_core_pred = produce_core_pred
  end)

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
  let rec run_spec_aux
      ?(more_specs = [])
      ?(existential_bindings : (string * vt) list = [])
      (name : string)
      (params : string list)
      (mp : MP.t)
      (x : string option)
      (args : vt list)
      (astate : t) : (t * Flag.t, SMatcher.err_t) Res_list.t =
    let open Res_list.Syntax in
    let open Syntaxes.List in
    L.verbose (fun m ->
        m "INSIDE RUN spec of %s (%d more) with the following MP:@\n%a@\n" name
          (List.length more_specs) MP.pp mp);
    let old_store = get_store astate in
    let** new_store =
      try SStore.init (List.combine params args) |> Res_list.return
      with Invalid_argument _ ->
        let msg =
          Fmt.str
            "Running spec of %s which takes %i parameters with the following \
             %i arguments : %a"
            name (List.length params) (List.length args) (Fmt.Dump.list Expr.pp)
            args
        in
        Res_list.error_with (StateErr.EOther msg)
    in

    let astate' = set_store astate new_store in
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

    let res = SMatcher.match_ astate' subst mp (FunctionCall name) in
    if List.exists Result.is_error res then
      L.normal (fun m ->
          m "WARNING: Failed to match against the precondition of procedure %s"
            name);
    let** frame_state, subst, posts = res in

    let fl, posts =
      match posts with
      | Some p -> p
      | None -> Fmt.kstr L.fail "Spec of %s has no postcondition" name
    in

    let** frame_state, frame_store =
      match more_specs with
      | [] -> Res_list.return (frame_state, old_store)
      | (name, params, mp, x, args, existential_bindings) :: more_specs ->
          let frame_state = set_store frame_state (SStore.copy old_store) in
          let++ frame_state, _ =
            run_spec_aux ~more_specs ?existential_bindings name params mp x args
              frame_state
          in
          let frame_store = get_store frame_state in
          let frame_state = set_store frame_state (SStore.copy new_store) in
          (frame_state, frame_store)
    in

    (* OK FOR DELAY ENTAILMENT *)
    let* final_state = SMatcher.produce_posts frame_state subst posts in

    let final_store = get_store final_state in
    let v_ret = SStore.get final_store Names.return_variable in
    let final_state = set_store final_state (SStore.copy frame_store) in
    let v_ret = Option.value ~default:(Lit Undefined) v_ret in
    let final_state = update_store final_state x v_ret in
    let _, final_states = simplify ~matching:true final_state in
    (* Concrete-ins predicates are eagerly unfolded by the memory itself (as a
       post-pass on its operations), so there is nothing left to unfold at the
       state level here. *)
    List.map (fun final_state -> Ok (final_state, fl)) final_states

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

  let consume astate (a : Asrt.t) binders =
    if not (List.for_all Names.is_lvar_name binders) then
      failwith "Binding of pure variables in *-assert.";
    let store = get_store astate in
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
    let state_lvars = get_lvars astate in
    let known_lvars =
      SS.elements
        (SS.diff (SS.inter state_lvars (Asrt.lvars a)) (SS.of_list binders))
    in
    let known_lvars = List.map (fun x -> Expr.LVar x) known_lvars in
    let asrt_alocs =
      List.map (fun x -> Expr.ALoc x) (SS.elements (Asrt.alocs a))
    in
    let known_matchables = Expr.Set.of_list (known_lvars @ asrt_alocs) in

    let mp = MP.init known_matchables Expr.Set.empty [ (a, (None, None)) ] in
    let vars_to_forget = SS.inter state_lvars (SS.of_list binders) in
    let astate =
      (* The memory is immutable, so the substituted state must be used (the
         pure part of the state is still substituted in place). *)
      if SS.is_empty vars_to_forget then astate
      else
        let oblivion_subst = fresh_subst vars_to_forget in
        L.verbose (fun m ->
            m "Forget @[%a@] with subst: %a"
              Fmt.(iter ~sep:comma SS.iter string)
              vars_to_forget SVal.SESubst.pp oblivion_subst);

        (* TODO: THIS SUBST IN PLACE MUST NOT BRANCH *)
        let subst_in_place = substitution_in_place oblivion_subst astate in
        assert (List.length subst_in_place = 1);
        let astate = List.hd subst_in_place in

        L.verbose (fun m -> m "State after substitution:@\n@[%a@]\n" pp astate);
        astate
    in
    let mp =
      match mp with
      | Error asrts ->
          raise (Preprocessing_Error [ (MPAssert (a, asrts), None) ])
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
          let new_state' = add_spec_vars new_astate (SS.of_list binders) in
          let _, new_states = simplify ~kill_new_lvars:true new_state' in
          let+ new_state = new_states in
          Ok new_state
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
        let fail_pfs : Expr.t = get_failing_constraint err in

        let failing_model = sat_check_f astate [ fail_pfs ] in
        let msg =
          Fmt.str
            "Assert failed with argument @[<h>%a@]. matching failed.@\n\
             @[<v 2>Errors:@\n\
             %a.@]@\n\
             @[<v 2>Failing Model:@\n\
             %a@]@\n"
            Asrt.pp a pp_err err
            Fmt.(option ~none:(any "CANNOT CREATE MODEL") SVal.SESubst.pp)
            failing_model
        in
        L.print_to_all msg;
        Res_list.error_with (StateErr.EPure fail_pfs)

  let produce_lcmd astate a =
    let store = get_store astate in
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
    let _, new_states = simplify ~kill_new_lvars:true new_astate in
    let+ new_state = new_states in
    Ok new_state

  let match_invariant
      (revisited : bool)
      (astate : t)
      (a : Asrt.t)
      (binders : string list) : (t * t, err_t) Res_list.t =
    let store = get_store astate in
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
    let state_lvars = get_lvars astate in
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
    let mp =
      (* FIXME: UNDERSTAND IF THE OX SHOULD BE [] *)
      MP.init known_matchables Expr.Set.empty [ (a, (None, None)) ]
    in
    (* This will not do anything in the original pass,
       but will do precisely what is needed in the re-establishment *)
    let vars_to_forget = SS.inter state_lvars (SS.of_list lvar_binders) in
    let astate =
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

        L.verbose (fun m -> m "State after substitution:@\n@[%a@]\n" pp astate);
        astate)
      else astate
    in
    let mp =
      match mp with
      | Error asrts ->
          raise (Preprocessing_Error [ (MPAssert (a, asrts), None) ])
      | Ok mp -> mp
    in
    let bindings =
      List.map
        (fun (e : Expr.t) ->
          let binding =
            match e with
            | PVar x -> SStore.get (get_store astate) x
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
      L.verbose (fun m -> m "State before matching:@\n@[%a@]\n" pp astate);
      let+ result = SMatcher.match_ astate subst mp Invariant in
      match result with
      | Ok state -> Ok state
      | Error err ->
          let fail_pfs : Expr.t = get_failing_constraint err in
          let failing_model = sat_check_f astate [ fail_pfs ] in
          let () =
            L.print_to_all
              (Format.asprintf
                 "MATCH INVARIANT FAILURE: with argument @[<h>%a@]. matching \
                  failed.@\n\
                  @[<v 2>Errors:@\n\
                  %a.@]@\n\
                  @[<v 2>Failing Model:@\n\
                  %a@]@\n"
                 Asrt.pp a pp_err err
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
          (fun x -> (Expr.PVar x, Option.get (SStore.get (get_store astate) x)))
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
          let invariant_state =
            add_spec_vars new_astate (SS.of_list lvar_binders)
          in
          let _, invariant_states =
            simplify ~kill_new_lvars:true invariant_state
          in
          let+ invariant_state = invariant_states in
          Ok (copy frame_state, invariant_state)
      | Error e ->
          let msg =
            Fmt.str
              "MATCH INVARIANT FAILURE: %a\n\
               unable to produce variable bindings: %a."
              Asrt.pp a pp_err_t e
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

  (** Evaluation of logic commands

      @param prog GIL program
      @param lcmd Logic command to be evaluated
      @param state Current state
      @return List of states resulting from the evaluation *)
  let evaluate_slcmd (prog : 'a MP.prog) (lcmd : SLCmd.t) (astate : t) :
      (t, err_t) Res_list.t =
    let eval_expr e = eval_expr astate e in
    let open Res_list.Syntax in
    let** resulting_astate =
      match lcmd with
      | SymbExec -> failwith "Impossible: Untreated SymbExec"
      | Fold _ | Unfold _ | GUnfold _ | Package _ ->
          failwith
            "Fold/Unfold/GUnfold/Package must be routed through execute_action"
      | SepAssert (a, binders) -> (
          if not (List.for_all Names.is_lvar_name binders) then
            failwith "Binding of pure variables in *-assert.";
          let store = get_store astate in
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
          let state_lvars = get_lvars astate in
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

          let mp =
            MP.init known_matchables Expr.Set.empty [ (a, (None, None)) ]
          in
          let vars_to_forget = SS.inter state_lvars (SS.of_list binders) in
          let astate =
            (* The memory is immutable, so the substituted state must be used
               (the pure part of the state is still substituted in place). *)
            if SS.is_empty vars_to_forget then astate
            else
              let oblivion_subst = fresh_subst vars_to_forget in
              L.verbose (fun m ->
                  m "Forget @[%a@] with subst: %a"
                    Fmt.(iter ~sep:comma SS.iter string)
                    vars_to_forget SVal.SESubst.pp oblivion_subst);

              (* TODO: THIS SUBST IN PLACE MUST NOT BRANCH *)
              let subst_in_place =
                substitution_in_place oblivion_subst astate
              in
              assert (List.length subst_in_place = 1);
              let astate = List.hd subst_in_place in

              L.verbose (fun m ->
                  m "State after substitution:@\n@[%a@]\n" pp astate);
              astate
          in
          let mp =
            match mp with
            | Error asrts ->
                raise (Preprocessing_Error [ (MPAssert (a, asrts), None) ])
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
                  add_spec_vars new_astate (SS.of_list binders)
                in
                let _, new_states = simplify ~kill_new_lvars:true new_state' in
                let+ new_state = new_states in

                Ok new_state
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
              let fail_pfs : Expr.t = get_failing_constraint err in

              let failing_model = sat_check_f astate [ fail_pfs ] in
              let msg =
                Fmt.str
                  "Assert failed with argument @[<h>%a@]. matching failed.@\n\
                   @[<v 2>Errors:@\n\
                   %a.@]@\n\
                   @[<v 2>Failing Model:@\n\
                   %a@]@\n"
                  Asrt.pp a pp_err err
                  Fmt.(option ~none:(any "CANNOT CREATE MODEL") SVal.SESubst.pp)
                  failing_model
              in
              L.print_to_all msg;
              Res_list.error_with (StateErr.EPure fail_pfs))
      | Consume (asrt, binders) -> consume astate asrt binders
      | Produce asrt -> produce_lcmd astate asrt
      | ApplyLem (lname, args, binders) ->
          if not (List.for_all Names.is_lvar_name binders) then
            failwith "Binding of pure variables in lemma application.";
          let lemma =
            match MP.get_lemma prog lname with
            | Error _ -> Fmt.failwith "Lemma %s does not exist" lname
            | Ok lemma -> lemma
          in
          let v_args : vt list = List.map eval_expr args in
          let existential_bindings =
            List.map2
              (fun x y -> (x, Expr.LVar y))
              lemma.data.lemma_existentials binders
          in
          let** astate, _ =
            run_spec_aux ~existential_bindings lname lemma.data.lemma_params
              lemma.mp None v_args astate
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
      (x : string)
      (args : vt list)
      (subst : (string * (string * vt) list) option)
      (astate : t) : (t * Flag.t, err_t) Res_list.t =
    run_spec_aux ?existential_bindings:(Option.map snd subst)
      spec.data.spec_name spec.data.spec_params spec.mp (Some x) args astate

  let run_par_spec specs astate =
    let specs =
      List.map
        (fun ((spec, x, args, subst) :
               MP.spec * string * vt list * (string * (string * vt) list) option)
           ->
          ( spec.data.spec_name,
            spec.data.spec_params,
            spec.mp,
            Some x,
            args,
            Option.map snd subst ))
        specs
    in
    match specs with
    | [] -> Res_list.return (astate, Flag.Normal)
    | (a, b, c, d, e, f) :: more_specs ->
        run_spec_aux ~more_specs ?existential_bindings:f a b c d e astate

  let produce (astate : t) (subst : st) (a : Asrt.t) : (t, err_t) Res_list.t =
    SMatcher.produce astate subst a

  let match_assertion (astate : t) (subst : st) (step : MP.step) =
    SMatcher.match_assertion astate subst step

  let produce_posts (astate : t) (subst : st) (asrts : Asrt.t list) : t list =
    SMatcher.produce_posts astate subst asrts

  let try_recovering (astate : t) (tactic : vt Recovery_tactic.t) :
      (t list, string) result =
    SMatcher.try_recovering astate tactic |> Result.map fst

  (* Shadows the plain assume: [~unfold:true] additionally asks the memory to
     unfold around the assumed expression (low-level automation). Only
     predicate-carrying (verification / bi-abduction) stacks understand the
     reserved recover action — plain symbolic memories fail hard on unknown
     actions — so the automation is gated on the execution mode; in pure
     symbolic execution there are no predicates to unfold anyway. *)
  let assume ?(unfold = false) (astate : t) (v : Expr.t) : t list =
    let open Syntaxes.List in
    let* astate' = assume astate v in
    let mode = !Config.current_exec_mode in
    let should_unfold =
      !Config.unfolding && unfold
      && (Exec_mode.is_verification_exec mode
         || Exec_mode.is_biabduction_exec mode)
    in
    match (should_unfold, v) with
    | _, Lit (Bool true) -> [ astate' ]
    | false, _ -> [ astate' ]
    | true, _ -> (
        (* No recovered state (an error) keeps the original state; zero
           branches kill the path. *)
        let unfold_vals = Expr.base_elements v in
        let enc_args =
          [
            Expr.Lit Nono;
            Expr.EList unfold_vals;
            Expr.Lit (String "low");
            Expr.EList [];
          ]
        in
        let results = execute_action SLCmd.recover_action astate' enc_args in
        let oks =
          List.filter_map
            (function
              | Ok (st, _) -> Some st
              | Error _ -> None)
            results
        in
        match (oks, results) with
        | [], _ :: _ -> [ astate' ]
        | _ ->
            let* astate = oks in
            (* Mirrors the legacy path: the unfold itself simplified with
               [~matching:true], and this function then simplified again. *)
            let* astate =
              snd (simplify ~kill_new_lvars:false ~matching:true astate)
            in
            let _, astates = simplify ~kill_new_lvars:false astate in
            astates)
end
