open SVal
module L = Logging
module SB = Containers.SB

type simpl_key_type = {
  kill_new_lvars : bool option;
  gamma_list : (Var.t * Type.t) list;
  pfs_list : Formula.t list;
  existentials : SS.t;
  matching : bool;
  save_spec_vars : (SS.t * bool) option; (* rpfs_lvars  : CCommon.SS.t *)
}

type simpl_val_type = {
  simpl_gamma : (Var.t * Type.t) list;
  simpl_pfs : Formula.t list;
  simpl_existentials : SS.t;
  subst : SVal.SESubst.t;
}

(* Simplification cache *)
let simplification_cache : (simpl_key_type, simpl_val_type) Hashtbl.t =
  Hashtbl.create 1

(* Reduction of assertions *)

(*************************************)
(** Symbolic state simplification   **)

(*************************************)

let reduce_pfs_in_place ?(matching = false) _ gamma (pfs : PFS.t) =
  PFS.map_inplace (Reduction.reduce_formula ~matching ~gamma ~pfs) pfs

let sanitise_pfs ?(matching = false) store gamma pfs =
  let old_pfs = ref (PFS.init ()) in
  while not (PFS.equal !old_pfs pfs) do
    old_pfs := PFS.copy pfs;
    reduce_pfs_in_place ~matching store gamma pfs
  done;
  PFS.remove_duplicates pfs

let sanitise_pfs_no_store ?(matching = false) =
  sanitise_pfs ~matching (Hashtbl.create 1)

(* *********** *)
(*   CLEANUP   *)
(* *********** *)

let clean_up_stuff (left : PFS.t) (right : PFS.t) =
  let sleft = PFS.to_set left in
  let pf_sym pfa pfb =
    match ((pfa, pfb) : Formula.t * Formula.t) with
    | Eq (a, b), Eq (c, d) when a = d && b = c -> true
    | Not (Eq (a, b)), Not (Eq (c, d)) when a = d && b = c -> true
    | _ -> false
  in
  let eq_or_sym pfa pfb = pfa = pfb || pf_sym pfa pfb in
  let keep pf = not (Formula.Set.exists (eq_or_sym pf) sleft) in
  let cond pf =
    let npf =
      match pf with
      | Formula.Not pf -> pf
      | _ -> Not pf
    in
    Formula.Set.exists (eq_or_sym npf) sleft
  in
  if PFS.filter_stop_cond ~keep ~cond right then
    let () = PFS.clear right in
    PFS.set left [ False ]

(* Set intersections *)
let get_num_set_intersections pfs =
  let lvars = Hashtbl.create 1 in
  let rvars = Hashtbl.create 1 in

  List.iter
    (fun pf ->
      match (pf : Formula.t) with
      | ForAll
          ( [ (x, Some NumberType) ],
            Or (Not (SetMem (LVar y, LVar set)), FLess (LVar elem, LVar z)) )
        when x = y && x = z ->
          L.(verbose (fun m -> m "Got left: %s, %s" elem set));
          Hashtbl.add lvars elem set
      | ForAll
          ( [ (x, Some NumberType) ],
            Or (Not (SetMem (LVar y, LVar set)), FLess (LVar z, LVar elem)) )
        when x = y && x = z ->
          L.(verbose (fun m -> m "Got right: %s, %s" elem set));
          Hashtbl.add rvars elem set
      | _ -> ())
    pfs;

  L.verbose (fun m -> m "v <# set :");
  Hashtbl.iter (fun v s -> L.(verbose (fun m -> m "\t%s, %s" v s))) lvars;
  L.verbose (fun m -> m "set <# v :");
  Hashtbl.iter (fun v s -> L.(verbose (fun m -> m "\t%s, %s" v s))) rvars;

  (*
   *   1. forall (v, s) in lvars -> inter { v }, s = 0
   *   2. forall (v, s) in rvars -> inter { v }, s = 0
   *   3. if (v, s1) in lvars and (v, s2) in rvars, then inter s1 s2 = 0
   *   4. if v1 < v2 and (v1, s1) in lvars and (v2, s2) in lvars, then inter { v1 } { v2 } = 0 and inter { v1 } s2 = 0
   *
   *   THERE ARE MORE
   *)
  let intersections = ref [] in

  (* 1. *)
  Hashtbl.iter
    (fun v s ->
      intersections :=
        Expr.Set.of_list [ ESet [ LVar v ]; LVar s ] :: !intersections)
    lvars;
  (* 2. *)
  Hashtbl.iter
    (fun v s ->
      intersections :=
        Expr.Set.of_list [ ESet [ LVar v ]; LVar s ] :: !intersections)
    rvars;
  (* 3. *)
  Hashtbl.iter
    (fun v s1 ->
      if Hashtbl.mem rvars v then
        let rsets = Hashtbl.find_all rvars v in
        List.iter
          (fun s2 ->
            intersections :=
              Expr.Set.of_list [ LVar s1; LVar s2 ] :: !intersections)
          rsets)
    lvars;
  (* 4. *)
  List.iter
    (fun a ->
      match (a : Formula.t) with
      | FLess (LVar v1, LVar v2) -> (
          match (Hashtbl.mem lvars v1, Hashtbl.mem lvars v2) with
          | true, true ->
              intersections :=
                Expr.Set.of_list [ ESet [ LVar v1 ]; ESet [ LVar v2 ] ]
                :: !intersections;
              let rsets = Hashtbl.find_all lvars v2 in
              List.iter
                (fun s2 ->
                  intersections :=
                    Expr.Set.of_list [ ESet [ LVar v1 ]; LVar s2 ]
                    :: !intersections)
                rsets
          | true, false | false, true ->
              intersections :=
                Expr.Set.of_list [ ESet [ LVar v1 ]; ESet [ LVar v2 ] ]
                :: !intersections
          | _, _ -> ())
      | _ -> ())
    pfs;
  let intersections = List.map (fun s -> Expr.Set.elements s) !intersections in
  List.sort compare intersections

let _resolve_set_existentials
    (lpfs : PFS.t)
    (rpfs : PFS.t)
    exists
    (gamma : Type_env.t) =
  let exists = ref exists in

  let set_exists =
    SS.filter (fun x -> Type_env.get gamma x = Some SetType) !exists
  in
  if SS.cardinal set_exists > 0 then (
    let intersections =
      get_num_set_intersections (PFS.to_list lpfs @ PFS.to_list rpfs)
    in
    L.(
      verbose (fun m ->
          m "Intersections we have:\n%s"
            (String.concat "\n"
               (List.map
                  (fun s ->
                    String.concat ", "
                      (List.map (fun e -> (Fmt.to_to_string Expr.pp) e) s))
                  intersections))));

    let filter_map_fun (formula_to_filter : Formula.t) =
      match formula_to_filter with
      | Eq (NOp (SetUnion, ul), NOp (SetUnion, ur)) ->
          (* Expand ESets *)
          let ul =
            List.flatten
              (List.map
                 (fun (u : Expr.t) : Expr.t list ->
                   match (u : Expr.t) with
                   | ESet x ->
                       List.map (fun (x : Expr.t) : Expr.t -> ESet [ x ]) x
                   | _ -> [ u ])
                 ul)
          in
          let ur =
            List.flatten
              (List.map
                 (fun (u : Expr.t) : Expr.t list ->
                   match u with
                   | ESet x -> List.map (fun x : Expr.t -> ESet [ x ]) x
                   | _ -> [ u ])
                 ur)
          in

          let sul = Expr.Set.of_list ul in
          let sur = Expr.Set.of_list ur in
          L.verbose (fun m ->
              m "Resolve set existentials: I have found a union equality.");
          L.verbose (fun m -> m "%a" Formula.pp formula_to_filter);

          (* Trying to cut the union *)
          let same_parts = Expr.Set.inter sul sur in
          L.(
            verbose (fun m ->
                m "Same parts: %s"
                  (String.concat ", "
                     (List.map
                        (fun x -> (Fmt.to_to_string Expr.pp) x)
                        (Expr.Set.elements same_parts)))));
          if Expr.Set.cardinal same_parts = 1 then (
            let must_not_intersect =
              Expr.Set.diff (Expr.Set.union sul sur) same_parts
            in
            L.(
              verbose (fun m ->
                  m "%s must not intersect any of %s"
                    (String.concat ", "
                       (List.map
                          (fun x -> (Fmt.to_to_string Expr.pp) x)
                          (Expr.Set.elements same_parts)))
                    (String.concat ", "
                       (List.map
                          (fun x -> (Fmt.to_to_string Expr.pp) x)
                          (Expr.Set.elements must_not_intersect)))));
            let element : Expr.t = List.hd (Expr.Set.elements same_parts) in
            let must_not_intersect =
              List.map
                (fun s -> List.sort compare [ s; element ])
                (Expr.Set.elements must_not_intersect)
            in
            L.(
              verbose (fun m ->
                  m "Intersections we need:\n%s"
                    (String.concat "\n"
                       (List.map
                          (fun s ->
                            String.concat ", "
                              (List.map
                                 (fun e -> (Fmt.to_to_string Expr.pp) e)
                                 s))
                          must_not_intersect))));
            let must_not_intersect =
              List.map (fun s -> List.mem s intersections) must_not_intersect
            in
            L.(
              verbose (fun m ->
                  m "And we have: %s"
                    (String.concat ", "
                       (List.map
                          (fun (x : bool) ->
                            if x = true then "true" else "false")
                          must_not_intersect))));
            let must_not_intersect =
              SB.elements (SB.of_list must_not_intersect)
            in
            if must_not_intersect = [ true ] then
              let cl = Expr.Set.diff sul same_parts in
              let cr = Expr.Set.diff sur same_parts in
              let lhs =
                if Expr.Set.cardinal cl = 1 then List.hd (Expr.Set.elements cl)
                else NOp (SetUnion, Expr.Set.elements cl)
              in
              let rhs =
                if Expr.Set.cardinal cr = 1 then List.hd (Expr.Set.elements cr)
                else NOp (SetUnion, Expr.Set.elements cr)
              in
              (* CAREFULLY substitute *)
              match lhs with
              | LVar v when SS.mem v set_exists ->
                  L.(
                    verbose (fun m ->
                        m "Managed to instantiate a set existential: %s" v));
                  let temp_subst = SESubst.init [] in
                  SESubst.put temp_subst (LVar v) rhs;
                  PFS.substitution temp_subst rpfs;
                  exists := SS.remove v !exists;
                  while Type_env.mem gamma v do
                    Type_env.remove gamma v
                  done;
                  None
              | _ -> Some (Formula.Eq (lhs, rhs))
            else Some formula_to_filter)
          else Some formula_to_filter
      | _ -> Some formula_to_filter
    in
    PFS.filter_map filter_map_fun rpfs;
    (rpfs, !exists, gamma))
  else (rpfs, !exists, gamma)

(**
  Pure entailment: simplify pure formulae and typing environment

  @param pfs Pure formulae (modified destructively)
  @param gamma Typing environment (modified destructively)
  @param vars_to_save Logical variables that cannot be deleted
  @return Substitution from logical variables to logical expressions
*)
let simplify_pfs_and_gamma
    ?(matching = false)
    ?(kill_new_lvars : bool option)
    ?(save_spec_vars : (SS.t * bool) option)
    ?(existentials : SS.t option)
    (lpfs : PFS.t)
    ?(rpfs : PFS.t option)
    (gamma : Type_env.t) : SESubst.t * SS.t =
  (* let t = Sys.time () in *)
  let rpfs : PFS.t = Option.value ~default:(PFS.init ()) rpfs in
  let existentials : SS.t ref =
    ref (Option.value ~default:SS.empty existentials)
  in

  let key : simpl_key_type =
    {
      kill_new_lvars;
      gamma_list = Type_env.to_list gamma;
      pfs_list = PFS.to_list lpfs;
      existentials = !existentials;
      matching;
      save_spec_vars (* rpfs_lvars = (PFS.lvars rpfs) *);
    }
  in
  match Hashtbl.mem simplification_cache key with
  | true ->
      (* update_statistics "Simpl: cached" 0.; *)
      let { simpl_gamma; simpl_pfs; simpl_existentials; subst } =
        Hashtbl.find simplification_cache key
      in
      Type_env.reset gamma simpl_gamma;
      PFS.set lpfs simpl_pfs;

      (* Deal with rpfs *)
      if PFS.length lpfs > 0 && PFS.get_nth 0 lpfs == Some False then (
        PFS.clear rpfs;
        PFS.extend rpfs True);

      (SESubst.copy subst, simpl_existentials)
  | false ->
      L.verbose (fun m -> m "PFS/Gamma simplification:");
      L.verbose (fun m ->
          m "With matching: %s" (if matching then "Yes" else "No"));
      L.verbose (fun m -> m "PFS:@\n@[%a@]\n" PFS.pp lpfs);
      L.verbose (fun m -> m "Gamma:@\n@[%a@]\n" Type_env.pp gamma);

      let result = SESubst.init [] in

      let vars_to_save, save_all =
        Option.value ~default:(SS.empty, false) save_spec_vars
      in

      let vars_to_kill = ref SS.empty in
      let kill_new_lvars = Option.value ~default:false kill_new_lvars in

      (* Unit types *)
      let simplify_unit_types () =
        Type_env.iter gamma (fun x t ->
            let e = Expr.from_var_name x in
            match t with
            | UndefinedType -> SESubst.put result e (Lit Undefined)
            | NullType -> SESubst.put result e (Lit Null)
            | EmptyType -> SESubst.put result e (Lit Empty)
            | NoneType -> SESubst.put result e (Lit Nono)
            | _ -> ())
      in

      (* Pure formulae false *)
      let pfs_false lpfs rpfs : unit =
        PFS.clear lpfs;
        PFS.extend lpfs False;
        PFS.clear rpfs;
        PFS.extend rpfs True
      in

      let stop_explain (msg : string) : [> `Stop ] =
        L.verbose (fun m -> m "Pure formulae false: %s" msg);
        `Stop
      in
      (* PF simplification *)
      let rec filter_mapper_formula (pfs : PFS.t) (pf : Formula.t) :
          [ `Stop | `Replace of Formula.t | `Filter ] =
        (* Reduce current assertion *)
        let rec_call = filter_mapper_formula pfs in
        let extend_with = PFS.extend pfs in
        let whole = Reduction.reduce_formula ~matching ~gamma ~pfs pf in
        match whole with
        (* These we must not encounter here *)
        | ForAll (bt, _) ->
            let lx, _ = List.split bt in
            List.iter (fun x -> Type_env.remove gamma x) lx;
            `Replace whole
        (* And is expanded *)
        | And (a1, a2) ->
            extend_with a2;
            rec_call a1
        (* If we find true, we can delete it *)
        | True -> `Filter
        (* If we find false, the entire pfs are false *)
        | False -> stop_explain "False in pure formulae"
        (* Inequality of things with different types *)
        | Not (Eq (le1, le2)) -> (
            let te1, _ = Typing.type_lexpr gamma le1 in
            let te2, _ = Typing.type_lexpr gamma le2 in
            match (te1, te2) with
            | Some te1, Some te2 when te1 <> te2 -> `Filter
            | Some te1, Some te2
              when te1 = te2
                   && (te1 = UndefinedType || te1 = NullType || te1 = EmptyType
                     || te1 = NoneType) ->
                stop_explain "Inequality of two undefined/null/empty/none"
            | _ -> `Replace whole)
        | Eq (BinOp (lst, LstNth, idx), elem)
        | Eq (elem, BinOp (lst, LstNth, idx)) -> (
            match idx with
            | Lit (Int nx) ->
                let prepend_lvars =
                  List.init (Z.to_int nx) (fun _ -> LVar.alloc ())
                in
                let append_lvar = LVar.alloc () in
                (* Fresh variables can be removed *)
                vars_to_kill :=
                  SS.add append_lvar
                    (SS.add_seq (List.to_seq prepend_lvars) !vars_to_kill);
                let prepend = List.map (fun x -> Expr.LVar x) prepend_lvars in
                let append = Expr.LVar append_lvar in
                rec_call
                  (Eq
                     ( lst,
                       NOp
                         ( LstCat,
                           [ EList (List.append prepend [ elem ]); append ] ) ))
            | Lit (Num _) -> failwith "l-nth(l, f) where f is Num and not Int!"
            | _ -> `Replace whole)
        | Eq (UnOp (LstLen, le), Lit (Int z)) when Z.equal z Z.zero ->
            rec_call (Eq (le, EList []))
        | Eq (Lit (Int z), UnOp (LstLen, le)) when Z.equal z Z.zero ->
            rec_call (Eq (le, EList []))
        | Eq (UnOp (LstLen, le), Lit (Int len))
        | Eq (Lit (Int len), UnOp (LstLen, le))
          when (not matching) && Z.leq len (Z.of_int 100) ->
            let len = Z.to_int len in
            if len >= 0 then (
              let le_vars = List.init len (fun _ -> LVar.alloc ()) in
              vars_to_kill := SS.union !vars_to_kill (SS.of_list le_vars);
              let le' = List.map (fun x -> Expr.LVar x) le_vars in
              rec_call (Eq (le, EList le')))
            else stop_explain "List length an unexpected integer."
        | Eq (NOp (LstCat, les), EList [])
        | Eq (NOp (LstCat, les), Lit (LList []))
        | Eq (EList [], NOp (LstCat, les))
        | Eq (Lit (LList []), NOp (LstCat, les)) ->
            let eqs = List.map (fun le -> Formula.Eq (le, EList [])) les in
            List.iter (fun eq -> extend_with eq) eqs;
            `Filter
        (* Two list concats, Satan save us *)
        | Eq (NOp (LstCat, lcat), NOp (LstCat, rcat)) -> (
            match Reduction.understand_lstcat lpfs gamma lcat rcat with
            | None -> `Replace whole
            | Some (pf, new_vars) ->
                extend_with pf;
                vars_to_kill := SS.union !vars_to_kill new_vars;
                `Replace whole)
        (*  *)
        | Eq (UnOp (LstLen, x), BinOp (Lit (Int n), IPlus, LVar z))
          when Z.geq n Z.zero ->
            let new_lvars =
              List.init (Z.to_int n) (fun _ -> Expr.LVar (LVar.alloc ()))
            in
            let rest = LVar.alloc () in
            let lst_eq =
              Formula.Eq (x, NOp (LstCat, [ EList new_lvars; LVar rest ]))
            in
            let len_rest = Formula.Eq (UnOp (LstLen, LVar rest), LVar z) in
            extend_with len_rest;
            `Replace lst_eq
        (* Sublist *)
        | Eq (LstSub (lst, start, num), sl) | Eq (sl, LstSub (lst, start, num))
          ->
            let prefix_lvar = LVar.alloc () in
            let suffix_lvar = LVar.alloc () in
            vars_to_kill :=
              SS.add prefix_lvar (SS.add suffix_lvar !vars_to_kill);
            let suffix_len =
              let open Expr.Infix in
              Expr.list_length lst - (start + num)
            in
            let suffix_len =
              Reduction.reduce_lexpr ~pfs:lpfs ~gamma suffix_len
            in
            L.verbose (fun fmt ->
                fmt "Reduced suffix length: %a" Expr.pp suffix_len);
            let lst_eq =
              if suffix_len = Expr.zero_i then
                Formula.Eq (lst, NOp (LstCat, [ LVar prefix_lvar; sl ]))
              else
                Formula.Eq
                  (lst, NOp (LstCat, [ LVar prefix_lvar; sl; LVar suffix_lvar ]))
            in
            let len_pr = Formula.Eq (UnOp (LstLen, LVar prefix_lvar), start) in
            let len_sl = Formula.Eq (UnOp (LstLen, sl), num) in
            extend_with len_pr;
            extend_with len_sl;
            `Replace lst_eq
        | Eq (le1, le2) -> (
            let te1, _ = Typing.type_lexpr gamma le1 in
            let te2, _ = Typing.type_lexpr gamma le2 in
            match (te1, te2) with
            | Some te1, Some te2 when te1 <> te2 ->
                stop_explain
                  (Printf.sprintf "Type mismatch: %s:%s -> %s:%s"
                     ((Fmt.to_to_string Expr.pp) le1)
                     (Type.str te1)
                     ((Fmt.to_to_string Expr.pp) le2)
                     (Type.str te2))
            | _, _ -> (
                match (le1, le2) with
                | UnOp (LstLen, LVar x), UnOp (LstLen, LVar y) when x <> y ->
                    let x, y =
                      match
                        (Names.is_spec_var_name x, Names.is_spec_var_name y)
                      with
                      | true, false -> (x, y)
                      | false, true -> (y, x)
                      | _ -> (x, y)
                    in
                    PFS.subst_expr_for_expr
                      (UnOp (LstLen, LVar y))
                      (UnOp (LstLen, LVar x))
                      lpfs;
                    `Replace whole
                | Lit (Loc _), ALoc _ | ALoc _, Lit (Loc _) ->
                    (* TODO: What should actually happen here... *)
                    stop_explain
                      "Abtract location never equal to a concrete location"
                    (* SESubst.put result aloc (Lit (Loc lloc));
                       let temp_subst = SESubst.init [ aloc, Lit (Loc lloc) ] in
                         PFS.substitution_in_place temp_subst lpfs *)
                | ALoc alocl, ALoc alocr when matching ->
                    L.verbose (fun fmt ->
                        fmt "Two equal alocs: %s and %s" alocl alocr);
                    SESubst.put result (ALoc alocr) (ALoc alocl);
                    let temp_subst =
                      SESubst.init [ (ALoc alocr, ALoc alocl) ]
                    in
                    PFS.substitution temp_subst lpfs;
                    let substituted =
                      SESubst.substitute_formula ~partial:true temp_subst whole
                    in
                    rec_call substituted
                | ALoc alocl, ALoc alocr when not matching ->
                    if alocl = alocr then `Filter
                    else
                      stop_explain
                        "Two different abstract locations are never equal"
                (* Equal variables - what happens if they are numbers? *)
                | LVar v1, LVar v2 when v1 = v2 -> `Filter
                (* Variable and something else *)
                | LVar v, le | le, LVar v -> (
                    (* Understand, if there are two lvars, which should be substituted *)
                    let v, (le : Expr.t) =
                      match le with
                      | LVar w -> (
                          let save_v = save_all || SS.mem v vars_to_save in
                          let save_w = save_all || SS.mem w vars_to_save in
                          match (save_v, save_w) with
                          | true, false -> (w, LVar v)
                          | true, true -> (v, le)
                          | false, true -> (v, le)
                          | false, false ->
                              if
                                Names.is_spec_var_name v
                                && not (Names.is_spec_var_name w)
                              then (w, LVar v)
                              else (v, le))
                      | _ -> (v, le)
                    in

                    let lvars_le = Expr.lvars le in
                    match SS.mem v lvars_le with
                    (* Cannot substitute if variable on both sides or not substitutable *)
                    | true -> `Replace whole
                    | false -> (
                        let ( let* ) = Result.bind in
                        let res : (unit, string) result =
                          let tv, _ = Typing.type_lexpr gamma (LVar v) in
                          let tle, _ = Typing.type_lexpr gamma le in
                          match (tv, tle) with
                          | Some tv, Some tle when tv <> tle ->
                              Error "Type mismatch"
                          | _ ->
                              let temp_subst = SESubst.init [ (LVar v, le) ] in
                              PFS.substitution temp_subst lpfs;

                              (if SESubst.mem result (LVar v) then
                                 let le' =
                                   Option.get (SESubst.get result (LVar v))
                                 in
                                 (* L.(
                                    verbose (fun m ->
                                        m "Multiples in subst: %s %s"
                                          ((Fmt.to_to_string Expr.pp) le)
                                          ((Fmt.to_to_string Expr.pp) le'))); *)
                                 if le <> le' then
                                   PFS.extend lpfs (Eq (le, le')));
                              SESubst.iter result (fun x le ->
                                  let sle =
                                    SESubst.subst_in_expr temp_subst
                                      ~partial:true le
                                  in
                                  SESubst.put result x sle);
                              SESubst.put result (LVar v) le;

                              existentials := SS.remove v !existentials;

                              (* Understand gamma if subst is another LVar *)
                              let* () =
                                match le with
                                | LVar v' -> (
                                    match Type_env.get gamma v with
                                    | None -> Ok ()
                                    | Some t -> (
                                        match Type_env.get gamma v' with
                                        | None ->
                                            Type_env.update gamma v' t;
                                            Ok ()
                                        | Some t' ->
                                            if t <> t' then
                                              Error "Type mismatch"
                                            else Ok ()))
                                | _ -> Ok ()
                              in

                              (* Remove (or add) from (or to) gamma *)
                              let* () =
                                match save_all || SS.mem v vars_to_save with
                                | true -> (
                                    let le_type, _ =
                                      Typing.type_lexpr gamma le
                                    in
                                    match le_type with
                                    | None -> Ok ()
                                    | Some t -> (
                                        match Type_env.get gamma v with
                                        | None ->
                                            Type_env.update gamma v t;
                                            Ok ()
                                        | Some tv ->
                                            if t <> tv then
                                              Error "Type mismatch"
                                            else Ok ()))
                                | false ->
                                    Type_env.remove gamma v;
                                    Ok ()
                              in
                              Ok ()
                        in
                        match res with
                        | Error s -> stop_explain s
                        | Ok () -> `Filter))
                | UnOp (TypeOf, LVar v), Lit (Type t)
                | Lit (Type t), UnOp (TypeOf, LVar v) -> (
                    match Type_env.get gamma v with
                    | None ->
                        Type_env.update gamma v t;
                        `Filter
                    | Some tv ->
                        if t <> tv then stop_explain "Type mismatch"
                        else `Filter)
                | _, _ -> `Replace whole))
        (* All other cases *)
        | _ -> `Replace whole
      in

      let analyse_list_structure pfs =
        L.verbose (fun fmt -> fmt "Analysing list structure.");
        let map_add k v map =
          match Expr.Map.mem k map with
          | true -> Expr.Map.add k (Expr.Set.add v (Expr.Map.find k map)) map
          | false -> Expr.Map.add k (Expr.Set.singleton v) map
        in

        let map_map_add k v x map =
          let xmap =
            match Expr.Map.mem k map with
            | true -> Expr.Map.find k map
            | false -> Expr.Map.empty
          in
          Expr.Map.add k (map_add v x xmap) map
        in

        let lens, cats, xcats =
          PFS.fold_left
            (fun (lens, cats, xcats) pf ->
              match pf with
              (* List length direct equality *)
              | Eq (UnOp (LstLen, LVar x), UnOp (LstLen, LVar y))
                when not (String.equal x y) ->
                  let lens = map_add (UnOp (LstLen, LVar y)) (LVar x) lens in
                  (map_add (UnOp (LstLen, LVar x)) (LVar y) lens, cats, xcats)
              (* List length equals some other expression on the right *)
              | Eq (UnOp (LstLen, LVar x), rhs)
                when not (List.mem (Expr.LVar x) (Expr.base_elements rhs)) ->
                  (map_add rhs (LVar x) lens, cats, xcats)
              (* List length equals some other expression on the left *)
              | Eq (lhs, UnOp (LstLen, LVar x))
                when not (List.mem (Expr.LVar x) (Expr.base_elements lhs)) ->
                  (map_add lhs (LVar x) lens, cats, xcats)
              (*************** CATS **************)
              (* Two cats *)
              | Eq (NOp (LstCat, LVar a :: b), NOp (LstCat, LVar c :: d))
                when a <> c ->
                  let cats =
                    map_map_add
                      (NOp (LstCat, LVar c :: d))
                      (Expr.LVar a)
                      (NOp (LstCat, b))
                      cats
                  in
                  let xcats =
                    map_add
                      (NOp (LstCat, LVar a :: b))
                      (NOp (LstCat, LVar c :: d))
                      xcats
                  in
                  ( lens,
                    map_map_add
                      (NOp (LstCat, LVar a :: b))
                      (Expr.LVar c)
                      (NOp (LstCat, d))
                      cats,
                    map_add
                      (NOp (LstCat, LVar c :: d))
                      (NOp (LstCat, LVar a :: b))
                      xcats )
              (* One cat on the left *)
              | Eq (NOp (LstCat, LVar a :: b), rhs) ->
                  ( lens,
                    map_map_add rhs (Expr.LVar a) (NOp (LstCat, b)) cats,
                    map_add (NOp (LstCat, LVar a :: b)) rhs xcats )
              (* One cat on the right *)
              | Eq (lhs, NOp (LstCat, LVar a :: b)) ->
                  ( lens,
                    map_map_add lhs (Expr.LVar a) (NOp (LstCat, b)) cats,
                    map_add (NOp (LstCat, LVar a :: b)) lhs xcats )
              | _ -> (lens, cats, xcats))
            (Expr.Map.empty, Expr.Map.empty, Expr.Map.empty)
            pfs
        in
        let lens =
          Expr.Map.filter (fun _ eqlens -> Expr.Set.cardinal eqlens > 1) lens
        in
        Expr.Map.iter
          (fun _ eqs ->
            Expr.Map.iter
              (fun _ eqs ->
                let eqs = Expr.Set.elements eqs in
                for i = 0 to List.length eqs - 1 do
                  for j = i + 1 to List.length eqs - 1 do
                    let x = List.nth eqs i in
                    let y = List.nth eqs j in
                    L.verbose (fun fmt ->
                        fmt "ULTRA LSTCAT: cat equality: %a and %a" Expr.pp x
                          Expr.pp y);
                    PFS.extend pfs (Eq (x, y))
                  done
                done)
              eqs)
          cats;
        Expr.Map.iter
          (fun _ eqs ->
            let eqs = Expr.Set.elements eqs in
            for i = 0 to List.length eqs - 1 do
              for j = i + 1 to List.length eqs - 1 do
                let x = List.nth eqs i in
                let y = List.nth eqs j in
                L.verbose (fun fmt ->
                    fmt "ULTRA LSTCAT: xcat equality: %a and %a" Expr.pp x
                      Expr.pp y);
                PFS.extend pfs (Eq (x, y))
              done
            done)
          xcats;
        let cats =
          Expr.Map.filter (fun _ eqlens -> Expr.Map.cardinal eqlens > 1) cats
        in
        let cats =
          Expr.Map.map
            (fun eqlens ->
              Expr.Map.map (fun eqs -> Expr.Set.min_elt eqs) eqlens)
            cats
        in
        Expr.Map.iter
          (fun _ eqlens ->
            Expr.Map.iter
              (fun _ (eqcats : Expr.t Expr.Map.t) ->
                let domain, _ = List.split (Expr.Map.bindings eqcats) in
                let domain = Expr.Set.of_list domain in
                let inter = Expr.Set.elements (Expr.Set.inter eqlens domain) in
                let len_inter = List.length inter in
                if len_inter > 1 then
                  L.verbose (fun fmt ->
                      fmt "ULTRA LSTCAT: found equals: %a"
                        Fmt.(brackets (list ~sep:comma Expr.pp))
                        inter);
                for i = 0 to len_inter - 1 do
                  for j = i + 1 to len_inter - 1 do
                    let x = List.nth inter i in
                    let y = List.nth inter j in
                    L.verbose (fun fmt ->
                        fmt "ULTRA LSTCAT: head equality: %a and %a" Expr.pp x
                          Expr.pp y);
                    PFS.extend pfs (Eq (x, y));
                    let x = Expr.Map.find x eqcats in
                    let y = Expr.Map.find y eqcats in
                    L.verbose (fun fmt ->
                        fmt "ULTRA LSTCAT: tail equality: %a and %a" Expr.pp x
                          Expr.pp y);
                    PFS.extend pfs (Eq (x, y))
                  done
                done)
              cats)
          lens
      in

      (*****************************************
        ********* THIS IS THE BEGINNING *********
        *****************************************)
      PFS.sort lpfs;
      let old_pfs = ref (PFS.init ()) in
      let iteration_count = ref 0 in

      while not (PFS.equal lpfs !old_pfs) do
        iteration_count := !iteration_count + 1;
        L.tmi (fun fmt -> fmt "Iteration: %d" !iteration_count);
        L.tmi (fun fmt -> fmt "PFS:\n%a" PFS.pp lpfs);

        old_pfs := PFS.copy lpfs;

        (* Step 1 - Simplify unit types and sort *)
        simplify_unit_types ();
        PFS.sort lpfs;

        (* Step 2 - Main loop *)
        if PFS.filter_map_stop (filter_mapper_formula lpfs) lpfs then
          pfs_false lpfs rpfs;

        PFS.substitution result lpfs;

        if
          PFS.length lpfs = 0
          || (PFS.length lpfs > 0 && not (PFS.get_nth 0 lpfs = Some False))
        then (
          (* Step 3 - Bring back my variables *)
          SESubst.iter result (fun v le ->
              match v with
              | LVar v ->
                  if
                    (not (SS.mem v !vars_to_kill))
                    && (save_all
                       || (kill_new_lvars && SS.mem v vars_to_save)
                       || ((not kill_new_lvars) && vars_to_save <> SS.empty))
                    && not (Names.is_aloc_name v)
                  then PFS.extend lpfs (Eq (LVar v, le))
              | _ -> ());

          sanitise_pfs_no_store ~matching gamma lpfs;

          let current_lvars = SS.union (PFS.lvars lpfs) (PFS.lvars rpfs) in
          Type_env.iter gamma (fun v _ ->
              if SS.mem v !vars_to_kill && not (SS.mem v current_lvars) then
                Type_env.remove gamma v);

          Type_env.iter gamma (fun v t ->
              match t with
              | Type.ListType ->
                  PFS.extend lpfs
                    (ILessEq (Expr.zero_i, UnOp (LstLen, Expr.from_var_name v)))
              | _ -> ());

          analyse_list_structure lpfs;

          PFS.sort lpfs)
      done;

      L.verbose (fun m -> m "PFS/Gamma simplification completed:\n");
      L.(verbose (fun m -> m "PFS:@\n%a@\n" PFS.pp lpfs));
      L.(verbose (fun m -> m "Gamma:@\n%a@\n" Type_env.pp gamma));

      let cached_simplification =
        {
          simpl_gamma = Type_env.to_list gamma;
          simpl_pfs = PFS.to_list lpfs;
          simpl_existentials = !existentials;
          subst = SESubst.copy result;
        }
      in
      Hashtbl.replace simplification_cache key cached_simplification;

      (* Utils.Statistics.update_statistics "FOS: SimplifyPFSandGamma"
         (Sys.time () -. t); *)

      (* Step 5 - Sort ALoc transitivity *)
      let rec find_loc_all_the_way aloc res =
        let f = find_loc_all_the_way in
        match SESubst.get result aloc with
        | None -> res
        | Some res -> f res res
      in
      let aloc_subst =
        SESubst.fold result
          (fun l _ acc ->
            match l with
            | ALoc aloc ->
                (Expr.ALoc aloc, find_loc_all_the_way (ALoc aloc) (ALoc aloc))
                :: acc
            | _ -> acc)
          []
      in
      let aloc_subst = SESubst.init aloc_subst in
      let () =
        SESubst.iter aloc_subst (fun old_aloc new_aloc ->
            SESubst.put result old_aloc new_aloc;
            PFS.substitution aloc_subst lpfs;
            PFS.substitution aloc_subst rpfs)
      in

      (* Step 6 - Conclude *)
      (result, !existentials)

let simplify_implication
    ~matching
    (exists : SS.t)
    (lpfs : PFS.t)
    (rpfs : PFS.t)
    (gamma : Type_env.t) =
  (* let t = Sys.time () in *)
  List.iter
    (fun (pf : Formula.t) ->
      match pf with
      | Eq (NOp (LstCat, lex), NOp (LstCat, ley)) ->
          let flen_eq =
            Reduction.reduce_formula ~gamma ~pfs:lpfs
              (Eq
                 ( UnOp (LstLen, NOp (LstCat, lex)),
                   UnOp (LstLen, NOp (LstCat, ley)) ))
          in
          PFS.extend lpfs flen_eq
      | _ -> ())
    (PFS.to_list lpfs);
  let subst, exists =
    simplify_pfs_and_gamma ~matching lpfs gamma ~rpfs ~existentials:exists
  in
  PFS.substitution subst rpfs;

  (* Additional *)
  PFS.map_inplace (Reduction.reduce_formula ~rpfs:true ~gamma ~pfs:lpfs) rpfs;
  L.verbose (fun fmt -> fmt "REDUCED RPFS:\n%a" PFS.pp rpfs);

  sanitise_pfs_no_store ~matching gamma rpfs;
  clean_up_stuff lpfs rpfs;

  L.(
    verbose (fun m ->
        m
          "Finished existential simplification.@\n\
           Existentials:@\n\
           %a@\n\
           @[<hv 2>Left:@ %a@]@\n\
           @[<hv 2>Right:@ %a@]\n\
           Gamma:\n\
           %a\n"
          (Fmt.iter ~sep:Fmt.comma SS.iter Fmt.string)
          exists PFS.pp lpfs PFS.pp rpfs Type_env.pp gamma));
  (* Utils.Statistics.update_statistics "FOS: SimplifyImplication"
     (Sys.time () -. t); *)
  exists

let admissible_assertion (a : Asrt.t) : bool =
  L.(
    tmi (fun m ->
        m "-----------\nAdmissible?\n----------\n%s"
          ((Fmt.to_to_string Asrt.full_pp) a)));

  let pfs = PFS.init () in
  let gamma = Type_env.init () in

  let a = Asrt.pvars_to_lvars a in

  let rec separate (a : Asrt.t) =
    match a with
    | Star (a1, a2) ->
        separate a1;
        separate a2
    | Pure f -> PFS.extend pfs f
    | Types ets ->
        List.iter
          (fun (le, t) ->
            match (le : Expr.t) with
            | LVar x | PVar x -> Type_env.update gamma x t
            | _ -> ())
          ets
    | _ -> ()
  in
  try
    separate a;
    let _ = simplify_pfs_and_gamma ~kill_new_lvars:true pfs gamma in
    let res = not (PFS.mem pfs Formula.False) in
    if res then L.tmi (fun m -> m "Admissible !!")
    else L.tmi (fun m -> m "Not admissible !!");
    res
  with e ->
    L.tmi (fun m ->
        m "Considered not admissible because of exception!! %s"
          (Printexc.to_string e));
    false
