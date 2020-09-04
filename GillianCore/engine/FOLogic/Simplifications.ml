open Containers
open Generators
open SVal
module L = Logging

exception FoundIt of Expr.t

exception UnionInUnion of Expr.t list

let interactive = ref false

type simpl_key_type = {
  kill_new_lvars : bool option;
  gamma_list : (Var.t * Type.t) list;
  pfs_list : Formula.t list;
  existentials : SS.t;
  unification : bool;
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

let rec find_me_Im_a_loc pfs lvar =
  match (pfs : Formula.t list) with
  | [] -> None
  | Eq (lvar', Expr.ALoc loc) :: rest
  | Eq (lvar', Lit (Loc loc)) :: rest
  | Eq (ALoc loc, lvar') :: rest
  | Eq (Lit (Loc loc), lvar') :: rest ->
      if lvar = lvar' then Some loc else find_me_Im_a_loc rest lvar
  | _ :: rest -> find_me_Im_a_loc rest lvar

let find_me_in_the_pi (pfs : PFS.t) nle =
  PFS.fold_left
    (fun ac a ->
      match (a : Formula.t) with
      | Eq (LVar lvar, le) | Eq (le, LVar lvar) ->
          if le = nle then Some lvar else ac
      | _ -> ac)
    None pfs

let rec replace_nle_with_lvars pfs nle =
  match nle with
  | Expr.BinOp (le, op, le') -> (
      match find_me_in_the_pi pfs nle with
      | Some lvar -> Expr.LVar lvar
      | None      ->
          let lhs = replace_nle_with_lvars pfs le in
          let rhs = replace_nle_with_lvars pfs le' in
          BinOp (lhs, op, rhs) )
  | UnOp (op, le)            -> (
      match find_me_in_the_pi pfs nle with
      | Some lvar -> LVar lvar
      | None      -> UnOp (op, replace_nle_with_lvars pfs le) )
  | EList le                 -> (
      match find_me_in_the_pi pfs nle with
      | Some lvar -> LVar lvar
      | None      ->
          let le_list =
            List.map (fun le' -> replace_nle_with_lvars pfs le') le
          in
          EList le_list )
  | ESet le                  -> (
      match find_me_in_the_pi pfs nle with
      | Some lvar -> LVar lvar
      | None      ->
          let le_list =
            List.map (fun le' -> replace_nle_with_lvars pfs le') le
          in
          ESet le_list )
  | NOp (op, le)             -> (
      match find_me_in_the_pi pfs nle with
      | Some lvar -> LVar lvar
      | None      ->
          let le_list =
            List.map (fun le' -> replace_nle_with_lvars pfs le') le
          in
          NOp (op, le_list) )
  | _                        -> nle

let all_set_literals lset =
  List.fold_left
    (fun x le ->
      let result =
        match le with
        | Expr.ESet _ -> true
        | _           -> false
      in
      x && result)
    true lset

(* Reduction of assertions *)

(*************************************)
(** Symbolic state simplification   **)

(*************************************)

let reduce_pfs_in_place ?(unification = false) store gamma (pfs : PFS.t) =
  PFS.map_inplace (Reduction.reduce_formula ~unification ~gamma ~pfs) pfs

let sanitise_pfs ?(unification = false) store gamma pfs =
  let old_pfs = ref (PFS.init ()) in
  while not (PFS.equal !old_pfs pfs) do
    old_pfs := PFS.copy pfs;
    reduce_pfs_in_place ~unification store gamma pfs
  done;
  PFS.remove_duplicates pfs

let sanitise_pfs_no_store_no_gamma ?(unification = false) =
  sanitise_pfs ~unification (Hashtbl.create 1) (TypEnv.init ())

let sanitise_pfs_no_store ?(unification = false) =
  sanitise_pfs ~unification (Hashtbl.create 1)

let filter_gamma_pfs pfs gamma =
  let pfs_vars = PFS.lvars pfs in
  TypEnv.filter_vars_in_place gamma pfs_vars

(* *********** *)
(*   CLEANUP   *)
(* *********** *)

let clean_up_stuff exists (left : PFS.t) (right : PFS.t) =
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
      | _              -> Not pf
    in
    Formula.Set.exists (eq_or_sym npf) sleft
  in
  if PFS.filter_stop_cond ~keep ~cond right then (
    let () = PFS.clear right in
    PFS.set left [ False ];
    PFS.filter
      (function
        | Not (Eq ((EList _ | BinOp _ | UnOp _), Lit Empty)) -> false
        | _ -> true)
      right )

(* Set intersections *)
let get_set_intersections pfs =
  let lvars = Hashtbl.create 1 in
  let rvars = Hashtbl.create 1 in

  List.iter
    (fun pf ->
      match (pf : Formula.t) with
      | ForAll
          ( [ (x, Some NumberType) ],
            Or (Not (SetMem (LVar y, LVar set)), Less (LVar elem, LVar z)) )
        when x = y && x = z ->
          L.(verbose (fun m -> m "Got left: %s, %s" elem set));
          Hashtbl.add lvars elem set
      | ForAll
          ( [ (x, Some NumberType) ],
            Or (Not (SetMem (LVar y, LVar set)), Less (LVar z, LVar elem)) )
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
      | Less (LVar v1, LVar v2) -> (
          match (Hashtbl.mem lvars v1, Hashtbl.mem lvars v2) with
          | true, true                ->
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
          | _, _                      -> () )
      | _                       -> ())
    pfs;
  let intersections = List.map (fun s -> Expr.Set.elements s) !intersections in
  List.sort compare intersections

let resolve_set_existentials
    (lpfs : PFS.t) (rpfs : PFS.t) exists (gamma : TypEnv.t) =
  let exists = ref exists in

  let set_exists =
    SS.filter (fun x -> TypEnv.get gamma x = Some SetType) !exists
  in
  if SS.cardinal set_exists > 0 then (
    let intersections =
      get_set_intersections (PFS.to_list lpfs @ PFS.to_list rpfs)
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
                 (fun (u : Expr.t) ->
                   ( match (u : Expr.t) with
                     | ESet x ->
                         List.map (fun (x : Expr.t) -> (ESet [ x ] : Expr.t)) x
                     | _      -> [ u ]
                     : Expr.t list ))
                 ul)
          in
          let ur =
            List.flatten
              (List.map
                 (fun (u : Expr.t) ->
                   ( match u with
                     | ESet x -> List.map (fun x -> (ESet [ x ] : Expr.t)) x
                     | _      -> [ u ]
                     : Expr.t list ))
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
                  while TypEnv.mem gamma v do
                    TypEnv.remove gamma v
                  done;
                  None
              | _ -> Some (Formula.Eq (lhs, rhs))
            else Some formula_to_filter )
          else Some formula_to_filter
      | _ -> Some formula_to_filter
    in
    PFS.filter_map filter_map_fun rpfs;
    (rpfs, !exists, gamma) )
  else (rpfs, !exists, gamma)

let find_impossible_unions
    (lpfs : PFS.t) (rpfs : PFS.t) exists (gamma : TypEnv.t) =
  let exists = ref exists in

  let set_exists =
    SS.filter (fun x -> TypEnv.get gamma x = Some SetType) !exists
  in
  if SS.cardinal set_exists > 0 then (
    let intersections =
      get_set_intersections (PFS.to_list lpfs @ PFS.to_list rpfs)
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
    let test_formula = function
      | Formula.Eq (NOp (SetUnion, ul), NOp (SetUnion, ur)) ->
          let sul = Expr.Set.of_list ul in
          let sur = Expr.Set.of_list ur in
          L.verbose (fun m ->
              m "Find impossible unions: I have found a union equality.");

          (* Just for the left *)
          Expr.Set.exists
            (fun s1 ->
              let must_not_intersect =
                List.map
                  (fun s -> List.sort compare [ s; s1 ])
                  (Expr.Set.elements sur)
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
              List.mem true must_not_intersect
              && not (List.mem false must_not_intersect))
            sul
      | _ -> false
    in
    if PFS.exists test_formula rpfs then
      (PFS.of_list [ Formula.False ], SS.empty, TypEnv.init ())
    else (rpfs, !exists, gamma) )
  else (rpfs, !exists, gamma)

let trim_down (exists : SS.t) (lpfs : PFS.t) (rpfs : PFS.t) gamma =
  let lhs_lvars = PFS.lvars lpfs in
  let rhs_lvars = PFS.lvars rpfs in
  let diff = SS.diff (SS.diff rhs_lvars lhs_lvars) exists in

  if
    PFS.length rpfs = 1
    &&
    match PFS.get_nth 0 rpfs with
    | Some
        ( Eq (LVar v1, LVar v2)
        | Less (LVar v1, LVar v2)
        | LessEq (LVar v1, LVar v2)
        | Not (Eq (LVar v1, LVar v2))
        | Not (Less (LVar v1, LVar v2))
        | Not (LessEq (LVar v1, LVar v2)) )
      when v1 <> v2 && (SS.mem v1 diff || SS.mem v2 diff) -> true
    | _ -> false
  then (false, exists, lpfs, rpfs, gamma)
  else
    (* FIXME: THIS IS UNSOUND, FIX *)
    let () =
      PFS.filter
        (fun pf ->
          let pf_lvars = Formula.lvars pf in
          let inter_empty = SS.inter rhs_lvars pf_lvars = SS.empty in
          not inter_empty)
        lpfs
    in
    (true, exists, lpfs, rpfs, gamma)

(**
  Pure entailment: simplify pure formulae and typing environment

  @param pfs Pure formulae (modified destructively)
  @param gamma Typing environment (modified destructively)
  @param vars_to_save Logical variables that cannot be deleted
  @return Substitution from logical variables to logical expressions
*)
let simplify_pfs_and_gamma
    ?(unification = false)
    ?(kill_new_lvars : bool option)
    ?(save_spec_vars : (SS.t * bool) option)
    ?(existentials : SS.t option)
    (lpfs : PFS.t)
    ?(rpfs : PFS.t option)
    (gamma : TypEnv.t) : SESubst.t * SS.t =
  L.verbose (fun m -> m "Simplifications.simplify_pfs_and_gamma");
  L.verbose (fun m ->
      m "With unification: %s" (if unification then "Yes" else "No"));
  L.verbose (fun m -> m "  @[%a@]" PFS.pp lpfs);
  L.verbose (fun m -> m "  @[%a@]" TypEnv.pp gamma);

  let rpfs : PFS.t = Option.value ~default:(PFS.init ()) rpfs in
  let existentials : SS.t ref =
    ref (Option.value ~default:SS.empty existentials)
  in

  let key : simpl_key_type =
    {
      kill_new_lvars;
      gamma_list = TypEnv.to_list gamma;
      pfs_list = PFS.to_list lpfs;
      existentials = !existentials;
      unification;
      save_spec_vars (* rpfs_lvars = (PFS.lvars rpfs) *);
    }
  in
  match Hashtbl.mem simplification_cache key with
  | true  ->
      (* update_statistics "Simpl: cached" 0.; *)
      let { simpl_gamma; simpl_pfs; simpl_existentials; subst } =
        Hashtbl.find simplification_cache key
      in
      TypEnv.reset gamma simpl_gamma;
      PFS.set lpfs simpl_pfs;

      (* Deal with rpfs *)
      if PFS.length lpfs > 0 && PFS.get_nth 0 lpfs == Some False then (
        PFS.clear rpfs;
        PFS.extend rpfs True );

      (SESubst.copy subst, simpl_existentials)
  | false ->
      let result = SESubst.init [] in

      let vars_to_save, save_all =
        Option.value ~default:(SS.empty, false) save_spec_vars
      in

      let vars_to_kill = ref SS.empty in
      let kill_new_lvars = Option.value ~default:false kill_new_lvars in

      (* Unit types *)
      let simplify_unit_types () =
        TypEnv.iter gamma (fun x t ->
            let e = Expr.from_var_name x in
            match t with
            | UndefinedType -> SESubst.put result e (Lit Undefined)
            | NullType      -> SESubst.put result e (Lit Null)
            | EmptyType     -> SESubst.put result e (Lit Empty)
            | NoneType      -> SESubst.put result e (Lit Nono)
            | _             -> ())
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
        let whole = Reduction.reduce_formula ~unification ~gamma ~pfs pf in
        match whole with
        (* These we must not encounter here *)
        | ForAll (bt, _) ->
            let lx, _ = List.split bt in
            List.iter (fun x -> TypEnv.remove gamma x) lx;
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
            let te1, _, _ = Typing.type_lexpr gamma le1 in
            let te2, _, _ = Typing.type_lexpr gamma le2 in
            match (te1, te2) with
            | Some te1, Some te2 when te1 <> te2 -> `Filter
            | Some te1, Some te2
              when te1 = te2
                   && ( te1 = UndefinedType || te1 = NullType || te1 = EmptyType
                      || te1 = NoneType ) ->
                stop_explain "Inequality of two undefined/null/empty/none"
            | _ -> `Replace whole )
        | Eq (BinOp (lst, LstNth, idx), elem)
        | Eq (elem, BinOp (lst, LstNth, idx)) -> (
            match idx with
            | Lit (Num nx) when Arith_Utils.is_int nx ->
                let prepend_lvars =
                  Array.to_list
                    (Array.init (int_of_float nx) (fun _ -> LVar.alloc ()))
                in
                let append_lvar = LVar.alloc () in
                (* Fresh variables can be removed *)
                vars_to_kill :=
                  SS.add append_lvar
                    (SS.union !vars_to_kill (SS.of_list prepend_lvars));
                let prepend = List.map (fun x -> Expr.LVar x) prepend_lvars in
                let append = Expr.LVar append_lvar in
                rec_call
                  (Eq
                     ( lst,
                       NOp
                         ( LstCat,
                           [ EList (List.append prepend [ elem ]); append ] ) ))
            | _ -> `Replace whole )
        | Eq (UnOp (LstLen, le), Lit (Num len))
        | Eq (Lit (Num len), UnOp (LstLen, le)) -> (
            match Arith_Utils.is_int len with
            | false -> stop_explain "List length not an integer."
            | true  ->
                let len = int_of_float len in
                let le_vars =
                  Array.to_list (Array.init len (fun _ -> LVar.alloc ()))
                in
                vars_to_kill := SS.union !vars_to_kill (SS.of_list le_vars);
                let le' = List.map (fun x -> Expr.LVar x) le_vars in
                rec_call (Eq (le, EList le')) )
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
            | None                -> `Replace whole
            | Some (pf, new_vars) ->
                extend_with pf;
                vars_to_kill := SS.union !vars_to_kill new_vars;
                `Replace whole )
        (* Sublist *)
        | Eq (LstSub (lst, start, num), sl) | Eq (sl, LstSub (lst, start, num))
          ->
            let prefix_lvar = LVar.alloc () in
            let suffix_lvar = LVar.alloc () in
            vars_to_kill :=
              SS.add prefix_lvar (SS.add suffix_lvar !vars_to_kill);
            let lst_eq =
              Formula.Eq
                (lst, NOp (LstCat, [ LVar prefix_lvar; sl; LVar suffix_lvar ]))
            in
            let len_pr = Formula.Eq (UnOp (LstLen, LVar prefix_lvar), start) in
            let len_sl = Formula.Eq (UnOp (LstLen, sl), num) in
            extend_with len_pr;
            extend_with len_sl;
            `Replace lst_eq
        | Eq (le1, le2) -> (
            let te1, _, _ = Typing.type_lexpr gamma le1 in
            let te2, _, _ = Typing.type_lexpr gamma le2 in
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
                      | _           -> (x, y)
                    in
                    PFS.subst_expr_for_expr
                      (UnOp (LstLen, LVar y))
                      (UnOp (LstLen, LVar x))
                      lpfs;
                    `Replace whole
                | Lit (Loc lloc), ALoc aloc | ALoc aloc, Lit (Loc lloc) ->
                    (* TODO: What should actually happen here... *)
                    stop_explain
                      "Abtract location never equal to a concrete location"
                    (* SESubst.put result aloc (Lit (Loc lloc));
                       let temp_subst = SESubst.init [ aloc, Lit (Loc lloc) ] in
                         PFS.substitution_in_place temp_subst lpfs *)
                | ALoc alocl, ALoc alocr when unification ->
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
                | ALoc alocl, ALoc alocr when not unification ->
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
                          | true, false  -> (w, LVar v)
                          | true, true   -> (v, le)
                          | false, true  -> (v, le)
                          | false, false ->
                              if
                                Names.is_spec_var_name v
                                && not (Names.is_spec_var_name w)
                              then (w, LVar v)
                              else (v, le) )
                      | _      -> (v, le)
                    in

                    let lvars_le = Expr.lvars le in
                    match SS.mem v lvars_le with
                    (* Cannot substitute if variable on both sides or not substitutable *)
                    | true -> `Replace whole
                    | false -> (
                        let ( let* ) = Result.bind in
                        let res : (unit, string) result =
                          let tv, _, _ = Typing.type_lexpr gamma (LVar v) in
                          let tle, _, _ = Typing.type_lexpr gamma le in
                          match (tv, tle) with
                          | Some tv, Some tle when tv <> tle ->
                              Error "Type mismatch"
                          | _ ->
                              let temp_subst = SESubst.init [ (LVar v, le) ] in
                              PFS.substitution temp_subst lpfs;

                              ( if SESubst.mem result (LVar v) then
                                let le' =
                                  Option.get (SESubst.get result (LVar v))
                                in
                                (* L.(
                                   verbose (fun m ->
                                       m "Multiples in subst: %s %s"
                                         ((Fmt.to_to_string Expr.pp) le)
                                         ((Fmt.to_to_string Expr.pp) le'))); *)
                                if le <> le' then PFS.extend lpfs (Eq (le, le'))
                              );
                              SESubst.iter result (fun x le ->
                                  let sle =
                                    SESubst.subst_in_expr temp_subst true le
                                  in
                                  SESubst.put result x sle);
                              SESubst.put result (LVar v) le;

                              existentials := SS.remove v !existentials;

                              (* Understand gamma if subst is another LVar *)
                              let* () =
                                match le with
                                | LVar v' -> (
                                    match TypEnv.get gamma v with
                                    | None   -> Ok ()
                                    | Some t -> (
                                        match TypEnv.get gamma v' with
                                        | None    ->
                                            TypEnv.update gamma v' t;
                                            Ok ()
                                        | Some t' ->
                                            if t <> t' then
                                              Error "Type mismatch"
                                            else Ok () ) )
                                | _       -> Ok ()
                              in

                              (* Remove (or add) from (or to) gamma *)
                              let* () =
                                match save_all || SS.mem v vars_to_save with
                                | true  -> (
                                    let le_type, _, _ =
                                      Typing.type_lexpr gamma le
                                    in
                                    match le_type with
                                    | None   -> Ok ()
                                    | Some t -> (
                                        match TypEnv.get gamma v with
                                        | None    ->
                                            TypEnv.update gamma v t;
                                            Ok ()
                                        | Some tv ->
                                            if t <> tv then
                                              Error "Type mismatch"
                                            else Ok () ) )
                                | false ->
                                    TypEnv.remove gamma v;
                                    Ok ()
                              in
                              Ok ()
                        in
                        match res with
                        | Error s -> stop_explain s
                        | Ok ()   -> `Filter ) )
                | UnOp (TypeOf, LVar v), Lit (Type t)
                | Lit (Type t), UnOp (TypeOf, LVar v) -> (
                    match TypEnv.get gamma v with
                    | None    ->
                        TypEnv.update gamma v t;
                        `Filter
                    | Some tv ->
                        if t <> tv then stop_explain "Type mismatch"
                        else `Filter )
                | _, _ -> `Replace whole ) )
        (* All other cases *)
        | _ -> `Replace whole
      in

      (*****************************************
        ********* THIS IS THE BEGINNING *********
        *****************************************)
      PFS.sort lpfs;
      let old_pfs = ref (PFS.init ()) in
      let iteration_count = ref 0 in

      while not (PFS.equal lpfs !old_pfs) do
        iteration_count := !iteration_count + 1;
        L.verbose (fun fmt -> fmt "Iteration: %d" !iteration_count);
        L.verbose (fun fmt -> fmt "PFS:\n%a" PFS.pp lpfs);

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
                    && ( save_all
                       || (kill_new_lvars && SS.mem v vars_to_save)
                       || ((not kill_new_lvars) && vars_to_save <> SS.empty) )
                    && not (Names.is_aloc_name v)
                  then PFS.extend lpfs (Eq (LVar v, le))
              | _      -> ());

          sanitise_pfs_no_store ~unification gamma lpfs;
          PFS.sort lpfs;

          let current_lvars = SS.union (PFS.lvars lpfs) (PFS.lvars rpfs) in
          TypEnv.iter gamma (fun v _ ->
              if SS.mem v !vars_to_kill && not (SS.mem v current_lvars) then
                TypEnv.remove gamma v) )
      done;

      L.verbose (fun m -> m "simplify_pfs_and_gamma completed");
      L.(verbose (fun m -> m "PFS:%a" PFS.pp lpfs));
      L.(verbose (fun m -> m "Gamma:\n%a" TypEnv.pp gamma));

      let cached_simplification =
        {
          simpl_gamma = TypEnv.to_list gamma;
          simpl_pfs = PFS.to_list lpfs;
          simpl_existentials = !existentials;
          subst = SESubst.copy result;
        }
      in
      Hashtbl.replace simplification_cache key cached_simplification;
      (* Step 5 - conclude *)
      (result, !existentials)

let simplify_implication
    (exists : SS.t) (lpfs : PFS.t) (rpfs : PFS.t) (gamma : TypEnv.t) =
  List.iter
    (fun (pf : Formula.t) ->
      match pf with
      | Eq (NOp (LstCat, lex), NOp (LstCat, ley)) ->
          L.verbose (fun fmt -> fmt "SI: LstLen equality: %a" Formula.pp pf);
          let flen_eq =
            Reduction.reduce_formula ~gamma ~pfs:lpfs
              (Eq
                 ( UnOp (LstLen, NOp (LstCat, lex)),
                   UnOp (LstLen, NOp (LstCat, ley)) ))
          in
          L.verbose (fun fmt -> fmt "SI: Extending with: %a" Formula.pp flen_eq);
          PFS.extend lpfs flen_eq
      | _ -> ())
    (PFS.to_list lpfs);
  let subst, exists =
    simplify_pfs_and_gamma lpfs gamma ~rpfs ~existentials:exists
  in
  PFS.substitution subst rpfs;

  (* Additional *)
  PFS.map_inplace (Reduction.reduce_formula ~gamma ~pfs:lpfs) rpfs;

  sanitise_pfs_no_store gamma rpfs;
  clean_up_stuff exists lpfs rpfs;

  L.(
    verbose (fun m ->
        m
          "Finished existential simplification.\n\
           Existentials:\n\
           %a\n\
           Left:\n\
           %a\n\
           Right:\n\
           %a\n\
           Gamma:\n\
           %a\n"
          (Fmt.iter ~sep:(Fmt.any ", ") SS.iter Fmt.string)
          exists PFS.pp lpfs PFS.pp rpfs TypEnv.pp gamma));
  exists

let admissible_assertion (a : Asrt.t) : bool =
  L.(
    verbose (fun m ->
        m "-----------\nAdmissible?\n----------\n%s"
          ((Fmt.to_to_string Asrt.full_pp) a)));

  let pfs = PFS.init () in
  let gamma = TypEnv.init () in

  let rec separate (a : Asrt.t) =
    match a with
    | Star (a1, a2) ->
        separate a1;
        separate a2
    | Pure f        -> PFS.extend pfs f
    | Types ets     ->
        List.iter
          (fun (le, t) ->
            match (le : Expr.t) with
            | LVar x | PVar x -> TypEnv.update gamma x t
            | _               -> ())
          ets
    | _             -> ()
  in
  try
    separate a;
    let _ = simplify_pfs_and_gamma ~kill_new_lvars:true pfs gamma in
    let res = not (PFS.mem pfs Formula.False) in
    if res then L.verbose (fun m -> m "Admissible !!")
    else L.verbose (fun m -> m "Not admissible !!");
    res
  with _ -> false
