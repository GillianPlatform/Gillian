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
  subst : SVal.SSubst.t;
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
  PFS.iteri
    (fun i pf ->
      let rpf =
        Reduction.reduce_formula ~unification ?gamma:(Some gamma)
          ?pfs:(Some pfs) pf
      in
      PFS.nth_set pfs i rpf)
    pfs

let sanitise_pfs ?(unification = false) store gamma pfs =
  let old_pfs = ref (PFS.init ()) in
  while not (PFS.equal !old_pfs pfs) do
    old_pfs := PFS.copy pfs;
    reduce_pfs_in_place ~unification store gamma pfs
  done;

  let length = PFS.length pfs in
  let dindex = DynArray.init length (fun x -> false) in
  let clc = ref 0 in
  let rec find_duplicates l =
    match (l : Formula.t list) with
    | []     -> ()
    | a :: l ->
        let imem = List.mem a l in
        if imem || a = True then DynArray.set dindex !clc true;
        clc := !clc + 1;
        find_duplicates l
  in
  let ll = PFS.to_list pfs in
  find_duplicates ll;
  for i = 0 to length - 1 do
    if DynArray.get dindex (length - 1 - i) then
      PFS.nth_delete pfs (length - 1 - i)
  done

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
  let sleft = Formula.Set.of_list (PFS.to_list left) in
  let i = ref 0 in
  while !i < PFS.length right do
    let pf = PFS.nth_get right !i in
    let pf_sym pf =
      match (pf : Formula.t) with
      | Eq (e1, e2)       -> Formula.Set.mem (Eq (e2, e1)) sleft
      | Not (Eq (e1, e2)) -> Formula.Set.mem (Not (Eq (e2, e1))) sleft
      | _                 -> false
    in
    match Formula.Set.mem pf sleft || pf_sym pf with
    | false -> (
        let npf =
          match pf with
          | Not pf -> pf
          | _      -> Not pf
        in
        match Formula.Set.mem npf sleft || pf_sym npf with
        | false -> i := !i + 1
        | true  ->
            PFS.clear left;
            PFS.clear right;
            PFS.extend left False )
    | true  -> PFS.nth_delete right !i
  done;

  i := 0;
  while !i < PFS.length right do
    let pf = PFS.nth_get right !i in
    match pf with
    | Not (Eq (le, Lit Empty)) -> (
        match le with
        | EList _ | BinOp (_, _, _) | UnOp (_, _) -> PFS.nth_delete right !i
        | _ -> i := !i + 1 )
    | _                        -> i := !i + 1
  done

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

    let i = ref 0 in
    while !i < PFS.length rpfs do
      let a = PFS.nth_get rpfs !i in
      match a with
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
          L.verbose (fun m -> m "%a" Formula.pp a);

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
            if must_not_intersect = [ true ] then (
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
                  let temp_subst = SSubst.init [] in
                  SSubst.put temp_subst v rhs;
                  PFS.substitution temp_subst rpfs;
                  exists := SS.remove v !exists;
                  while TypEnv.mem gamma v do
                    TypEnv.remove gamma v
                  done;
                  PFS.nth_delete rpfs !i
              | _ ->
                  PFS.nth_set rpfs !i (Eq (lhs, rhs));
                  i := !i + 1 )
            else i := !i + 1 )
          else i := !i + 1
      | _ -> i := !i + 1
    done;

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

    try
      let i = ref 0 in
      while !i < PFS.length rpfs do
        let a = PFS.nth_get rpfs !i in
        match a with
        | Eq (NOp (SetUnion, ul), NOp (SetUnion, ur)) ->
            let sul = Expr.Set.of_list ul in
            let sur = Expr.Set.of_list ur in
            L.verbose (fun m ->
                m "Find impossible unions: I have found a union equality.");

            (* Just for the left *)
            Expr.Set.iter
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
                  List.map
                    (fun s -> List.mem s intersections)
                    must_not_intersect
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
                if must_not_intersect = [ true ] then raise (Failure "Oopsie!"))
              sul;

            (* Continue if we survived *)
            i := !i + 1
        | _ -> i := !i + 1
      done;

      (rpfs, !exists, gamma)
    with Failure _ -> (PFS.of_list [ Formula.False ], SS.empty, TypEnv.init ())
    )
  else (rpfs, !exists, gamma)

let trim_down (exists : SS.t) (lpfs : PFS.t) (rpfs : PFS.t) gamma =
  try
    let lhs_lvars = PFS.lvars lpfs in
    let rhs_lvars = PFS.lvars rpfs in
    let diff = SS.diff (SS.diff rhs_lvars lhs_lvars) exists in

    ( if PFS.length rpfs = 1 then
      let pf = PFS.nth_get rpfs 0 in
      match pf with
      | Eq (LVar v1, LVar v2)
      | Less (LVar v1, LVar v2)
      | LessEq (LVar v1, LVar v2)
      | Not (Eq (LVar v1, LVar v2))
      | Not (Less (LVar v1, LVar v2))
      | Not (LessEq (LVar v1, LVar v2)) ->
          if v1 <> v2 && (SS.mem v1 diff || SS.mem v2 diff) then
            raise (Failure "")
      | _ -> () );

    (* THIS IS UNSOUND, FIX *)
    let i = ref 0 in
    while !i < PFS.length lpfs do
      let pf = PFS.nth_get lpfs !i in
      let pf_lvars = Formula.lvars pf in
      let inter_empty = SS.inter rhs_lvars pf_lvars = SS.empty in
      match inter_empty with
      | true  -> PFS.nth_delete lpfs !i
      | false -> i := !i + 1
    done;
    (true, exists, lpfs, rpfs, gamma)
  with Failure _ -> (false, exists, lpfs, rpfs, gamma)

(**
  Pure entailment: simplify pure formulae and typing environment

  @param pfs Pure formulae (modified destructively)
  @param pfs Typing environment (modified destructively)
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
    (gamma : TypEnv.t) : SSubst.t * SS.t =
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
      if PFS.length lpfs > 0 && PFS.nth_get lpfs 0 == False then (
        PFS.clear rpfs;
        PFS.extend rpfs True );

      (SSubst.copy subst, simpl_existentials)
  | false ->
      let n = ref 0 in
      let result = SSubst.init [] in

      let vars_to_save, save_all =
        match save_spec_vars with
        | Some v -> v
        | None   -> (SS.empty, false)
      in

      let vars_to_kill = ref SS.empty in

      let kill_new_lvars =
        match kill_new_lvars with
        | None   -> false
        | Some b -> b
      in

      (* Unit types *)
      let simplify_unit_types () =
        TypEnv.iter gamma (fun x t ->
            match t with
            | UndefinedType -> SSubst.put result x (Lit Undefined)
            | NullType      -> SSubst.put result x (Lit Null)
            | EmptyType     -> SSubst.put result x (Lit Empty)
            | NoneType      -> SSubst.put result x (Lit Nono)
            | _             -> ())
      in

      (* Pure formulae false *)
      let pfs_false (msg : string) : unit =
        L.verbose (fun m -> m "Pure formulae false: %s" msg);
        PFS.clear lpfs;
        PFS.extend lpfs False;
        PFS.clear rpfs;
        PFS.extend rpfs True;
        n := 1
      in

      (* PF simplification *)
      let simplify_pf (pf : Formula.t) : unit =
        (* Reduce current assertion *)
        match pf with
        (* These we must not encounter here *)
        | ForAll (bt, _) ->
            let lx, _ = List.split bt in
            List.iter (fun x -> TypEnv.remove gamma x) lx;
            n := !n + 1
        (* And is expanded *)
        | And (a1, a2) ->
            PFS.nth_set lpfs !n a1;
            PFS.extend lpfs a2
        (* If we find true, we can delete it *)
        | True -> PFS.nth_delete lpfs !n
        (* If we find false, the entire pfs are false *)
        | False -> pfs_false "false in pure formulae"
        (* Inequality of things with different types *)
        | Not (Eq (le1, le2)) -> (
            let te1, _, _ = Typing.type_lexpr gamma le1 in
            let te2, _, _ = Typing.type_lexpr gamma le2 in
            match (te1, te2) with
            | Some te1, Some te2 when te1 <> te2 -> PFS.nth_delete lpfs !n
            | Some te1, Some te2
              when te1 = te2
                   && ( te1 = UndefinedType || te1 = NullType || te1 = EmptyType
                      || te1 = NoneType ) ->
                pfs_false "Inequality of two undefined/null/empty/none"
            | _ -> n := !n + 1 )
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
                PFS.nth_set lpfs !n
                  (Eq
                     ( lst,
                       NOp
                         ( LstCat,
                           [ EList (List.append prepend [ elem ]); append ] ) ))
            | _ -> n := !n + 1 )
        | (Eq (LstSub (lst, start, num), sl) | Eq (sl, LstSub (lst, start, num)))
          when unification -> n := !n + 1
        | Eq (UnOp (LstLen, le), Lit (Num len))
        | Eq (Lit (Num len), UnOp (LstLen, le)) -> (
            match Arith_Utils.is_int len with
            | false -> pfs_false "List length not an integer."
            | true  ->
                let len = int_of_float len in
                let le_vars =
                  Array.to_list (Array.init len (fun _ -> LVar.alloc ()))
                in
                vars_to_kill := SS.union !vars_to_kill (SS.of_list le_vars);
                let le' = List.map (fun x -> Expr.LVar x) le_vars in
                PFS.nth_set lpfs !n (Eq (le, EList le')) )
        | Eq (NOp (LstCat, les), EList [])
        | Eq (NOp (LstCat, les), Lit (LList [])) ->
            let eqs = List.map (fun le -> Formula.Eq (le, EList [])) les in
            PFS.nth_delete lpfs !n;
            List.iter (fun eq -> PFS.extend lpfs eq) eqs
        (* Sublist *)
        | Eq (LstSub (lst, start, num), sl) | Eq (sl, LstSub (lst, start, num))
          -> (
            match (start, num) with
            (* We know both the start and the length *)
            | Lit (Num st), Lit (Num el)
              when Arith_Utils.is_int st && Arith_Utils.is_int el ->
                (* Prefix *)
                let prefix_lvars =
                  Array.to_list
                    (Array.init (int_of_float st) (fun _ -> LVar.alloc ()))
                in
                vars_to_kill := SS.union !vars_to_kill (SS.of_list prefix_lvars);
                let prefix = List.map (fun x -> Expr.LVar x) prefix_lvars in
                (* Create sublist *)
                let sublist_lvars =
                  Array.to_list
                    (Array.init (int_of_float el) (fun _ -> LVar.alloc ()))
                in
                vars_to_kill :=
                  SS.union !vars_to_kill (SS.of_list sublist_lvars);
                let sublist = List.map (fun x -> Expr.LVar x) sublist_lvars in
                (* Suffix *)
                let suffix = LVar.alloc () in
                vars_to_kill := SS.add suffix !vars_to_kill;
                PFS.nth_set lpfs !n
                  (Eq
                     ( lst,
                       NOp (LstCat, [ EList (prefix @ sublist); LVar suffix ])
                     ));
                PFS.extend lpfs (Eq (sl, EList sublist))
            (* We know just the start *)
            | Lit (Num st), _ when Arith_Utils.is_int st ->
                (* Prefix *)
                let prefix_lvars =
                  Array.to_list
                    (Array.init (int_of_float st) (fun _ -> LVar.alloc ()))
                in
                vars_to_kill := SS.union !vars_to_kill (SS.of_list prefix_lvars);
                let prefix = List.map (fun x -> Expr.LVar x) prefix_lvars in
                (* Suffix *)
                let suffix = LVar.alloc () in
                let ns_var = LVar.alloc () in
                let ns_len_var = LVar.alloc () in
                vars_to_kill :=
                  SS.add suffix
                    (SS.add ns_var (SS.add ns_len_var !vars_to_kill));
                PFS.nth_set lpfs !n
                  (Eq (lst, NOp (LstCat, [ EList prefix; LVar ns_var ])));
                PFS.extend lpfs
                  (Eq (LVar ns_var, NOp (LstCat, [ sl; LVar suffix ])));
                PFS.extend lpfs (Eq (UnOp (LstLen, sl), num));
                PFS.extend lpfs
                  (Eq
                     ( LVar suffix,
                       LstSub
                         ( LVar ns_var,
                           UnOp (LstLen, sl),
                           BinOp
                             ( UnOp (LstLen, LVar ns_var),
                               FMinus,
                               UnOp (LstLen, sl) ) ) ))
            | _, _
              when ( match sl with
                   | Lit (LList _) | EList _ -> false
                   | _                       -> true )
                   && (not (num = UnOp (LstLen, sl)))
                   &&
                   match num with
                   | Lit (Num _) | LVar _ -> true
                   | _                    -> false ->
                let new_pf = Formula.Eq (UnOp (LstLen, sl), num) in
                L.(
                  verbose (fun m ->
                      m "LSTSUBADD: %s" ((Fmt.to_to_string Formula.pp) new_pf)));
                PFS.extend lpfs new_pf;
                n := !n + 1
            | _ -> n := !n + 1 )
        | Eq (le1, le2) -> (
            let te1, _, _ = Typing.type_lexpr gamma le1 in
            let te2, _, _ = Typing.type_lexpr gamma le2 in
            match (te1, te2) with
            | Some te1, Some te2 when te1 <> te2 ->
                pfs_false
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
                    PFS.nth_set lpfs !n pf;
                    n := !n + 1
                | Lit (Loc lloc), ALoc aloc | ALoc aloc, Lit (Loc lloc) ->
                    (* TODO: What should actually happen here... *)
                    pfs_false
                      "Abtract location never equal to a concrete location"
                    (* SSubst.put result aloc (Lit (Loc lloc));
                       let temp_subst = SSubst.init [ aloc, Lit (Loc lloc) ] in
                         PFS.substitution_in_place temp_subst lpfs *)
                | ALoc alocl, ALoc alocr when unification ->
                    L.verbose (fun fmt ->
                        fmt "Two equal alocs: %s and %s" alocl alocr);
                    SSubst.put result alocr (ALoc alocl);
                    let temp_subst = SSubst.init [ (alocr, ALoc alocl) ] in
                    PFS.substitution temp_subst lpfs
                | ALoc alocl, ALoc alocr when not unification ->
                    if alocl = alocr then PFS.nth_delete lpfs !n
                    else
                      pfs_false
                        "Two different abstract locations are never equal"
                (* Equal variables - what happens if they are numbers? *)
                | LVar v1, LVar v2 when v1 = v2 -> PFS.nth_delete lpfs !n
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
                    | true -> n := !n + 1
                    | false -> (
                        PFS.nth_delete lpfs !n;

                        let tv, _, _ = Typing.type_lexpr gamma (LVar v) in
                        let tle, _, _ = Typing.type_lexpr gamma le in
                        match (tv, tle) with
                        | Some tv, Some tle when tv <> tle ->
                            pfs_false "Type mismatch"
                        | _ -> (
                            let temp_subst = SSubst.init [ (v, le) ] in
                            PFS.substitution temp_subst lpfs;

                            if SSubst.mem result v then (
                              let le' = Option.get (SSubst.get result v) in
                              L.(
                                verbose (fun m ->
                                    m "Multiples in subst: %s %s"
                                      ((Fmt.to_to_string Expr.pp) le)
                                      ((Fmt.to_to_string Expr.pp) le')));
                              if le <> le' && not (PFS.mem lpfs (Eq (le, le')))
                              then PFS.extend lpfs (Eq (le, le')) );
                            SSubst.iter result (fun x le ->
                                let sle =
                                  SSubst.subst_in_expr temp_subst true le
                                in
                                SSubst.put result x sle);
                            SSubst.put result v le;

                            existentials := SS.remove v !existentials;

                            (* Understand gamma if subst is another LVar *)
                            ( match le with
                            | LVar v' -> (
                                match TypEnv.get gamma v with
                                | None   -> ()
                                | Some t -> (
                                    match TypEnv.get gamma v' with
                                    | None    -> TypEnv.update gamma v' t
                                    | Some t' ->
                                        if t <> t' then
                                          pfs_false "Type mismatch" ) )
                            | _       -> () );

                            (* Remove (or add) from (or to) gamma *)
                            match save_all || SS.mem v vars_to_save with
                            | true  -> (
                                let le_type, _, _ =
                                  Typing.type_lexpr gamma le
                                in
                                match le_type with
                                | None   -> ()
                                | Some t -> (
                                    match TypEnv.get gamma v with
                                    | None    -> TypEnv.update gamma v t
                                    | Some tv ->
                                        if t <> tv then
                                          pfs_false "Type mismatch" ) )
                            | false -> TypEnv.remove gamma v ) ) )
                | UnOp (TypeOf, LVar v), Lit (Type t)
                | Lit (Type t), UnOp (TypeOf, LVar v) ->
                    ( match TypEnv.get gamma v with
                    | None    -> TypEnv.update gamma v t
                    | Some tv -> if t <> tv then pfs_false "Type mismatch" );
                    PFS.nth_delete lpfs !n
                | _, _ -> n := !n + 1 ) )
        (* All other cases *)
        | _ -> n := !n + 1
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
        n := 0;
        while !n < PFS.length lpfs do
          let pf = PFS.nth_get lpfs !n in
          let pf =
            Reduction.reduce_formula ~unification ~gamma ?pfs:(Some lpfs) pf
          in
          PFS.nth_set lpfs !n pf;
          simplify_pf pf
        done;

        PFS.substitution result lpfs;

        if
          PFS.length lpfs = 0
          || (PFS.length lpfs > 0 && not (PFS.nth_get lpfs 0 = False))
        then (
          (* Step 3 - Bring back my variables *)
          SSubst.iter result (fun v le ->
              if
                (not (SS.mem v !vars_to_kill))
                && ( save_all
                   || (kill_new_lvars && SS.mem v vars_to_save)
                   || ((not kill_new_lvars) && vars_to_save <> SS.empty) )
                && not (Names.is_aloc_name v)
              then PFS.extend lpfs (Eq (LVar v, le)));

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
          subst = SSubst.copy result;
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
  PFS.iteri
    (fun i pf ->
      let pf' = Reduction.reduce_formula ~gamma ~pfs:lpfs pf in
      PFS.nth_set rpfs i pf')
    rpfs;

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
          ((Fmt.to_to_string Asrt.pp) a)));

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
  separate a;
  let _ = simplify_pfs_and_gamma ~kill_new_lvars:true pfs gamma in
  let res = not (PFS.mem pfs Formula.False) in
  if res then L.verbose (fun m -> m "Admissible !!")
  else L.verbose (fun m -> m "Not admissible !!");
  res
