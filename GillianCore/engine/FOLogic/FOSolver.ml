open SVal
open Containers
module L = Logging

type model = Z3.Model.model

let concretise (a : Formula.t) (x : string) (les : Expr.t list) : Formula.t list
    =
  List.map
    (fun le -> SSubst.substitute_formula (SSubst.init [ (x, le) ]) true a)
    les

let concretise2 (a : Formula.t) (x : string) (y : string) (les : Expr.t list) :
    Formula.t list =
  List.map
    (fun (le1, le2) ->
      SSubst.substitute_formula (SSubst.init [ (x, le1); (y, le2) ]) true a)
    (List_utils.cross_product les les (fun x y -> (x, y)))

let make_global_axioms
    (list_vars : Expr.t list)
    (string_vars : Expr.t list)
    (list_exprs : Expr.t list) : Formula.t list =
  let x_name = "#x" in
  let lvar_x = Expr.LVar x_name in

  (* forall x. 0 <= slen(x) *)
  let slen1 : Formula.t = LessEq (Lit (Num 0.), UnOp (StrLen, lvar_x)) in
  let slen1_s = concretise slen1 x_name string_vars in

  (* forall x. 0 <= llen(x) *)
  let llen1 : Formula.t = LessEq (Lit (Num 0.), UnOp (LstLen, lvar_x)) in
  let llen1_s = concretise llen1 x_name list_vars in

  (* forall x. (x = nil) \/ (0 < llen(x))
     (LLess ((Lit (Num 0.), LUnop (LstLen, lvar_x)))) *)
  let llen2 : Formula.t =
    Or (Eq (lvar_x, Lit (LList [])), Less (Lit (Num 0.), UnOp (LstLen, lvar_x)))
  in
  let llen2_s = concretise llen2 x_name list_vars in

  (* forall x. (car(x) = l-nth(x, 0) *)
  let carlnth0 : Formula.t =
    Eq (UnOp (Car, lvar_x), BinOp (lvar_x, LstNth, Lit (Num 0.)))
  in
  let carlnth0_s = concretise carlnth0 x_name list_vars in

  (* forall x. (l-rev (l-rev (x))) == x *)
  let rev0 : Formula.t = Eq (UnOp (LstRev, UnOp (LstRev, lvar_x)), lvar_x) in
  let rev0_s = concretise rev0 x_name list_vars in

  (* forall x. (x = nil) \/ (nil <> l-rev(x)) *)
  let rev1 : Formula.t =
    Or
      ( Eq (lvar_x, Lit (LList [])),
        Not (Eq (Lit (LList []), UnOp (LstRev, lvar_x))) )
  in
  let rev1_s = concretise rev1 x_name list_vars in

  (* TODO: These two axioms need to be adapted to the n-ary LstCat
     (* forall x, y. ((x = nil) /\ (y = nil)) \/ (! (x @ y = nil)) *)
     let l_disjunct : Formula.t = And (Eq (lvar_x, Lit (LList [])), Eq (lvar_y, Lit (LList []))) in
     let r_disjunct : Formula.t = Not (Eq (Lit (LList []), BinOp (lvar_x, LstCat, lvar_y))) in
     let lstcat1    : Formula.t = Or (l_disjunct, r_disjunct) in
     let lstcat1_s  : Formula.t list = concretise2 lstcat1 x_name y_name list_exprs in

     (* forall x. x @ nil = x *)
     let cons_nil_idem_r : Formula.t = Eq (BinOp (lvar_x, LstCat, Lit (LList [])), lvar_x) in
     let cons_nil_idem_r_s : Formula.t list = concretise cons_nil_idem_r x_name list_vars in *)
  rev0_s @ rev1_s @ slen1_s @ llen1_s @ llen2_s @ carlnth0_s

(* @ lstcat1_s @ cons_nil_idem_r_s *)

let make_list_axioms (le : Expr.t) : Formula.t list =
  let rec loop_nth original_les les i axioms =
    match les with
    | []             -> axioms
    | le :: rest_les ->
        let cur_axiom : Formula.t =
          Eq (BinOp (EList original_les, LstNth, Lit (Num (float_of_int i))), le)
        in
        loop_nth original_les rest_les (i + 1) (cur_axiom :: axioms)
  in

  match (le : Expr.t) with
  | EList les ->
      let len_axiom : Formula.t =
        Eq (UnOp (LstLen, le), Lit (Num (float_of_int (List.length les))))
      in
      let nth_axioms = loop_nth les les 0 [] in
      len_axiom :: nth_axioms
  (* TODO: These two axioms need to be adapted to the n-ary LstCat
     | BinOp (EList lst, LstCat, le_tail) ->
       let len = List.length lst in
         let len_axiom : Formula.t =
           Eq (UnOp (LstLen, le),
             BinOp (Lit (Num (float_of_int len)), Plus, UnOp (LstLen, le_tail))) in
         let is = Array.to_list (Array.init (len - 1) (fun i -> i)) in
         let nth_axioms = List.map (fun i -> Formula.Eq ((BinOp (le, LstNth, Lit (Num (float_of_int i)))), (List.nth lst i))) is in
         let car_axiom : Formula.t =
           if (List.length lst > 0)
             then Eq (UnOp (Car, le), List.hd lst)
             else Eq (UnOp (Car, le), UnOp (Car, le_tail)) in
         let rev_axiom : Formula.t =
           Eq (UnOp (LstRev, le), BinOp(UnOp (LstRev, le_tail), LstCat, EList (List.rev lst))) in
         let cdr_axiom : Formula.t =
           if (List.length lst > 1)
             then Eq (UnOp (Cdr, le), BinOp (EList (List.tl lst), LstCat, le_tail))
             else if (List.length lst = 1)
               then Eq (UnOp (Cdr, le), le_tail)
               else Eq (UnOp (Cdr, le), UnOp (Cdr, le_tail)) in
         cdr_axiom :: rev_axiom :: car_axiom :: len_axiom :: nth_axioms *)
  | _ -> []

let make_string_axioms (s : string) : Formula.t list =
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (String.make 1 s.[i] :: l)
    in
    exp (String.length s - 1) []
  in

  let rec loop_nth chars i axioms =
    match chars with
    | []              -> axioms
    | c :: rest_chars ->
        let cur_axiom : Formula.t =
          Eq
            ( BinOp (Lit (String s), StrNth, Lit (Num (float_of_int i))),
              Lit (String c) )
        in
        loop_nth rest_chars (i + 1) (cur_axiom :: axioms)
  in

  let slen_axiom : Formula.t =
    Eq
      (UnOp (StrLen, Lit (String s)), Lit (Num (float_of_int (String.length s))))
  in
  slen_axiom :: loop_nth (explode s) 0 []

let make_relevant_axioms
    (a : Formula.t)
    (list_vars : Expr.t list)
    (string_vars : Expr.t list)
    (list_exprs : Expr.t list) : Formula.t list =
  (* string axioms *)
  let a_strings, _ = Formula.strings_and_numbers a in
  let a_strings = List_utils.remove_duplicates a_strings in
  let s_axioms = List.concat (List.map make_string_axioms a_strings) in

  (* list axioms *)
  let a_lists = Formula.lists a in
  let l_axioms = List.concat (List.map make_list_axioms a_lists) in

  let constant_axioms = make_global_axioms list_vars string_vars list_exprs in

  s_axioms @ l_axioms @ constant_axioms

(** For a given set of pure formulae and its associated gamma, return the corresponding axioms *)
let get_axioms assertions gamma =
  (* Get list variables *)
  let list_vars =
    List.map
      (fun x -> (LVar x : Expr.t))
      (TypEnv.get_vars_of_type gamma ListType)
  in
  (* Get string variables *)
  let string_vars =
    List.map
      (fun x -> (LVar x : Expr.t))
      (TypEnv.get_vars_of_type gamma StringType)
  in
  (* Get list expressions *)
  let list_exprs = List.concat (List.map Formula.list_lexprs assertions) in
  (* Remove duplicates *)
  let list_exprs = Expr.Set.elements (Expr.Set.of_list list_exprs) in
  (* Put list-related expressions together *)
  let list_exprs = list_exprs @ list_vars in
  (* Get all axioms *)
  let axioms =
    List.concat
      (List.map
         (fun a -> make_relevant_axioms a list_vars string_vars list_exprs)
         assertions)
  in
  (* Remove duplicates *)
  let axioms = Formula.Set.elements (Formula.Set.of_list axioms) in
  (* Return *)
  axioms

(** ****************
  * SATISFIABILITY *
  * **************** **)

let simplify_pfs_and_gamma
    ?(unification = false) (fs : Formula.t list) (gamma : TypEnv.t) :
    Formula.Set.t * TypEnv.t * SESubst.t =
  let gamma = TypEnv.copy gamma in
  let pfs = PFS.of_list fs in
  let subst, _ =
    Simplifications.simplify_pfs_and_gamma ~unification pfs gamma
  in
  let fs_lst = PFS.to_list pfs in
  let fs_set = Formula.Set.of_list fs_lst in
  (fs_set, gamma, subst)

let check_satisfiability_with_model (fs : Formula.t list) (gamma : TypEnv.t) :
    SESubst.t option =
  let fs, gamma, subst = simplify_pfs_and_gamma fs gamma in
  let model = Z3Encoding.check_sat_core fs gamma in
  let lvars =
    List.fold_left
      (fun ac vs ->
        let vs =
          Expr.Set.of_list (List.map (fun x -> Expr.LVar x) (SS.elements vs))
        in
        Expr.Set.union ac vs)
      Expr.Set.empty
      (List.map Formula.lvars (Formula.Set.elements fs))
  in
  let z3_vars = Expr.Set.diff lvars (SESubst.domain subst None) in
  L.(
    verbose (fun m ->
        m "OBTAINED VARS: %s\n"
          (String.concat ", "
             (List.map
                (fun e -> Format.asprintf "%a" Expr.pp e)
                (Expr.Set.elements z3_vars)))));
  match model with
  | None       -> None
  | Some model -> (
      try
        Z3Encoding.lift_z3_model model gamma subst z3_vars;
        Some subst
      with _ -> None )

let check_satisfiability
    ?(unification = false) (fs : Formula.t list) (gamma : TypEnv.t) : bool =
  (* let t = Sys.time () in *)
  L.verbose (fun m -> m "Entering FOSolver.check_satisfiability");
  let fs, gamma, _ = simplify_pfs_and_gamma ~unification fs gamma in
  (* let axioms    = get_axioms (Formula.Set.elements fs) gamma in
     let fs           = Formula.Set.union fs (Formula.Set.of_list axioms) in *)
  let result = Z3Encoding.check_sat fs gamma in
  (* Utils.Statistics.update_statistics "FOS: CheckSat" (Sys.time () -. t); *)
  result

let sat ~pfs ~gamma formulae : bool =
  check_satisfiability (formulae @ PFS.to_list pfs) gamma

(** ************
  * ENTAILMENT *
  * ************ **)

let check_entailment
    (existentials : SS.t)
    (left_fs : Formula.t list)
    (right_fs : Formula.t list)
    (gamma : TypEnv.t) : bool =
  L.verbose (fun m ->
      m
        "Preparing entailment check:@\n\
         Existentials:@\n\
         @[<h>%a@]@\n\
         Left:%a@\n\
         Right:%a@\n\
         Gamma:@\n\
         %a@\n"
        (Fmt.iter ~sep:Fmt.comma SS.iter Fmt.string)
        existentials PFS.pp (PFS.of_list left_fs) PFS.pp (PFS.of_list right_fs)
        TypEnv.pp gamma);

  (* SOUNDNESS !!DANGER!!: call to simplify_implication       *)
  (* Simplify maximally the implication to be checked         *)
  (* Remove from the typing environment the unused variables  *)
  let gamma = TypEnv.copy gamma in
  let left_fs = PFS.of_list left_fs in
  let right_fs = PFS.of_list right_fs in
  let left_lvars = PFS.lvars left_fs in
  let right_lvars = PFS.lvars right_fs in
  let existentials =
    Simplifications.simplify_implication existentials left_fs right_fs gamma
  in
  TypEnv.filter_vars_in_place gamma (SS.union left_lvars right_lvars);

  (* Separate gamma into existentials and non-existentials *)
  let left_fs = PFS.to_list left_fs in
  let right_fs = PFS.to_list right_fs in
  let gamma_left = TypEnv.filter gamma (fun v -> not (SS.mem v existentials)) in
  let gamma_right = TypEnv.filter gamma (fun v -> SS.mem v existentials) in

  (* If left side is false, return false *)
  if List.length left_fs > 0 && List.hd left_fs = False then false
    (* If right side is false, return false *)
  else if List.length right_fs > 0 && List.hd right_fs = False then false
  else
    (* Check satisfiability of left side *)
    let left_sat =
      Z3Encoding.check_sat (Formula.Set.of_list left_fs) gamma_left
    in

    (* If the right side is empty or left side is not satisfiable, return the result of
       checking left-side satisfiability *)
    if List.length right_fs = 0 || not left_sat then left_sat
    else
      (* A => B -> Axioms(A) /\ Axioms(B) /\ A => B
                -> !(Axioms(A) /\ Axioms(B) /\ A) \/ B
                -> Axioms(A) /\ Axioms(B) /\ A /\ !B is SAT *)
      (* Existentials in B need to be turned into universals *)
      (* A => Exists (x1, ..., xn) B
                -> Axioms(A) /\ A => (Exists (x1, ..., xn) (Axioms(B) => B)
                -> !(Axioms(A) /\ A) \/ (Exists (x1, ..., xn) (Axioms(B) => B))
                -> Axioms(A) /\ A /\ (ForAll (x1, ..., x2) Axioms(B) /\ !B) is SAT
                -> ForAll (x1, ..., x2)  Axioms(A) /\ Axioms(B) /\ A /\ !B is SAT *)

      (* Get axioms *)
      (* let axioms   = get_axioms (left_fs @ right_fs) gamma in *)
      let right_fs =
        List.map
          (fun f -> (Formula.push_in_negations (Not f) : Formula.t))
          right_fs
      in
      let right_f : Formula.t =
        if SS.is_empty existentials then Formula.disjunct right_fs
        else
          let binders =
            List.map
              (fun x -> (x, TypEnv.get gamma_right x))
              (SS.elements existentials)
          in
          ForAll (binders, Formula.disjunct right_fs)
      in

      let formulae = PFS.of_list (right_f :: (left_fs @ [] (* axioms *))) in
      let _ = Simplifications.simplify_pfs_and_gamma formulae gamma_left in

      let ret =
        Z3Encoding.check_sat
          (Formula.Set.of_list (PFS.to_list formulae))
          gamma_left
      in
      L.(verbose (fun m -> m "Entailment returned %b" (not ret)));
      not ret

let is_equal_on_lexprs e1 e2 pfs : bool option =
  match e1 = e2 with
  (* This true check is not good enough, things could creep in with Unknowns *)
  | true -> Some true
  | false -> (
      match ((e1 : Expr.t), (e2 : Expr.t)) with
      | Lit (String str), LVar x | LVar x, Lit (String str) ->
          if str.[0] = '@' then
            if
              List.mem
                (Formula.Not
                   (Eq (BinOp (LVar x, StrNth, Lit (Num 0.)), Lit (String "@"))))
                pfs
              || List.mem
                   (Formula.Not
                      (Eq
                         (Lit (String "@"), BinOp (LVar x, StrNth, Lit (Num 0.)))))
                   pfs
            then Some false
            else None
          else None
      (* Variables *)
      | PVar x, PVar y | LVar x, LVar y -> if x = y then Some true else None
      | PVar _, _ | _, PVar _ | LVar _, _ | _, LVar _ -> None
      (* Now we have no more variables *)

      (* None *)
      | Lit Nono, _ | _, Lit Nono -> Some false
      (* Literals *)
      | Lit l1, Lit l2 -> Some (l1 = l2)
      (* ALocs *)
      | ALoc a1, ALoc a2 -> Some (a1 = a2)
      | ALoc _, Lit (Loc _) | Lit (Loc _), ALoc _ -> None
      | ALoc _, _ | _, ALoc _ -> Some false
      (* ELists *)
      | Lit (LList _), EList _ | EList _, Lit (LList _) -> None
      | Lit _, EList _ | EList _, Lit _ -> Some false
      (* other *)
      | _, _ -> None )

let is_equal ~pfs ~gamma e1 e2 =
  let feq =
    Reduction.reduce_formula ?gamma:(Some gamma) ?pfs:(Some pfs) (Eq (e1, e2))
  in
  let result =
    match feq with
    | True         -> true
    | False        -> false
    | Eq _ | And _ -> check_entailment SS.empty (PFS.to_list pfs) [ feq ] gamma
    | _            ->
        raise
          (Failure
             ( "Equality reduced to something unexpected: "
             ^ (Fmt.to_to_string Formula.pp) feq ))
  in
  result

let is_different ~pfs ~gamma e1 e2 =
  let feq = Reduction.reduce_formula ~gamma ~pfs (Not (Eq (e1, e2))) in
  let result =
    match feq with
    | True  -> true
    | False -> false
    | Not _ -> check_entailment SS.empty (PFS.to_list pfs) [ feq ] gamma
    | _     ->
        raise
          (Failure
             ( "Inequality reduced to something unexpected: "
             ^ (Fmt.to_to_string Formula.pp) feq ))
  in
  result

let is_less_or_equal ~pfs ~gamma e1 e2 =
  let feq = Reduction.reduce_formula ~gamma ~pfs (LessEq (e1, e2)) in
  let result =
    match feq with
    | True        -> true
    | False       -> false
    | Eq (ra, rb) -> is_equal ~pfs ~gamma ra rb
    | LessEq _    -> check_entailment SS.empty (PFS.to_list pfs) [ feq ] gamma
    | _           ->
        raise
          (Failure
             ( "Inequality reduced to something unexpected: "
             ^ (Fmt.to_to_string Formula.pp) feq ))
  in
  result

let resolve_loc_name ~pfs ~gamma loc =
  Logging.tmi (fun fmt -> fmt "get_loc_name: %a" Expr.pp loc);
  match Reduction.reduce_lexpr ~pfs ~gamma loc with
  | Lit (Loc loc) | ALoc loc -> Some loc
  | LVar x                   -> Reduction.resolve_expr_to_location pfs gamma
                                  (LVar x)
  | loc'                     -> (
      match Reduction.resolve_expr_to_location pfs gamma loc' with
      | Some loc_name -> Some loc_name
      | None          ->
          let msg =
            Format.asprintf "Unsupported location: %a with pfs:\n%a" Expr.pp
              loc' PFS.pp pfs
          in
          Logging.verbose (fun fmt -> fmt "%s" msg);
          raise (Failure msg) )
