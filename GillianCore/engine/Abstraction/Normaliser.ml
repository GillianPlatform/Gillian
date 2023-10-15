open Names
open Containers
module L = Logging
module SESubst = SVal.SESubst
module SPreds = Preds.SPreds

let new_lvar_name var = lvar_prefix ^ var

module Make
    (SPState : PState.S
                 with type vt = SVal.M.t
                  and type st = SVal.SESubst.t
                  and type store_t = SStore.t
                  and type preds_t = SPreds.t) =
struct
  (*  ------------------------------------------------------------------
   *  List Preprocessing
   *  ------------------------------------------------------------------
   *  Preprocess list logical expressions for which we know
   *  the length statically. If a |- l-len(le) = i, where i is
   *  a concrete number, we add the assertion le = {{ #x1, ..., #xi }}
   *  to a and replace all the occurrences of l-nth(le, j) for #xj in a
   *  ------------------------------------------------------------------
**)

  let preprocess_lists (a : Asrt.t) =
    (* 1 - Find the lists for which we know the length *)
    let find_list_exprs_to_concretize (a : Asrt.t) :
        (Expr.t, Expr.t list) Hashtbl.t =
      let rec collect_concretizable_lists = function
        | Asrt.Pure (Eq (EList _, EList _)) -> []
        | Pure (Eq (le, EList les)) | Pure (Eq (EList les, le)) -> [ (le, les) ]
        | Pure (Eq (UnOp (LstLen, le), Lit (Int i)))
        | Pure (Eq (Lit (Int i), UnOp (LstLen, le))) ->
            let les =
              List.init (Z.to_int i) (fun _ -> Expr.LVar (LVar.alloc ()))
            in
            [ (le, les) ]
        | Star (a1, a2) ->
            List.rev_append
              (collect_concretizable_lists a1)
              (collect_concretizable_lists a2)
        | _ -> []
      in
      let lst_exprs = collect_concretizable_lists a in
      let lists_tbl = Hashtbl.create 1 in
      List.iter
        (fun (le, les) ->
          if Hashtbl.mem lists_tbl le then ()
          else Hashtbl.replace lists_tbl le les)
        lst_exprs;
      lists_tbl
    in

    (* 2 - Replace expressions of the form l-nth(le, i) where le denotes a list for which
         we know the length and i a concrete number with the newly created logical variable.
         E.g. if we associated in 2) le with a the list of logical variables
              {{ V1, ..., Vi, ..., Vn}}, l-nth(le, i) is replaced with Vi *)
    let concretize_list_accesses
        (a : Asrt.t)
        (new_lists : (Expr.t, Expr.t list) Hashtbl.t) : Asrt.t =
      let mapper =
        object
          inherit [_] Visitors.endo as super

          method! visit_BinOp ctx this le' op n =
            match (op, n) with
            | LstNth, Lit (Int i) -> (
                let ( >>= ) = Option.bind in
                match
                  Hashtbl.find_opt new_lists le' >>= fun vs ->
                  List.nth_opt vs (Z.to_int i)
                with
                | Some v -> v
                | None -> this)
            | _ -> super#visit_BinOp ctx this le' op n
        end
      in
      mapper#visit_assertion () a
    in

    (* 3 - Generate the equalities relating the expressions that denote lists whose
         length is statically known with the lists of the newly created logical variables *)
    let make_new_list_as
        (a : Asrt.t)
        (new_lists : (Expr.t, Expr.t list) Hashtbl.t) : Asrt.t =
      let new_list_as =
        Hashtbl.fold
          (fun le les (ac : Asrt.t list) -> Pure (Eq (le, EList les)) :: ac)
          new_lists [ a ]
      in
      Asrt.star new_list_as
    in

    (* Doing IT *)
    let new_lists = find_list_exprs_to_concretize a in
    let a' = concretize_list_accesses a new_lists in
    make_new_list_as a' new_lists

  (**
  le -> non - normalised logical expression
  subst -> table mapping variable and logical variable
  gamma -> table mapping logical variables + variables to types

  the store is assumed to contain all the program variables in le
*)
  let rec normalise_lexpr
      ?(no_types = false)
      ?(store = SStore.init [])
      ?(subst = SESubst.init [])
      (gamma : Type_env.t)
      (le : Expr.t) =
    let f = normalise_lexpr ~no_types ~store ~subst gamma in

    let result : Expr.t =
      match (le : Expr.t) with
      | Lit _ -> le
      | LVar lvar ->
          Option.value ~default:(Expr.LVar lvar) (SESubst.get subst le)
      | ALoc aloc ->
          ALoc aloc
          (* raise (Failure "Unsupported expression during normalization: ALoc") Why not ALoc aloc? *)
      | PVar pvar -> (
          let value = SStore.get store pvar in
          match value with
          | Some value -> value
          | None ->
              let new_lvar = LVar.alloc () in
              SStore.put store pvar (LVar new_lvar);
              SESubst.put subst (PVar pvar) (LVar new_lvar);
              LVar new_lvar)
      | BinOp (le1, bop, le2) -> (
          let nle1 = f le1 in
          let nle2 = f le2 in
          match bop with
          | LstNth -> (
              match ((nle1 : Expr.t), (nle2 : Expr.t)) with
              | Lit (LList list), Lit (Int n) ->
                  let lit_n = List.nth list (Z.to_int n) in
                  Lit lit_n
              | EList list, Lit (Int n) ->
                  let le_n = List.nth list (Z.to_int n) in
                  f le_n
              | EList _, Lit (Num _) -> raise (Failure "Non-integer list index")
              | _, _ -> BinOp (nle1, LstNth, nle2))
          | StrNth -> (
              match (nle1, nle2) with
              | Lit (String s), Lit (Num n) when Arith_utils.is_int n ->
                  let s =
                    try String.make 1 s.[int_of_float n]
                    with _ -> raise (Failure "String index out of bounds")
                  in
                  Lit (String s)
              | Lit (String _), Lit (Num _) ->
                  raise (Failure "Non-integer string index")
              | _, _ -> BinOp (nle1, StrNth, nle2))
          | _ -> (
              match ((nle1 : Expr.t), (nle2 : Expr.t)) with
              | Lit lit1, Lit lit2 ->
                  let lit =
                    CExprEval.evaluate_binop (CStore.init []) bop (Lit lit1)
                      (Lit lit2)
                  in
                  Lit lit
              | _, _ -> BinOp (nle1, bop, nle2)))
      | UnOp (uop, le1) -> (
          let nle1 = f le1 in
          match nle1 with
          | Lit lit1 ->
              let lit = CExprEval.evaluate_unop uop lit1 in
              Lit lit
          | _ -> (
              match uop with
              | TypeOf -> (
                  let nle1 = f le1 in
                  match nle1 with
                  | Lit llit -> Lit (Type (Literal.type_of llit))
                  | LVar lvar -> (
                      try Lit (Type (Type_env.get_unsafe gamma lvar))
                      with _ ->
                        UnOp (TypeOf, LVar lvar)
                        (* raise (Failure (Printf.sprintf "Logical variables always have a type, in particular: %s." lvar))) *)
                      )
                  | ALoc _ -> Lit (Type ObjectType)
                  | PVar _ ->
                      raise
                        (Exceptions.Impossible
                           "normalise_lexpr: program variable in normalised \
                            expression")
                  | BinOp (_, _, _) | UnOp (_, _) -> UnOp (TypeOf, nle1)
                  | EList _ | LstSub _ | NOp (LstCat, _) -> Lit (Type ListType)
                  | NOp (_, _) | ESet _ -> Lit (Type SetType))
              | _ -> UnOp (uop, nle1)))
      | EList le_list ->
          let n_le_list = List.map (fun le -> f le) le_list in
          let all_literals, lit_list =
            List.fold_left
              (fun (ac, list) le ->
                match (le : Expr.t) with
                | Lit lit -> (ac, list @ [ lit ])
                | _ -> (false, list))
              (true, []) n_le_list
          in
          if all_literals then Lit (LList lit_list) else EList n_le_list
      | ESet le_list ->
          let n_le_list = List.map (fun le -> f le) le_list in
          ESet n_le_list
      | NOp (op, le_list) ->
          let n_le_list = List.map (fun le -> f le) le_list in
          NOp (op, n_le_list)
      | LstSub (le1, le2, le3) -> (
          let nle1 = f le1 in
          let nle2 = f le2 in
          let nle3 = f le3 in
          match (nle1, nle2, nle3) with
          | EList lst, Lit (Int start), Lit (Int len) ->
              List_utils.list_sub lst (Z.to_int start) (Z.to_int len)
              |> Option.get |> Expr.list
          | Lit (LList lst), Lit (Int start), Lit (Int len) ->
              List_utils.list_sub lst (Z.to_int start) (Z.to_int len)
              |> Option.get
              |> fun x -> Expr.Lit (LList x)
          | _, Lit (Num _), Lit (Num _) ->
              raise (Failure "Sublist indexes non-integer")
          | _, _, _ -> LstSub (nle1, nle2, nle3))
    in
    if not no_types then Typing.infer_types_expr gamma result;
    result

  let extend_typing_env_using_assertion_info
      (gamma : Type_env.t)
      (a_list : Formula.t list) : unit =
    List.iter
      (fun a ->
        match (a : Formula.t) with
        | Eq (LVar x, le) | Eq (le, LVar x) | Eq (PVar x, le) | Eq (le, PVar x)
          -> (
            let x_type = Type_env.get gamma x in
            match x_type with
            | None ->
                let le_type, _, _ = Typing.type_lexpr gamma le in
                Option.fold
                  ~some:(fun x_type -> Type_env.update gamma x x_type)
                  ~none:() le_type
            | Some _ -> ())
        | _ -> ())
      a_list

  (* -----------------------------------------------------
     Normalise Logic Expressions
     -----------------------------------------------------
     _____________________________________________________
  *)
  let normalise_logic_expression
      ?(no_types = false)
      (store : SStore.t)
      (gamma : Type_env.t)
      (subst : SESubst.t)
      (le : Expr.t) : Expr.t =
    let le' = normalise_lexpr ~no_types ~store ~subst gamma le in
    le'

  (* -----------------------------------------------------
     Normalise Pure Assertion (only one!)
     -----------------------------------------------------
     Invoke normalise_logic_expression on all the logic
     expressions of a
     _____________________________________________________
  *)
  let rec normalise_pure_assertion
      ?(no_types = false)
      (store : SStore.t)
      (gamma : Type_env.t)
      (subst : SESubst.t)
      (assertion : Formula.t) : Formula.t =
    let fa = normalise_pure_assertion ~no_types store gamma subst in
    let fant = normalise_pure_assertion ~no_types:true store gamma subst in
    let fe = normalise_logic_expression ~no_types store gamma subst in
    let result : Formula.t =
      match (assertion : Formula.t) with
      | Eq (le1, le2) -> Eq (fe le1, fe le2)
      | ILess (le1, le2) -> ILess (fe le1, fe le2)
      | ILessEq (le1, le2) -> ILessEq (fe le1, fe le2)
      | FLess (le1, le2) -> FLess (fe le1, fe le2)
      | FLessEq (le1, le2) -> FLessEq (fe le1, fe le2)
      | Not (Eq (le1, le2)) -> Not (Eq (fe le1, fe le2))
      | Not (FLessEq (le1, le2)) -> Not (FLessEq (fe le1, fe le2))
      | Not (FLess (le1, le2)) -> Not (FLess (fe le1, fe le2))
      | Not (ILessEq (le1, le2)) -> Not (ILessEq (fe le1, fe le2))
      | Not (ILess (le1, le2)) -> Not (ILess (fe le1, fe le2))
      | Not (SetSub (le1, le2)) -> Not (SetSub (fe le1, fe le2))
      | Not (SetMem (le1, le2)) -> Not (SetMem (fe le1, fe le2))
      | And (a1, a2) -> And (fa a1, fa a2)
      | Or (a1, a2) -> Or (fa a1, fa a2)
      | False -> False
      | True -> True
      | SetSub (le1, le2) -> SetSub (fe le1, fe le2)
      | SetMem (le1, le2) -> SetMem (fe le1, fe le2)
      | ForAll (bt, a) -> ForAll (bt, fant a)
      | IsInt e -> IsInt (fe e)
      | _ ->
          let msg =
            Fmt.str
              "normalise_pure_assertion can only process pure assertions: %a"
              Formula.pp assertion
          in
          raise (Failure msg)
    in
    if not no_types then Typing.infer_types_formula gamma result;
    result

  let normalise_pure_assertions
      (store : SStore.t)
      (gamma : Type_env.t)
      (subst : SESubst.t)
      (args : SS.t option)
      (fs : Formula.t list) : PFS.t =
    let pvar_equalities = Hashtbl.create 1 in
    let non_store_pure_assertions = Stack.create () in

    (*
     * Step 1 - Get equalities involving program variables
     * -----------------------------------------------------------------------------------
     * Returns the list of equalities in a involving program variables of the form x = E
     * or E = x, for a logical expression E and a variable x
     * -----------------------------------------------------------------------------------
     *)
    let init_pvar_equalities (fs : Formula.t list) : unit =
      List.iter
        (fun (f : Formula.t) : unit ->
          match f with
          | Eq (PVar x, e) | Eq (e, PVar x) ->
              if
                (not (Hashtbl.mem pvar_equalities x))
                && not (SStore.mem store x)
              then Hashtbl.add pvar_equalities x e
              else Stack.push (Formula.Eq (PVar x, e)) non_store_pure_assertions
          | _ -> Stack.push f non_store_pure_assertions)
        fs
    in

    (*
     * Step 2 - Build a table mapping pvars to integers
     * ------------------------------------------------
     *)
    let get_vars_tbl (vars : SS.t) : (string, int) Hashtbl.t =
      let len = SS.cardinal vars in
      let vars_tbl = Hashtbl.create len in
      List.iteri (fun i var -> Hashtbl.add vars_tbl var i) (SS.elements vars);
      vars_tbl
    in

    (*
     * Step 3 - PVars Dependency Graph
     * ------------------------------------------------------------------------
     * Compute a dependency graph between PVar equalities (which are treated as
     * definitions)
     * ------------------------------------------------------------------------
     *)
    let pvars_graph (p_vars : SS.t) (p_vars_tbl : (string, int) Hashtbl.t) :
        int list array =
      let len = SS.cardinal p_vars in
      let graph = Array.make len [] in

      List.iteri
        (fun u cur_var ->
          let cur_le = Hashtbl.find pvar_equalities cur_var in
          let cur_var_deps = Expr.pvars cur_le in
          SS.iter
            (fun v ->
              try
                let v_num = Hashtbl.find p_vars_tbl v in
                graph.(u) <- v_num :: graph.(u)
              with _ -> ())
            cur_var_deps)
        (SS.elements p_vars);

      graph
    in

    (*
     * Step 4 - Normalise PVar equalities
     * ------------------------------------------------------------------------
     * Normalise equalities involving pvars
     * Detect cyclic dependencies between pvar definitions
     * Break dependencies by introducing new logical variables
     * E.g.
     *      (x = y + 1) * (y = #z) ==> (x = #z + 1) * (y = #z)
     *      (x = y + 1) * (y = (3 * x) - 2) ==>
          (x = #w + 1) * (y = #y) * (#y = (3 * (#y + 1)) - 2)
          where #y = new_lvar_name (y)
     * ------------------------------------------------------------------------
     *)
    let normalise_pvar_equalities
        (graph : int list array)
        (p_vars : SS.t)
        (_ : (string, int) Hashtbl.t) =
      let p_vars = Array.of_list (SS.elements p_vars) in
      let len = Array.length p_vars in
      let visited_tbl = Array.make len false in

      let is_searchable u =
        List.fold_left (fun ac v -> ac && not visited_tbl.(v)) true graph.(u)
      in

      (* a pvar-equality that cannot be lifted to the abstract store
         has to remain in the pure formulae *)
      let remove_assignment var =
        try
          let e = Hashtbl.find pvar_equalities var in
          Stack.push
            (Formula.Eq (LVar (new_lvar_name var), e))
            non_store_pure_assertions;
          Hashtbl.remove pvar_equalities var
        with _ ->
          let msg =
            Printf.sprintf
              "DEATH. normalise_pure_assertions -> normalise_pvar_equalities \
               -> remove_assignment. Var: %s."
              var
          in
          raise (Failure msg)
      in

      (* lifting an assignment to the abstract store *)
      let rewrite_assignment var =
        try
          let le = Hashtbl.find pvar_equalities var in
          let le' = normalise_lexpr ~store ~subst gamma le in
          SStore.put store var le';
          ()
        with _ ->
          let msg =
            Printf.sprintf
              "DEATH. normalise_pure_assertions ->  normalise_pvar_equalities \
               -> rewrite_assignment. Var: %s\n"
              var
          in
          raise (Failure msg)
      in

      (* DFS on pvar dependency graph *)
      let rec search (u : int) =
        let u_var = p_vars.(u) in
        visited_tbl.(u) <- true;
        match is_searchable u with
        | false -> remove_assignment u_var
        | true ->
            List.iter
              (fun v ->
                (* Given that u is searchable this if is very strange *)
                if visited_tbl.(v) then () else search v)
              graph.(u);
            rewrite_assignment u_var
      in
      for i = 0 to len - 1 do
        if not visited_tbl.(i) then search i else ()
      done
    in

    (*
     * Step 5 - The store is always full
     * ------------------------------------------------------------------------
     * PVars that were not associated with a logical expression in the store
     * are mapped to a newly created logical variable
     * ------------------------------------------------------------------------
     *)
    let fill_store args =
      let def_pvars =
        SS.of_list
          (List.concat (List.map (fun f -> SS.elements (Formula.pvars f)) fs))
      in
      let p_vars = Option.value ~default:def_pvars args in
      SS.iter
        (fun var ->
          if not (SStore.mem store var) then (
            SStore.put store var (LVar (new_lvar_name var));
            ()))
        p_vars
    in

    (*
     * Step 6 - Normalise Pure Assertions
     * ------------------------------------------------------------------------
     * The pure assertions that were not lifted to the store need to be
     * normalised
     * ------------------------------------------------------------------------
     *)
    let normalise_pure_assertions () =
      let pfs = PFS.init () in
      let cur_index = ref 0 in

      while not (Stack.is_empty non_store_pure_assertions) do
        let p_assertion = Stack.pop non_store_pure_assertions in
        let p_assertion' =
          normalise_pure_assertion store gamma subst p_assertion
        in
        PFS.extend pfs p_assertion';
        cur_index := !cur_index + 1
      done;

      let store_as_subst = SStore.to_ssubst store in
      PFS.substitution store_as_subst pfs;

      L.verbose (fun m -> m "About to simplify.");
      let _ =
        Simplifications.simplify_pfs_and_gamma pfs gamma ~unification:true
          ~save_spec_vars:(SS.empty, true)
      in
      L.verbose (fun m -> m "Done simplifying.");
      pfs
    in

    (* Doing IT *)
    (* 1 *)
    init_pvar_equalities fs;
    let p_vars =
      Hashtbl.fold (fun var _ ac -> SS.add var ac) pvar_equalities SS.empty
    in

    L.verbose (fun m ->
        m "PVars in normalise_pvar_equalities: %s\n"
          (String.concat ", " (SS.elements p_vars)));

    (* 2 *)
    let p_vars_tbl = get_vars_tbl p_vars in
    (* 3 *)
    let pvars_graph = pvars_graph p_vars p_vars_tbl in
    (* 4 *)
    normalise_pvar_equalities pvars_graph p_vars p_vars_tbl;
    L.verbose (fun m -> m "Going to fill the store now.");
    (* 5 *) fill_store args;
    L.verbose (fun m -> m "Filled store.");
    (* 6 *)
    let result = normalise_pure_assertions () in
    L.verbose (fun m -> m "Finished normalising pure assertions.");
    result

  (** Separate an assertion into:  core_asrts, pure, typing and predicates *)
  let rec separate_assertion (a : Asrt.t) :
      (string * Expr.t list * Expr.t list) list
      * Formula.t list
      * (Expr.t * Type.t) list
      * (string * Expr.t list) list =
    let f = separate_assertion in

    match a with
    | Star (al, ar) ->
        let core_asrts_l, pure_l, types_l, preds_l = f al in
        let core_asrts_r, pure_r, types_r, preds_r = f ar in
        ( core_asrts_l @ core_asrts_r,
          pure_l @ pure_r,
          types_l @ types_r,
          preds_l @ preds_r )
    | GA (a, es1, es2) -> ([ (a, es1, es2) ], [], [], [])
    | Emp -> ([], [], [], [])
    | Types lst -> ([], [], lst, [])
    | Pred (name, params) -> ([], [], [], [ (name, params) ])
    | Pure f -> ([], [ f ], [], [])

  (** Normalise type assertions (Intialise type environment *)
  let normalise_types
      (store : SStore.t)
      (gamma : Type_env.t)
      (subst : SESubst.t)
      (type_list : (Expr.t * Type.t) list) : bool =
    L.verbose (fun m -> m "Normalising types: %d" (List.length type_list));
    List.iter
      (fun (e, t) ->
        L.verbose (fun m ->
            m "%s : %s" ((Fmt.to_to_string Expr.pp) e) (Type.str t)))
      type_list;

    let fe = normalise_logic_expression store gamma subst in

    let type_check_lexpr (le : Expr.t) (t : Type.t) : bool =
      let le_type, success, _ = Typing.type_lexpr gamma le in
      if not success then
        raise
          (Failure
             (Printf.sprintf
                "DEATH. normalise_type_assertions: expression %s not typable"
                ((Fmt.to_to_string Expr.pp) le)))
      else
        Option.fold
          ~some:(fun tt -> t = tt)
          ~none:
            (let new_gamma =
               Typing.reverse_type_lexpr false gamma [ (le, t) ]
             in
             Option.fold
               ~some:(fun new_gamma ->
                 Type_env.extend gamma new_gamma;
                 true)
               ~none:false new_gamma)
          le_type
    in

    let result =
      List.fold_left
        (fun ac (le, t) ->
          if not ac then false
          else
            match fe le with
            | Lit lit -> Literal.type_of lit = t
            | LVar x ->
                Type_env.update gamma x t;
                true
            | PVar _ -> raise (Failure "DEATH. normalise_type_assertions")
            | le -> type_check_lexpr le t)
        true type_list
    in

    result

  (** Normalise Predicate Assertions (Initialise Predicate Set) *)
  let normalise_preds
      (pred_defs : (string, UP.pred) Hashtbl.t)
      (store : SStore.t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (subst : SVal.SESubst.t)
      (pred_asrts : (string * Expr.t list) list) : SPreds.t =
    let fe = normalise_logic_expression store gamma subst in
    let preds = SPreds.init [] in

    List.iter
      (fun (pn, les) ->
        let pred_def = Hashtbl.find_opt pred_defs pn in
        match pred_def with
        | None ->
            L.fail
              (Format.asprintf
                 "Impossible: Predicate %s not found in predicate table during \
                  normalisation."
                 pn)
        | Some pred_def ->
            let params, _ = List.split pred_def.pred.pred_params in
            let params = List.map (fun x -> Expr.PVar x) params in
            let facts =
              List.fold_left
                (fun facts (param, le) ->
                  List.map
                    (fun fact ->
                      Formula.subst_expr_for_expr ~to_subst:param ~subst_with:le
                        fact)
                    facts)
                pred_def.pred.pred_facts (List.combine params les)
            in
            List.iter (fun fact -> PFS.extend pfs fact) facts;
            SPreds.extend preds (pn, List.map fe les))
      pred_asrts;

    preds

  let generate_overlapping_constraints
      (c_asrts : (string * Expr.t list * Expr.t list) list) : Formula.t list =
    let partition (c_asrts : (string * Expr.t list * Expr.t list) list) :
        (string, (Expr.t list * Expr.t list) list) Hashtbl.t =
      let summary : (string, (Expr.t list * Expr.t list) list) Hashtbl.t =
        Hashtbl.create Config.small_tbl_size
      in
      let f_iter (c_asrt : string * Expr.t list * Expr.t list) : unit =
        let a, ins, outs = c_asrt in
        if SPState.is_overlapping_asrt a then
          try
            let other_asrts = Hashtbl.find summary a in
            Hashtbl.replace summary a ((ins, outs) :: other_asrts);
            ()
          with Not_found -> Hashtbl.replace summary a [ (ins, outs) ]
      in
      List.iter f_iter c_asrts;
      summary
    in

    let generate_constraint
        (ins_outs_pair : (Expr.t * Expr.t) list * (Expr.t * Expr.t) list) :
        Formula.t =
      let ins_pairs, outs_pairs = ins_outs_pair in
      let ins_fo =
        Formula.disjunct
          (List.map
             (fun (i1, i2) -> Formula.Not (Formula.Eq (i1, i2)))
             ins_pairs)
      in
      let outs_fo =
        Formula.conjunct
          (List.map (fun (o1, o2) -> Formula.Eq (o1, o2)) outs_pairs)
      in
      Or (ins_fo, outs_fo)
    in

    let summary_to_constraints
        (summary : (string, (Expr.t list * Expr.t list) list) Hashtbl.t) :
        Formula.t list =
      let f_aux (ins1, outs1) (ins2, outs2) =
        if
          List.length ins1 <> List.length ins2
          || List.length outs1 <> List.length outs2
        then raise (Failure "DEATH. generate_overlapping_constraints")
        else (List.combine ins1 ins2, List.combine outs1 outs2)
      in

      Hashtbl.fold
        (fun (_ : _) (a_asrts : (Expr.t list * Expr.t list) list)
             (_ : Formula.t list) : Formula.t list ->
          let pre_constraints =
            List_utils.cross_product a_asrts a_asrts f_aux
          in
          List.map generate_constraint pre_constraints)
        summary []
    in

    summary_to_constraints (partition c_asrts)

  let compose_substs (subst1 : SESubst.t) (subst2 : SESubst.t) : SESubst.t =
    let bnds = SESubst.to_list subst1 in
    let bnds' =
      List.map
        (fun (x, e) -> (x, SESubst.subst_in_expr subst2 ~partial:true e))
        bnds
    in
    SESubst.init bnds'

  let normalise_core_asrts
      (store : SStore.t)
      (pfs : PFS.t)
      (gamma : Type_env.t)
      (svars : SS.t)
      (subst : SESubst.t)
      (c_asrts : (string * Expr.t list * Expr.t list) list) :
      (string * Expr.t list * Expr.t list) list * SESubst.t * SESubst.t =
    let new_pfs = PFS.copy pfs in
    let fe = normalise_logic_expression store gamma subst in
    let c_asrts' =
      List.map
        (fun (a, ins, outs) -> (a, List.map fe ins, List.map fe outs))
        c_asrts
    in
    let fos = generate_overlapping_constraints c_asrts' in
    List.iter (fun fo -> PFS.extend new_pfs fo) fos;
    let subst', _ =
      Simplifications.simplify_pfs_and_gamma new_pfs gamma ~unification:true
        ~save_spec_vars:(SS.empty, true)
    in
    let subst = compose_substs subst subst' in
    let lsvars =
      Expr.Set.of_list (List.map (fun x -> Expr.LVar x) (SS.elements svars))
    in
    let subst' =
      SESubst.filter subst' (fun x _ -> not (Expr.Set.mem x lsvars))
    in

    L.verbose (fun m ->
        m "pfs after overlapping constraints:\n%a\nSubst:\n%a\nSubst':\n%a\n"
          (* FIXME: Shouldn't use PFS.to_list but Fmt.iter and PFS.iter *)
          (Fmt.list ~sep:(Fmt.any "@\n") Formula.pp)
          (PFS.to_list new_pfs) SESubst.pp subst SESubst.pp subst');

    let f_subst = SESubst.subst_in_expr subst' ~partial:true in
    let c_asrts'' =
      List.map
        (fun (a, ins, outs) -> (a, List.map f_subst ins, List.map f_subst outs))
        c_asrts'
    in
    (* ( List.map (normalise_core_asrt store new_pfs pfs gamma subst) c_asrts'',
       subst ) *)
    (c_asrts'', subst', subst)

  let produce_core_asrts
      (astate : SPState.t)
      (core_asrts : (string * Expr.t list * Expr.t list) list) : SPState.t list
      =
    let f_aux (es : Expr.t list) : SS.t * SS.t =
      List.fold_left
        (fun (ret1, ret2) e ->
          (SS.union ret1 (Expr.lvars e), SS.union ret2 (Expr.alocs e)))
        (SS.empty, SS.empty) es
    in

    let (lvars : SS.t), (alocs : SS.t) =
      List.fold_left
        (fun (ret1, ret2) (id, ins, outs) ->
          L.verbose (fun m ->
              m "(%s\n%a\n%a)\n" id SVal.M.full_pp_list ins SVal.M.full_pp_list
                outs);
          let lv_ins, al_ins = f_aux ins in
          let lv_outs, al_outs = f_aux outs in
          let ret1' = SS.union ret1 (SS.union lv_ins lv_outs) in
          let ret2' = SS.union ret2 (SS.union al_ins al_outs) in
          (ret1', ret2'))
        (SS.empty, SS.empty) core_asrts
    in

    let lv_bnds =
      List.map (fun x -> (Expr.LVar x, Expr.LVar x)) (SS.elements lvars)
    in
    let al_bnds =
      List.map (fun l -> (Expr.ALoc l, Expr.ALoc l)) (SS.elements alocs)
    in
    let subst = SESubst.init (lv_bnds @ al_bnds) in

    L.verbose (fun m ->
        m "Subst in produce asrts:\n%a"
          (* FIXME: Shouldn't use PFS.to_list but Fmt.iter and PFS.iter *)
          SESubst.full_pp subst);
    L.verbose (fun m ->
        m "CORE ASSERTIONS TO PRODUCE: %a"
          (Fmt.Dump.list (fun fmt (a, b, c) ->
               Fmt.pf fmt "(%s, %a, %a)" a (Fmt.Dump.list Expr.pp) b
                 (Fmt.Dump.list Expr.pp) c))
          core_asrts);
    core_asrts
    |> List.fold_left
         (fun current_states (a, ins, outs) ->
           let open Syntaxes.List in
           let* current_state = current_states in
           SPState.produce current_state subst (Asrt.GA (a, ins, outs))
           |> (* If some production fails, we ignore *)
           List.filter_map (function
             | Ok x -> Some x
             | Error msg ->
                 L.verbose (fun m ->
                     m
                       "One branch of produce GA failed for: %a!\n\
                        with Message: %a. Might have lost some paths ?" Asrt.pp
                       (Asrt.GA (a, ins, outs))
                       SPState.pp_err msg);
                 None))
         [ astate ]

  let subst_to_pfs ?(svars : SS.t option) (subst : SESubst.t) : Formula.t list =
    let subst_lvs = SESubst.to_list subst in
    let subst_lvs' =
      match svars with
      | Some svars ->
          List.filter
            (fun (e, _) ->
              match e with
              | Expr.LVar x -> SS.mem x svars
              | _ -> false)
            subst_lvs
      | None ->
          List.filter
            (fun (e, _) ->
              match e with
              | Expr.LVar _ -> true
              | _ -> false)
            subst_lvs
    in
    List.map (fun (e, le) -> Formula.Eq (e, le)) subst_lvs'

  let normalise_a_bit (a : Asrt.t) =
    let a = Reduction.reduce_assertion a in
    let subst = SESubst.init [] in

    let rec find_spec_var_eqs (a : Asrt.t) =
      let f = find_spec_var_eqs in
      match a with
      | Star (al, ar) ->
          f al;
          f ar
      | Pure (Eq (LVar x, LVar y))
        when is_spec_var_name x && not (is_spec_var_name y) ->
          SESubst.put subst (LVar y) (LVar x)
      | Pure (Eq (LVar x, LVar y))
        when is_spec_var_name y && not (is_spec_var_name x) ->
          SESubst.put subst (LVar x) (LVar y)
      | _ -> ()
    in
    find_spec_var_eqs a;
    SESubst.substitute_asrt subst ~partial:true a

  (** Given an assertion creates a symbolic state and a substitution *)
  let normalise_assertion
      ~(pred_defs : UP.preds_tbl_t)
      ~(init_data : SPState.init_data)
      ?(pvars : SS.t option)
      (a : Asrt.t) : ((SPState.t * SESubst.t) list, string) result =
    let falsePFs pfs = PFS.mem pfs False in
    let a = normalise_a_bit a in
    let svars = SS.filter is_spec_var_name (Asrt.lvars a) in
    L.verbose (fun m ->
        m "@[<v 2>Normalising assertion:@ %a@]@ svars: @[<h>%a@]" Asrt.pp a
          (Fmt.iter ~sep:Fmt.comma SS.iter Fmt.string)
          svars);

    (* Step 1 -- Preprocess list expressions - resolve l-nth(E, i) when possible  *)
    let a = preprocess_lists a in

    (* Step 2a -- Create empty symbolic heap, symbolic store, typing environment, and substitution *)
    let store = SStore.init [] in
    let gamma = Type_env.init () in
    let subst = SESubst.init [] in

    (* Step 2b -- Separate assertion *)
    let c_asrts, pfs, types, preds =
      try separate_assertion a
      with Failure msg ->
        L.verbose (fun m -> m "I died here terribly with msg: %s!\n" msg);
        raise (Failure msg)
    in

    L.verbose (fun m -> m "Separate assertion subst: %a" SESubst.pp subst);
    L.verbose (fun m -> m "Here are the pfs: %a" PFS.pp (PFS.of_list pfs));

    (* Step 3 -- Normalise type assertions and pure assertions
       * 3.1 - type assertions -> initialises gamma
       * 3.2 - pure assertions -> initialises store and pfs
    *)
    let success = normalise_types store gamma subst types in
    if not success then (
      L.verbose (fun m ->
          m
            "WARNING: normalise_assertion: type assertions could not be \
             normalised");
      Error "normalise_assertion: type assertions could not be normalised")
    else
      let pfs = normalise_pure_assertions store gamma subst pvars pfs in
      if falsePFs pfs then (
        L.verbose (fun m ->
            m "WARNING: normalise_assertion: pure formulae false");
        Error "normalise_assertion: pure formulae false")
      else (
        L.verbose (fun m -> m "Here is the store: %a" SStore.pp store);
        (* Step 4 -- Extend the typing environment using equalities in the pfs *)
        extend_typing_env_using_assertion_info gamma (PFS.to_list pfs);

        (* Step 5 -- normalise core assertions *)
        let c_asrts', subst', subst =
          normalise_core_asrts store pfs gamma svars subst c_asrts
        in

        (* Step 6 -- Extend pfs with info on subst *)
        L.verbose (fun m -> m "Subst before extenzion:\n%a" SESubst.pp subst');
        List.iter (fun pf -> PFS.extend pfs pf) (subst_to_pfs subst');
        List.iter (fun pf -> PFS.extend pfs pf) (subst_to_pfs subst);
        L.verbose (fun m -> m "PFS after extenzion:\n%a" PFS.pp pfs);

        (* Step 7 -- Construct the state *)
        let preds' = normalise_preds pred_defs store pfs gamma subst preds in
        let astate : SPState.t =
          SPState.make_p ~preds:pred_defs ~init_data ~store ~pfs ~gamma
            ~spec_vars:svars ()
        in
        let astate = SPState.set_preds astate preds' in
        let open Syntaxes.List in
        let res =
          let* astate = produce_core_asrts astate c_asrts' in

          (* Step 8 -- Check if the symbolic state makes sense *)
          let mem_constraints = SPState.mem_constraints astate in
          if
            FOSolver.check_satisfiability
              (mem_constraints @ PFS.to_list pfs)
              gamma
          then (
            (* Step 9 -- Final simplifications - TO SIMPLIFY!!! *)
            let _, states = SPState.simplify ~unification:true astate in
            let+ state = states in
            L.verbose (fun m ->
                m "AFTER NORMALISATION: %d states: @\n%a" (List.length states)
                  SPState.pp astate);
            (state, SESubst.copy subst))
          else (
            L.verbose (fun m ->
                m "WARNING: normalise_assertion: returning None");
            [])
        in
        Ok res)
end
