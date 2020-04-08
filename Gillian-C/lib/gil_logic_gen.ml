(* We do these operations in a separate file so that we do not clutter gilgen *)
open Gillian.Gil_syntax
open CConstants
open CLogic
open Compcert
open CompileState
module Str_set = Gillian.Utils.Containers.SS

let id_of_string = Camlcoq.intern_string

let true_name = Camlcoq.extern_atom

let loc_param_name = "loc"

let offs_param_name = "offs"

let pred_name_of_struct struct_name =
  Prefix.generated_preds ^ "struct_" ^ struct_name

let rec_pred_name_of_struct struct_name =
  Prefix.generated_preds ^ "rec_struct_" ^ struct_name

let opt_rec_pred_name_of_struct struct_name =
  Prefix.generated_preds ^ "opt_rec_struct_" ^ struct_name

let ( ++ ) e1 e2 =
  match e2 with
  (* Small optim *)
  | Expr.Lit (Literal.Num 0.) -> e1
  | Expr.Lit (Literal.Num n) -> (
      match e1 with
      | Expr.Lit (Literal.Num 0.) -> e2
      | Expr.BinOp (e1p, BinOp.FPlus, Expr.Lit (Literal.Num k)) ->
          Expr.BinOp (e1p, BinOp.FPlus, Expr.Lit (Literal.Num (n +. k)))
      | e1 -> Expr.BinOp (e1, BinOp.FPlus, e2) )
  | e2 -> Expr.BinOp (e1, BinOp.FPlus, e2)

let ( == ) e1 e2 = if e1 = e2 then Asrt.Emp else Asrt.Pure (Eq (e1, e2))

let types t e =
  let static_error () =
    Fmt.failwith "Statically infered that %a should be of type %s" Expr.pp e
      (Type.str t)
  in
  match e with
  | Expr.PVar _ | LVar _ -> Asrt.Types [ (e, t) ]
  | Lit l when t <> Literal.type_of l -> static_error ()
  | ALoc _ when t <> ObjectType -> static_error ()
  | EList _ when t <> ListType -> static_error ()
  | LstSub _ when t <> ListType -> static_error ()
  | ESet _ when t <> SetType -> static_error ()
  | BinOp _ | UnOp _ | NOp _ ->
      static_error () (* Maybe a more precise message ? *)
  | _ -> Emp

let ( ** ) a1 a2 =
  match (a1, a2) with
  | Asrt.Emp, a2 -> a2
  | a1, Asrt.Emp -> a1
  | _, _         -> Star (a1, a2)

let fold_star l = List.fold_left (fun a b -> a ** b) Asrt.Emp l

let ( &&& ) f1 f2 =
  let open Formula in
  match (f1, f2) with
  | True, f | f, True   -> f
  | False, _ | _, False -> False
  | _                   -> And (f1, f2)

let fold_and l = List.fold_left (fun a b -> a &&& b) Formula.True l

let to_assrt_of_gen_form f =
  match f with
  | Formula.True -> Asrt.Emp
  | _            -> Pure f

type gil_annots = {
  preds : Pred.t list;
  specs : Spec.t list;
  bispecs : BiSpec.t list;
  cenv : Ctypes.composite_env;
}

let empty = { preds = []; specs = []; bispecs = []; cenv = Maps.PTree.empty }

let get_structs_not_annot struct_types =
  let get_name (Ctypes.Composite (id, _, _, _)) = true_name id in
  let struct_names = List.map get_name struct_types in
  let already_annot = !already_annot_structs in
  let structs_not_annot =
    List.filter (fun name -> not (Str_set.mem name already_annot)) struct_names
  in
  let newly_annot =
    Str_set.union already_annot (Str_set.of_list structs_not_annot)
  in
  already_annot_structs := newly_annot;
  structs_not_annot

let assert_of_member cenv members id typ =
  let open Ctypes in
  let mk t v = Expr.EList [ Lit (String t); v ] in
  let field_name = true_name id in
  let pvmember = Expr.PVar field_name in
  let field_val_name = "#i__" ^ field_name ^ "_v" in
  let lvval = Expr.LVar field_val_name in
  let pvloc = Expr.PVar loc_param_name in
  let pvoffs = Expr.Lit (Num 0.) in
  (* let pvoffs = Expr.PVar offs_param_name in *)
  let e_to_use, getter_or_type_pred =
    let open Internal_Predicates in
    let open VTypes in
    match typ with
    | Tint _     -> (mk int_type lvval, Asrt.Pred (int_get, [ pvmember; lvval ]))
    | Tlong _    -> ( mk long_type lvval,
                      Asrt.Pred (long_get, [ pvmember; lvval ]) )
    | Tfloat _   ->
        (mk float_type lvval, Asrt.Pred (float_get, [ pvmember; lvval ]))
    | Tpointer _ -> (pvmember, Asrt.Pred (is_ptr_to_0_opt, [ pvmember ]))
    | _          ->
        failwith
          (Printf.sprintf "unhandled struct field type for now : %s"
             (PrintCsyntax.name_cdecl field_name typ))
  in
  let fo =
    match field_offset cenv id members with
    | Errors.OK f    -> Expr.Lit (Num (float_of_int (Camlcoq.Z.to_int f)))
    | Errors.Error e ->
        failwith
          (Format.asprintf "Invalid member offset : %a@?" Driveraux.print_error
             e)
  in
  let sz = Expr.Lit (Num (float_of_int (Camlcoq.Z.to_int (sizeof cenv typ)))) in
  let perm_exp =
    Expr.Lit (String (ValueTranslation.string_of_permission Memtype.Freeable))
  in
  let mem_ga = LActions.str_ga (GMem SVal) in
  let ga_asrt =
    Asrt.GA
      ( mem_ga,
        [ pvloc; pvoffs ++ fo; pvoffs ++ fo ++ sz ],
        [ e_to_use; perm_exp ] )
  in
  getter_or_type_pred ** ga_asrt

let assert_of_hole (low, high) =
  let pvloc = Expr.PVar loc_param_name in
  (* let pvoffs = Expr.PVar offs_param_name in *)
  let pvoffs = Expr.Lit (Num 0.) in
  let num k = Expr.Lit (Num (float_of_int k)) in
  let perm_e =
    Expr.Lit (String (ValueTranslation.string_of_permission Memtype.Freeable))
  in
  let mem_ga = LActions.str_ga (GMem SVal) in
  Asrt.GA
    ( mem_ga,
      [ pvloc; pvoffs ++ num low; pvoffs ++ num high ],
      [ Expr.Lit Literal.Undefined; perm_e ] )

let gen_pred_of_struct cenv ann struct_name =
  let pred_name = pred_name_of_struct struct_name in
  let pred_ins = [ 0 ] in
  let id = id_of_string struct_name in
  let comp_opt = Maps.PTree.get id cenv in
  let comp =
    match comp_opt with
    | None   -> Fmt.failwith "Structure %s is undefined !" struct_name
    | Some c -> c
  in
  let open Ctypes in
  let () =
    match comp.co_su with
    | Union  -> failwith "union shouldn't be handled by this function"
    | Struct -> ()
  in
  let first_params =
    [
      (loc_param_name, Some Type.ObjectType);
      (* (offs_param_name, Some Type.NumberType) *)
      (* TODO: For now, offset HAS to be 0, that will change *)
    ]
  in
  let struct_params =
    List.map (fun (i, _) -> (true_name i, Some Type.ListType)) comp.co_members
  in
  let pred_params = first_params @ struct_params in
  let pred_num_params = List.length pred_params in
  let def_without_holes =
    List.fold_left
      (fun asrt (id, typ) ->
        asrt ** assert_of_member cenv comp.co_members id typ)
      Asrt.Emp comp.co_members
  in
  let fo idp =
    match field_offset cenv idp comp.co_members with
    | Errors.OK f    -> Camlcoq.Z.to_int f
    | Errors.Error e ->
        failwith
          (Format.asprintf "Invalid member offset : %a@?" Driveraux.print_error
             e)
  in
  let sz t = Camlcoq.Z.to_int (sizeof cenv t) in
  let rec get_holes memb =
    match memb with
    | []                        -> []
    | [ _a ]                    -> []
    | (ida, t) :: (idb, _) :: r ->
        let end_a = fo ida + sz t in
        let start_b = fo idb in
        if end_a < start_b then (end_a, start_b) :: get_holes r else get_holes r
  in
  let holes = get_holes comp.co_members in
  let holes_asserts = List.map assert_of_hole holes in
  let def = fold_star holes_asserts ** def_without_holes in
  let n_pred =
    Pred.
      {
        pred_name;
        pred_ins;
        pred_num_params;
        pred_params;
        pred_pure = false;
        pred_normalised = false;
        pred_definitions = [ (None, def) ];
      }
  in
  { ann with preds = n_pred :: ann.preds }

let trans_binop b =
  match b with
  | CBinOp.LstCons -> failwith "LstCons shouldn't be compiled that way"
  | LstCat         -> failwith "LstCat shouldn't be compiled that way"
  | Plus           -> BinOp.FPlus
  | Equal          -> Equal
  | SetSub         -> BSetSub
  | SetDiff        -> SetDiff

let trans_unop u =
  match u with
  | CUnOp.LstLen -> UnOp.LstLen
  | Not          -> UNot

let trans_nop n =
  match n with
  | CNOp.SetUnion -> NOp.SetUnion

let trans_simpl_expr se =
  match se with
  | CSimplExpr.PVar s -> Expr.PVar s
  | LVar s            -> LVar s
  | Loc s             -> Lit (Loc s)
  | Num f             -> Lit (Num f)
  | Bool b            -> Lit (Bool b)

(* The first element of the result should be a pure assertion : either a formula, or overlapping assertions *)
let trans_sval (sv : CSVal.t) : Asrt.t * Expr.t =
  let open CConstants.VTypes in
  let mk str v = Expr.EList [ Expr.Lit (String str); v ] in
  let tnum = types Type.NumberType in
  let tloc = types Type.ObjectType in
  let tse = trans_simpl_expr in
  match sv with
  | CSVal.Sint se   ->
      let eg = tse se in
      (tnum eg, mk int_type (tse se))
  | Slong se        ->
      let eg = tse se in
      (tnum eg, mk long_type (tse se))
  | Ssingle se      ->
      let eg = tse se in
      (tnum eg, mk single_type (tse se))
  | Sfloat se       ->
      let eg = tse se in
      (tnum eg, mk float_type (tse se))
  | Sptr (se1, se2) ->
      let eg1, eg2 = (tse se1, tse se2) in
      (tloc eg1 ** tnum eg2, Expr.EList [ tse se1; tse se2 ])
  | Sfunptr symb    ->
      let lvar = LVar.alloc () in
      let pred =
        Asrt.Pred
          ( CConstants.Internal_Predicates.fun_ptr,
            [ Lit (String symb); LVar lvar ] )
      in
      (pred, Expr.LVar lvar)

let rec trans_expr (e : CExpr.t) : Asrt.t * Expr.t =
  match e with
  | CExpr.SExpr se          -> (Asrt.Emp, trans_simpl_expr se)
  | SVal sv                 -> trans_sval sv
  | EList el                ->
      let asrts, elp = List.split (List.map trans_expr el) in
      let asrt = Asrt.star asrts in
      (asrt, Expr.EList elp)
  | ESet es                 ->
      let asrts, elp = List.split (List.map trans_expr es) in
      let asrt = Asrt.star asrts in
      (asrt, Expr.ESet elp)
  | BinOp (e1, LstCat, e2)  ->
      let a1, eg1 = trans_expr e1 in
      let a2, eg2 = trans_expr e2 in
      (a1 ** a2, NOp (LstCat, [ eg1; eg2 ]))
  | BinOp (e1, LstCons, e2) ->
      let a1, eg1 = trans_expr e1 in
      let a2, eg2 = trans_expr e2 in
      (a1 ** a2, NOp (LstCat, [ EList [ eg1 ]; eg2 ]))
  | BinOp (e1, b, e2)       ->
      let a1, eg1 = trans_expr e1 in
      let a2, eg2 = trans_expr e2 in
      (a1 ** a2, BinOp (eg1, trans_binop b, eg2))
  | UnOp (u, e)             ->
      let a, eg = trans_expr e in
      (a, UnOp (trans_unop u, eg))
  | NOp (nop, el)           ->
      let asrts, elp = List.split (List.map trans_expr el) in
      let asrt = Asrt.star asrts in
      let gnop = trans_nop nop in
      (asrt, Expr.NOp (gnop, elp))

let rec trans_form (f : CFormula.t) : Asrt.t * Formula.t =
  match f with
  | CFormula.True     -> (Emp, Formula.True)
  | False             -> (Emp, False)
  | Eq (ce1, ce2)     ->
      let f1, eg1 = trans_expr ce1 in
      let f2, eg2 = trans_expr ce2 in
      (f1 ** f2, Eq (eg1, eg2))
  | LessEq (ce1, ce2) ->
      let f1, eg1 = trans_expr ce1 in
      let f2, eg2 = trans_expr ce2 in
      (f1 ** f2, LessEq (eg1, eg2))
  | Less (ce1, ce2)   ->
      let f1, eg1 = trans_expr ce1 in
      let f2, eg2 = trans_expr ce2 in
      (f1 ** f2, Less (eg1, eg2))
  | SetMem (ce1, ce2) ->
      let f1, eg1 = trans_expr ce1 in
      let f2, eg2 = trans_expr ce2 in
      (f1 ** f2, SetMem (eg1, eg2))
  | Not fp            ->
      let a, fpp = trans_form fp in
      (a, Not fpp)
  | Implies (f1, f2)  ->
      let a1, fp1 = trans_form f1 in
      let a2, fp2 = trans_form f2 in
      (a1 ** a2, Or (Not fp1, fp2))
  | ForAll (lvts, f)  ->
      let a, fp = trans_form f in
      (a, ForAll (lvts, fp))

let malloc_chunk_asrt loc struct_sz =
  let num k = Expr.Lit (Num (float_of_int k)) in
  let perm_e =
    Expr.Lit (String (ValueTranslation.string_of_permission Memtype.Freeable))
  in
  let mem_ga = LActions.str_ga (GMem SVal) in
  let ptr_sz = if Archi.ptr64 then 8 else 4 in
  let mk_val i =
    if Archi.ptr64 then
      Expr.EList [ Lit (String CConstants.VTypes.long_type); num i ]
    else Expr.EList [ Lit (String CConstants.VTypes.int_type); num i ]
  in
  Asrt.GA (mem_ga, [ loc; num (-ptr_sz); num 0 ], [ mk_val struct_sz; perm_e ])

let trans_constr ?fname:_ ~malloc ann s c =
  let cenv = ann.cenv in
  let gen_loc_var () = Expr.LVar (LVar.alloc ()) in
  let open CConstants.VTypes in
  let cse = trans_simpl_expr in
  let tnum = types NumberType in
  let tloc = types ObjectType in
  let mem_ga = LActions.str_ga (GMem SVal) in
  (* let mk_num n = Expr.Lit (Num (float_of_int n)) in *)
  (* let zero = mk_num 0 in *)
  let ptr_call p l = Asrt.Pred (Internal_Predicates.ptr_to_0_get, [ p; l ]) in
  let sz = function
    | CSVal.Sint _       -> 4
    | Slong _            -> 8
    | Ssingle _          -> 4
    | Sfloat _           -> 4
    | Sptr _ | Sfunptr _ -> if Archi.ptr64 then 8 else 4
  in
  let a_s, s_e = trans_expr s in
  let locv = gen_loc_var () in
  let pc = ptr_call s_e locv in
  let ga_call loc low high sv =
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission Memtype.Freeable))
    in
    let low_e = Expr.Lit (Num (float_of_int low)) in
    let high_e = Expr.Lit (Num (float_of_int high)) in
    Asrt.GA (mem_ga, [ loc; low_e; high_e ], [ sv; perm ])
  in
  let malloc_chunk siz =
    if malloc then malloc_chunk_asrt locv siz else Asrt.Emp
  in
  let mk str v = Expr.EList [ Expr.Lit (String str); v ] in
  let mk_ptr l o = Expr.EList [ l; o ] in
  match c with
  | CConstructor.ConsExpr (SVal (Sint se)) ->
      let e = cse se in
      let siz = sz (Sint se) in
      let sv = mk int_type e in
      let ga = ga_call locv 0 siz sv in
      ga ** pc ** a_s ** tnum e ** malloc_chunk siz
  | CConstructor.ConsExpr (SVal (Sfloat se)) ->
      let e = cse se in
      let siz = sz (Sfloat se) in
      let sv = mk float_type e in
      let ga = ga_call locv 0 siz sv in
      ga ** pc ** a_s ** tnum e ** malloc_chunk siz
  | CConstructor.ConsExpr (SVal (Ssingle se)) ->
      let e = cse se in
      let siz = sz (Ssingle se) in
      let sv = mk single_type e in
      let ga = ga_call locv 0 siz sv in
      ga ** pc ** a_s ** tnum e
  | CConstructor.ConsExpr (SVal (Slong se)) ->
      let e = cse se in
      let siz = sz (Slong se) in
      let sv = mk long_type e in
      let ga = ga_call locv 0 siz sv in
      ga ** pc ** a_s ** tnum e ** malloc_chunk siz
  | CConstructor.ConsExpr (SVal (Sptr (sl, so))) ->
      let l = cse sl in
      let o = cse so in
      let siz = sz (Sptr (sl, so)) in
      let sv = mk_ptr l o in
      let ga = ga_call locv 0 siz sv in
      ga ** pc ** a_s ** tloc l ** tnum o ** malloc_chunk siz
  | CConstructor.ConsExpr _ ->
      Fmt.failwith "Constructor %a is not handled yet" CConstructor.pp c
  | CConstructor.ConsStruct (sname, el) ->
      let struct_pred = pred_name_of_struct sname in
      let id = id_of_string sname in
      let comp_opt = Maps.PTree.get id cenv in
      let comp =
        match comp_opt with
        | None   -> failwith (Printf.sprintf "Structure %s is undefined !" sname)
        | Some c -> c
      in
      let siz = Camlcoq.Z.to_int comp.Ctypes.co_sizeof in
      let more_asrt, params_fields = List.split (List.map trans_expr el) in
      let pr =
        Asrt.Pred (struct_pred, [ locv (*; zero *) ] @ params_fields)
        ** fold_star more_asrt
      in
      pr ** pc ** malloc_chunk siz

let rec trans_asrt ?(fname = "main") ?(ann = empty) asrt =
  match asrt with
  | CAssert.Star (a1, a2) -> trans_asrt ~ann a1 ** trans_asrt ~ann a2
  | Pure f                ->
      let ma, fp = trans_form f in
      ma ** Pure fp
  | Pred (p, cel)         ->
      let ap, gel = List.split (List.map trans_expr cel) in
      fold_star ap ** Pred (p, gel)
  | Emp                   -> Emp
  | PointsTo (s, c)       -> trans_constr ~fname ~malloc:false ann s c
  | MallocPointsTo (s, c) -> trans_constr ~fname ~malloc:true ann s c

let rec trans_lcmd ?(fname = "main") ?(ann = empty) lcmd =
  let trans_lcmd = trans_lcmd ~fname ~ann in
  let trans_asrt = trans_asrt ~fname ~ann in
  let make_assert = function
    | Asrt.Emp -> []
    | a        -> [ LCmd.SL (SepAssert (a, [])) ]
  in
  match lcmd with
  | CLCmd.Fold (pn, el) ->
      let aps, gel = List.split (List.map trans_expr el) in
      let to_assert = Asrt.star aps in
      make_assert to_assert @ [ SL (Fold (pn, gel, None)) ]
  | Unfold (pn, el)     ->
      let ap, gel = List.split (List.map trans_expr el) in
      let to_assert = Asrt.star ap in
      make_assert to_assert @ [ SL (Unfold (pn, gel, None, false)) ]
  | Assert (a, ex)      -> [ SL (SepAssert (trans_asrt a, ex)) ]
  | If (e, cl1, cl2)    ->
      let f, ge = trans_expr e in
      let gcl1 = List.concat (List.map trans_lcmd cl1) in
      let gcl2 = List.concat (List.map trans_lcmd cl2) in
      make_assert f @ [ If (ge, gcl1, gcl2) ]

let trans_asrt_annot da =
  let { label; existentials } = da in
  let exs, typsb =
    List.split
      (List.map
         (fun (ex, topt) ->
           match topt with
           | None   -> (ex, Asrt.Emp)
           | Some t -> (ex, types t (Expr.LVar ex)))
         existentials)
  in
  let a = fold_star typsb in
  (a, (label, exs))

let trans_pred ?(ann = empty) cl_pred =
  let CPred.
        { name = pred_name; params = pred_params; definitions; ins = pred_ins }
      =
    cl_pred
  in
  let pred_num_params = List.length pred_params in
  let pred_definitions =
    List.map
      (fun (d, a) ->
        match d with
        | None    -> (None, trans_asrt ~fname:pred_name ~ann a)
        | Some da ->
            let ada, gda = trans_asrt_annot da in
            (Some gda, ada ** trans_asrt ~fname:pred_name ~ann a))
      definitions
  in
  Pred.
    {
      pred_name;
      pred_num_params;
      pred_params;
      pred_ins;
      pred_definitions;
      pred_pure = false;
      pred_normalised = false;
    }

let add_trans_pred ann cl_pred =
  { ann with preds = trans_pred ~ann cl_pred :: ann.preds }

let trans_sspec ?(ann = empty) fname sspecs =
  let CSpec.{ pre; posts; spec_annot } = sspecs in
  let tap, spa =
    match spec_annot with
    | None     -> (Asrt.Emp, None)
    | Some spa ->
        let a, (label, exs) = trans_asrt_annot spa in
        (a, Some (label, exs))
  in
  let genv = Asrt.Pred (CConstants.Internal_Predicates.global_env, []) in
  let ta = trans_asrt ~fname ~ann in
  Spec.
    {
      ss_pre = tap ** ta pre ** genv;
      ss_posts = List.map (fun p -> ta p ** genv) posts;
      ss_flag = Flag.Normal;
      ss_to_verify = true;
      ss_label = spa;
    }

let trans_spec ?(ann = empty) cl_spec =
  let CSpec.{ fname; params; sspecs } = cl_spec in
  let result =
    Spec.
      {
        spec_name = fname;
        spec_params = params;
        spec_sspecs = List.map (trans_sspec ~ann fname) sspecs;
        spec_normalised = false;
        spec_to_verify = true;
      }
  in
  let _ =
    List.iter
      (fun (sspec : Spec.st) ->
        if sspec.ss_posts = [] then
          failwith
            ( "Gillian-C: Specification without post-condition for function "
            ^ fname ))
      result.spec_sspecs
  in
  result

let add_trans_spec ann cl_spec =
  { ann with specs = trans_spec ~ann cl_spec :: ann.specs }

let trans_annots clight_prog log_prog =
  let open Ctypes in
  let structs_not_annot = get_structs_not_annot clight_prog.prog_types in
  let struct_annots =
    List.fold_left
      (gen_pred_of_struct clight_prog.prog_comp_env)
      { empty with cenv = clight_prog.prog_comp_env }
      structs_not_annot
  in
  let with_preds =
    List.fold_left add_trans_pred struct_annots log_prog.CProg.preds
  in
  let with_specs = List.fold_left add_trans_spec with_preds log_prog.specs in
  with_specs

let glob_fun_pred symbol target =
  let pname = Internal_Predicates.glob_fun in
  Asrt.Pred (pname, [ Lit (String symbol); Lit (String target) ])

let make_global_env_pred init_asrts =
  let def = fold_star init_asrts in
  Pred.
    {
      pred_name = CConstants.Internal_Predicates.global_env;
      pred_num_params = 0;
      pred_params = [];
      pred_ins = [];
      pred_definitions = [ (None, def) ];
      pred_pure = false;
      pred_normalised = false;
    }

let get_clight_fun clight_prog ident =
  let _, f =
    List.find (fun (a, _) -> Camlcoq.P.eq a ident) clight_prog.Ctypes.prog_defs
  in
  let act_f =
    match f with
    | Gfun (Internal fp) -> fp
    | _                  ->
        failwith
          (Printf.sprintf "This is not a function it seems : %s"
             (true_name ident))
  in
  act_f

let opt_gen param_name pred_name struct_params =
  let lv_params = List.map (fun (s, _) -> Expr.LVar ("#" ^ s)) struct_params in
  let loc = Expr.LVar "#loc" in
  let null = Expr.Lit (LList [ String VTypes.long_type; Num 0. ]) in
  let pvar = Expr.PVar param_name in
  let loc_list = Expr.EList [ loc; Lit (Num 0.) ] in
  let def_null = pvar == null in
  let def_rec =
    (pvar == loc_list) ** types ObjectType loc
    ** Pred (pred_name, loc :: lv_params)
  in
  [ def_null; def_rec ]

let asserts_of_rec_member cenv members id typ =
  let open Ctypes in
  let mk t v = Expr.EList [ Lit (String t); v ] in
  let field_name = true_name id in
  let pvmember = Expr.PVar field_name in
  let field_val_name = "#i__" ^ field_name ^ "_v" in
  let lvval = Expr.LVar field_val_name in
  let pvloc = Expr.PVar loc_param_name in
  let pvoffs = Expr.Lit (Num 0.) in
  (* let pvoffs = Expr.PVar offs_param_name in *)
  let res_to_map =
    let open VTypes in
    match typ with
    | Tint _ ->
        let e = mk int_type lvval in
        [ (e, (pvmember == mk int_type lvval) ** types NumberType lvval) ]
    (* (mk int_type lvval, Asrt.Pred (int_get, [ pvmember; lvval ])) *)
    | Tlong _ ->
        let e = mk int_type lvval in
        [ (e, (pvmember == mk long_type lvval) ** types NumberType lvval) ]
    | Tfloat _ ->
        let e = mk int_type lvval in
        [ (e, (pvmember == mk float_type lvval) ** types NumberType lvval) ]
    | Tpointer (Tstruct (id, _), _) ->
        let struct_name = true_name id in
        let p_name = rec_pred_name_of_struct struct_name in
        let null = Expr.Lit (LList [ String long_type; Num 0. ]) in
        let obj = Expr.EList [ lvval; Lit (Num 0.) ] in
        let comp_opt = Maps.PTree.get id cenv in
        let comp =
          match comp_opt with
          | None   ->
              failwith
                (Printf.sprintf "Structure %s is undefined !" struct_name)
          | Some c -> c
        in
        let struct_params =
          List.map
            (fun (i, _) -> Expr.LVar ("#" ^ field_name ^ "__" ^ true_name i))
            comp.co_members
        in
        [
          (null, pvmember == null);
          ( obj,
            (pvmember == obj) ** types ObjectType lvval
            ** Pred (p_name, lvval :: struct_params) );
        ]
        (* (pvmember, Asrt.Pred (is_ptr_to_0_opt, [ pvmember ])) *)
    | _ ->
        failwith
          (Printf.sprintf "unhandled struct field type for now : %s"
             (PrintCsyntax.name_cdecl field_name typ))
  in
  let fo =
    match field_offset cenv id members with
    | Errors.OK f    -> Expr.Lit (Num (float_of_int (Camlcoq.Z.to_int f)))
    | Errors.Error e ->
        failwith
          (Format.asprintf "Invalid member offset : %a@?" Driveraux.print_error
             e)
  in
  let sz = Expr.Lit (Num (float_of_int (Camlcoq.Z.to_int (sizeof cenv typ)))) in
  let perm_exp =
    Expr.Lit (String (ValueTranslation.string_of_permission Memtype.Freeable))
  in
  let mem_ga = LActions.str_ga (GMem SVal) in
  let ga_asrt sval =
    Asrt.GA
      (mem_ga, [ pvloc; pvoffs ++ fo; pvoffs ++ fo ++ sz ], [ sval; perm_exp ])
  in
  List.map (fun (sval, asrt) -> ga_asrt sval ** asrt) res_to_map

let gen_rec_pred_of_struct cenv ann struct_name =
  let pred_name = rec_pred_name_of_struct struct_name in
  let opt_pred_name = opt_rec_pred_name_of_struct struct_name in
  let pred_ins = [ 0 ] in
  let id = id_of_string struct_name in
  let comp_opt = Maps.PTree.get id cenv in
  let comp =
    match comp_opt with
    | None   ->
        failwith (Printf.sprintf "Structure %s is undefined !" struct_name)
    | Some c -> c
  in
  let open Ctypes in
  let () =
    match comp.co_su with
    | Union  -> failwith "union shouldn't be handled by this function"
    | Struct -> ()
  in
  let first_params =
    [
      (loc_param_name, Some Type.ObjectType);
      (* (offs_param_name, Some Type.NumberType) *)
      (* TODO: For now, offset HAS to be 0, that will change *)
    ]
  in
  let struct_params =
    List.map (fun (i, _) -> (true_name i, Some Type.ListType)) comp.co_members
  in
  let pred_params = first_params @ struct_params in
  let pred_num_params = List.length pred_params in
  let defs_without_holes =
    List.fold_left
      (fun al (id, typ) ->
        let new_al = asserts_of_rec_member cenv comp.co_members id typ in
        let list_of_list =
          List.map (fun a -> List.map (fun na -> a ** na) new_al) al
        in
        List.concat list_of_list)
      [ Asrt.Emp ] comp.co_members
  in
  let fo idp =
    match field_offset cenv idp comp.co_members with
    | Errors.OK f    -> Camlcoq.Z.to_int f
    | Errors.Error e ->
        failwith
          (Format.asprintf "Invalid member offset : %a@?" Driveraux.print_error
             e)
  in
  let sz t = Camlcoq.Z.to_int (sizeof cenv t) in
  let rec get_holes memb =
    match memb with
    | []                        -> []
    | [ _a ]                    -> []
    | (ida, t) :: (idb, _) :: r ->
        let end_a = fo ida + sz t in
        let start_b = fo idb in
        if end_a < start_b then (end_a, start_b) :: get_holes r else get_holes r
  in
  let holes = get_holes comp.co_members in
  let hole_assert = fold_star (List.map assert_of_hole holes) in
  let siz = Camlcoq.Z.to_int comp.Ctypes.co_sizeof in
  let malloc = malloc_chunk_asrt (Expr.PVar loc_param_name) siz in
  let pred_definitions =
    List.map
      (fun def -> (None, def ** hole_assert ** malloc))
      defs_without_holes
  in
  let rec_pred =
    Pred.
      {
        pred_name;
        pred_ins;
        pred_num_params;
        pred_params;
        pred_pure = false;
        pred_normalised = false;
        pred_definitions;
      }
  in
  let opt_param_name = "x" in
  let opt_defs = opt_gen opt_param_name pred_name struct_params in
  let opt_pred =
    Pred.
      {
        pred_name = opt_pred_name;
        pred_ins = [ 0 ];
        pred_num_params = 1;
        pred_params = [ (opt_param_name, Some Type.ListType) ];
        pred_pure = false;
        pred_normalised = false;
        pred_definitions = List.map (fun x -> (None, x)) opt_defs;
      }
  in
  { ann with preds = opt_pred :: rec_pred :: ann.preds }

let gen_bi_preds clight_prog =
  let open Ctypes in
  let structs_not_annot = get_structs_not_annot clight_prog.prog_types in
  List.fold_left
    (gen_rec_pred_of_struct clight_prog.prog_comp_env)
    { empty with cenv = clight_prog.prog_comp_env }
    structs_not_annot

let predicate_from_triple (pn, csmt, ct) =
  let is_c_ptr_to_struct = function
    | Ctypes.Tpointer (Ctypes.Tstruct _, _) -> true
    | _ -> false
  in
  let struct_name = function
    | Ctypes.Tpointer (Ctypes.Tstruct (id, _), _) -> true_name id
    | _ -> failwith "Cannot happen"
  in
  let pred pname = Asrt.Pred (pname, [ Expr.PVar pn ]) in
  let open Internal_Predicates in
  match csmt with
  | AST.Tint when (not Archi.ptr64) && is_c_ptr_to_struct ct ->
      pred (opt_rec_pred_name_of_struct (struct_name ct))
  | AST.Tlong when Archi.ptr64 && is_c_ptr_to_struct ct ->
      pred (opt_rec_pred_name_of_struct (struct_name ct))
  | AST.Tint -> pred is_int
  | AST.Tlong -> pred is_long
  | AST.Tsingle -> pred is_single
  | AST.Tfloat -> pred is_float
  | _ ->
      failwith
        (Printf.sprintf
           "Don't know how to handle the following type as a bispec function \
            parameter %s"
           (PrintAST.name_of_type csmt))

let generate_bispec clight_prog fname ident f =
  let rec combine a b c =
    match (a, b, c) with
    | [], [], [] -> []
    | tn :: ra, csmt :: rb, (_, ct) :: rc -> (tn, csmt, ct) :: combine ra rb rc
    | _ -> failwith "unmatching number of params/types/ctypes"
  in
  let params = f.Csharpminor.fn_params in
  let sig_args = f.fn_sig.sig_args in
  let true_params = List.map true_name params in
  let clight_fun = get_clight_fun clight_prog ident in
  let cligh_params = clight_fun.Clight.fn_params in
  let triples = combine true_params sig_args cligh_params in
  (* Right now, triples are : (param_name, csharpminor type, c type)
     The C type will be used to discriminate long/int from pointers *)
  let pred_list = List.map predicate_from_triple triples in
  let prec_without_genv = fold_star pred_list in
  let genv_pred = Asrt.Pred (CConstants.Internal_Predicates.global_env, []) in
  let prec = prec_without_genv ** genv_pred in
  BiSpec.
    {
      bispec_name = fname;
      bispec_params = true_params;
      bispec_pres = [ prec ];
      bispec_normalised = false;
    }

let add_bispec ann clight_prog fname ident f =
  {
    ann with
    bispecs = generate_bispec clight_prog fname ident f :: ann.bispecs;
  }
