(* We do these operations in a separate file so that we do not clutter gilgen *)
open Gillian.Gil_syntax
open CConstants
open CLogic
open Compcert
open CompileState
module Str_set = Gillian.Utils.Containers.SS
open Asrt.Infix
open Formula.Infix
module CoreP = Constr.Core

let id_of_string = Camlcoq.intern_string
let true_name = Camlcoq.extern_atom
let loc_param_name = "loc"
let ofs_param_name = "ofs"

let pred_name_of_struct struct_name =
  Prefix.generated_preds ^ "struct_" ^ struct_name

let rec_pred_name_of_struct struct_name =
  Prefix.generated_preds ^ "rec_struct_" ^ struct_name

let opt_rec_pred_name_of_struct struct_name =
  Prefix.generated_preds ^ "opt_rec_struct_" ^ struct_name

let fresh_lvar ?(fname = "") () =
  let pre = "_lvar_i_" in
  Generators.gen_str ~fname pre

let rec split3_expr_comp = function
  | [] -> ([], [], [])
  | (x, y, z) :: l ->
      let rx, ry, rz = split3_expr_comp l in
      (x :: rx, y @ ry, z :: rz)

let ( ++ ) = Expr.Infix.( + )

let ( == ) e1 e2 =
  match e1 #== e2 with
  | True -> Asrt.Emp
  | f -> Pure f

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

let fold_star l = List.fold_left ( ** ) Emp l
let fold_and l = List.fold_left (fun a b -> a #&& b) Formula.True l

let to_assrt_of_gen_form f =
  match f with
  | Formula.True -> Asrt.Emp
  | _ -> Pure f

type gil_annots = {
  preds : Pred.t list;
  specs : Spec.t list;
  onlyspecs : Spec.t list;
  lemmas : Lemma.t list;
  bispecs : BiSpec.t list;
  cenv : Ctypes.composite_env;
  imports : (string * bool) list;
}

let empty =
  {
    preds = [];
    specs = [];
    bispecs = [];
    onlyspecs = [];
    lemmas = [];
    cenv = Maps.PTree.empty;
    imports = [];
  }

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
  let field_name = true_name id in
  let pvloc = Expr.PVar loc_param_name in
  let pvofs = Expr.PVar ofs_param_name in
  let pvmember = Expr.PVar field_name in
  let fo =
    match field_offset cenv id members with
    | Errors.OK (f, Full) -> Expr.int_z (ValueTranslation.int_of_z f)
    | Errors.OK _ -> Fmt.failwith "Unsupported: bitfield members"
    | Errors.Error e ->
        Fmt.failwith "Invalid member offset : %a@?" Driveraux.print_error e
  in
  (* The following bit of code should be refactored to be made cleaner ... *)
  if
    match typ with
    | Tstruct _ -> true
    | _ -> false
  then
    let struct_name, struct_id =
      match typ with
      | Tstruct (id, _) -> (true_name id, id)
      | _ -> failwith "impossible"
    in
    let pred_name = pred_name_of_struct struct_name in
    let arg_number =
      List.length (Option.get (Maps.PTree.get struct_id cenv)).co_members
    in
    let args_without_ins =
      List.init arg_number (fun k ->
          Expr.LVar ("#i__" ^ field_name ^ "_" ^ string_of_int k))
    in
    let list_is_components =
      let open Formula.Infix in
      Asrt.Pure pvmember #== (Expr.list args_without_ins)
    in
    let ofs =
      let open Expr.Infix in
      pvofs + fo
    in
    let args = pvloc :: ofs :: args_without_ins in
    let pred_call = Asrt.Pred (pred_name, args) in
    list_is_components ** pred_call
  else if
    match typ with
    | Tarray _ -> true
    | _ -> false
  then
    let ty, n =
      match typ with
      | Tarray (ty, n, _) -> (ty, n)
      | _ -> failwith "impossible"
    in
    let n = ValueTranslation.int_of_z n in
    let n_e = Expr.int_z n in
    let chunk =
      match Ctypes.access_mode ty with
      | By_value chunk -> chunk
      | _ -> failwith "Array in a structure containing complicated types"
    in
    Constr.Core.array ~loc:pvloc ~ofs:(pvofs ++ fo) ~chunk ~size:n_e
      ~sval_arr:pvmember ~perm:(Some Freeable)
  else
    let mk t v = Expr.list [ Expr.string t; v ] in
    let field_val_name = "#i__" ^ field_name ^ "_v" in
    let lvval = Expr.LVar field_val_name in
    let e_to_use, getter_or_type_pred =
      let open Internal_Predicates in
      let open VTypes in
      match typ with
      | Tint _ -> (mk int_type lvval, Asrt.Pred (int_get, [ pvmember; lvval ]))
      | Tlong _ ->
          (mk long_type lvval, Asrt.Pred (long_get, [ pvmember; lvval ]))
      | Tfloat _ ->
          (mk float_type lvval, Asrt.Pred (float_get, [ pvmember; lvval ]))
      | Tpointer _ -> (pvmember, Asrt.Pred (is_ptr_opt, [ pvmember ]))
      | _ ->
          failwith
            (Printf.sprintf "unhandled struct field type for now : %s"
               (PrintCsyntax.name_cdecl field_name typ))
    in
    let chunk =
      match Ctypes.access_mode typ with
      | By_value chunk -> chunk
      | _ -> failwith "Invalid access mode for some type"
    in
    let ga_asrt =
      CoreP.single ~loc:pvloc ~ofs:(pvofs ++ fo) ~chunk ~sval:e_to_use
        ~perm:(Some Freeable)
    in
    getter_or_type_pred ** ga_asrt

let assert_of_hole (low, high) =
  let pvloc = Expr.PVar loc_param_name in
  let pvoffs = Expr.PVar ofs_param_name in
  let num k = Expr.Lit (Int k) in
  CoreP.hole ~loc:pvloc
    ~low:(pvoffs ++ num low)
    ~high:(pvoffs ++ num high)
    ~perm:(Some Freeable)

let gen_pred_of_struct cenv ann struct_name =
  let pred_name = pred_name_of_struct struct_name in
  let pred_ins = [ 0; 1 ] in
  let id = id_of_string struct_name in
  let comp_opt = Maps.PTree.get id cenv in
  let comp =
    match comp_opt with
    | None -> Fmt.failwith "Structure %s is undefined !" struct_name
    | Some c -> c
  in
  let open Ctypes in
  let () =
    match comp.co_su with
    | Union -> failwith "union shouldn't be handled by this function"
    | Struct -> ()
  in
  let first_params =
    [
      (loc_param_name, Some Type.ObjectType); (ofs_param_name, Some Type.IntType);
    ]
  in
  let struct_params =
    List.map
      (function
        | Member_plain (i, _) -> (true_name i, Some Type.ListType)
        | Member_bitfield _ -> failwith "Unsupported bitfield members")
      comp.co_members
  in
  let pred_params = first_params @ struct_params in
  let pred_num_params = List.length pred_params in
  let def_without_holes =
    List.fold_left
      (fun asrt member ->
        match member with
        | Member_plain (id, typ) ->
            asrt ** assert_of_member cenv comp.co_members id typ
        | Member_bitfield _ -> failwith "Unsupported bitfield members")
      Asrt.Emp comp.co_members
  in
  let fo idp =
    match field_offset cenv idp comp.co_members with
    | Errors.OK (f, Full) -> ValueTranslation.int_of_z f
    | Errors.OK _ -> failwith "Unsupported bitfield members"
    | Errors.Error e ->
        failwith
          (Format.asprintf "Invalid member offset : %a@?" Driveraux.print_error
             e)
  in
  let sz t = ValueTranslation.int_of_z (sizeof cenv t) in
  let rec get_holes memb =
    match memb with
    | [] -> []
    | [ _a ] -> []
    | Member_plain (ida, t) :: (Member_plain (idb, _) :: _ as r) ->
        let end_a = Z.add (fo ida) (sz t) in
        let start_b = fo idb in
        if end_a < start_b then (end_a, start_b) :: get_holes r else get_holes r
    | _ -> failwith "Unsupported bitfield members"
  in

  let holes = get_holes comp.co_members in
  let holes_asserts = List.map assert_of_hole holes in
  let def = fold_star holes_asserts ** def_without_holes in
  (* TODO (Alexis): How to handle changes in structs? *)
  let n_pred =
    Pred.
      {
        pred_name;
        pred_source_path = None;
        pred_internal = true;
        pred_ins;
        pred_num_params;
        pred_params;
        pred_facts = [ (* FIXME: there are probably some facts to get *) ];
        pred_pure = false;
        pred_abstract = false;
        pred_nounfold = false;
        pred_normalised = false;
        pred_definitions = [ (None, def, []) ];
      }
  in
  { ann with preds = n_pred :: ann.preds }

let trans_binop b =
  match b with
  | CBinOp.LstCons -> failwith "LstCons shouldn't be compiled that way"
  | LstCat -> failwith "LstCat shouldn't be compiled that way"
  | PtrPlus -> failwith "PtrPlus shouldn't be compiled that way"
  | Plus -> BinOp.IPlus
  | Times -> BinOp.ITimes
  | Minus -> BinOp.IMinus
  | Div -> BinOp.IDiv
  | Equal -> Equal
  | SetSub -> BSetSub
  | SetDiff -> SetDiff
  | SetMem -> BSetMem
  | LessThan -> ILessThan
  | And -> BAnd
  | Or -> BOr

let trans_unop u =
  match u with
  | CUnOp.LstLen -> UnOp.LstLen
  | Not -> UNot

let trans_nop n =
  match n with
  | CNOp.SetUnion -> NOp.SetUnion

let trans_simpl_expr se =
  match se with
  | CSimplExpr.PVar s -> Expr.PVar s
  | LVar s -> LVar s
  | Loc s -> Lit (Loc s)
  | Int i -> Lit (Int i)
  | Bool b -> Lit (Bool b)
  | String s -> Lit (String s)

(* The first element of the result should be a pure assertion : either a formula, or overlapping assertions,
   The second element is the list of created variables, the third is the expression to be used
*)
let trans_sval (sv : CSVal.t) : Asrt.t * Var.t list * Expr.t =
  let open CConstants.VTypes in
  let mk str v = Expr.EList [ Expr.Lit (String str); v ] in
  let tnum = types Type.NumberType in
  let tint = types Type.IntType in
  let tloc = types Type.ObjectType in
  let tse = trans_simpl_expr in
  match sv with
  | CSVal.Sint se ->
      let eg = tse se in
      (tint eg, [], mk int_type (tse se))
  | Slong se ->
      let eg = tse se in
      (tint eg, [], mk long_type (tse se))
  | Ssingle se ->
      let eg = tse se in
      (tnum eg, [], mk single_type (tse se))
  | Sfloat se ->
      let eg = tse se in
      (tnum eg, [], mk float_type (tse se))
  | Sptr (se1, se2) ->
      let eg1, eg2 = (tse se1, tse se2) in
      (tloc eg1 ** tint eg2, [], Expr.EList [ tse se1; tse se2 ])
  | Sfunptr symb ->
      let lvar = fresh_lvar () in
      let ptr = Expr.LVar lvar in
      let pred = Constr.Others.fun_ptr ~ptr ~symb in
      (pred, [ lvar ], ptr)

(** Returns assertions that are necessary to define the expression,
      the created variable for binding when necessary, and the used expression *)
let rec trans_expr (e : CExpr.t) : Asrt.t * Var.t list * Expr.t =
  match e with
  | CExpr.SExpr se -> (Asrt.Emp, [], trans_simpl_expr se)
  | SVal sv -> trans_sval sv
  | EList el ->
      let asrts, vars, elp = split3_expr_comp (List.map trans_expr el) in
      let asrt = Asrt.star asrts in
      (asrt, vars, Expr.EList elp)
  | ESet es ->
      let asrts, vars, elp = split3_expr_comp (List.map trans_expr es) in
      let asrt = Asrt.star asrts in
      (asrt, vars, Expr.ESet elp)
  | BinOp (e1, LstCat, e2) ->
      let a1, v1, eg1 = trans_expr e1 in
      let a2, v2, eg2 = trans_expr e2 in
      (a1 ** a2, v1 @ v2, Expr.list_cat eg1 eg2)
  | BinOp (e1, LstCons, e2) ->
      let a1, v1, eg1 = trans_expr e1 in
      let a2, v2, eg2 = trans_expr e2 in
      (a1 ** a2, v1 @ v2, Expr.list_cat (EList [ eg1 ]) eg2)
  | BinOp (e1, PtrPlus, e2) -> (
      let a1, v1, ptr = trans_expr e1 in
      let a2, v2, to_add = trans_expr e2 in
      match ptr with
      | Expr.EList [ loc; ofs ] ->
          (a1 ** a2, v1 @ v2, Expr.EList [ loc; Expr.Infix.( + ) ofs to_add ])
      | ptr ->
          let res_lvar = fresh_lvar () in
          let res = Expr.LVar res_lvar in
          ( a1 ** a2 ** Constr.Others.ptr_add ~ptr ~to_add ~res,
            res_lvar :: (v1 @ v2),
            res ))
  | BinOp (e1, b, e2) ->
      let a1, v1, eg1 = trans_expr e1 in
      let a2, v2, eg2 = trans_expr e2 in
      (a1 ** a2, v1 @ v2, BinOp (eg1, trans_binop b, eg2))
  | UnOp (u, e) ->
      let a, v, eg = trans_expr e in
      (a, v, UnOp (trans_unop u, eg))
  | NOp (nop, el) ->
      let asrts, vs, elp = split3_expr_comp (List.map trans_expr el) in
      let asrt = Asrt.star asrts in
      let gnop = trans_nop nop in
      (asrt, vs, Expr.NOp (gnop, elp))
  | LstSub (lst, start, len) ->
      let a1, v1, lst = trans_expr lst in
      let a2, v2, start = trans_expr start in
      let a3, v3, len = trans_expr len in
      (a1 ** a2 ** a3, v1 @ v2 @ v3, Expr.list_sub ~lst ~start ~size:len)

let rec trans_form (f : CFormula.t) : Asrt.t * Var.t list * Formula.t =
  let open Formula.Infix in
  match f with
  | CFormula.True -> (Emp, [], Formula.True)
  | False -> (Emp, [], False)
  | Eq (ce1, ce2) ->
      let f1, v1, eg1 = trans_expr ce1 in
      let f2, v2, eg2 = trans_expr ce2 in
      (f1 ** f2, v1 @ v2, eg1 #== eg2)
  | LessEq (ce1, ce2) ->
      let f1, v1, eg1 = trans_expr ce1 in
      let f2, v2, eg2 = trans_expr ce2 in
      (f1 ** f2, v1 @ v2, eg1 #<= eg2)
  | Less (ce1, ce2) ->
      let f1, v1, eg1 = trans_expr ce1 in
      let f2, v2, eg2 = trans_expr ce2 in
      (f1 ** f2, v1 @ v2, eg1 #< eg2)
  | SetMem (ce1, ce2) ->
      let f1, v1, eg1 = trans_expr ce1 in
      let f2, v2, eg2 = trans_expr ce2 in
      (f1 ** f2, v1 @ v2, SetMem (eg1, eg2))
  | Not fp ->
      let a, v, fpp = trans_form fp in
      (a, v, fnot fpp)
  | Or (f1, f2) ->
      let a1, v1, fp1 = trans_form f1 in
      let a2, v2, fp2 = trans_form f2 in
      (a1 ** a2, v1 @ v2, fp1 #|| fp2)
  | And (f1, f2) ->
      let a1, v1, fp1 = trans_form f1 in
      let a2, v2, fp2 = trans_form f2 in
      (a1 ** a2, v1 @ v2, fp1 #&& fp2)
  | Implies (f1, f2) ->
      let a1, v1, fp1 = trans_form f1 in
      let a2, v2, fp2 = trans_form f2 in
      (a1 ** a2, v1 @ v2, fp1 #=> fp2)
  | ForAll (lvts, f) ->
      let a, v, fp = trans_form f in
      (a, v, ForAll (lvts, fp))

let malloc_chunk_asrt loc beg_ofs struct_sz =
  Constr.Others.malloced ~ptr:(loc, beg_ofs) ~total_size:struct_sz

let trans_constr ?fname:_ ~(typ : CAssert.points_to_type) ann s c =
  let malloc =
    match typ with
    | Malloced -> true
    | _ -> false
  in
  let cenv = ann.cenv in
  let gen_loc_var () = Expr.LVar (fresh_lvar ()) in
  let gen_ofs_var () = Expr.LVar (fresh_lvar ()) in
  let open CConstants.VTypes in
  let cse = trans_simpl_expr in
  let tnum = types NumberType in
  let tint = types IntType in
  let tloc = types ObjectType in
  (* let mk_num n = Expr.Lit (Num (float_of_int n)) in *)
  (* let zero = mk_num 0 in *)
  let ptr_call p l o = Asrt.Pred (Internal_Predicates.ptr_get, [ p; l; o ]) in
  let sz = function
    | CSVal.Sint _ -> 4
    | Slong _ -> 8
    | Ssingle _ -> 4
    | Sfloat _ -> 4
    | Sptr _ | Sfunptr _ -> if Archi.ptr64 then 8 else 4
  in
  let sz x = sz x |> Z.of_int in
  let interpret_s ~typ s =
    match typ with
    | CAssert.Global ->
        (* In case it is not just a variable, we crash. It is not a handled feature yet since the permission
           for global environment is "Writable" and not "Freeable", we need to modify the generated predicates. *)
        let () =
          match c with
          | CConstructor.ConsStruct _ ->
              failwith "global structures not handled yet"
          | _ -> ()
        in
        let symb =
          match s with
          | CExpr.SExpr (String symb) -> symb
          | _ -> failwith "Impossible by parser"
        in
        let locv = gen_loc_var () in
        let ofsv = Expr.int 0 in
        let p = Constr.Core.symbol ~symb ~loc:locv in
        (p, locv, ofsv)
    | _ ->
        let a_s, _, s_e = trans_expr s in
        let locv = gen_loc_var () in
        let ofsv = gen_ofs_var () in
        let pc = ptr_call s_e locv ofsv in
        (pc ** a_s, locv, ofsv)
  in
  let to_assert, locv, ofsv = interpret_s ~typ s in
  let malloc_chunk siz =
    if malloc then malloc_chunk_asrt locv ofsv siz else Asrt.Emp
  in
  let mk str v = Expr.list [ Expr.string str; v ] in
  let mk_ptr l o = Expr.list [ l; o ] in
  match c with
  | CConstructor.ConsExpr (SVal (Sint se)) ->
      let e = cse se in
      let chunk = Chunk.Mint32 in
      let sv = mk int_type e in
      let siz = sz (Sint se) in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval:sv ~perm:(Some Freeable)
      in
      ga ** to_assert ** tint e ** malloc_chunk siz
  | ConsExpr (SVal (Sfloat se)) ->
      let e = cse se in
      let chunk = Chunk.Mfloat32 in
      let siz = sz (Sfloat se) in
      let sv = mk float_type e in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval:sv ~perm:(Some Freeable)
      in
      ga ** to_assert ** tnum e ** malloc_chunk siz
  | ConsExpr (SVal (Ssingle se)) ->
      let e = cse se in
      let chunk = Chunk.Mfloat32 in
      let siz = sz (Ssingle se) in
      let sv = mk single_type e in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval:sv ~perm:(Some Freeable)
      in
      ga ** to_assert ** tnum e ** malloc_chunk siz
  | ConsExpr (SVal (Slong se)) ->
      let e = cse se in
      let chunk = Chunk.Mint64 in
      let siz = sz (Slong se) in
      let sv = mk long_type e in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval:sv ~perm:(Some Freeable)
      in
      ga ** to_assert ** tint e ** malloc_chunk siz
  | ConsExpr (SVal (Sptr (sl, so))) ->
      let l = cse sl in
      let o = cse so in
      let chunk = Chunk.ptr in
      let siz = sz (Sptr (sl, so)) in
      let sv = mk_ptr l o in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval:sv ~perm:(Some Freeable)
      in
      ga ** to_assert ** tloc l ** tint o ** malloc_chunk siz
  | ConsExpr (SVal (Sfunptr fname)) ->
      let l = gen_loc_var () in
      let o = gen_ofs_var () in
      let chunk = Chunk.ptr in
      let ptr = mk_ptr l o in
      let siz = sz (Sfunptr fname) in
      let ga_single =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval:ptr ~perm:(Some Freeable)
      in
      let funptr_pred = Constr.Others.fun_ptr ~ptr ~symb:fname in
      ga_single ** to_assert ** funptr_pred ** tloc l ** tint o
      ** malloc_chunk siz
  | ConsExpr _ ->
      Fmt.failwith "Constructor %a is not handled yet" CConstructor.pp c
  | ConsStruct (sname, el) ->
      let struct_pred = pred_name_of_struct sname in
      let id = id_of_string sname in
      let comp_opt = Maps.PTree.get id cenv in
      let comp =
        match comp_opt with
        | None -> failwith (Printf.sprintf "Structure %s is undefined !" sname)
        | Some c -> c
      in
      let siz = ValueTranslation.int_of_z comp.Ctypes.co_sizeof in
      let more_asrt, _, params_fields =
        split3_expr_comp (List.map trans_expr el)
      in
      let pr =
        Asrt.Pred (struct_pred, [ locv; ofsv ] @ params_fields)
        ** fold_star more_asrt
      in
      pr ** to_assert ** malloc_chunk siz

let rec trans_asrt ~fname ~ann asrt =
  match asrt with
  | CAssert.Star (a1, a2) ->
      trans_asrt ~fname ~ann a1 ** trans_asrt ~fname ~ann a2
  | Array { ptr; chunk; size; content; malloced } ->
      let a1, _, ptr = trans_expr ptr in
      let a2, _, size = trans_expr size in
      let a3, _, content = trans_expr content in
      let malloc_p =
        if malloced then
          let open Expr.Infix in
          let csize = Expr.int (Chunk.size chunk) in
          let total_size = size * csize in
          Constr.Others.malloced_abst ~ptr ~total_size
        else Asrt.Emp
      in
      a1 ** a2 ** a3
      ** Constr.Others.array_ptr ~ptr ~chunk ~size ~content
      ** malloc_p
  | Malloced (e1, e2) ->
      let a1, _, ce1 = trans_expr e1 in
      let a2, _, ce2 = trans_expr e2 in
      a1 ** a2 ** Constr.Others.malloced_abst ~ptr:ce1 ~total_size:ce2
  | Zeros (e1, e2) ->
      let a1, _, ce1 = trans_expr e1 in
      let a2, _, ce2 = trans_expr e2 in
      a1 ** a2 ** Constr.Others.zeros_ptr_size ~ptr:ce1 ~size:ce2
  | Undefs (e1, e2) ->
      let a1, _, ce1 = trans_expr e1 in
      let a2, _, ce2 = trans_expr e2 in
      a1 ** a2 ** Constr.Others.undefs_ptr_size ~ptr:ce1 ~size:ce2
  | Pure f ->
      let ma, _, fp = trans_form f in
      ma ** Pure fp
  | Pred (p, cel) ->
      let ap, _, gel = split3_expr_comp (List.map trans_expr cel) in
      fold_star ap ** Pred (p, gel)
  | Emp -> Emp
  | PointsTo { ptr = s; constr = c; typ } -> trans_constr ~fname ~typ ann s c

let rec trans_lcmd ~fname ~ann lcmd =
  let trans_lcmd = trans_lcmd ~fname ~ann in
  let trans_asrt = trans_asrt ~fname ~ann in
  let make_assert ~bindings = function
    | Asrt.Emp -> []
    | a -> [ LCmd.SL (SepAssert (a, bindings)) ]
  in
  match lcmd with
  | CLCmd.Apply (pn, el) ->
      let aps, bindings, gel = split3_expr_comp (List.map trans_expr el) in
      let to_assert = Asrt.star aps in
      `Normal (make_assert ~bindings to_assert @ [ SL (ApplyLem (pn, gel, [])) ])
  | CLCmd.Fold (pn, el) ->
      let aps, bindings, gel = split3_expr_comp (List.map trans_expr el) in
      let to_assert = Asrt.star aps in
      `Normal (make_assert ~bindings to_assert @ [ SL (Fold (pn, gel, None)) ])
  | Unfold { pred; params; bindings; recursive } ->
      let ap, vs, gel = split3_expr_comp (List.map trans_expr params) in
      let to_assert = Asrt.star ap in
      `Normal
        (make_assert ~bindings:vs to_assert
        @ [ SL (Unfold (pred, gel, bindings, recursive)) ])
  | Unfold_all pred_name -> `Normal [ SL (GUnfold pred_name) ]
  | Assert (a, ex) -> `Normal [ SL (SepAssert (trans_asrt a, ex)) ]
  | Branch f ->
      let to_assert, bindings, f_gil = trans_form f in
      `Normal (make_assert ~bindings to_assert @ [ Branch f_gil ])
  | If (e, cl1, cl2) ->
      let trans_normal_lcmd lcmd =
        match trans_lcmd lcmd with
        | `Normal x -> x
        | `Invariant _ ->
            Fmt.failwith "Invariant inside if/else in logic command"
      in
      let f, vs, ge = trans_expr e in
      let gcl1 = List.concat_map trans_normal_lcmd cl1 in
      let gcl2 = List.concat_map trans_normal_lcmd cl2 in
      `Normal (make_assert ~bindings:vs f @ [ If (ge, gcl1, gcl2) ])
  | Invariant { bindings; assertion } ->
      let asrt = trans_asrt assertion in
      let genv = Asrt.Pred (CConstants.Internal_Predicates.global_env, []) in
      `Invariant (SLCmd.Invariant (genv ** asrt, bindings))
  | SymbExec -> `Normal [ SL SymbExec ]

let trans_asrt_annot da =
  let { label; existentials } = da in
  let exs, typsb =
    List.split
      (List.map
         (fun (ex, topt) ->
           match topt with
           | None -> (ex, Asrt.Emp)
           | Some t -> (ex, types t (Expr.LVar ex)))
         existentials)
  in
  let a = fold_star typsb in
  (a, (label, exs))

let trans_abs_pred ~filepath cl_pred =
  let CAbsPred.
        {
          name = pred_name;
          params = pred_params;
          ins = pred_ins;
          pure = pred_pure;
        } =
    cl_pred
  in
  let pred_num_params = List.length pred_params in
  Pred.
    {
      pred_name;
      pred_source_path = Some filepath;
      pred_internal = false;
      pred_num_params;
      pred_params;
      pred_ins;
      pred_definitions = [];
      pred_facts = [];
      pred_pure;
      pred_abstract = true;
      pred_nounfold = true;
      pred_normalised = false;
    }

let trans_pred ~ann ~filepath cl_pred =
  let CPred.
        {
          name = pred_name;
          params = pred_params;
          definitions;
          ins = pred_ins;
          no_unfold;
          pure = pred_pure;
        } =
    cl_pred
  in
  let pred_num_params = List.length pred_params in
  let pred_definitions =
    List.map
      (fun (d, a) ->
        match d with
        | None -> (None, trans_asrt ~fname:pred_name ~ann a, [])
        | Some da ->
            let ada, gda = trans_asrt_annot da in
            (Some gda, ada ** trans_asrt ~fname:pred_name ~ann a, []))
      definitions
  in
  Pred.
    {
      pred_name;
      pred_source_path = Some filepath;
      pred_internal = false;
      pred_num_params;
      pred_params;
      pred_ins;
      pred_definitions;
      (* FIXME: ADD SUPPORT FOR FACTS *)
      pred_facts = [];
      pred_pure;
      pred_abstract = false;
      pred_nounfold = no_unfold;
      pred_normalised = false;
    }

let add_trans_pred filepath ann cl_pred =
  { ann with preds = trans_pred ~filepath ~ann cl_pred :: ann.preds }

let add_trans_abs_pred filepath ann cl_pred =
  { ann with preds = trans_abs_pred ~filepath cl_pred :: ann.preds }

let trans_sspec ~ann fname sspecs =
  let CSpec.{ pre; posts; spec_annot } = sspecs in
  let tap, spa =
    match spec_annot with
    | None -> (Asrt.Emp, None)
    | Some spa ->
        let a, (label, exs) = trans_asrt_annot spa in
        (a, Some (label, exs))
  in
  let genv = Asrt.Pred (CConstants.Internal_Predicates.global_env, []) in
  let ta = trans_asrt ~fname ~ann in
  let make_post p =
    if !Config.allocated_functions then ta p ** genv else ta p
  in
  Spec.
    {
      ss_pre = tap ** ta pre ** genv;
      ss_posts = List.map make_post posts;
      (* FIXME: bring in variant *)
      ss_variant = None;
      ss_flag = Flag.Normal;
      ss_to_verify = true;
      ss_label = spa;
    }

let trans_lemma ~ann ~filepath lemma =
  let CLemma.{ name; params; hypothesis; conclusions; proof } = lemma in
  let trans_asrt = trans_asrt ~ann ~fname:name in
  let trans_lcmd = trans_lcmd ~ann ~fname:name in
  let genv = Asrt.Pred (CConstants.Internal_Predicates.global_env, []) in
  let make_post p =
    if !Config.allocated_functions then trans_asrt p ** genv else trans_asrt p
  in
  let lemma_hyp = trans_asrt hypothesis ** genv in
  let lemma_concs = List.map make_post conclusions in
  let lemma_proof =
    Option.map
      (List.concat_map (fun lcmd ->
           match trans_lcmd lcmd with
           | `Normal x -> x
           | `Invariant _ -> failwith "Invariant in lemma proof"))
      proof
  in
  (* TODO: Bring in variant and OX *)
  let lemma_specs =
    [
      Lemma.
        {
          lemma_hyp;
          lemma_concs;
          lemma_spec_variant = None;
          lemma_spec_hides = None;
        };
    ]
  in
  Lemma.
    {
      lemma_name = name;
      lemma_params = params;
      lemma_source_path = Some filepath;
      lemma_existentials = [];
      lemma_internal = false;
      lemma_variant = None;
      lemma_specs;
      lemma_proof;
    }

let trans_spec ~ann ?(only_spec = false) cl_spec =
  let CSpec.{ fname; params; sspecs } = cl_spec in
  let result =
    Spec.
      {
        spec_name = fname;
        spec_params = params;
        spec_sspecs = List.map (trans_sspec ~ann fname) sspecs;
        spec_normalised = false;
        spec_incomplete = false;
        spec_to_verify = not only_spec;
      }
  in
  let _ =
    List.iter
      (fun (sspec : Spec.st) ->
        if sspec.ss_posts = [] then
          failwith
            ("Gillian-C: Specification without post-condition for function "
           ^ fname))
      result.spec_sspecs
  in
  result

let add_trans_spec ann cl_spec =
  { ann with specs = trans_spec ~ann cl_spec :: ann.specs }

let add_trans_only_spec ann cl_spec =
  {
    ann with
    onlyspecs = trans_spec ~ann ~only_spec:true cl_spec :: ann.onlyspecs;
  }

let add_trans_lemma filepath ann cl_lemma =
  { ann with lemmas = trans_lemma ~filepath ~ann cl_lemma :: ann.lemmas }

let trans_annots clight_prog log_prog filepath =
  let open Ctypes in
  let structs_not_annot = get_structs_not_annot clight_prog.prog_types in
  let struct_annots =
    List.fold_left
      (gen_pred_of_struct clight_prog.prog_comp_env)
      { empty with cenv = clight_prog.prog_comp_env }
      structs_not_annot
  in
  let with_preds =
    List.fold_left (add_trans_pred filepath) struct_annots log_prog.CProg.preds
  in
  let with_abs_preds =
    List.fold_left
      (add_trans_abs_pred filepath)
      with_preds log_prog.CProg.abs_preds
  in
  let with_lemmas =
    List.fold_left (add_trans_lemma filepath) with_abs_preds log_prog.lemmas
  in
  let with_specs = List.fold_left add_trans_spec with_lemmas log_prog.specs in
  let with_only_specs =
    List.fold_left add_trans_only_spec with_specs log_prog.only_specs
  in
  let imports =
    List.map
      (fun (i, b) -> (Filename.concat (Filename.dirname filepath) i, b))
      log_prog.imports
  in
  { with_only_specs with imports }

let glob_fun_pred symb target =
  Constr.Others.glob_fun ~symb ~fname:(Expr.Lit (String target))

let make_global_env_pred init_asrts =
  let def = fold_star init_asrts in
  Pred.
    {
      pred_name = CConstants.Internal_Predicates.global_env;
      pred_source_path = None;
      pred_internal = true;
      pred_num_params = 0;
      pred_params = [];
      pred_ins = [];
      pred_definitions = [ (None, def, []) ];
      (* FIXME: ADD SUPPORT FOR PURE, ABSTRACT, NOUNFOLD *)
      pred_facts = [];
      pred_pure = false;
      pred_abstract = false;
      pred_nounfold = false;
      pred_normalised = false;
    }

let get_clight_fun clight_prog ident =
  let _, f =
    List.find (fun (a, _) -> Camlcoq.P.eq a ident) clight_prog.Ctypes.prog_defs
  in
  let act_f =
    match f with
    | Gfun (Internal fp) -> fp
    | _ ->
        failwith
          (Printf.sprintf "This is not a function it seems : %s"
             (true_name ident))
  in
  act_f

let opt_gen param_name pred_name struct_params =
  let lv_params = List.map (fun (s, _) -> Expr.LVar ("#" ^ s)) struct_params in
  let loc = Expr.LVar "#loc" in
  let null = Expr.Lit (LList [ String VTypes.long_type; Int Z.zero ]) in
  let pvar = Expr.PVar param_name in
  let loc_list = Expr.EList [ loc; Expr.zero_i ] in
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
  let pvoffs = Expr.zero_i in
  (* let pvoffs = Expr.PVar offs_param_name in *)
  let res_to_map =
    let open VTypes in
    match typ with
    | Tint _ ->
        let e = mk int_type lvval in
        [ (e, (pvmember == mk int_type lvval) ** types IntType lvval) ]
    (* (mk int_type lvval, Asrt.Pred (int_get, [ pvmember; lvval ])) *)
    | Tlong _ ->
        let e = mk int_type lvval in
        [ (e, (pvmember == mk long_type lvval) ** types IntType lvval) ]
    | Tfloat _ ->
        let e = mk int_type lvval in
        [ (e, (pvmember == mk float_type lvval) ** types NumberType lvval) ]
    | Tpointer (Tstruct (id, _), _) ->
        let struct_name = true_name id in
        let p_name = rec_pred_name_of_struct struct_name in
        let null = Expr.Lit (LList [ String long_type; Int Z.zero ]) in
        let obj = Expr.EList [ lvval; Expr.zero_i ] in
        let comp_opt = Maps.PTree.get id cenv in
        let comp =
          match comp_opt with
          | None ->
              failwith
                (Printf.sprintf "Structure %s is undefined !" struct_name)
          | Some c -> c
        in
        let struct_params =
          List.map
            (function
              | Member_plain (i, _) ->
                  Expr.LVar ("#" ^ field_name ^ "__" ^ true_name i)
              | Member_bitfield _ -> failwith "Unsupported bitfield members")
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
    | Errors.OK (f, Full) -> Expr.Lit (Int (ValueTranslation.int_of_z f))
    | Errors.OK _ -> failwith "Unsupported bitfield members"
    | Errors.Error e ->
        failwith
          (Format.asprintf "Invalid member offset : %a@?" Driveraux.print_error
             e)
  in
  let sz = Expr.Lit (Int (ValueTranslation.int_of_z (sizeof cenv typ))) in
  let perm_exp =
    Expr.Lit (String (ValueTranslation.string_of_permission Memtype.Freeable))
  in
  let mem_ga = LActions.str_ga (GMem Single) in
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
    | None ->
        failwith (Printf.sprintf "Structure %s is undefined !" struct_name)
    | Some c -> c
  in
  let open Ctypes in
  let () =
    match comp.co_su with
    | Union -> failwith "union shouldn't be handled by this function"
    | Struct -> ()
  in
  let first_params =
    [
      (loc_param_name, Some Type.ObjectType)
      (* (offs_param_name, Some Type.NumberType) *)
      (* TODO: For now, offset HAS to be 0, that will change *);
    ]
  in
  let struct_params =
    List.map
      (function
        | Member_plain (i, _) -> (true_name i, Some Type.ListType)
        | _ -> failwith "Unsupported bitfield members")
      comp.co_members
  in
  let pred_params = first_params @ struct_params in
  let pred_num_params = List.length pred_params in
  let defs_without_holes =
    List.fold_left
      (fun al member ->
        match member with
        | Member_bitfield _ -> failwith "Unsupported bitfield members"
        | Member_plain (id, typ) ->
            let new_al = asserts_of_rec_member cenv comp.co_members id typ in
            let list_of_list =
              List.map (fun a -> List.map (fun na -> a ** na) new_al) al
            in
            List.concat list_of_list)
      [ Asrt.Emp ] comp.co_members
  in
  let fo idp =
    match field_offset cenv idp comp.co_members with
    | Errors.OK (f, Full) -> ValueTranslation.int_of_z f
    | Errors.OK _ -> failwith "Unsupported bitfield members"
    | Errors.Error e ->
        failwith
          (Format.asprintf "Invalid member offset : %a@?" Driveraux.print_error
             e)
  in
  let sz t = ValueTranslation.int_of_z (sizeof cenv t) in
  let rec get_holes memb =
    match memb with
    | [] -> []
    | [ _a ] -> []
    | Member_plain (ida, t) :: Member_plain (idb, _) :: r ->
        let end_a = Z.add (fo ida) (sz t) in
        let start_b = fo idb in
        if end_a < start_b then (end_a, start_b) :: get_holes r else get_holes r
    | _ -> failwith "Unsupported bitfield members"
  in
  let holes = get_holes comp.co_members in
  let hole_assert = fold_star (List.map assert_of_hole holes) in
  let siz = ValueTranslation.int_of_z comp.Ctypes.co_sizeof in
  let zero = Expr.int 0 in
  let malloc = malloc_chunk_asrt (Expr.PVar loc_param_name) zero siz in
  let pred_definitions =
    List.map
      (fun def -> (None, def ** hole_assert ** malloc, []))
      defs_without_holes
  in
  let rec_pred =
    Pred.
      {
        pred_name;
        pred_source_path = None;
        pred_internal = true;
        pred_ins;
        pred_num_params;
        pred_params;
        pred_facts = [];
        pred_pure = false;
        pred_abstract = false;
        pred_nounfold = false;
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
        pred_source_path = None;
        pred_internal = true;
        pred_ins = [ 0 ];
        pred_num_params = 1;
        pred_params = [ (opt_param_name, Some Type.ListType) ];
        pred_facts = [];
        pred_pure = false;
        pred_abstract = false;
        pred_nounfold = false;
        pred_normalised = false;
        pred_definitions = List.map (fun x -> (None, x, [])) opt_defs;
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
  let is_c_ptr_to_scalar = function
    | Ctypes.Tpointer ((Tfloat _ | Tint _ | Tlong _), _) -> true
    | _ -> false
  in
  let pred_name_of_ptr_scal =
    let open Internal_Predicates in
    function
    | Ctypes.Tpointer (Tfloat _, _) -> is_ptr_to_single_opt
    | Ctypes.Tpointer (Tint _, _) -> is_ptr_to_int_opt
    | Ctypes.Tpointer (Tlong _, _) -> is_ptr_to_long_opt
    | _ -> failwith "Cannot happen"
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
  | AST.Tint when (not Archi.ptr64) && is_c_ptr_to_scalar ct ->
      pred (pred_name_of_ptr_scal ct)
  | AST.Tlong when Archi.ptr64 && is_c_ptr_to_scalar ct ->
      pred (pred_name_of_ptr_scal ct)
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
