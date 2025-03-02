(* We do these operations in a separate file so that we do not clutter gilgen *)
open Gillian.Gil_syntax
open CConstants
open CLogic
open Compcert
open CompileState
module Str_set = Gillian.Utils.Containers.SS
module CoreP = Constr.Core
open Expr.Infix

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
      (x @ rx, y @ ry, z :: rz)

let ( ++ ) = Expr.Infix.( + )

let to_assrt_of_gen_form = function
  | Expr.Lit (Bool true) -> Asrt.Emp
  | f -> Pure f

let ( #== ) e1 e2 = to_assrt_of_gen_form (e1 == e2)

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
    List.filter
      (fun name -> Stdlib.not (Str_set.mem name already_annot))
      struct_names
  in
  let newly_annot =
    Str_set.union already_annot (Str_set.of_list structs_not_annot)
  in
  already_annot_structs := newly_annot;
  structs_not_annot

(* Custom Ctypes.access_mode function for our custom Chunk type *)
let access_mode_by_value =
  let open Ctypes in
  function
  | Tint (i, s, _) -> (
      match i with
      | I8 -> (
          match s with
          | Signed -> Some Chunk.Mint8signed
          | Unsigned -> Some Chunk.Mint8unsigned)
      | I16 -> (
          match s with
          | Signed -> Some Chunk.Mint16signed
          | Unsigned -> Some Chunk.Mint16unsigned)
      | I32 -> Some Chunk.Mint32
      | IBool -> Some Chunk.Mint8unsigned)
  | Tlong (_, _) -> Some Chunk.Mint64
  | Tfloat (f, _) -> (
      match f with
      | F32 -> Some Chunk.Mfloat32
      | F64 -> Some Chunk.Mfloat64)
  | Tpointer (_, _) -> Some Chunk.Mptr
  | _ -> None

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
  match typ with
  | Tstruct (struct_id, _) ->
      let struct_name = true_name struct_id in
      let pred_name = pred_name_of_struct struct_name in
      let arg_number =
        List.length (Option.get (Maps.PTree.get struct_id cenv)).co_members
      in
      let args_without_ins =
        List.init arg_number (fun k ->
            Expr.LVar ("i__" ^ field_name ^ "_" ^ string_of_int k))
      in
      let list_is_components = pvmember #== (Expr.list args_without_ins) in
      let ofs = Expr.Infix.(pvofs + fo) in
      let args = pvloc :: ofs :: args_without_ins in
      let pred_call = Asrt.Pred (pred_name, args) in
      [ list_is_components; pred_call ]
  | Tarray (ty, n, _) ->
      let n = ValueTranslation.int_of_z n in
      let n_e = Expr.int_z n in
      let chunk =
        match access_mode_by_value ty with
        | Some chunk -> chunk
        | _ -> failwith "Array in a structure containing complicated types"
      in
      [
        Constr.Core.array ~loc:pvloc ~ofs:(pvofs ++ fo) ~chunk ~size:n_e
          ~sval_arr:pvmember ~perm:(Some Freeable);
      ]
  | _ ->
      let mk t v = Expr.list [ Expr.string t; v ] in
      let field_val_name = "i__" ^ field_name ^ "_v" in
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
        match access_mode_by_value typ with
        | Some chunk -> chunk
        | _ -> failwith "Invalid access mode for some type"
      in
      let ga_asrt =
        CoreP.single ~loc:pvloc ~ofs:(pvofs ++ fo) ~chunk ~sval:e_to_use
          ~perm:(Some Freeable)
      in
      [ getter_or_type_pred; ga_asrt ]

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
    comp.co_members
    |> List.map @@ function
       | Member_plain (i, _) -> (true_name i, Some Type.ListType)
       | Member_bitfield _ -> failwith "Unsupported bitfield members"
  in
  let pred_params = first_params @ struct_params in
  let pred_num_params = List.length pred_params in
  let def_without_holes =
    comp.co_members
    |> List.concat_map @@ function
       | Member_plain (id, typ) -> assert_of_member cenv comp.co_members id typ
       | Member_bitfield _ -> failwith "Unsupported bitfield members"
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
        if Stdlib.( < ) end_a start_b then (end_a, start_b) :: get_holes r
        else get_holes r
    | _ -> failwith "Unsupported bitfield members"
  in

  let holes = get_holes comp.co_members in
  let holes_asserts = List.map assert_of_hole holes in
  let def = holes_asserts @ def_without_holes in
  (* TODO (Alexis): How to handle changes in structs? *)
  let n_pred =
    Pred.
      {
        pred_name;
        pred_source_path = None;
        pred_loc = None;
        pred_internal = true;
        pred_ins;
        pred_num_params;
        pred_params;
        pred_facts = [ (* FIXME: there are probably some facts to get *) ];
        pred_guard = None;
        pred_pure = false;
        pred_abstract = false;
        pred_nounfold = false;
        pred_normalised = false;
        pred_definitions = [ (None, def) ];
      }
  in
  { ann with preds = n_pred :: ann.preds }

let trans_binop : CBinOp.t -> BinOp.t = function
  | LstCons -> failwith "LstCons shouldn't be compiled that way"
  | LstCat -> failwith "LstCat shouldn't be compiled that way"
  | PtrPlus -> failwith "PtrPlus shouldn't be compiled that way"
  | Plus -> IPlus
  | Times -> ITimes
  | Minus -> IMinus
  | Div -> IDiv
  | Equal -> Equal
  | SetSub -> SetSub
  | SetDiff -> SetDiff
  | SetMem -> SetMem
  | LessThan -> ILessThan
  | And -> And
  | Or -> Or

let trans_unop : CUnOp.t -> UnOp.t = function
  | LstLen -> LstLen
  | Not -> Not

let trans_nop : CNOp.t -> NOp.t = function
  | SetUnion -> SetUnion

let trans_simpl_expr : CSimplExpr.t -> Expr.t = function
  | PVar s -> PVar s
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
      ([ tint eg ], [], mk int_type (tse se))
  | Slong se ->
      let eg = tse se in
      ([ tint eg ], [], mk long_type (tse se))
  | Ssingle se ->
      let eg = tse se in
      ([ tnum eg ], [], mk single_type (tse se))
  | Sfloat se ->
      let eg = tse se in
      ([ tnum eg ], [], mk float_type (tse se))
  | Sptr (se1, se2) ->
      let eg1, eg2 = (tse se1, tse se2) in
      ([ tloc eg1; tint eg2 ], [], Expr.EList [ tse se1; tse se2 ])
  | Sfunptr symb ->
      let loc = Global_env.location_of_symbol symb in
      let ptr = Expr.EList [ Lit (Loc loc); Expr.zero_i ] in
      ([], [], ptr)

(** Returns assertions that are necessary to define the expression,
      the created variable for binding when necessary, and the used expression *)
let rec trans_expr (e : CExpr.t) : Asrt.t * Var.t list * Expr.t =
  match e with
  | CExpr.SExpr se -> ([], [], trans_simpl_expr se)
  | SVal sv -> trans_sval sv
  | EList el ->
      let asrt, vars, elp = split3_expr_comp (List.map trans_expr el) in
      (asrt, vars, Expr.EList elp)
  | ESet es ->
      let asrt, vars, elp = split3_expr_comp (List.map trans_expr es) in
      (asrt, vars, Expr.ESet elp)
  | BinOp (e1, LstCat, e2) ->
      let a1, v1, eg1 = trans_expr e1 in
      let a2, v2, eg2 = trans_expr e2 in
      (a1 @ a2, v1 @ v2, Expr.list_cat eg1 eg2)
  | BinOp (e1, LstCons, e2) ->
      let a1, v1, eg1 = trans_expr e1 in
      let a2, v2, eg2 = trans_expr e2 in
      (a1 @ a2, v1 @ v2, Expr.list_cat (EList [ eg1 ]) eg2)
  | BinOp (e1, PtrPlus, e2) -> (
      let a1, v1, ptr = trans_expr e1 in
      let a2, v2, to_add = trans_expr e2 in
      match ptr with
      | Expr.EList [ loc; ofs ] ->
          (a1 @ a2, v1 @ v2, Expr.EList [ loc; Expr.Infix.( + ) ofs to_add ])
      | ptr ->
          let res_lvar = fresh_lvar () in
          let res = Expr.LVar res_lvar in
          ( a1 @ a2 @ [ Constr.Others.ptr_add ~ptr ~to_add ~res ],
            res_lvar :: (v1 @ v2),
            res ))
  | BinOp (e1, b, e2) ->
      let a1, v1, eg1 = trans_expr e1 in
      let a2, v2, eg2 = trans_expr e2 in
      (a1 @ a2, v1 @ v2, BinOp (eg1, trans_binop b, eg2))
  | UnOp (u, e) ->
      let a, v, eg = trans_expr e in
      (a, v, UnOp (trans_unop u, eg))
  | NOp (nop, el) ->
      let asrt, vs, elp = split3_expr_comp (List.map trans_expr el) in
      let gnop = trans_nop nop in
      (asrt, vs, Expr.NOp (gnop, elp))
  | LstSub (lst, start, len) ->
      let a1, v1, lst = trans_expr lst in
      let a2, v2, start = trans_expr start in
      let a3, v3, len = trans_expr len in
      (a1 @ a2 @ a3, v1 @ v2 @ v3, Expr.list_sub ~lst ~start ~size:len)

let rec trans_form : CFormula.t -> Asrt.t * Var.t list * Expr.t = function
  | True -> ([], [], Expr.true_)
  | False -> ([], [], Expr.false_)
  | Eq (ce1, ce2) ->
      let f1, v1, eg1 = trans_expr ce1 in
      let f2, v2, eg2 = trans_expr ce2 in
      (f1 @ f2, v1 @ v2, eg1 == eg2)
  | LessEq (ce1, ce2) ->
      let f1, v1, eg1 = trans_expr ce1 in
      let f2, v2, eg2 = trans_expr ce2 in
      (f1 @ f2, v1 @ v2, eg1 <= eg2)
  | Less (ce1, ce2) ->
      let f1, v1, eg1 = trans_expr ce1 in
      let f2, v2, eg2 = trans_expr ce2 in
      (f1 @ f2, v1 @ v2, eg1 < eg2)
  | SetMem (ce1, ce2) ->
      let f1, v1, eg1 = trans_expr ce1 in
      let f2, v2, eg2 = trans_expr ce2 in
      (f1 @ f2, v1 @ v2, BinOp (eg1, SetMem, eg2))
  | Not fp ->
      let a, v, fpp = trans_form fp in
      (a, v, not fpp)
  | Or (f1, f2) ->
      let a1, v1, fp1 = trans_form f1 in
      let a2, v2, fp2 = trans_form f2 in
      (a1 @ a2, v1 @ v2, fp1 || fp2)
  | And (f1, f2) ->
      let a1, v1, fp1 = trans_form f1 in
      let a2, v2, fp2 = trans_form f2 in
      (a1 @ a2, v1 @ v2, fp1 && fp2)
  | Implies (f1, f2) ->
      let a1, v1, fp1 = trans_form f1 in
      let a2, v2, fp2 = trans_form f2 in
      (a1 @ a2, v1 @ v2, fp1 ==> fp2)
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
        let loc = Global_env.location_of_symbol symb in
        let loc = Expr.Lit (Loc loc) in
        let ofsv = Expr.int 0 in
        ([], loc, ofsv)
    | _ ->
        let a_s, _, s_e = trans_expr s in
        let locv = gen_loc_var () in
        let ofsv = gen_ofs_var () in
        let pc = ptr_call s_e locv ofsv in
        (pc :: a_s, locv, ofsv)
  in
  let to_assert, locv, ofsv = interpret_s ~typ s in
  let malloc_chunk siz =
    if malloc then malloc_chunk_asrt locv ofsv siz else Asrt.Emp
  in
  let mk str v = Expr.list [ Expr.string str; v ] in
  let mk_ptr l o = Expr.list [ l; o ] in
  match c with
  | CConstructor.ConsExpr (SVal (Sint v as se))
  | ConsExpr (SVal (Sfloat v as se))
  | ConsExpr (SVal (Ssingle v as se))
  | ConsExpr (SVal (Slong v as se)) ->
      let chunk, typ, asrtfn =
        match se with
        | Sint _ -> (Chunk.Mint32, int_type, tint)
        | Sfloat _ -> (Chunk.Mfloat32, float_type, tnum)
        | Ssingle _ -> (Chunk.Mfloat32, single_type, tnum)
        | Slong _ -> (Chunk.Mint64, long_type, tint)
        | _ -> failwith "Impossible"
      in
      let e = cse v in
      let sval = mk typ e in
      let siz = sz se in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval ~perm:(Some Freeable)
      in
      [ ga; asrtfn e; malloc_chunk siz ] @ to_assert
  | ConsExpr (SVal (Sptr (sl, so))) ->
      let l = cse sl in
      let o = cse so in
      let chunk = Chunk.ptr in
      let siz = sz (Sptr (sl, so)) in
      let sval = mk_ptr l o in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval ~perm:(Some Freeable)
      in
      [ ga; tloc l; tint o; malloc_chunk siz ] @ to_assert
  | ConsExpr (SVal (Sfunptr fname)) ->
      let l = Global_env.location_of_symbol fname in
      let ptr = Expr.EList [ Expr.Lit (Loc l); Expr.zero_i ] in
      let chunk = Chunk.ptr in
      let siz = sz (Sfunptr fname) in
      let ga_single =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval:ptr ~perm:(Some Freeable)
      in
      [ ga_single; malloc_chunk siz ] @ to_assert
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
        Asrt.Pred (struct_pred, [ locv; ofsv ] @ params_fields) :: more_asrt
      in
      pr @ to_assert @ [ malloc_chunk siz ]

let rec trans_asrt ~fname ~ann asrt =
  let a =
    match asrt with
    | CAssert.Star (a1, a2) ->
        trans_asrt ~fname ~ann a1 @ trans_asrt ~fname ~ann a2
    | Array { ptr; chunk; size; content; malloced } ->
        let a1, _, ptr = trans_expr ptr in
        let a2, _, size = trans_expr size in
        let a3, _, content = trans_expr content in
        let malloc_p =
          if malloced then
            let open Expr.Infix in
            let csize = Expr.int (Chunk.size chunk) in
            let total_size = size * csize in
            [ Constr.Others.malloced_abst ~ptr ~total_size ]
          else []
        in
        a1 @ a2 @ a3
        @ [ Constr.Others.array_ptr ~ptr ~chunk ~size ~content ]
        @ malloc_p
    | Malloced (e1, e2) ->
        let a1, _, ce1 = trans_expr e1 in
        let a2, _, ce2 = trans_expr e2 in
        a1 @ a2 @ [ Constr.Others.malloced_abst ~ptr:ce1 ~total_size:ce2 ]
    | Zeros (e1, e2) ->
        let a1, _, ce1 = trans_expr e1 in
        let a2, _, ce2 = trans_expr e2 in
        a1 @ a2 @ [ Constr.Others.zeros_ptr_size ~ptr:ce1 ~size:ce2 ]
    | Undefs (e1, e2) ->
        let a1, _, ce1 = trans_expr e1 in
        let a2, _, ce2 = trans_expr e2 in
        a1 @ a2 @ [ Constr.Others.undefs_ptr_size ~ptr:ce1 ~size:ce2 ]
    | Pure f ->
        let ma, _, fp = trans_form f in
        Pure fp :: ma
    | Pred (p, cel) ->
        let ap, _, gel = split3_expr_comp (List.map trans_expr cel) in
        Pred (p, gel) :: ap
    | Emp -> [ Asrt.Emp ]
    | PointsTo { ptr = s; constr = c; typ } -> trans_constr ~fname ~typ ann s c
  in
  match List.filter (fun x -> x <> Asrt.Emp) a with
  | [] -> [ Asrt.Emp ]
  | a -> a

let rec trans_lcmd ~fname ~ann lcmd =
  let trans_lcmd = trans_lcmd ~fname ~ann in
  let trans_asrt = trans_asrt ~fname ~ann in
  let make_assert ~bindings = function
    | [] | [ Asrt.Emp ] -> []
    | a -> [ LCmd.SL (SepAssert (a, bindings)) ]
  in
  match lcmd with
  | CLCmd.Apply (pn, el) ->
      let aps, bindings, gel = split3_expr_comp (List.map trans_expr el) in
      `Normal (make_assert ~bindings aps @ [ SL (ApplyLem (pn, gel, [])) ])
  | CLCmd.Fold (pn, el) ->
      let aps, bindings, gel = split3_expr_comp (List.map trans_expr el) in
      `Normal (make_assert ~bindings aps @ [ SL (Fold (pn, gel, None)) ])
  | Unfold { pred; params; bindings; recursive } ->
      let ap, vs, gel = split3_expr_comp (List.map trans_expr params) in
      `Normal
        (make_assert ~bindings:vs ap
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
      `Invariant (SLCmd.Invariant (asrt, bindings))
  | SymbExec -> `Normal [ SL SymbExec ]

let trans_asrt_annot da =
  let { label; existentials } = da in
  let exs, typsb =
    existentials
    |> ( List.map @@ fun (ex, topt) ->
         match topt with
         | None -> (ex, Asrt.Emp)
         | Some t -> (ex, types t (Expr.LVar ex)) )
    |> List.split
  in
  (typsb, (label, exs))

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
      pred_loc = None;
      pred_internal = false;
      pred_num_params;
      pred_params;
      pred_ins;
      pred_definitions = [];
      pred_facts = [];
      pred_guard = None;
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
        | None -> (None, trans_asrt ~fname:pred_name ~ann a)
        | Some da ->
            let ada, gda = trans_asrt_annot da in
            (Some gda, ada @ trans_asrt ~fname:pred_name ~ann a))
      definitions
  in
  Pred.
    {
      pred_name;
      pred_source_path = Some filepath;
      pred_loc = None;
      pred_internal = false;
      pred_num_params;
      pred_params;
      pred_ins;
      pred_definitions;
      (* FIXME: ADD SUPPORT FOR FACTS *)
      pred_facts = [];
      pred_guard = None;
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
    | None -> ([], None)
    | Some spa ->
        let a, (label, exs) = trans_asrt_annot spa in
        (a, Some (label, exs))
  in
  let ta = trans_asrt ~fname ~ann in
  let make_post p = if !Config.allocated_functions then ta p else ta p in
  Spec.
    {
      ss_pre = (tap @ ta pre, None);
      ss_posts = List.map (fun post -> (make_post post, None)) posts;
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
  let make_post p = (trans_asrt p, None) in
  let lemma_hyp = (trans_asrt hypothesis, None) in
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
    [ Lemma.{ lemma_hyp; lemma_concs; lemma_spec_variant = None } ]
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
        spec_to_verify = Stdlib.not only_spec;
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

let bit_size = function
  | Ctypes.I8 -> 8
  | I16 -> 16
  | I32 -> 32
  | IBool -> 1

let bounds signedness bit_size =
  let bit_size_m_1 = Stdlib.( - ) bit_size 1 in
  let open Z in
  let min, max =
    match signedness with
    | Ctypes.Signed -> (neg (one lsl bit_size_m_1), (one lsl bit_size_m_1) - one)
    | Unsigned -> (zero, (one lsl bit_size) - one)
  in
  (Expr.int_z min, Expr.int_z max)

let predicate_from_triple (e, csmt, ct) =
  let pred pname = Asrt.Pred (pname, [ e ]) in
  let open Internal_Predicates in
  match (csmt, ct) with
  | _, Ctypes.Tpointer (Tfunction _, _) -> pred is_ptr_to_0
  | _, Ctypes.Tpointer _ -> pred is_ptr_opt
  | AST.Tint, Tint (size, signedness, _) ->
      let min, max = bounds signedness (bit_size size) in
      Asrt.Pred (Internal_Predicates.is_bounded_int, [ e; min; max ])
  | AST.Tlong, Tlong (signedness, _) ->
      let min, max = bounds signedness 64 in
      Asrt.Pred (Internal_Predicates.is_bounded_long, [ e; min; max ])
  | AST.Tsingle, _ -> pred is_single
  | AST.Tfloat, _ -> pred is_float
  | _ ->
      failwith
        (Printf.sprintf
           "Don't know how to handle the following type as a bispec function \
            parameter %s"
           (PrintAST.name_of_type csmt))

let simple_predicate_from_triple (pn, _, _) =
  Asrt.Pure (BinOp (Expr.PVar pn, Equal, Expr.LVar ("" ^ pn)))

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
  let mk_lvar x = Expr.LVar ("" ^ x) in
  let lvars = List.map mk_lvar true_params in
  let equalities =
    List.map
      (fun x -> Asrt.Pure (Expr.BinOp (Expr.PVar x, Equal, mk_lvar x)))
      true_params
  in
  (* Right now, triples are : (param_name, csharpminor type, c type)
     The C type will be used to discriminate long/int from pointers *)
  let triples = combine lvars sig_args cligh_params in
  let pred_list = List.map predicate_from_triple triples in
  let pre = equalities @ pred_list in
  BiSpec.
    {
      bispec_name = fname;
      bispec_params = true_params;
      bispec_pres = [ (pre, None) ];
      bispec_normalised = false;
    }

let add_bispec ann clight_prog fname ident f =
  {
    ann with
    bispecs = generate_bispec clight_prog fname ident f :: ann.bispecs;
  }
