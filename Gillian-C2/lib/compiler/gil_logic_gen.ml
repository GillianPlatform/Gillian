(* We do these operations in a separate file so that we do not clutter gilgen *)
open Gillian.Gil_syntax
open Kcommons.Constants
open C2_lprog
module Str_set = Gillian.Utils.Containers.SS
module CoreP = Constr.Core
open Expr.Infix

let loc_param_name = "loc"
let ofs_param_name = "ofs"

let pred_name_of_struct struct_name =
  Prefix.generated_pred ^ "struct_" ^ struct_name

let rec_pred_name_of_struct struct_name =
  Prefix.generated_pred ^ "rec_struct_" ^ struct_name

let opt_rec_pred_name_of_struct struct_name =
  Prefix.generated_pred ^ "opt_rec_struct_" ^ struct_name

let fresh_lvar ?(fname = "") () =
  let pre = "_lvar_i_" in
  Generators.gen_str ~fname pre

let location_of_symbol s = Prefix.location ^ s

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
  imports : (string * bool) list;
}

let empty =
  {
    preds = [];
    specs = [];
    bispecs = [];
    onlyspecs = [];
    lemmas = [];
    imports = [];
  }

let already_annot_structs = ref Str_set.empty

let get_structs_not_annot ctx =
  let struct_names =
    ctx.prog.types |> Hashtbl.to_seq_values
    |> Seq.filter_map (Ctx.resolve_struct_tag_opt ctx)
    |> Str_set.of_seq |> Str_set.to_list
  in
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

let assert_of_hole low high =
  let pvloc = Expr.PVar loc_param_name in
  let pvoffs = Expr.PVar ofs_param_name in
  CoreP.hole ~loc:pvloc
    ~low:(pvoffs ++ Expr.int low)
    ~high:(pvoffs ++ Expr.int high)
    ~perm:(Some Freeable)

let convert_struct_field
    ~ctx
    ~offset
    ~struct_name
    name
    (type_ : Goto_lib.Type.t) : (string * GilType.t option) list * Asrt.t * int
    =
  let pvloc = Expr.PVar loc_param_name in
  let pvofs = Expr.PVar ofs_param_name in
  let pvmember = Expr.PVar name in
  let size = Ctx.size_of ctx type_ in
  let ofs = pvofs + Expr.int offset in
  let gil_type, asrts =
    match type_ with
    | Struct _ | StructTag _ ->
        (* If the field is a struct, then pack the inner fields into a list *)
        let tag = Ctx.resolve_struct_tag_opt ctx type_ |> Option.get in
        let pred_name = pred_name_of_struct tag in
        let components = Ctx.resolve_struct_components ctx type_ in
        let field_args, _ =
          List.fold_left
            (fun (acc, i) field ->
              match field with
              | Datatype_component.Field { name; _ } ->
                  let arg = Expr.LVar (Fmt.str "i__%s_%d" name i) in
                  (arg :: acc, Stdlib.(i + 1))
              | Padding _ -> (acc, i))
            ([], 0) components
        in
        let field_arg_list = pvmember #== (Expr.list field_args) in
        let args = pvloc :: ofs :: field_args in
        let pred_call = Asrt.Pred (pred_name, args) in
        (GilType.ListType, [ field_arg_list; pred_call ])
    | Array (type_', len) ->
        let chunk =
          match Memory.chunk_for_type ~ctx type_' with
          | Some c -> c
          | None ->
              Fmt.failwith "Field %s of struct %s has unsupported type %s[]"
                name struct_name
                (Goto_lib.Type.show_simple type_)
        in
        let ofs = pvofs ++ Expr.int offset in
        let mem_array =
          CoreP.array ~loc:pvloc ~ofs ~chunk ~size:(Expr.int len)
            ~sval_arr:pvmember ~perm:(Some Freeable)
        in
        (GilType.ListType, [ mem_array ])
    | _ ->
        let chunk, gil_type, extra_asrts =
          match
            let open Utils.Syntaxes.Option in
            let* chunk = Memory.chunk_for_type ~ctx type_ in
            let* gil_type, extra_asrts =
              let open GilType in
              match type_ with
              | CInteger _ | Signedbv _ | Unsignedbv _ | Enum _ | EnumTag _ ->
                  Some (IntType, [])
              | Double | Float -> Some (NumberType, [])
              | Pointer _ ->
                  let is_ptr_asrt =
                    Asrt.Pred (Internal_Predicates.is_ptr, [ pvmember ])
                  in
                  Some (ListType, [ is_ptr_asrt ])
              | _ -> None
            in
            Some (chunk, gil_type, extra_asrts)
          with
          | Some x -> x
          | None ->
              Fmt.failwith "Field %s of struct %s has unsupported type %s" name
                struct_name
                (Goto_lib.Type.show_simple type_)
        in
        let mem =
          CoreP.single ~loc:pvloc ~ofs ~chunk ~sval:pvmember
            ~perm:(Some Freeable)
        in
        (gil_type, extra_asrts @ [ mem ])
  in
  ([ (name, Some gil_type) ], asrts, Stdlib.(offset + size))

let convert_struct_padding ~offset ~struct_name ~bits =
  let open Stdlib in
  if bits mod 8 <> 0 then
    Fmt.failwith "Unsupported padding bits (%d) in struct %s" bits struct_name;
  let bytes = bits / 8 in
  let low = offset in
  let high = offset + bytes in
  ([], [ assert_of_hole low high ], high)

let convert_struct_param ~ctx ~offset ~struct_name = function
  | Datatype_component.Field { name; type_ } ->
      convert_struct_field ~ctx ~offset ~struct_name name type_
  | Padding { bits; _ } -> convert_struct_padding ~offset ~struct_name ~bits

let gen_pred_of_struct ctx ann struct_name =
  let pred_name = pred_name_of_struct struct_name in
  let pred_ins = [ 0; 1 ] in
  let components =
    let struct_tag = "tag-" ^ struct_name in
    Ctx.resolve_struct_components ctx (Ctx.tag_lookup ctx struct_tag)
  in
  let first_params =
    [
      (loc_param_name, Some Type.ObjectType); (ofs_param_name, Some Type.IntType);
    ]
  in
  let struct_params, def, _ =
    List.fold_left
      (fun (params, def, offset) comp ->
        let params', def', offset' =
          convert_struct_param ~ctx ~offset ~struct_name comp
        in
        (params @ params', def @ def', offset'))
      ([], [], 0) components
  in
  let pred_params = first_params @ struct_params in
  let pred_num_params = List.length pred_params in
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

let trans_simpl_expr ~pvar_map : CSimplExpr.t -> Expr.t = function
  | PVar s ->
      let s' = List.assoc_opt s pvar_map |> Option.value ~default:s in
      PVar s'
  | LVar s -> LVar s
  | Loc s -> Lit (Loc s)
  | Int i -> Lit (Int i)
  | Bool b -> Lit (Bool b)
  | String s -> Lit (String s)

(* The first element of the result should be a pure assertion : either a formula, or overlapping assertions,
   The second element is the list of created variables, the third is the expression to be used
*)
let trans_sval ~pvar_map (sv : CSVal.t) : Asrt.t * Var.t list * Expr.t =
  let tnum = types Type.NumberType in
  let tint = types Type.IntType in
  let tloc = types Type.ObjectType in
  let tse = trans_simpl_expr ~pvar_map in
  match sv with
  | Sint se | Slong se ->
      let eg = tse se in
      ([ tint eg ], [], tse se)
  | Ssingle se | Sfloat se ->
      let eg = tse se in
      ([ tnum eg ], [], tse se)
  | Sptr (se1, se2) ->
      let eg1, eg2 = (tse se1, tse se2) in
      ([ tloc eg1; tint eg2 ], [], Expr.EList [ tse se1; tse se2 ])
  | Sfunptr symb ->
      let loc = location_of_symbol symb in
      let ptr = Expr.EList [ Lit (Loc loc); Expr.zero_i ] in
      ([], [], ptr)

(** Returns assertions that are necessary to define the expression,
      the created variable for binding when necessary, and the used expression *)
let rec trans_expr ~pvar_map (e : CExpr.t) : Asrt.t * Var.t list * Expr.t =
  let te = trans_expr ~pvar_map in
  match e with
  | CExpr.SExpr se -> ([], [], trans_simpl_expr ~pvar_map se)
  | SVal sv -> trans_sval ~pvar_map sv
  | EList el ->
      let asrt, vars, elp = split3_expr_comp (List.map te el) in
      (asrt, vars, Expr.EList elp)
  | ESet es ->
      let asrt, vars, elp = split3_expr_comp (List.map te es) in
      (asrt, vars, Expr.ESet elp)
  | BinOp (e1, LstCat, e2) ->
      let a1, v1, eg1 = te e1 in
      let a2, v2, eg2 = te e2 in
      (a1 @ a2, v1 @ v2, Expr.list_cat eg1 eg2)
  | BinOp (e1, LstCons, e2) ->
      let a1, v1, eg1 = te e1 in
      let a2, v2, eg2 = te e2 in
      (a1 @ a2, v1 @ v2, Expr.list_cat (EList [ eg1 ]) eg2)
  | BinOp (e1, PtrPlus, e2) -> (
      let a1, v1, ptr = te e1 in
      let a2, v2, to_add = te e2 in
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
      let a1, v1, eg1 = te e1 in
      let a2, v2, eg2 = te e2 in
      (a1 @ a2, v1 @ v2, BinOp (eg1, trans_binop b, eg2))
  | UnOp (u, e) ->
      let a, v, eg = te e in
      (a, v, UnOp (trans_unop u, eg))
  | NOp (nop, el) ->
      let asrt, vs, elp = split3_expr_comp (List.map te el) in
      let gnop = trans_nop nop in
      (asrt, vs, Expr.NOp (gnop, elp))
  | LstSub (lst, start, len) ->
      let a1, v1, lst = te lst in
      let a2, v2, start = te start in
      let a3, v3, len = te len in
      (a1 @ a2 @ a3, v1 @ v2 @ v3, Expr.list_sub ~lst ~start ~size:len)

let rec trans_form ~pvar_map (f : CFormula.t) : Asrt.t * Var.t list * Expr.t =
  let te = trans_expr ~pvar_map in
  let tf = trans_form ~pvar_map in
  match f with
  | True -> ([], [], Expr.true_)
  | False -> ([], [], Expr.false_)
  | Eq (ce1, ce2) ->
      let f1, v1, eg1 = te ce1 in
      let f2, v2, eg2 = te ce2 in
      (f1 @ f2, v1 @ v2, eg1 == eg2)
  | LessEq (ce1, ce2) ->
      let f1, v1, eg1 = te ce1 in
      let f2, v2, eg2 = te ce2 in
      (f1 @ f2, v1 @ v2, eg1 <= eg2)
  | Less (ce1, ce2) ->
      let f1, v1, eg1 = te ce1 in
      let f2, v2, eg2 = te ce2 in
      (f1 @ f2, v1 @ v2, eg1 < eg2)
  | SetMem (ce1, ce2) ->
      let f1, v1, eg1 = te ce1 in
      let f2, v2, eg2 = te ce2 in
      (f1 @ f2, v1 @ v2, BinOp (eg1, SetMem, eg2))
  | Not fp ->
      let a, v, fpp = tf fp in
      (a, v, not fpp)
  | Or (f1, f2) ->
      let a1, v1, fp1 = tf f1 in
      let a2, v2, fp2 = tf f2 in
      (a1 @ a2, v1 @ v2, fp1 || fp2)
  | And (f1, f2) ->
      let a1, v1, fp1 = tf f1 in
      let a2, v2, fp2 = tf f2 in
      (a1 @ a2, v1 @ v2, fp1 && fp2)
  | Implies (f1, f2) ->
      let a1, v1, fp1 = tf f1 in
      let a2, v2, fp2 = tf f2 in
      (a1 @ a2, v1 @ v2, fp1 ==> fp2)
  | ForAll (lvts, f) ->
      let a, v, fp = tf f in
      (a, v, ForAll (lvts, fp))

let malloc_chunk_asrt loc beg_ofs struct_sz =
  Constr.Others.malloced ~ptr:(loc, beg_ofs) ~total_size:struct_sz

let trans_constr ~(ctx : Ctx.t) ~(typ : CAssert.points_to_type) ~pvar_map s c =
  let malloc =
    match typ with
    | Malloced -> true
    | _ -> false
  in
  let gen_loc_var () = Expr.LVar (fresh_lvar ()) in
  let gen_ofs_var () = Expr.LVar (fresh_lvar ()) in
  let te = trans_expr ~pvar_map in
  let tse = trans_simpl_expr ~pvar_map in
  let ptr_call p l o = Asrt.Pred (Internal_Predicates.ptr_get, [ p; l; o ]) in
  let sz x = CSVal.size_of ~ctx x |> Z.of_int in
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
        let loc = location_of_symbol symb in
        let loc = Expr.Lit (Loc loc) in
        let ofsv = Expr.int 0 in
        ([], loc, ofsv)
    | _ ->
        let a_s, _, s_e = te s in
        let locv = gen_loc_var () in
        let ofsv = gen_ofs_var () in
        let pc = ptr_call s_e locv ofsv in
        (pc :: a_s, locv, ofsv)
  in
  let to_assert, locv, ofsv = interpret_s ~typ s in
  let malloc_chunk siz =
    if malloc then malloc_chunk_asrt locv ofsv siz else Asrt.Emp
  in
  let mk_ptr l o = Expr.list [ l; o ] in
  match c with
  | CConstructor.ConsExpr (SVal (Sint v as se))
  | ConsExpr (SVal (Sfloat v as se))
  | ConsExpr (SVal (Ssingle v as se))
  | ConsExpr (SVal (Slong v as se)) ->
      let chunk = CSVal.chunk_of ~ctx se in
      let asrtfn = types (CSVal.gil_type_of se) in
      let sval = tse v in
      let siz = sz se in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval ~perm:(Some Freeable)
      in
      [ ga; asrtfn sval; malloc_chunk siz ] @ to_assert
  | ConsExpr (SVal (Sptr (sl, so) as se)) ->
      let l = tse sl in
      let o = tse so in
      let chunk = CSVal.chunk_of ~ctx se in
      let siz = sz (Sptr (sl, so)) in
      let sval = mk_ptr l o in
      let ga =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval ~perm:(Some Freeable)
      in
      [ ga; types ObjectType l; types IntType o; malloc_chunk siz ] @ to_assert
  | ConsExpr (SVal (Sfunptr fname as se)) ->
      let l = location_of_symbol fname in
      let ptr = Expr.EList [ Expr.Lit (Loc l); Expr.zero_i ] in
      let chunk = CSVal.chunk_of ~ctx se in
      let siz = sz (Sfunptr fname) in
      let ga_single =
        CoreP.single ~loc:locv ~ofs:ofsv ~chunk ~sval:ptr ~perm:(Some Freeable)
      in
      [ ga_single; malloc_chunk siz ] @ to_assert
  | ConsExpr _ ->
      Fmt.failwith "Constructor %a is not handled yet" CConstructor.pp c
  | ConsStruct (sname, el) ->
      let struct_pred = pred_name_of_struct sname in
      let size =
        (* In specs, you must always refer to `struct x` and not a typedef, so the tag must be 'tag-'^sname *)
        let type_ = Ctx.tag_lookup ctx ("tag-" ^ sname) in
        Ctx.size_of ctx type_ |> Z.of_int
      in
      let more_asrt, _, params_fields = split3_expr_comp (List.map te el) in
      let pr =
        Asrt.Pred (struct_pred, [ locv; ofsv ] @ params_fields) :: more_asrt
      in
      pr @ to_assert @ [ malloc_chunk size ]

let rec trans_asrt ~ctx ~pvar_map asrt =
  let ta = trans_asrt ~ctx ~pvar_map in
  let te = trans_expr ~pvar_map in
  let a =
    match asrt with
    | CAssert.Star (a1, a2) ->
        let a1 = ta a1 in
        let a2 = ta a2 in
        a1 @ a2
    | Array { ptr; chunk; size; content; malloced } ->
        let a1, _, ptr = te ptr in
        let a2, _, size = te size in
        let a3, _, content = te content in
        let malloc_p =
          if malloced then
            let csize = Expr.int (Chunk.size chunk) in
            let total_size = size * csize in
            [ Constr.Others.malloced_abst ~ptr ~total_size ]
          else []
        in
        a1 @ a2 @ a3
        @ [ Constr.Others.array_ptr ~ptr ~chunk ~size ~content ]
        @ malloc_p
    | Malloced (e1, e2) ->
        let a1, _, ce1 = te e1 in
        let a2, _, ce2 = te e2 in
        a1 @ a2 @ [ Constr.Others.malloced_abst ~ptr:ce1 ~total_size:ce2 ]
    | Zeros (e1, e2) ->
        let a1, _, ce1 = te e1 in
        let a2, _, ce2 = te e2 in
        a1 @ a2 @ [ Constr.Others.zeros_ptr_size ~ptr:ce1 ~size:ce2 ]
    | Undefs (e1, e2) ->
        let a1, _, ce1 = te e1 in
        let a2, _, ce2 = te e2 in
        a1 @ a2 @ [ Constr.Others.undefs_ptr_size ~ptr:ce1 ~size:ce2 ]
    | Pure f ->
        let ma, _, fp = trans_form ~pvar_map f in
        Pure fp :: ma
    | Pred (p, cel) ->
        let ap, _, gel = split3_expr_comp (List.map te cel) in
        Pred (p, gel) :: ap
    | Emp -> [ Asrt.Emp ]
    | PointsTo { ptr = s; constr = c; typ } ->
        trans_constr ~pvar_map ~ctx ~typ s c
  in
  match List.filter (fun x -> x <> Asrt.Emp) a with
  | [] -> [ Asrt.Emp ]
  | a -> a

let rec trans_lcmd ~ctx ~pvar_map lcmd =
  let trans_expr = trans_expr ~pvar_map in
  let trans_lcmd = trans_lcmd in
  let trans_asrt = trans_asrt ~pvar_map in
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
  | Assert (a, ex) -> `Normal [ SL (SepAssert (trans_asrt ~ctx a, ex)) ]
  | Branch f ->
      let to_assert, bindings, f_gil = trans_form ~pvar_map f in
      `Normal (make_assert ~bindings to_assert @ [ Branch f_gil ])
  | If (e, cl1, cl2) ->
      let trans_normal_lcmd lcmd =
        match trans_lcmd ~ctx ~pvar_map lcmd with
        | `Normal x -> x
        | `Invariant _ ->
            Fmt.failwith "Invariant inside if/else in logic command"
      in
      let f, vs, ge = trans_expr e in
      let gcl1 = List.concat_map trans_normal_lcmd cl1 in
      let gcl2 = List.concat_map trans_normal_lcmd cl2 in
      `Normal (make_assert ~bindings:vs f @ [ If (ge, gcl1, gcl2) ])
  | Invariant { bindings; assertion } ->
      let asrt = trans_asrt ~ctx assertion in
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

let trans_pred ~ctx ~pvar_map ~filepath cl_pred =
  let ta = trans_asrt ~ctx ~pvar_map in
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
        | None -> (None, ta a)
        | Some da ->
            let ada, gda = trans_asrt_annot da in
            (Some gda, ada @ ta a))
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

let add_trans_pred ~ctx ~pvar_map filepath ann cl_pred =
  { ann with preds = trans_pred ~ctx ~pvar_map ~filepath cl_pred :: ann.preds }

let add_trans_abs_pred filepath ann cl_pred =
  { ann with preds = trans_abs_pred ~filepath cl_pred :: ann.preds }

let trans_sspec ~ctx ~pvar_map sspecs =
  let CSpec.{ pre; posts; spec_annot } = sspecs in
  let tap, spa =
    match spec_annot with
    | None -> ([], None)
    | Some spa ->
        let a, (label, exs) = trans_asrt_annot spa in
        (a, Some (label, exs))
  in
  let ta = trans_asrt ~ctx ~pvar_map in
  Spec.
    {
      ss_pre = (tap @ ta pre, None);
      ss_posts = List.map (fun post -> (ta post, None)) posts;
      (* FIXME: bring in variant *)
      ss_variant = None;
      ss_flag = Flag.Normal;
      ss_to_verify = true;
      ss_label = spa;
    }

let trans_lemma ~ctx ~pvar_map ~filepath lemma =
  let CLemma.{ name; params; hypothesis; conclusions; proof } = lemma in
  let trans_asrt = trans_asrt ~pvar_map ~ctx in
  let trans_lcmd = trans_lcmd ~pvar_map ~ctx in
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

let trans_spec ~ctx ?(only_spec = false) cl_spec =
  let CSpec.{ fname; params; sspecs } = cl_spec in
  let pvar_map =
    let f = Hashtbl.find ctx.Ctx.prog.funs fname in
    f.param_map
  in
  let spec_params = List.map (fun p -> List.assoc p pvar_map) params in
  let result =
    Spec.
      {
        spec_name = fname;
        spec_params;
        spec_sspecs = List.map (trans_sspec ~ctx ~pvar_map) sspecs;
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

let add_trans_spec ~ctx ann cl_spec =
  { ann with specs = trans_spec ~ctx cl_spec :: ann.specs }

let add_trans_only_spec ~ctx ann cl_spec =
  {
    ann with
    onlyspecs = trans_spec ~ctx ~only_spec:true cl_spec :: ann.onlyspecs;
  }

let add_trans_lemma ~ctx ~pvar_map filepath ann cl_lemma =
  {
    ann with
    lemmas = trans_lemma ~ctx ~pvar_map ~filepath cl_lemma :: ann.lemmas;
  }

let trans_annots ~(ctx : Ctx.t) ?(pvar_map = []) (lprog : C2_lprog.t) filepath =
  let structs_not_annot = get_structs_not_annot ctx in
  let struct_annots =
    List.fold_left (gen_pred_of_struct ctx) empty structs_not_annot
  in
  let with_preds =
    List.fold_left
      (add_trans_pred ~ctx ~pvar_map filepath)
      struct_annots lprog.CProg.preds
  in
  let with_abs_preds =
    List.fold_left
      (add_trans_abs_pred filepath)
      with_preds lprog.CProg.abs_preds
  in
  let with_lemmas =
    List.fold_left
      (add_trans_lemma ~ctx ~pvar_map filepath)
      with_abs_preds lprog.lemmas
  in
  let with_specs =
    List.fold_left (add_trans_spec ~ctx) with_lemmas lprog.specs
  in
  let with_only_specs =
    List.fold_left (add_trans_only_spec ~ctx) with_specs lprog.only_specs
  in
  let imports =
    List.map
      (fun (i, b) -> (Filename.concat (Filename.dirname filepath) i, b))
      lprog.imports
  in
  { with_only_specs with imports }
