open Gil_syntax
module GType = Goto_lib.Type

let rec asrt_of_scalar_like ~ctx (type_ : GType.t) (expr : Expr.t) : Asrt.t =
  match type_ with
  | CInteger I_bool ->
      (* Special case, the bounds are different *)
      let assume_int = Asrt.Types [ (expr, IntType) ] in
      let condition =
        let open Expr.Infix in
        expr == Expr.one_i || expr == Expr.zero_i
      in
      let asrt_range = Asrt.Pure condition in
      [ assume_int; asrt_range ]
  | CInteger _ | Signedbv _ | Unsignedbv _ ->
      let assume_int = Asrt.Types [ (expr, IntType) ] in
      let bounds =
        Option.bind (Memory.chunk_for_type ~ctx type_) Chunk.bounds
      in
      let assume_range =
        match bounds with
        | None -> Asrt.Emp
        | Some (low, high) ->
            let open Expr.Infix in
            let condition = Expr.int_z low <= expr && expr <= Expr.int_z high in
            Asrt.Pure condition
      in
      [ assume_int; assume_range ]
  | Double | Float -> [ Asrt.Types [ (expr, NumberType) ] ]
  | Pointer _ ->
      let loc = LVar.alloc () in
      let ofs = LVar.alloc () in
      let e_loc = Expr.LVar loc in
      let e_ofs = Expr.LVar ofs in
      let assume_list =
        let f = Expr.BinOp (expr, Equal, EList [ e_loc; e_ofs ]) in
        Asrt.Pure f
      in
      let types = Asrt.Types [ (e_loc, ObjectType); (e_ofs, IntType) ] in
      [ assume_list; types ]
  | Bool -> [ Asrt.Types [ (expr, BooleanType) ] ]
  | StructTag _ | Struct _ ->
      let ty =
        let fields = Ctx.resolve_struct_components ctx type_ in
        match Ctx.one_representable_field ctx fields with
        | Some (_, ty) -> ty
        | None ->
            Error.code_error "No representable type for assume_type struct"
      in
      asrt_of_scalar_like ~ctx ty expr
  | _ -> Error.code_error "Unreachable: assume_type for non-scalar"

let assumption_of_param ~ctx ~(v : Var.t) ~(ty : GType.t) =
  (* The logic of what formulaes is generated should be factorised
     with [Compiled_expr.nondet_expr] *)
  if Ctx.representable_in_store ctx ty then
    let e_s = Expr.LVar (LVar.alloc ()) in
    let f = Expr.BinOp (Expr.PVar v, Equal, e_s) in
    Asrt.Pure f :: asrt_of_scalar_like ~ctx ty e_s
  else failwith "unhandled: composit parameter"

let assumption_of_ret_by_copy ~ctx ty =
  let size = Expr.int (Ctx.size_of ctx ty) in
  let loc = Expr.LVar (LVar.alloc ()) in
  let hole =
    Memory_model.Predicates.Core.hole ~loc ~low:Expr.zero_i ~high:size
      ~perm:(Some Freeable)
  in
  let types = Asrt.Types [ (loc, ObjectType) ] in
  [ types; hole ]

let bispec ~ctx ~(compiled : (C2_annot.t, string) Proc.t) (f : Program.Func.t) =
  let ret_type_assume =
    if Ctx.representable_in_store ctx f.return_type then []
    else assumption_of_ret_by_copy ~ctx f.return_type
  in
  let param_names =
    (* If the return type is by copy, then it was added in front of the parameters *)
    if Ctx.representable_in_store ctx f.return_type then compiled.proc_params
    else List.tl compiled.proc_params
  in
  let param_asrts =
    List.map2
      (fun v Param.{ type_ = ty; _ } -> assumption_of_param ~ctx ~v ~ty)
      param_names f.params
  in
  let pre = ret_type_assume @ List.flatten param_asrts in
  BiSpec.
    {
      bispec_name = compiled.proc_name;
      bispec_params = compiled.proc_params;
      bispec_pres = [ (pre, None) ];
      bispec_normalised = false;
    }
