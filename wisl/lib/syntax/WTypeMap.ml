open WType

module TypeMap = Map.Make (struct
  type t = WLExpr.tt

  let compare = Stdlib.compare
end)

type t = WType.t TypeMap.t

let type_of_variable (var : string) (type_context : t) : WType.t option =
  TypeMap.find_opt (WLExpr.PVar var) type_context

let of_val : WVal.t -> WType.t = function
  | Bool _ -> WBool
  | Int _ -> WInt
  | Float _ -> WFloat
  | Str _ -> WString
  | Null -> WNull
  | VList _ -> WList

(** returns (x, y) when unop takes type x and returns type y *)
let of_unop u =
  match u with
  | WUnOp.NOT -> (WBool, WBool)
  | WUnOp.LEN -> (WList, WInt)
  | WUnOp.REV -> (WList, WList)
  | WUnOp.HEAD -> (WList, WAny)
  | WUnOp.TAIL -> (WList, WList)

(** returns (x, y, z) when binop takes types x and y and returns type z *)
let of_binop : WBinOp.t -> WType.t * WType.t * WType.t = function
  | EQUAL -> (WAny, WAny, WBool)
  | LESSTHAN | GREATERTHAN | LESSEQUAL | GREATEREQUAL -> (WInt, WInt, WBool)
  | FLESSTHAN | FGREATERTHAN | FLESSEQUAL | FGREATEREQUAL ->
      (WFloat, WFloat, WBool)
  | TIMES | DIV | MOD -> (WInt, WInt, WInt)
  | FPLUS | FMINUS | FTIMES | FDIV | FMOD -> (WFloat, WFloat, WFloat)
  | AND | OR -> (WBool, WBool, WBool)
  | LSTCONS -> (WAny, WList, WList)
  | LSTCAT -> (WList, WList, WList)
  | LSTNTH -> (WList, WInt, WAny)
  | PLUS | MINUS -> (WAny, WAny, WAny)

(* TODO: improve this, because we can add Ints AND Pointers *)

(** checks and adds to typemap *)
let needs_to_be expr t knownp =
  let bare_expr = WLExpr.get expr in
  match TypeMap.find_opt bare_expr knownp with
  | Some tp when not (compatible t tp) ->
      failwith
        (Format.asprintf
           "I inferred both types %a and %a on expression %a at location %s" pp
           tp pp t WLExpr.pp expr
           (CodeLoc.str (WLExpr.get_loc expr)))
  | Some tp -> TypeMap.add bare_expr (strongest t tp) knownp
  | None -> TypeMap.add bare_expr t knownp

let same_type e1 e2 knownp =
  let bare_e1, bare_e2 = (WLExpr.get e1, WLExpr.get e2) in
  let topt1 = TypeMap.find_opt bare_e1 knownp in
  let topt2 = TypeMap.find_opt bare_e2 knownp in
  match (topt1, topt2) with
  | Some t1, Some t2 when not (compatible t1 t2) ->
      failwith
        (Format.asprintf
           "Expressions %a and %a should have the same type but are of types \
            %a and %a."
           WLExpr.pp e1 WLExpr.pp e2 pp t1 pp t2)
  | Some t1, Some t2 -> Some (strongest t1 t2)
  | Some t1, None -> Some t1
  | None, Some t2 -> Some t2
  | None, None -> None

(** Infers a TypeMap from a logic_expr *)
let rec infer_logic_expr knownp lexpr =
  let open WLExpr in
  let bare_lexpr = get lexpr in
  match bare_lexpr with
  | LVal v -> TypeMap.add bare_lexpr (of_val v) knownp
  | LBinOp (le1, EQUAL, le2) -> (
      let bare_le1, bare_le2 = (WLExpr.get le1, WLExpr.get le2) in
      let inferred = infer_logic_expr (infer_logic_expr knownp le1) le2 in
      let topt = same_type le1 le2 inferred in
      match topt with
      | Some t -> TypeMap.add bare_le1 t (TypeMap.add bare_le2 t inferred)
      | None -> inferred)
  | LBinOp (le1, b, le2) ->
      let inferred = infer_logic_expr (infer_logic_expr knownp le1) le2 in
      let t1, t2, t3 = of_binop b in
      TypeMap.add bare_lexpr t3
        (needs_to_be le1 t1 (needs_to_be le2 t2 inferred))
  | LUnOp (u, le) ->
      let inferred = infer_logic_expr knownp le in
      let t1, t2 = of_unop u in
      TypeMap.add bare_lexpr t2 (needs_to_be le t1 inferred)
  | LLSub (le1, le2, le3) ->
      let inferred =
        infer_logic_expr
          (infer_logic_expr (infer_logic_expr knownp le1) le2)
          le3
      in
      let t0, t1, t2, t3 = (WList, WList, WInt, WInt) in
      TypeMap.add bare_lexpr t0
        (needs_to_be le1 t1 (needs_to_be le2 t2 (needs_to_be le3 t3 inferred)))
  | LVar _ -> knownp
  | PVar _ -> knownp
  | LEList lel ->
      TypeMap.add bare_lexpr WList (List.fold_left infer_logic_expr knownp lel)
  | LESet lel ->
      TypeMap.add bare_lexpr WSet (List.fold_left infer_logic_expr knownp lel)

(** Single step of inference for that gets a TypeMap from a single assertion *)
let rec infer_single_assert_step asser known =
  match WLAssert.get asser with
  | WLAssert.LEmp -> known
  | WLAssert.LStar (la1, la2) ->
      infer_single_assert_step la2 (infer_single_assert_step la1 known)
  | WLAssert.LPred (_, lel) -> List.fold_left infer_logic_expr known lel
  | WLAssert.LWand { lhs = _, largs; rhs = _, rargs } ->
      List.fold_left infer_logic_expr known largs |> fun acc ->
      List.fold_left infer_logic_expr acc rargs
  | WLAssert.LPointsTo (le1, le2) ->
      let le_perm = List.filter_map fst le2 in
      let le2 = List.map snd le2 in
      let inferred =
        List.fold_left infer_logic_expr (infer_logic_expr known le1) le2
      in
      let inferred =
        List.fold_left (fun acc p -> needs_to_be p WFloat acc) inferred le_perm
      in
      needs_to_be le1 WList inferred
  | WLAssert.LBlockPointsTo (le1, le2) ->
      let le_perm = List.filter_map fst le2 in
      let le2 = List.map snd le2 in
      let inferred =
        List.fold_left infer_logic_expr (infer_logic_expr known le1) le2
      in
      let inferred =
        List.fold_left (fun acc p -> needs_to_be p WFloat acc) inferred le_perm
      in
      needs_to_be le1 WList inferred
  | WLAssert.LPure f -> infer_logic_expr known f
  | WLAssert.LType (le, ty) -> needs_to_be le ty known

let infer_single_assert known asser =
  let rec find_fixed_point f a =
    let b = f a in
    if Stdlib.compare a b = 0 then b else find_fixed_point f b
  in
  find_fixed_point (infer_single_assert_step asser) known

let infer_types_pred (params : (string * WType.t option) list) assert_list =
  let join_params_and_asserts _le topt1 topt2 =
    match (topt1, topt2) with
    | Some t1, Some t2 when t1 = t2 -> Some t1
    | Some t, None when t <> WAny -> Some t
    | None, Some t when t <> WAny -> Some t
    | _ -> None
  in
  let join_asserts _le topt1 topt2 =
    match (topt1, topt2) with
    | Some t1, Some t2 when t1 = t2 -> Some t1
    | _ -> None
  in
  let infers_on_params =
    List.fold_left
      (fun (map : 'a TypeMap.t) (x, ot) ->
        match ot with
        | None -> map
        | Some t -> TypeMap.add (PVar x) t map)
      TypeMap.empty params
  in
  let infers_on_asserts =
    List.map (infer_single_assert TypeMap.empty) assert_list
  in
  let hd, tl = (List.hd infers_on_asserts, List.tl infers_on_asserts) in
  let infers_on_asserts = List.fold_left (TypeMap.merge join_asserts) hd tl in
  let result =
    TypeMap.merge join_params_and_asserts infers_on_params infers_on_asserts
  in
  result
