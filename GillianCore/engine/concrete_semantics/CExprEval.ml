open Arith_utils
open BVOps
module CStore = Store.Make (CVal.M)

(* Expression Evaluation *)

exception TypeError of string
exception EvaluationError of string

let evalerr msg = raise (EvaluationError (Fmt.str "Evaluation Error: %s" msg))

let typeerr ?msg typ lit =
  raise
    (TypeError
       (match msg with
       | Some msg -> Fmt.str "Expected %s, got %a (%s)" typ Literal.pp lit msg
       | None -> Fmt.str "Expected %s, got %a" typ Literal.pp lit))

let as_str ?msg = function
  | Literal.String s -> s
  | lit -> typeerr ?msg "string" lit

let as_bool ?msg = function
  | Literal.Bool b -> b
  | lit -> typeerr ?msg "boolean" lit

let as_int ?msg = function
  | Literal.Int i -> i
  | lit -> typeerr ?msg "integer" lit

let as_bvorint ?msg = function
  | Literal.Int i -> i
  | Literal.LBitvector (i, _) -> i
  | lit -> typeerr ?msg "bitvector or integer" lit

let as_num ?msg = function
  | Literal.Num n -> n
  | lit -> typeerr ?msg "number" lit

let as_list ?msg = function
  | Literal.LList l -> l
  | lit -> typeerr ?msg "list" lit

let unary_int_thing ?msg (lit : CVal.M.t) (f : Z.t -> Z.t) : CVal.M.t =
  let num = as_int ?msg lit in
  let res = f num in
  Int res

let unary_num_thing ?msg (lit : CVal.M.t) (f : float -> float) : CVal.M.t =
  let num = as_num ?msg lit in
  let res = f num in
  Num res

let binary_num_thing
    ?msg
    (lit1 : CVal.M.t)
    (lit2 : CVal.M.t)
    (f : float -> float -> float) =
  let num1 = as_num ?msg lit1 in
  let num2 = as_num ?msg lit2 in
  Literal.Num (f num1 num2)

let binary_int_thing
    ?msg
    (lit1 : CVal.M.t)
    (lit2 : CVal.M.t)
    (f : Z.t -> Z.t -> Z.t) =
  let num1 = as_int ?msg lit1 in
  let num2 = as_int ?msg lit2 in
  Literal.Int (f num1 num2)

let binary_int_bool_thing
    ?msg
    (lit1 : CVal.M.t)
    (lit2 : CVal.M.t)
    (f : Z.t -> Z.t -> bool) =
  let num1 = as_int ?msg lit1 in
  let num2 = as_int ?msg lit2 in
  Literal.Bool (f num1 num2)

let binary_num_bool_thing
    ?msg
    (lit1 : CVal.M.t)
    (lit2 : CVal.M.t)
    (f : float -> float -> bool) =
  let num1 = as_num ?msg lit1 in
  let num2 = as_num ?msg lit2 in
  Literal.Bool (f num1 num2)

let evaluate_unop (op : UnOp.t) (lit : CVal.M.t) : CVal.M.t =
  let unary_int_thing = unary_int_thing ~msg:(UnOp.str op) in
  let unary_num_thing = unary_num_thing ~msg:(UnOp.str op) in
  match op with
  | Not ->
      let b = as_bool lit in
      Bool (not b)
  | IUnaryMinus -> unary_int_thing lit Z.neg
  | FUnaryMinus -> unary_num_thing lit (fun x -> -.x)
  | BitwiseNot -> unary_num_thing lit int32_bitwise_not
  | M_abs -> unary_num_thing lit abs_float
  | M_acos -> unary_num_thing lit acos
  | M_asin -> unary_num_thing lit asin
  | M_atan -> unary_num_thing lit atan
  | M_ceil -> unary_num_thing lit ceil
  | M_cos -> unary_num_thing lit cos
  | M_exp -> unary_num_thing lit exp
  | M_floor -> unary_num_thing lit floor
  | M_log -> unary_num_thing lit log
  | M_round ->
      let f n =
        let sign = copysign 1.0 n in
        match sign < 0.0 && n >= -0.5 with
        | true -> -0.0
        | _ ->
            (* This complex rounding is needed for edge case in OCaml: 0.49999999999999994 *)
            let round_nearest_lb = -.(2. ** 52.) in
            let round_nearest_ub = 2. ** 52. in

            let round_nearest t =
              if t >= round_nearest_lb && t <= round_nearest_ub then
                floor (t +. 0.49999999999999994)
              else t
            in
            round_nearest n
      in
      unary_num_thing lit f
  | M_sgn -> unary_num_thing lit (fun x -> copysign 1.0 x)
  | M_sin -> unary_num_thing lit sin
  | M_sqrt -> unary_num_thing lit sqrt
  | M_tan -> unary_num_thing lit tan
  | ToStringOp ->
      let n = as_num lit in
      String (float_to_string_inner n)
  | ToIntOp -> unary_num_thing lit to_int
  | ToUint16Op -> unary_num_thing lit to_uint16
  | ToInt32Op -> unary_num_thing lit to_int32
  | ToUint32Op -> unary_num_thing lit to_uint32
  | ToNumberOp ->
      let s = as_str lit in
      if s = "" then Num 0.
      else
        let num = try Float.of_string s with Failure _ -> nan in
        Num num
  | IntToNum ->
      let x = as_int lit in
      Num (Z.to_float x)
  | NumToInt ->
      let x = as_num lit in
      Int (Z.of_float x)
  | TypeOf -> Type (Literal.type_of lit)
  | Car -> (
      let ll = as_list lit in
      match ll with
      | [] -> evalerr "List head of empty list"
      | lit :: _ -> lit)
  | Cdr -> (
      let ll = as_list lit in
      match ll with
      | [] -> evalerr "List tail of empty list"
      | _ :: ll -> LList ll)
  | LstLen ->
      let ll = as_list lit in
      Int (Z.of_int (List.length ll))
  | LstRev ->
      let ll = as_list lit in
      LList (List.rev ll)
  | StrLen ->
      let s = as_str lit in
      Num (float_of_int (String.length s))
  | M_isNaN ->
      let x = as_num lit in
      Bool (x <> x)
  | SetToList ->
      raise (Exceptions.Unsupported "eval_unop concrete: set-to-list")
  | IsInt ->
      let x = as_num lit in
      Bool (is_int x)

let rec evaluate_binop
    (store : CStore.t)
    (op : BinOp.t)
    (e1 : Expr.t)
    (e2 : Expr.t) : CVal.M.t =
  let ee = evaluate_expr store in
  let lit1 = ee e1 in
  let binary_int_bool_thing = binary_int_bool_thing ~msg:(BinOp.str op) in
  let binary_num_bool_thing = binary_num_bool_thing ~msg:(BinOp.str op) in
  let binary_int_thing = binary_int_thing ~msg:(BinOp.str op) in
  let binary_num_thing = binary_num_thing ~msg:(BinOp.str op) in
  match op with
  | Impl -> ee (BinOp (UnOp (Not, Expr.Lit lit1), Or, e2))
  | And ->
      let b1 = as_bool ~msg:"And" lit1 in
      if not b1 then Bool false
      else
        let b2 = as_bool ~msg:"And" @@ ee e2 in
        Bool b2
  | Or ->
      let b1 = as_bool ~msg:"Or" lit1 in
      if b1 then Bool true
      else
        let b2 = as_bool ~msg:"Or" @@ ee e2 in
        Bool b2
  | _ -> (
      let lit2 = ee e2 in
      match op with
      | SetDiff | SetMem | SetSub ->
          raise (Exceptions.Unsupported "eval_binop concrete: set operator")
      | Or | And | Impl ->
          raise (Exceptions.Impossible "eval_binop concrete: by construction")
      | Equal -> (
          match (lit1, lit2) with
          | Undefined, Undefined -> Bool true
          | Null, Null -> Bool true
          | Empty, Empty -> Bool true
          | Constant c1, Constant c2 -> Bool (c1 = c2)
          | Bool b1, Bool b2 -> Bool (b1 = b2)
          | Int n1, Int n2 -> Bool (n1 = n2)
          | Num n1, Num n2 -> Bool (n1 = n2)
          | String s1, String s2 -> Bool (s1 = s2)
          | Loc l1, Loc l2 -> Bool (l1 = l2)
          | Type t1, Type t2 -> Bool (t1 = t2)
          | LList l1, LList l2 -> Bool (l1 = l2)
          | Nono, Nono -> Bool true
          | _, _ -> Bool false)
      | LstNth -> (
          let list = as_list ~msg:"LstNth" lit1 in
          match lit2 with
          | Int n -> List.nth list (Z.to_int n)
          | Num n when is_int n -> List.nth list (int_of_float n)
          | Num -0. -> List.nth list 0
          | _ -> typeerr ~msg:"LstNth" "integer or number" lit2)
      | LstRepeat ->
          let n = as_int ~msg:"LstRepeat" lit2 in
          let n = Z.to_int n in
          let elements = List.init n (fun _ -> lit1) in
          LList elements
      | StrNth -> (
          let s = as_str ~msg:"StrNth" lit1 in
          match lit2 with
          | Num n when is_int n -> String (String.make 1 s.[int_of_float n])
          | Num -0. -> String (String.make 1 s.[0])
          | _ -> typeerr ~msg:"StrNth" "number" lit2)
      | ILessThan -> binary_int_bool_thing lit1 lit2 ( < )
      | FLessThan -> binary_num_bool_thing lit1 lit2 ( < )
      | StrLess ->
          let s1 = as_str lit1 in
          let s2 = as_str lit2 in
          Bool (s1 < s2)
      | ILessThanEqual -> binary_int_bool_thing lit1 lit2 ( <= )
      | FLessThanEqual -> binary_num_bool_thing lit1 lit2 ( <= )
      | IPlus -> binary_int_thing lit1 lit2 Z.add
      | IMinus -> binary_int_thing lit1 lit2 Z.sub
      | ITimes -> binary_int_thing lit1 lit2 Z.mul
      | IDiv -> binary_int_thing lit1 lit2 Z.div
      | IMod -> binary_int_thing lit1 lit2 Z.( mod )
      | FPlus -> binary_num_thing lit1 lit2 ( +. )
      | FMinus -> binary_num_thing lit1 lit2 ( -. )
      | FTimes -> binary_num_thing lit1 lit2 ( *. )
      | FDiv -> binary_num_thing lit1 lit2 ( /. )
      | FMod -> binary_num_thing lit1 lit2 mod_float
      | BitwiseAnd -> binary_int_thing lit1 lit2 Z.logand
      | BitwiseOr -> binary_int_thing lit1 lit2 Z.logor
      | BitwiseXor -> binary_int_thing lit1 lit2 Z.logxor
      | LeftShift ->
          binary_int_thing lit1 lit2 (fun x y -> Z.shift_left x (Z.to_int y))
      | SignedRightShift ->
          binary_int_thing lit1 lit2 (fun x y -> Z.shift_right x (Z.to_int y))
      | UnsignedRightShift ->
          binary_int_thing lit1 lit2 (fun x y -> Z.shift_right x (Z.to_int y))
      | BitwiseAndL -> binary_int_thing lit1 lit2 int64_bitwise_and
      | BitwiseOrL -> binary_int_thing lit1 lit2 int64_bitwise_or
      | BitwiseXorL -> binary_int_thing lit1 lit2 int64_bitwise_xor
      | LeftShiftL -> binary_int_thing lit1 lit2 int64_left_shift
      | SignedRightShiftL ->
          binary_int_thing lit1 lit2 (fun x y -> Z.shift_right x (Z.to_int y))
      | UnsignedRightShiftL ->
          binary_int_thing lit1 lit2 (fun x y -> Z.shift_right x (Z.to_int y))
      | BitwiseAndF -> binary_num_thing lit1 lit2 int32_bitwise_and
      | BitwiseOrF -> binary_num_thing lit1 lit2 int32_bitwise_or
      | BitwiseXorF -> binary_num_thing lit1 lit2 int32_bitwise_xor
      | LeftShiftF -> binary_num_thing lit1 lit2 int32_left_shift
      | SignedRightShiftF -> binary_num_thing lit1 lit2 int32_right_shift
      | UnsignedRightShiftF -> binary_num_thing lit1 lit2 uint32_right_shift_f
      | M_atan2 -> binary_num_thing lit1 lit2 atan2
      | M_pow -> binary_num_thing lit1 lit2 ( ** )
      | StrCat ->
          let s1 = as_str lit1 in
          let s2 = as_str lit2 in
          String (s1 ^ s2))

and evaluate_nop (nop : NOp.t) (ll : Literal.t list) : CVal.M.t =
  match nop with
  | LstCat -> LList (List.concat_map (as_list ~msg:"LstCat") ll)
  | SetInter | SetUnion ->
      raise (Exceptions.Unsupported "Concrete evaluate_nop: set operators")

and evaluate_elist store (ll : Expr.t list) : CVal.M.t =
  match ll with
  | [] -> LList []
  | e :: ll ->
      let ve = evaluate_expr store e in
      let vll = evaluate_expr store (EList ll) in
      let vll = as_list vll in
      LList (ve :: vll)

and evaluate_lstsub (store : CStore.t) (e1 : Expr.t) (e2 : Expr.t) (e3 : Expr.t)
    : CVal.M.t =
  let ee = evaluate_expr store in
  let ve1 = ee e1 in
  let ve2 = ee e2 in
  let ve3 = ee e3 in
  let les = as_list ve1 in
  let start = as_bvorint ve2 in
  let len = as_bvorint ve3 in
  let sub_list =
    List_utils.list_sub les (Z.to_int start) (Z.to_int len) |> Option.get
  in
  LList sub_list

and map_bvbinop width =
  let bv f l r = Literal.LBitvector (f l r, width)
  and bool f l r = Literal.Bool (f l r) in
  function
  | BVPlus -> bv Z.add
  | BVSub -> bv Z.sub
  | BVUleq -> bool Z.leq
  | BVUlt -> bool Z.lt
  | _ as op ->
      raise
        (Exceptions.Unsupported
           (Printf.sprintf "Unimplemented concrete BVOp %s" (BVOps.str op)))

and map_bvunop width =
  let _bv f x = Literal.LBitvector (f x, width)
  and _bool f x = Literal.Bool (f x) in
  function
  | _ as op ->
      raise
        (Exceptions.Unsupported
           (Printf.sprintf "Unimplemented concrete BVOp %s" (BVOps.str op)))

and evaluate_bvop
    (store : CStore.t)
    (op : BVOps.t)
    (es : Expr.bv_arg list)
    width : CVal.M.t =
  let _eval = evaluate_expr store
  and bv_lit = function
    | Expr.Literal i ->
        failwith "unhandled Literal in position expecting BvExpr, API misuse?"
    | Expr.BvExpr
        (Expr.Lit (Literal.LList [ String ty; (LBitvector (_, w) as bv) ]), _)
      -> (bv, w)
    | Expr.BvExpr (e, w) -> (evaluate_expr store e, w)
  in
  match (op, es) with
  | _, [ lhs; rhs ] -> (
      let lhs, lw = bv_lit lhs and rhs, rw = bv_lit rhs in
      assert (lw = rw);
      let f = map_bvbinop lw op in
      match (lhs, rhs) with
      | LBitvector (lhs, lw), LBitvector (rhs, rw) ->
          assert (lw = rw);
          f lhs rhs
      | _ -> failwith "Unhandled non-bitvector literal in evaluate_bvop")
  | _, [ e ] -> (
      let e, w = bv_lit e in
      let f = map_bvunop w op in
      match e with
      | LBitvector (e, w) -> f e
      | _ -> failwith "Unhandled non-bitvector literal in evaluate_binop")
  | BVExtract, [ Expr.Literal lo; Expr.Literal hi; (Expr.BvExpr _ as e) ] -> (
      let e, w = bv_lit e in
      let size_of_chunk x =
        let lst = String.split_on_char '-' x in
        if List.length lst = 2 && String.equal (List.hd lst) "int" then
          let st = List.nth lst 1 in
          int_of_string st
        else failwith ("invalid chunk " ^ x)
      in
      match e with
      | LBitvector (e, w) ->
          Literal.LBitvector (Z.extract e lo (hi - lo), hi - lo)
      | LList [ String ty; LBitvector (e, w) ] ->
          let w = hi - lo in
          let _chunk = size_of_chunk ty in
          Literal.LList
            [ String ty; Literal.LBitvector (Z.extract e lo (hi - lo), w) ]
      | _ as v ->
          Logging.tmi (fun m -> m "fallthru bvextract: %a" Literal.pp v);
          failwith "Unimplemented bvextract")
  | BVConcat, _ ->
      let v =
        List.fold_left
          (fun acc x ->
            match bv_lit x with
            | (LBitvector (v, w) | LList [ String _; LBitvector (v, w) ]), _ ->
                Logging.tmi (fun m -> m "+= %d" (Z.to_int v));
                (v, w)
            | (_ as l), _ ->
                Logging.tmi (fun m -> m "unhandled BVConcat: %a" Literal.pp l);
                failwith "?")
          (Z.zero, 0) es
      in
      Literal.LBitvector v
  | _ ->
      let op_name = BVOps.str op in
      failwith
        (Printf.sprintf "unhandled %s (%d args)" op_name (List.length es))

and evaluate_expr (store : CStore.t) (e : Expr.t) : CVal.M.t =
  try
    let ee = evaluate_expr store in
    match e with
    | Lit (Constant c) -> Literal.evaluate_constant c
    | Lit lit -> lit
    | PVar x -> (
        match CStore.get store x with
        | None ->
            let err_msg = Fmt.str "Variable %s not found in the store" x in
            (* if (!verbose) then Fmt.printf "The current store is: \n%s" CStore.pp store; *)
            raise (Failure err_msg)
        | Some v -> v)
    | BVExprIntrinsic (op, es, width) -> evaluate_bvop store op es width
    | BinOp (e1, bop, e2) -> evaluate_binop store bop e1 e2
    | UnOp (unop, e) -> evaluate_unop unop (ee e)
    | NOp (nop, le) -> evaluate_nop nop (List.map ee le)
    | EList ll -> evaluate_elist store ll
    | LstSub (e1, e2, e3) -> evaluate_lstsub store e1 e2 e3
    | ALoc _ | LVar _ | ESet _ | Exists _ | ForAll _ ->
        raise
          (Exceptions.Impossible
             "eval_expr concrete: aloc, lvar, set, exists or for all")
  with
  | TypeError msg -> raise (TypeError (msg ^ Fmt.str " in %a" Expr.pp e))
  | EvaluationError msg ->
      raise (EvaluationError (msg ^ Fmt.str " in %a" Expr.pp e))
  | Division_by_zero -> raise (EvaluationError "Division by zero")
  | e ->
      let msg = Printexc.to_string e in
      let stack = Printexc.get_backtrace () in
      failwith
        (Fmt.str "Expression evaluation: Untreatable Exception: %s%s\n" msg
           stack)
