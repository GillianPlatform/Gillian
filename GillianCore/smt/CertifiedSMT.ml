open Gil_syntax

module C : Cse.Smt.Coerce with type exp = Expr.t and type typ = Type.t = struct
  type exp = Expr.t
  type typ = Type.t

  let string_of_type = Type.str
  let string_of_literal lit = Fmt.str "%a" Literal.pp lit
  let string_of_symbexp e = Fmt.str "%a" Expr.full_pp e

  let coerce_type (t : Type.t) : Cse.Type.t option =
    match t with
    | Type.NullType -> Some Cse.Type.Null
    | Type.NoneType -> Some Cse.Type.None
    | Type.EmptyType -> Some Cse.Type.Empty
    | Type.BooleanType -> Some Cse.Type.Bool
    | Type.IntType -> Some Cse.Type.Nat
    | Type.NumberType -> Some Cse.Type.Rat
    | Type.StringType -> Some Cse.Type.String
    | Type.ListType -> Some (Cse.Type.List Cse.Type.Val)
    | Type.ObjectType -> Some (Cse.Type.List Cse.Type.Loc)
    | _ -> None

  let diagnose_type (t : Type.t) : string option =
    match coerce_type t with
    | Some _ -> None
    | None -> Some (Fmt.str "unsupported Gillian type %s" (Type.str t))

  let rec coerce_val (v : Literal.t) : Cse.Val.t option =
    match v with
    | Null -> Some Cse.Val.Null
    | Nono -> Some Cse.Val.None
    | Empty -> Some Cse.Val.Empty
    | Loc l -> Some (Cse.Val.Loc (Hashtbl.hash l))
    | Bool b -> Some (Cse.Val.Bool b)
    (* CSE's integers are naturals. A negative literal has no CSE value, and
       the extracted encoder assumes it never sees one. *)
    | Int i when Z.sign i >= 0 && Z.fits_int i ->
        Some (Cse.Val.Nat (Z.to_int i))
    | Int _ -> None
    | Num n -> (
        match Float.classify_float n with
        | FP_nan | FP_infinite -> None
        | FP_zero | FP_subnormal | FP_normal -> Some (Cse.Val.Rat n))
    | String s -> Some (Cse.Val.String s)
    | LList vs -> (
        let vs =
          List.fold_right
            (fun v acc ->
              match (coerce_val v, acc) with
              | Some v, Some vs -> Some (v :: vs)
              | _ -> None)
            vs (Some [])
        in
        match vs with
        | Some vs -> Some (Cse.Val.List vs)
        | None -> None)
    | _ -> None

  let rec diagnose_val (v : Literal.t) : string option =
    match v with
    | Null | Nono | Empty | Loc _ | Bool _ | String _ -> None
    | Int i when Z.sign i < 0 ->
        Some
          (Fmt.str "unsupported negative integer literal %s: CSE has naturals"
             (Z.to_string i))
    | Int i when not (Z.fits_int i) ->
        Some
          (Fmt.str "unsupported integer literal %s: too large" (Z.to_string i))
    | Int _ -> None
    | Num n -> (
        match Float.classify_float n with
        | FP_nan -> Some "unsupported NaN number literal"
        | FP_infinite -> Some "unsupported infinite number literal"
        | FP_zero | FP_subnormal | FP_normal -> None)
    | LList vs ->
        List.mapi (fun i v -> (i, diagnose_val v)) vs
        |> List.find_map (function
             | i, Some reason ->
                 Some
                   (Fmt.str "unsupported list literal element %d: %s" i reason)
             | _, None -> None)
    | Undefined ->
        Some
          (Fmt.str "unsupported literal %s: verified CSE has no undefined value"
             (string_of_literal v))
    | Constant _ ->
        Some
          (Fmt.str
             "unsupported literal %s: constants must be evaluated before \
              certified SMT coercion"
             (string_of_literal v))
    | Type _ ->
        Some
          (Fmt.str
             "unsupported literal %s: type literals are only supported in \
              TypeOf(e) == T tests"
             (string_of_literal v))

  let coerce_unop (op : UnOp.t) : Cse.Unop.t option =
    match op with
    | Not -> Some Cse.Unop.Not
    | LstLen -> Some Cse.Unop.Length
    | IsInt -> Some Cse.Unop.IsInt
    | NumToInt -> Some Cse.Unop.AsInt
    | IntToNum -> Some Cse.Unop.AsNum
    | _ -> None

  let diagnose_unop op =
    match coerce_unop op with
    | Some _ -> None
    | None -> Some (Fmt.str "unsupported unary operator %s" (UnOp.str op))

  let coerce_binop (op : BinOp.t) : Cse.Binop.t option =
    match op with
    | Equal -> Some Cse.Binop.Eq
    | ILessThan -> Some Cse.Binop.Lt
    | IPlus -> Some Cse.Binop.Add
    | IMinus -> Some Cse.Binop.Sub
    | ITimes -> Some Cse.Binop.Mul
    | IDiv -> Some Cse.Binop.Div
    | IMod -> Some Cse.Binop.Mod
    | FPlus -> Some Cse.Binop.RAdd
    | FMinus -> Some Cse.Binop.RSub
    | FTimes -> Some Cse.Binop.RMul
    | FDiv -> Some Cse.Binop.RDiv
    | FLessThan -> Some Cse.Binop.RLt
    | FLessThanEqual -> Some Cse.Binop.RLe
    (* GIL writes the list first, which is [Op2Nth]'s argument order too. *)
    | LstNth -> Some Cse.Binop.Nth
    (* Boolean *)
    | And -> Some Cse.Binop.And
    | _ -> None

  let diagnose_binop op =
    match coerce_binop op with
    | Some _ -> None
    | None -> Some (Fmt.str "unsupported binary operator %s" (BinOp.str op))

  let rec coerce_symbexp (e : Expr.t) =
    match e with
    | Lit v -> Option.map (fun v -> Cse.Symbexp.Val v) (coerce_val v)
    | LVar x -> Some (Cse.Symbexp.LVar x)
    | ALoc l -> Some (Cse.Symbexp.LVar l)
    | UnOp (op, e) -> (
        match op with
        | FUnaryMinus ->
            Option.map
              (fun e ->
                Cse.Symbexp.Binop
                  (Cse.Symbexp.Val (Cse.Val.Rat 0.), Cse.Binop.RSub, e))
              (coerce_symbexp e)
        | IUnaryMinus ->
            Option.map
              (fun e ->
                Cse.Symbexp.Binop
                  (Cse.Symbexp.Val (Cse.Val.Nat 0), Cse.Binop.Sub, e))
              (coerce_symbexp e)
        | _ -> (
            match (coerce_unop op, coerce_symbexp e) with
            | Some op, Some e -> Some (Cse.Symbexp.Unop (op, e))
            | _ -> None))
    | BinOp (UnOp (TypeOf, e1), Equal, Lit (Type t)) -> (
        match (coerce_symbexp e1, coerce_type t) with
        | Some e1', Some t' -> Some (Cse.Symbexp.In (e1', t'))
        | _ -> None)
    | BinOp (e1, op, e2) -> (
        match (coerce_symbexp e1, coerce_binop op, coerce_symbexp e2) with
        | Some e1, Some op, Some e2 -> Some (Cse.Symbexp.Binop (e1, op, e2))
        | Some e1', None, Some e2' -> (
            match op with
            | ILessThanEqual ->
                coerce_symbexp
                  (Expr.BinOp
                     ( Expr.BinOp (e1, BinOp.ILessThan, e2),
                       BinOp.Or,
                       Expr.BinOp (e1, BinOp.Equal, e2) ))
            | Or ->
                Some
                  (Cse.Symbexp.Unop
                     ( Cse.Unop.Not,
                       Cse.Symbexp.Binop
                         ( Cse.Symbexp.Unop (Cse.Unop.Not, e1'),
                           Cse.Binop.And,
                           Cse.Symbexp.Unop (Cse.Unop.Not, e2') ) ))
            | Impl -> (
                let lowered =
                  Expr.BinOp (Expr.UnOp (UnOp.Not, e1), BinOp.Or, e2)
                in
                match coerce_symbexp lowered with
                | Some e -> Some e
                | None -> None)
            | _ -> None)
        | _ -> None)
    | EList es ->
        let es =
          List.fold_right
            (fun e acc ->
              match (coerce_symbexp e, acc) with
              | Some e, Some es -> Some (e :: es)
              | _ -> None)
            es (Some [])
        in
        Option.map (fun es -> Cse.Symbexp.List es) es
    | NOp (LstCat, es) -> (
        (* CSE's concatenation is binary; fold the n-ary GIL one into it.
           The degenerate arities do not appear in the corpus but are still
           the identity and the singleton. *)
        match es with
        | [] -> Some (Cse.Symbexp.List [])
        | e :: es ->
            List.fold_left
              (fun acc e ->
                match (acc, coerce_symbexp e) with
                | Some acc, Some e ->
                    Some (Cse.Symbexp.Binop (acc, Cse.Binop.Cat, e))
                | _ -> None)
              (coerce_symbexp e) es)
    | _ -> None

  let rec diagnose_symbexp (e : Expr.t) =
    let diagnose_child context child =
      Option.map
        (fun reason -> Fmt.str "%s: %s" context reason)
        (diagnose_symbexp child)
    in
    let diagnose_two left right =
      match (diagnose_symbexp left, diagnose_symbexp right) with
      | Some reason, _ -> Some (Fmt.str "left operand failed: %s" reason)
      | None, Some reason -> Some (Fmt.str "right operand failed: %s" reason)
      | None, None -> None
    in
    match e with
    | Lit v -> diagnose_val v
    | LVar _ | ALoc _ -> None
    | PVar x -> Some (Fmt.str "unsupported program variable %s" x)
    | UnOp (op, e) -> (
        match op with
        | FUnaryMinus | IUnaryMinus -> diagnose_child "unary operand failed" e
        | _ -> (
            match diagnose_unop op with
            | Some reason -> Some reason
            | None -> diagnose_child "unary operand failed" e))
    | BinOp (UnOp (TypeOf, e1), Equal, Lit (Type t)) -> (
        match diagnose_symbexp e1 with
        | Some reason -> Some (Fmt.str "TypeOf operand failed: %s" reason)
        | None -> diagnose_type t)
    | BinOp (e1, op, e2) -> (
        match diagnose_two e1 e2 with
        | Some reason -> Some reason
        | None -> (
            match coerce_binop op with
            | Some _ -> None
            | None -> (
                match op with
                | ILessThanEqual ->
                    diagnose_symbexp
                      (Expr.BinOp
                         ( Expr.BinOp (e1, BinOp.ILessThan, e2),
                           BinOp.Or,
                           Expr.BinOp (e1, BinOp.Equal, e2) ))
                | Or ->
                    (* Or is supported by lowering through Not/And once both
                       operands are supported. *)
                    None
                | Impl ->
                    diagnose_symbexp
                      (Expr.BinOp (Expr.UnOp (UnOp.Not, e1), BinOp.Or, e2))
                | _ -> diagnose_binop op)))
    | EList es ->
        List.mapi (fun i e -> (i, diagnose_symbexp e)) es
        |> List.find_map (function
             | i, Some reason ->
                 Some (Fmt.str "list expression element %d failed: %s" i reason)
             | _, None -> None)
    | ESet _ -> Some "unsupported set expression"
    | LstSub _ -> Some "unsupported list-sub expression"
    | NOp (LstCat, es) ->
        List.mapi (fun i e -> (i, diagnose_symbexp e)) es
        |> List.find_map (function
             | i, Some reason ->
                 Some
                   (Fmt.str "list concatenation operand %d failed: %s" i reason)
             | _, None -> None)
    | NOp (op, _) ->
        Some
          (Fmt.str
             "unsupported n-ary expression %s: certified coercion currently \
              handles only EList list syntax and LstCat"
             (NOp.str op))
    | Exists _ -> Some "unsupported existential quantifier"
    | ForAll _ -> Some "unsupported universal quantifier"
end

module Smt = Cse.Smt.Make (C)
