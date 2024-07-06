open Arith_utils
module CStore = Store.Make (CVal.M)

(* Expression Evaluation *)

exception TypeError of string
exception EvaluationError of string

let unary_int_thing (lit : CVal.M.t) (f : Z.t -> Z.t) emsg : CVal.M.t =
  let num =
    match lit with
    | Int n -> n
    | _ -> raise (TypeError (Fmt.str "%s %a" emsg CVal.M.pp lit))
  in
  let res = f num in
  Int res

let unary_num_thing (lit : CVal.M.t) (f : float -> float) emsg : CVal.M.t =
  let num =
    match lit with
    | Num n -> n
    | _ -> raise (TypeError (Fmt.str "%s %a" emsg CVal.M.pp lit))
  in
  let res = f num in
  Num res

let evaluate_unop (op : UnOp.t) (lit : CVal.M.t) : CVal.M.t =
  match op with
  | UNot -> (
      match (lit : CVal.M.t) with
      | Bool b -> Bool (not b)
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: Negation: expected boolean, got %a"
                  CVal.M.pp lit)))
  | IUnaryMinus ->
      unary_int_thing lit
        (fun x -> Z.neg x)
        "Type Error: Integer unary minus: expected integer, got "
  | FUnaryMinus ->
      unary_num_thing lit
        (fun x -> -.x)
        "Type Error: Float unary minus: expected float, got "
  | BitwiseNot ->
      unary_num_thing lit int32_bitwise_not
        "Type Error: Bitwise not: expected number, got"
  | M_abs ->
      unary_num_thing lit abs_float
        "Type Error: Absolute value: expected number, got"
  | M_acos ->
      unary_num_thing lit acos "Type Error: Arc cosine: expected number, got"
  | M_asin ->
      unary_num_thing lit asin "Type Error: Arc sine: expected number, got"
  | M_atan ->
      unary_num_thing lit atan "Type Error: Arc tangent: expected number, got"
  | M_ceil ->
      unary_num_thing lit ceil "Type Error: Ceiling: expected number, got"
  | M_cos -> unary_num_thing lit cos "Type Error: Cosine: expected number, got"
  | M_exp ->
      unary_num_thing lit exp "Type Error: Exponentiation: expected number, got"
  | M_floor ->
      unary_num_thing lit floor "Type Error: Floor: expected number, got"
  | M_log ->
      unary_num_thing lit log "Type Error: Unary minus: expected number, got"
  | M_round -> (
      match lit with
      | Num n -> (
          let sign = copysign 1.0 n in
          match sign < 0.0 && n >= -0.5 with
          | true -> Num (-0.0)
          | _ ->
              (* This complex rounding is needed for edge case in OCaml: 0.49999999999999994 *)
              let round_nearest_lb = -.(2. ** 52.) in
              let round_nearest_ub = 2. ** 52. in

              let round_nearest t =
                if t >= round_nearest_lb && t <= round_nearest_ub then
                  floor (t +. 0.49999999999999994)
                else t
              in
              Num (round_nearest n))
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: Round: expected number, got %a" CVal.M.pp
                  lit)))
  | M_sgn ->
      unary_num_thing lit
        (fun x -> copysign 1.0 x)
        "Type Error: Sign: expected number, got"
  | M_sin -> unary_num_thing lit sin "Type Error: Sine: expected number, got"
  | M_sqrt ->
      unary_num_thing lit sqrt "Type Error: Square root: expected number, got"
  | M_tan -> unary_num_thing lit tan "Type Error: Tangent: expected number, got"
  | ToStringOp -> (
      match lit with
      | Num n -> String (float_to_string_inner n)
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: Number to string: expected number, got %a"
                  CVal.M.pp lit)))
  | ToIntOp ->
      unary_num_thing lit to_int
        "Type Error: Number to integer: expected number, got"
  | ToUint16Op ->
      unary_num_thing lit to_uint16
        "Type Error: Number to unsigned 16-bit integer: expected number, got"
  | ToInt32Op ->
      unary_num_thing lit to_int32
        "Type Error: Number to 32-bit integer: expected number, got"
  | ToUint32Op ->
      unary_num_thing lit to_uint32
        "Type Error: Number to unsigned 32-bit integer: expected number, got"
  | ToNumberOp -> (
      match lit with
      | String s ->
          if s = "" then Num 0.
          else
            let num = try Float.of_string s with Failure _ -> nan in
            Num num
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: ToNumber: expected string, got %a"
                  CVal.M.pp lit)))
  | IntToNum -> (
      match lit with
      | Int x -> Num (Z.to_float x)
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: IntToNum: expected integer, got %a"
                  CVal.M.pp lit)))
  | NumToInt -> (
      match lit with
      | Num x -> Int (Z.of_float x)
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: NumToInt: expected number, got %a"
                  CVal.M.pp lit)))
  | TypeOf -> Type (Literal.type_of lit)
  | Car -> (
      match lit with
      | LList ll -> (
          match ll with
          | [] ->
              raise
                (EvaluationError "Evaluation Error: List head of empty list")
          | lit :: _ -> lit)
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: List head: expected list, got %a" CVal.M.pp
                  lit)))
  | Cdr -> (
      match lit with
      | LList ll -> (
          match ll with
          | [] ->
              raise
                (EvaluationError "Evaluation Error: List tail of empty list")
          | _ :: ll -> LList ll)
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: List tail: expected list, got %a" CVal.M.pp
                  lit)))
  | LstLen -> (
      match lit with
      | LList l -> Int (Z.of_int (List.length l))
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: List length: expected list, got: %a"
                  CVal.M.pp lit)))
  | LstRev -> (
      match lit with
      | LList l -> LList (List.rev l)
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: List reverse: expected list, got: %a"
                  CVal.M.pp lit)))
  | StrLen -> (
      match lit with
      | String s -> Num (float_of_int (String.length s))
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: String length: expected string, got: %a"
                  CVal.M.pp lit)))
  | M_isNaN -> (
      match lit with
      | Num x when x == nan -> Bool true
      | Num _ -> Bool false
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: M_isNan: expected number, got: %a"
                  CVal.M.pp lit)))
  | SetToList ->
      raise (Exceptions.Unsupported "eval_unop concrete: set-to-list")

let binary_num_thing
    (lit1 : CVal.M.t)
    (lit2 : CVal.M.t)
    (f : float -> float -> float)
    emsg : CVal.M.t =
  let num1, num2 =
    match (lit1, lit2) with
    | Num n1, Num n2 -> (n1, n2)
    | _ ->
        raise
          (TypeError (Fmt.str "%s %a and %a" emsg CVal.M.pp lit1 CVal.M.pp lit2))
  in
  Num (f num1 num2)

let binary_int_thing
    (lit1 : CVal.M.t)
    (lit2 : CVal.M.t)
    (f : Z.t -> Z.t -> Z.t)
    emsg : CVal.M.t =
  let num1, num2 =
    match (lit1, lit2) with
    | Int n1, Int n2 -> (n1, n2)
    | _ ->
        raise
          (TypeError (Fmt.str "%s %a and %a" emsg CVal.M.pp lit1 CVal.M.pp lit2))
  in
  Int (f num1 num2)

let binary_int_bool_thing
    (lit1 : CVal.M.t)
    (lit2 : CVal.M.t)
    (f : Z.t -> Z.t -> bool)
    emsg : CVal.M.t =
  let num1, num2 =
    match (lit1, lit2) with
    | Int n1, Int n2 -> (n1, n2)
    | _ ->
        raise
          (TypeError (Fmt.str "%s %a and %a" emsg CVal.M.pp lit1 CVal.M.pp lit2))
  in
  Bool (f num1 num2)

let binary_num_bool_thing
    (lit1 : CVal.M.t)
    (lit2 : CVal.M.t)
    (f : float -> float -> bool)
    emsg : CVal.M.t =
  let num1, num2 =
    match (lit1, lit2) with
    | Num n1, Num n2 -> (n1, n2)
    | _ ->
        raise
          (TypeError (Fmt.str "%s %a and %a" emsg CVal.M.pp lit1 CVal.M.pp lit2))
  in
  Bool (f num1 num2)

let rec evaluate_binop
    (store : CStore.t)
    (op : BinOp.t)
    (e1 : Expr.t)
    (e2 : Expr.t) : CVal.M.t =
  let ee = evaluate_expr store in
  let lit1 = ee e1 in
  match op with
  | BImpl -> ee (BinOp (UnOp (UNot, Expr.Lit lit1), BOr, e2))
  | BAnd -> (
      match lit1 with
      | Bool false -> Bool false
      | Bool true -> (
          match ee e2 with
          | Bool b2 -> Bool b2
          | lit2 ->
              raise
                (TypeError
                   (Fmt.str "Type Error: Conjunction: expected boolean, got: %a"
                      CVal.M.pp lit2)))
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: Conjunction: expected boolean, got: %a"
                  CVal.M.pp lit1)))
  | BOr -> (
      match lit1 with
      | Bool true -> Bool true
      | Bool false -> (
          let lit2 = ee e2 in
          match lit2 with
          | Bool b2 -> Bool b2
          | _ ->
              raise
                (TypeError
                   (Fmt.str "Type Error: Disjunction: expected boolean, got: %a"
                      CVal.M.pp lit2)))
      | _ ->
          raise
            (TypeError
               (Fmt.str "Type Error: Disjunction: expected boolean, got: %a"
                  CVal.M.pp lit1)))
  | _ -> (
      let lit2 = ee e2 in
      match op with
      | SetDiff | BSetMem | BSetSub ->
          raise (Exceptions.Unsupported "eval_binop concrete: set operator")
      | BOr | BAnd | BImpl ->
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
          match (lit1, lit2) with
          | LList list, Int n -> List.nth list (Z.to_int n)
          | LList list, Num n when is_int n -> List.nth list (int_of_float n)
          | LList list, Num -0. -> List.nth list 0
          | _, _ ->
              raise
                (TypeError
                   (Fmt.str
                      "Type Error: List indexing: expected list and number, \
                       got %a and %a"
                      CVal.M.pp lit1 CVal.M.pp lit2)))
      | LstRepeat -> (
          match lit2 with
          | Int n ->
              let n = Z.to_int n in
              let elements = List.init n (fun _ -> lit1) in
              LList elements
          | _ ->
              raise
                (TypeError
                   (Fmt.str "Type Error: List repeat: expected integer, got %a"
                      CVal.M.pp lit2)))
      | StrNth -> (
          match (lit1, lit2) with
          | String s, Num n when is_int n ->
              String (String.make 1 s.[int_of_float n])
          | String s, Num -0. -> String (String.make 1 s.[0])
          | _, _ ->
              raise
                (TypeError
                   (Fmt.str
                      "Type Error: List indexing: expected string and number, \
                       got %a and %a"
                      CVal.M.pp lit1 CVal.M.pp lit2)))
      | ILessThan ->
          binary_int_bool_thing lit1 lit2
            (fun x y -> x < y)
            "Type Error: Less than: expected integers, got "
      | FLessThan ->
          binary_num_bool_thing lit1 lit2
            (fun x y -> x < y)
            "Type Error: Less than: expected numbers, got "
      | SLessThan -> (
          match (lit1, lit2) with
          | String s1, String s2 -> Bool (s1 < s2)
          | _, _ -> raise (Failure "Non-string arguments to LessThanString"))
      | ILessThanEqual ->
          binary_int_bool_thing lit1 lit2
            (fun x y -> x <= y)
            "Type Error: Less than or equal: expected integers, got "
      | FLessThanEqual ->
          binary_num_bool_thing lit1 lit2
            (fun x y -> x <= y)
            "Type Error: Less than or equal: expected numbers, got "
      | IPlus ->
          binary_int_thing lit1 lit2 Z.add
            "Type Error: Integer Addition: expected integers, got "
      | IMinus ->
          binary_int_thing lit1 lit2 Z.sub
            "Type Error: Subtraction: expected integers, got "
      | ITimes ->
          binary_int_thing lit1 lit2 Z.mul
            "Type Error: Multiplication: expected integers, got "
      | IDiv ->
          binary_int_thing lit1 lit2 Z.div
            "Type Error: Division: expected integers, got "
      | IMod ->
          binary_int_thing lit1 lit2 Z.( mod )
            "Type Error: IModulus: expected ints, got "
      | FPlus ->
          binary_num_thing lit1 lit2
            (fun x y -> x +. y)
            "Type Error: Addition: expected numbers, got "
      | FMinus ->
          binary_num_thing lit1 lit2
            (fun x y -> x -. y)
            "Type Error: Subtraction: expected numbers, got "
      | FTimes ->
          binary_num_thing lit1 lit2
            (fun x y -> x *. y)
            "Type Error: Multiplication: expected numbers, got "
      | FDiv ->
          binary_num_thing lit1 lit2
            (fun x y -> x /. y)
            "Type Error: Division: expected numbers, got "
      | FMod ->
          binary_num_thing lit1 lit2 mod_float
            "Type Error: FModulus: expected numbers, got "
      | BitwiseAnd ->
          binary_int_thing lit1 lit2 Z.logand
            "Type Error: Bitwise conjunction: expected numbers, got "
      | BitwiseOr ->
          binary_int_thing lit1 lit2 Z.logor
            "Type Error: Bitwise disjunction: expected numbers, got "
      | BitwiseXor ->
          binary_int_thing lit1 lit2 Z.logxor
            "Type Error: Bitwise exclusive disjunction: expected numbers, got "
      | LeftShift ->
          binary_int_thing lit1 lit2
            (fun x y -> Z.shift_left x (Z.to_int y))
            "Type Error: Left shift: expected numbers, got "
      | SignedRightShift ->
          binary_int_thing lit1 lit2
            (fun x y -> Z.shift_right x (Z.to_int y))
            "Type Error: Signed right shift: expected numbers, got "
      | UnsignedRightShift ->
          binary_int_thing lit1 lit2
            (fun x y -> Z.shift_right x (Z.to_int y))
            "Type Error: Unsigned right shift: expected integers, got "
      | BitwiseAndL ->
          binary_int_thing lit1 lit2 int64_bitwise_and
            "Type Error: Bitwise 64bit conjunction: expected integers, got "
      | BitwiseOrL ->
          binary_int_thing lit1 lit2 int64_bitwise_or
            "Type Error: Bitwise 64bit disjunction: expected integers, got "
      | BitwiseXorL ->
          binary_int_thing lit1 lit2 int64_bitwise_xor
            "Type Error: Bitwise 64bit exclusive disjunction: expected \
             numbers, got "
      | LeftShiftL ->
          binary_int_thing lit1 lit2 int64_left_shift
            "Type Error: 64bit Left shift: expected integers, got "
      | SignedRightShiftL ->
          binary_int_thing lit1 lit2
            (fun x y -> Z.shift_right x (Z.to_int y))
            "Type Error: 64bit Signed right shift: expected numbers, got "
      | UnsignedRightShiftL ->
          binary_int_thing lit1 lit2
            (fun x y -> Z.shift_right x (Z.to_int y))
            "Type Error: 64bit Unsigned right shift: expected integers, got "
      | BitwiseAndF ->
          binary_num_thing lit1 lit2 int32_bitwise_and
            "Type Error: Bitwise float conjunction: expected floats, got "
      | BitwiseOrF ->
          binary_num_thing lit1 lit2 int32_bitwise_or
            "Type Error: Bitwise float disjunction: expected floats, got "
      | BitwiseXorF ->
          binary_num_thing lit1 lit2 int32_bitwise_xor
            "Type Error: Bitwise float exclusive disjunction: expected floats, \
             got "
      | LeftShiftF ->
          binary_num_thing lit1 lit2 int32_left_shift
            "Type Error: Float Left shift: expected floats, got "
      | SignedRightShiftF ->
          binary_num_thing lit1 lit2 int32_right_shift
            "Type Error: Float Signed right shift: expected floats, got "
      | UnsignedRightShiftF ->
          binary_num_thing lit1 lit2 uint32_right_shift_f
            "Type Error: Float Unsigned right shift: expected floats, got "
      | M_atan2 ->
          binary_num_thing lit1 lit2 atan2
            "Type Error: Arc tangent: expected numbers, got "
      | M_pow ->
          binary_num_thing lit1 lit2
            (fun x y -> x ** y)
            "Type Error: Exponentiation: expected numbers, got "
      | StrCat -> (
          match (lit1, lit2) with
          | String s1, String s2 -> String (s1 ^ s2)
          | _, _ ->
              raise
                (Failure
                   (Fmt.str
                      "Type Error: List concatenation: expected lists, got %a \
                       and %a"
                      CVal.M.pp lit1 CVal.M.pp lit2))))

and evaluate_nop (nop : NOp.t) (ll : Literal.t list) : CVal.M.t =
  match nop with
  | LstCat ->
      LList
        (List.fold_left
           (fun ac (l : Literal.t) ->
             match l with
             | LList l -> List.append ac l
             | _ ->
                 raise (Failure "List concat: supplied expression not a list."))
           [] ll)
  | _ -> raise (Exceptions.Unsupported "Concrete evaluate_nop: set operators")

and evaluate_elist store (ll : Expr.t list) : CVal.M.t =
  match ll with
  | [] -> LList []
  | e :: ll -> (
      let ve = evaluate_expr store e in
      let vll = evaluate_expr store (EList ll) in
      match vll with
      | LList vll -> LList (ve :: vll)
      | _ ->
          raise
            (Exceptions.Impossible
               "eval_expr concrete: list reduces to non-list"))

and evaluate_lstsub (store : CStore.t) (e1 : Expr.t) (e2 : Expr.t) (e3 : Expr.t)
    : CVal.M.t =
  let ee = evaluate_expr store in
  let ve1 = ee e1 in
  let ve2 = ee e2 in
  let ve3 = ee e3 in
  match (ve1, ve2, ve3) with
  | LList les, Int start, Int len ->
      let sub_list =
        List_utils.list_sub les (Z.to_int start) (Z.to_int len) |> Option.get
      in
      LList sub_list
  | _ ->
      raise (Exceptions.Impossible "eval_expr concrete: lstsub type mismatch")

and evaluate_expr (store : CStore.t) (e : Expr.t) : CVal.M.t =
  try
    let ee = evaluate_expr store in
    match e with
    | Lit l -> (
        match l with
        | Constant c -> Literal.evaluate_constant c
        | x -> x)
    | PVar x -> (
        match CStore.get store x with
        | None ->
            let err_msg = Fmt.str "Variable %s not found in the store" x in
            (* if (!verbose) then Fmt.printf "The current store is: \n%s" CStore.pp store; *)
            raise (Failure err_msg)
        | Some v -> v)
    | BinOp (e1, bop, e2) -> evaluate_binop store bop e1 e2
    | UnOp (unop, e) -> evaluate_unop unop (ee e)
    | NOp (nop, le) -> evaluate_nop nop (List.map ee le)
    | EList ll -> evaluate_elist store ll
    | LstSub (e1, e2, e3) -> evaluate_lstsub store e1 e2 e3
    | ALoc _ | LVar _ | ESet _ | Exists _ | EForall _ ->
        raise
          (Exceptions.Impossible "eval_expr concrete: aloc, lvar, set or exists")
  with
  | TypeError msg -> raise (TypeError msg)
  | EvaluationError msg -> raise (EvaluationError msg)
  | Division_by_zero -> raise (EvaluationError "Division by zero")
  | e ->
      let msg = Printexc.to_string e in
      let stack = Printexc.get_backtrace () in
      failwith
        (Fmt.str "Expression evaluation: Untreatable Exception: %s%s\n" msg
           stack)
