open Gil_syntax
open Utils

(* open Names *)
module L = Logging
module Arithmetic = Z3.Arithmetic
module Boolean = Z3.Boolean
module Datatype = Z3.Datatype
module Enumeration = Z3.Enumeration
module FloatingPoint = Z3.FloatingPoint
module FuncDecl = Z3.FuncDecl
module Model = Z3.Model
module Quantifier = Z3.Quantifier
module Set = Z3.Set
module Solver = Z3.Solver
module Symbol = Z3.Symbol
module ZExpr = Z3.Expr
module Z3Num = Z3.FloatingPoint

exception Z3Unknown

(* Note: I could probably have some static check instead of dynamic check
   using GADTs that my z3 exprs are correctly typed. *)

(* [@@@ocaml.warning "-A"] *)

type tyenv = (string, Type.t) Hashtbl.t

let pp_tyenv =
  let open Fmt in
  Dump.hashtbl string (Fmt.of_to_string Type.str)

let encoding_cache : (Formula.Set.t, ZExpr.expr list) Hashtbl.t =
  Hashtbl.create Config.big_tbl_size

let sat_cache : (Formula.Set.t, bool) Hashtbl.t =
  Hashtbl.create Config.big_tbl_size

let cfg =
  [
    ("model", "true");
    ("proof", "false");
    ("unsat_core", "false");
    ("auto_config", "true");
    ("timeout", "30000");
  ]

let ctx : Z3.context = Z3.mk_context cfg
let ( <| ) constr e = ZExpr.mk_app ctx constr [ e ]
let ( $$ ) const l = ZExpr.mk_app ctx const l
let booleans_sort = Boolean.mk_sort ctx
let ints_sort = Arithmetic.Integer.mk_sort ctx
let reals_sort = Z3Num.mk_sort_double ctx
let numbers_sort = reals_sort
let mk_string_symb s = Symbol.mk_string ctx s
let mk_int_i = Arithmetic.Integer.mk_numeral_i ctx
let mk_int_s = Arithmetic.Integer.mk_numeral_s ctx

(* let mk_num_s s = Z3Num.mk_numeral_s ctx s reals_sort *)
let mk_lt = Arithmetic.mk_lt ctx
let mk_le = Arithmetic.mk_le ctx
let mk_add e1 e2 = Arithmetic.mk_add ctx [ e1; e2 ]
let mk_sub e1 e2 = Arithmetic.mk_sub ctx [ e1; e2 ]
let mk_mul e1 e2 = Arithmetic.mk_mul ctx [ e1; e2 ]
let mk_div e1 e2 = Arithmetic.mk_div ctx e1 e2
let mk_mod = Arithmetic.Integer.mk_mod ctx
let mk_or e1 e2 = Boolean.mk_or ctx [ e1; e2 ]
let mk_and e1 e2 = Boolean.mk_and ctx [ e1; e2 ]
let mk_eq = Boolean.mk_eq ctx

let z3_gil_type_sort =
  Enumeration.mk_sort ctx
    (mk_string_symb "GIL_Type")
    (List.map mk_string_symb
       [
         "UndefinedType";
         "NullType";
         "EmptyType";
         "NoneType";
         "BooleanType";
         "IntType";
         "NumberType";
         "StringType";
         "ObjectType";
         "ListType";
         "TypeType";
         "SetType";
       ])

module Type_operations = struct
  let z3_gil_type_constructors = Datatype.get_constructors z3_gil_type_sort
  let undefined_type_constructor = List.nth z3_gil_type_constructors 0
  let null_type_constructor = List.nth z3_gil_type_constructors 1
  let empty_type_constructor = List.nth z3_gil_type_constructors 2
  let none_type_constructor = List.nth z3_gil_type_constructors 3
  let boolean_type_constructor = List.nth z3_gil_type_constructors 4
  let int_type_constructor = List.nth z3_gil_type_constructors 5
  let number_type_constructor = List.nth z3_gil_type_constructors 6
  let string_type_constructor = List.nth z3_gil_type_constructors 7
  let object_type_constructor = List.nth z3_gil_type_constructors 8
  let list_type_constructor = List.nth z3_gil_type_constructors 9
  let type_type_constructor = List.nth z3_gil_type_constructors 10
  let set_type_constructor = List.nth z3_gil_type_constructors 11
end

module Lit_operations = struct
  let literal_symbol = mk_string_symb "GIL_Literal"
  let literal_sort_ref = Datatype.mk_sort_ref ctx literal_symbol

  let gil_undefined_constructor =
    Datatype.mk_constructor ctx
      (mk_string_symb "Undefined")
      (mk_string_symb "isUndefined")
      [] [] []

  let gil_null_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Null")
      (mk_string_symb "isNull") [] [] []

  let gil_empty_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Empty")
      (mk_string_symb "isEmpty") [] [] []

  let gil_bool_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Bool")
      (mk_string_symb "isBool")
      [ mk_string_symb "bValue" ]
      [ Some booleans_sort ] [ 0 ]

  let gil_int_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Int") (mk_string_symb "isInt")
      [ mk_string_symb "iValue" ]
      [ Some ints_sort ] [ 0 ]

  let gil_num_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Num") (mk_string_symb "isNum")
      [ mk_string_symb "nValue" ]
      [ Some numbers_sort ] [ 0 ]

  let gil_string_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "String")
      (mk_string_symb "isString")
      [ mk_string_symb "sValue" ]
      [ Some ints_sort ] [ 0 ]

  let gil_loc_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Loc") (mk_string_symb "isLoc")
      [ mk_string_symb "locValue" ]
      [ Some ints_sort ] [ 0 ]

  let gil_type_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Type")
      (mk_string_symb "isType")
      [ mk_string_symb "tValue" ]
      [ Some z3_gil_type_sort ] [ 0 ]

  let gil_list_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "List")
      (mk_string_symb "isList")
      [ mk_string_symb "listValue" ]
      [ Some (Z3.Seq.mk_seq_sort ctx literal_sort_ref) ]
      [ 0 ]

  let gil_none_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "None")
      (mk_string_symb "isNone") [] [] []

  let z3_gil_literal_sort =
    Datatype.mk_sort ctx literal_symbol
      [
        gil_undefined_constructor;
        gil_null_constructor;
        gil_empty_constructor;
        gil_bool_constructor;
        gil_int_constructor;
        gil_num_constructor;
        gil_string_constructor;
        gil_loc_constructor;
        gil_type_constructor;
        gil_list_constructor;
        gil_none_constructor;
      ]

  let z3_literal_constructors = Datatype.get_constructors z3_gil_literal_sort
  let undefined_constructor = List.nth z3_literal_constructors 0
  let null_constructor = List.nth z3_literal_constructors 1
  let empty_constructor = List.nth z3_literal_constructors 2
  let boolean_constructor = List.nth z3_literal_constructors 3
  let int_constructor = List.nth z3_literal_constructors 4
  let number_constructor = List.nth z3_literal_constructors 5
  let string_constructor = List.nth z3_literal_constructors 6
  let loc_constructor = List.nth z3_literal_constructors 7
  let type_constructor = List.nth z3_literal_constructors 8
  let list_constructor = List.nth z3_literal_constructors 9
  let none_constructor = List.nth z3_literal_constructors 10
  let gil_literal_accessors = Datatype.get_accessors z3_gil_literal_sort
  let boolean_accessor = List.nth (List.nth gil_literal_accessors 3) 0
  let int_accessor = List.nth (List.nth gil_literal_accessors 4) 0
  let number_accessor = List.nth (List.nth gil_literal_accessors 5) 0
  let string_accessor = List.nth (List.nth gil_literal_accessors 6) 0

  (* let loc_accessor = List.nth (List.nth gil_literal_accessors 7) 0 *)
  (* let type_accessor = List.nth (List.nth gil_literal_accessors 8) 0 *)
  let list_accessor = List.nth (List.nth gil_literal_accessors 9) 0
  let gil_literal_recognizers = Datatype.get_recognizers z3_gil_literal_sort
  let undefined_recognizer = List.nth gil_literal_recognizers 0
  let null_recognizer = List.nth gil_literal_recognizers 1
  let empty_recognizer = List.nth gil_literal_recognizers 2
  let boolean_recognizer = List.nth gil_literal_recognizers 3
  let int_recognizer = List.nth gil_literal_recognizers 4
  let number_recognizer = List.nth gil_literal_recognizers 5
  let string_recognizer = List.nth gil_literal_recognizers 6
  let loc_recognizer = List.nth gil_literal_recognizers 7
  let type_recognizer = List.nth gil_literal_recognizers 8
  let list_recognizer = List.nth gil_literal_recognizers 9
  let none_recognizer = List.nth gil_literal_recognizers 10
  let z3_list_of_literal_sort = Z3.Seq.mk_seq_sort ctx z3_gil_literal_sort
end

let z3_gil_literal_sort = Lit_operations.z3_gil_literal_sort
let z3_gil_set_sort = Set.mk_sort ctx z3_gil_literal_sort

module Extended_literal_operations = struct
  let gil_sing_elem_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Elem")
      (mk_string_symb "isSingular")
      [ mk_string_symb "singElem" ]
      [ Some z3_gil_literal_sort ]
      [ 0 ]

  let gil_set_elem_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Set") (mk_string_symb "isSet")
      [ mk_string_symb "setElem" ]
      [ Some z3_gil_set_sort ] [ 0 ]

  let extended_literal_sort =
    Datatype.mk_sort ctx
      (mk_string_symb "Extended_GIL_Literal")
      [ gil_sing_elem_constructor; gil_set_elem_constructor ]

  let gil_extended_literal_constructors =
    Datatype.get_constructors extended_literal_sort

  let singular_constructor = List.nth gil_extended_literal_constructors 0
  let set_constructor = List.nth gil_extended_literal_constructors 1

  let gil_extended_literal_accessors =
    Datatype.get_accessors extended_literal_sort

  let singular_elem_accessor =
    List.nth (List.nth gil_extended_literal_accessors 0) 0

  let set_accessor = List.nth (List.nth gil_extended_literal_accessors 1) 0

  let gil_extended_literal_recognizers =
    Datatype.get_recognizers extended_literal_sort

  (* let singular_elem_recognizer = List.nth gil_extended_literal_recognizers 0 *)
  let set_recognizer = List.nth gil_extended_literal_recognizers 1
end

let extended_literal_sort = Extended_literal_operations.extended_literal_sort

module Axiomatised_operations = struct
  let slen_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "s-len") [ ints_sort ]
      numbers_sort

  let llen_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-len")
      [ Lit_operations.z3_list_of_literal_sort ]
      ints_sort

  let num2str_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "num2str") [ numbers_sort ]
      ints_sort

  let str2num_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "str2num") [ ints_sort ]
      numbers_sort

  let num2int_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "num2int") [ numbers_sort ]
      numbers_sort

  let snth_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "s-nth")
      [ ints_sort; numbers_sort ]
      ints_sort

  let lrev_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-rev")
      [ Lit_operations.z3_list_of_literal_sort ]
      Lit_operations.z3_list_of_literal_sort
end

let mk_z3_set les =
  let empty_set = Set.mk_empty ctx z3_gil_literal_sort in
  let rec loop les cur_set =
    match les with
    | [] -> cur_set
    | le :: rest_les ->
        let new_cur_set = Set.mk_set_add ctx cur_set le in
        loop rest_les new_cur_set
  in
  let result = loop les empty_set in
  result

let mk_z3_list les =
  match les with
  | [] -> Z3.Seq.mk_seq_empty ctx Lit_operations.z3_list_of_literal_sort
  | _ -> List.map (Z3.Seq.mk_seq_unit ctx) les |> Z3.Seq.mk_seq_concat ctx

let str_codes = Hashtbl.create 1000
let str_codes_inv = Hashtbl.create 1000
let str_counter = ref 0

let encode_string str =
  try
    let str_number = Hashtbl.find str_codes str in
    let z3_code = mk_int_i str_number in
    z3_code
  with Not_found ->
    (* New string: add it to the hashtable *)
    let z3_code = mk_int_i !str_counter in
    Hashtbl.add str_codes str !str_counter;
    Hashtbl.add str_codes_inv !str_counter str;
    str_counter := !str_counter + 1;
    z3_code

let encode_type (t : Type.t) =
  try
    match t with
    | UndefinedType ->
        ZExpr.mk_app ctx Type_operations.undefined_type_constructor []
    | NullType -> ZExpr.mk_app ctx Type_operations.null_type_constructor []
    | EmptyType -> ZExpr.mk_app ctx Type_operations.empty_type_constructor []
    | NoneType -> ZExpr.mk_app ctx Type_operations.none_type_constructor []
    | BooleanType ->
        ZExpr.mk_app ctx Type_operations.boolean_type_constructor []
    | IntType -> ZExpr.mk_app ctx Type_operations.int_type_constructor []
    | NumberType -> ZExpr.mk_app ctx Type_operations.number_type_constructor []
    | StringType -> ZExpr.mk_app ctx Type_operations.string_type_constructor []
    | ObjectType -> ZExpr.mk_app ctx Type_operations.object_type_constructor []
    | ListType -> ZExpr.mk_app ctx Type_operations.list_type_constructor []
    | TypeType -> ZExpr.mk_app ctx Type_operations.type_type_constructor []
    | SetType -> ZExpr.mk_app ctx Type_operations.set_type_constructor []
  with _ ->
    raise
      (Failure (Printf.sprintf "DEATH: encode_type with arg: %s" (Type.str t)))

module Encoding = struct
  type kind = Native of Type.t | Simple_wrapped | Extended_wrapped

  let native_sort_of_type = function
    | Type.IntType | StringType | ObjectType -> ints_sort
    | ListType -> Lit_operations.z3_list_of_literal_sort
    | BooleanType -> booleans_sort
    | NumberType -> reals_sort
    | UndefinedType | NoneType | EmptyType | NullType -> z3_gil_literal_sort
    | SetType -> z3_gil_set_sort
    | TypeType -> z3_gil_type_sort

  type t = { kind : kind; expr : ZExpr.expr }

  let undefined_encoding =
    { kind = Simple_wrapped; expr = Lit_operations.undefined_constructor $$ [] }

  let null_encoding =
    { kind = Simple_wrapped; expr = Lit_operations.null_constructor $$ [] }

  let empty_encoding =
    { kind = Simple_wrapped; expr = Lit_operations.empty_constructor $$ [] }

  let none_encoding =
    { kind = Simple_wrapped; expr = Lit_operations.none_constructor $$ [] }

  let native ~ty expr = { kind = Native ty; expr }

  let get_native ~accessor { expr; kind } =
    (* Not additional check is performed on native type,
       it should be already type checked *)
    match kind with
    | Native _ -> expr
    | Simple_wrapped -> accessor <| expr
    | Extended_wrapped ->
        accessor <| (Extended_literal_operations.singular_elem_accessor <| expr)

  let simply_wrapped expr = { kind = Simple_wrapped; expr }
  let extended_wrapped expr = { kind = Extended_wrapped; expr }

  (** Takes a value either natively encoded or simply wrapped
    and returns a value simply wrapped.
    Careful: do not use wrap with a a set, as they cannot be simply wrapped *)
  let simple_wrap ({ expr; kind } as e) =
    let open Lit_operations in
    match kind with
    | Simple_wrapped -> e.expr
    | Native ty -> (
        match ty with
        | IntType -> int_constructor <| expr
        | NumberType -> number_constructor <| expr
        | StringType -> string_constructor <| expr
        | ObjectType -> loc_constructor <| expr
        | TypeType -> type_constructor <| expr
        | BooleanType -> boolean_constructor <| expr
        | ListType -> list_constructor <| expr
        | UndefinedType | NullType | EmptyType | NoneType | SetType ->
            Fmt.failwith "Cannot simple-wrap value of type %s" (Type.str ty))
    | Extended_wrapped ->
        Extended_literal_operations.singular_elem_accessor <| expr

  let extend_wrap e =
    match e.kind with
    | Extended_wrapped -> e.expr
    | Native SetType ->
        Extended_literal_operations.set_constructor <| simple_wrap e
    | _ -> Extended_literal_operations.singular_constructor <| simple_wrap e

  let get_num = get_native ~accessor:Lit_operations.number_accessor
  let get_int = get_native ~accessor:Lit_operations.int_accessor
  let get_bool = get_native ~accessor:Lit_operations.boolean_accessor
  let ( >- ) expr ty = native ~ty expr
  let get_list = get_native ~accessor:Lit_operations.list_accessor

  let get_set { kind; expr } =
    match kind with
    | Native SetType -> expr
    | Extended_wrapped -> Extended_literal_operations.set_accessor <| expr
    | _ -> failwith "wrong encoding of set"

  let get_string = get_native ~accessor:Lit_operations.string_accessor
end

let placeholder_sw =
  ZExpr.mk_fresh_const ctx "placeholder" Lit_operations.z3_gil_literal_sort

let placeholder_ew =
  ZExpr.mk_fresh_const ctx "placeholder"
    Extended_literal_operations.extended_literal_sort

let else_branch_placeholder =
  ZExpr.mk_fresh_const ctx "placeholder" z3_gil_type_sort

let ready_to_subst_expr_for_simply_wrapped_typeof =
  let guards =
    [
      (Lit_operations.null_recognizer <| placeholder_sw, Type.NullType);
      (Lit_operations.empty_recognizer <| placeholder_sw, Type.EmptyType);
      (Lit_operations.boolean_recognizer <| placeholder_sw, Type.BooleanType);
      (Lit_operations.string_recognizer <| placeholder_sw, Type.StringType);
      (Lit_operations.type_recognizer <| placeholder_sw, Type.TypeType);
      (Lit_operations.none_recognizer <| placeholder_sw, Type.NoneType);
      (Lit_operations.undefined_recognizer <| placeholder_sw, Type.UndefinedType);
      (Lit_operations.loc_recognizer <| placeholder_sw, Type.ObjectType);
      (Lit_operations.list_recognizer <| placeholder_sw, Type.ListType);
      (Lit_operations.int_recognizer <| placeholder_sw, Type.IntType);
      (Lit_operations.number_recognizer <| placeholder_sw, Type.NumberType);
    ]
  in
  List.fold_left
    (fun acc (guard, ty) -> Boolean.mk_ite ctx guard (encode_type ty) acc)
    (encode_type UndefinedType)
    guards

let ready_to_subst_expr_for_extended_wrapped_typeof =
  let set_guard =
    Extended_literal_operations.set_recognizer <| placeholder_ew
  in
  Boolean.mk_ite ctx set_guard (encode_type SetType) else_branch_placeholder

let typeof_expression (x : Encoding.t) =
  match x.kind with
  | Native ty -> encode_type ty
  | Simple_wrapped ->
      ZExpr.substitute_one ready_to_subst_expr_for_simply_wrapped_typeof
        placeholder_sw x.expr
  | Extended_wrapped ->
      ZExpr.substitute ready_to_subst_expr_for_extended_wrapped_typeof
        [ placeholder_ew; else_branch_placeholder ]
        [
          x.expr;
          ZExpr.substitute_one ready_to_subst_expr_for_simply_wrapped_typeof
            placeholder_sw
            (Extended_literal_operations.singular_elem_accessor <| x.expr);
        ]

(* Return a native Z3 expr, or a simply_wrapped expr.
   The information is given by the type. *)
let rec encode_lit (lit : Literal.t) : Encoding.t =
  let open Encoding in
  try
    match lit with
    | Undefined -> undefined_encoding
    | Null -> null_encoding
    | Empty -> empty_encoding
    | Nono -> none_encoding
    | Bool b ->
        let b_arg =
          match b with
          | true -> Boolean.mk_true ctx
          | false -> Boolean.mk_false ctx
        in
        native ~ty:BooleanType b_arg
    | Int i ->
        let i_arg = mk_int_s (Z.to_string i) in
        native ~ty:IntType i_arg
    | Num n ->
        let res =
          if Float.is_infinite n then
            Z3Num.mk_inf ctx reals_sort (Float.sign_bit n)
          else if Float.is_nan n then Z3Num.mk_nan ctx reals_sort
          else Z3Num.mk_numeral_f ctx n reals_sort
        in
        native ~ty:NumberType res
    | String s ->
        let s_arg = encode_string s in
        native ~ty:StringType s_arg
    | Loc l ->
        let l_arg = encode_string l in
        native ~ty:ObjectType l_arg
    | Type t ->
        let t_arg = encode_type t in
        native ~ty:TypeType t_arg
    | LList lits ->
        let args = List.map (fun lit -> simple_wrap (encode_lit lit)) lits in
        mk_z3_list args >- ListType
    | Constant _ -> raise (Exceptions.Unsupported "Z3 encoding: constants")
  with Failure msg ->
    raise
      (Failure
         (Printf.sprintf "DEATH: encode_lit %s. %s"
            ((Fmt.to_to_string Literal.pp) lit)
            msg))

let encode_equality (p1 : Encoding.t) (p2 : Encoding.t) : Encoding.t =
  let open Encoding in
  let res =
    match (p1.kind, p2.kind) with
    | Native t1, Native t2 when Type.equal t1 t2 -> mk_eq p1.expr p2.expr
    | Simple_wrapped, Simple_wrapped | Extended_wrapped, Extended_wrapped ->
        mk_eq p1.expr p2.expr
    | Native _, Native _ -> failwith "incompatible equality!"
    | Simple_wrapped, Native _ | Native _, Simple_wrapped ->
        mk_eq (simple_wrap p1) (simple_wrap p2)
    | Extended_wrapped, _ | _, Extended_wrapped ->
        mk_eq (extend_wrap p1) (extend_wrap p2)
  in
  res >- BooleanType

(** Encode GIL binary operators *)
let encode_binop (op : BinOp.t) (p1 : Encoding.t) (p2 : Encoding.t) : Encoding.t
    =
  let open Encoding in
  (* In the case of strongly typed operations, we do not perform any check.
     Type checking has happened before reaching z3, and therefore, isn't required here again.
     An unknown type is represented by the [None] variant of the option type.
     It is expected that values of unknown type are already wrapped into their constructors.
  *)
  match op with
  | IPlus -> mk_add (get_int p1) (get_int p2) >- IntType
  | IMinus -> mk_sub (get_int p1) (get_int p2) >- IntType
  | ITimes -> mk_mul (get_int p1) (get_int p2) >- IntType
  | IDiv -> mk_div (get_int p1) (get_int p2) >- IntType
  | IMod -> mk_mod (get_int p1) (get_int p2) >- IntType
  | ILessThan -> mk_lt (get_int p1) (get_int p2) >- BooleanType
  | ILessThanEqual -> mk_le (get_int p1) (get_int p2) >- BooleanType
  | FPlus -> mk_add (get_num p1) (get_num p2) >- NumberType
  | FMinus -> mk_sub (get_num p1) (get_num p2) >- NumberType
  | FTimes -> mk_mul (get_num p1) (get_num p2) >- NumberType
  | FDiv -> mk_div (get_num p1) (get_num p2) >- NumberType
  | FLessThan -> mk_lt (get_num p1) (get_num p2) >- BooleanType
  | FLessThanEqual -> mk_le (get_num p1) (get_num p2) >- BooleanType
  | Equal -> encode_equality p1 p2
  | BOr -> mk_or (get_bool p1) (get_bool p2) >- BooleanType
  | BAnd -> mk_and (get_bool p1) (get_bool p2) >- BooleanType
  | BSetMem ->
      (* p2 has to be already wrapped *)
      Set.mk_membership ctx (simple_wrap p1) (get_set p2) >- BooleanType
  | SetDiff -> Set.mk_difference ctx (get_set p1) (get_set p2) >- SetType
  | BSetSub -> Set.mk_subset ctx (get_set p1) (get_set p2) >- BooleanType
  | LstNth ->
      let lst' = get_list p1 in
      let index' = get_int p2 in
      Z3.Seq.mk_seq_nth ctx lst' index' |> simply_wrapped
  | StrNth ->
      let str' = get_string p1 in
      let index' = get_num p2 in
      let res = Axiomatised_operations.snth_fun $$ [ str'; index' ] in
      res >- StringType
  | FMod
  | SLessThan
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LeftShift
  | SignedRightShift
  | UnsignedRightShift
  | BitwiseAndL
  | BitwiseOrL
  | BitwiseXorL
  | LeftShiftL
  | SignedRightShiftL
  | UnsignedRightShiftL
  | BitwiseAndF
  | BitwiseOrF
  | BitwiseXorF
  | LeftShiftF
  | SignedRightShiftF
  | UnsignedRightShiftF
  | M_atan2
  | M_pow
  | StrCat ->
      raise
        (Failure
           (Printf.sprintf
              "SMT encoding: Construct not supported yet - binop: %s"
              (BinOp.str op)))

let encode_unop ~llen_lvars ~e (op : UnOp.t) le =
  let open Encoding in
  match op with
  | IUnaryMinus -> Arithmetic.mk_unary_minus ctx (get_int le) >- IntType
  | FUnaryMinus -> Arithmetic.mk_unary_minus ctx (get_num le) >- NumberType
  | LstLen ->
      (* If we only use an LVar as an argument to llen, then encode it as an uninterpreted function. *)
      let enc =
        match e with
        | Expr.LVar l when SS.mem l llen_lvars ->
            Axiomatised_operations.llen_fun <| get_list le
        | _ -> Z3.Seq.mk_seq_length ctx (get_list le)
      in
      enc >- IntType
  | StrLen -> Axiomatised_operations.slen_fun <| get_string le >- NumberType
  | ToStringOp -> Axiomatised_operations.num2str_fun <| get_num le >- StringType
  | ToNumberOp ->
      Axiomatised_operations.str2num_fun <| get_string le >- NumberType
  | ToIntOp -> Axiomatised_operations.num2int_fun <| get_num le >- NumberType
  | UNot -> Boolean.mk_not ctx (get_bool le) >- BooleanType
  | Cdr ->
      let list = get_list le in
      Z3.Seq.mk_seq_extract ctx list (mk_int_i 1)
        (Z3.Seq.mk_seq_length ctx list)
      >- ListType
  | Car -> Z3.Seq.mk_seq_nth ctx (get_list le) (mk_int_i 0) |> simply_wrapped
  | TypeOf -> typeof_expression le >- TypeType
  | ToUint32Op ->
      let op_le_n =
        Arithmetic.Integer.mk_int2real ctx
          (Arithmetic.Real.mk_real2int ctx (get_num le))
      in
      op_le_n >- NumberType
  | LstRev ->
      let le_lst = get_list le in
      let n_le = Axiomatised_operations.lrev_fun <| le_lst in
      n_le >- ListType
  | NumToInt ->
      Arithmetic.Real.mk_real2int ctx (Z3Num.mk_to_real ctx (get_num le))
      >- IntType
  | IntToNum -> failwith "IntToNum not available"
  | BitwiseNot
  | M_isNaN
  | M_abs
  | M_acos
  | M_asin
  | M_atan
  | M_ceil
  | M_cos
  | M_exp
  | M_floor
  | M_log
  | M_round
  | M_sgn
  | M_sin
  | M_sqrt
  | M_tan
  | ToUint16Op
  | ToInt32Op
  | SetToList ->
      let msg =
        Printf.sprintf "SMT encoding: Construct not supported yet - unop - %s!"
          (UnOp.str op)
      in
      print_string msg;
      flush stdout;
      raise (Failure msg)

let rec encode_logical_expression
    ~(gamma : tyenv)
    ~(llen_lvars : SS.t)
    (le : Expr.t) : Encoding.t =
  let open Encoding in
  let f = encode_logical_expression ~gamma ~llen_lvars in

  match le with
  | Lit lit -> encode_lit lit
  | LVar var -> (
      match Hashtbl.find_opt gamma var with
      | None ->
          ZExpr.mk_const_s ctx var extended_literal_sort |> extended_wrapped
      | Some ty ->
          let sort = native_sort_of_type ty in
          ZExpr.mk_const_s ctx var sort >- ty)
  | ALoc var -> ZExpr.mk_const_s ctx var ints_sort >- ObjectType
  | PVar _ -> raise (Failure "Program variable in pure formula: FIRE")
  | UnOp (op, le) -> encode_unop ~llen_lvars ~e:le op (f le)
  | BinOp (le1, op, le2) -> encode_binop op (f le1) (f le2)
  | NOp (SetUnion, les) ->
      let les = List.map (fun le -> get_set (f le)) les in
      Set.mk_union ctx les >- SetType
  | NOp (SetInter, les) ->
      let les = List.map (fun le -> get_set (f le)) les in
      Set.mk_intersection ctx les >- SetType
  | NOp (LstCat, les) ->
      let les = List.map (fun le -> get_list (f le)) les in
      Z3.Seq.mk_seq_concat ctx les >- ListType
  | EList les ->
      let args = List.map (fun le -> simple_wrap (f le)) les in
      mk_z3_list args >- ListType
  | ESet les ->
      let args = List.map (fun le -> simple_wrap (f le)) les in
      mk_z3_set args >- SetType
  | LstSub (lst, start, len) ->
      let lst = get_list (f lst) in
      let start = get_int (f start) in
      let len = get_int (f len) in
      Z3.Seq.mk_seq_extract ctx lst start len >- ListType

let rec encode_forall ~gamma ~llen_lvars quantified_vars assertion =
  let open Encoding in
  match quantified_vars with
  | [] ->
      (* A quantified assertion with no quantified variables is just the assertion *)
      encode_assertion ~gamma ~llen_lvars assertion
  | _ ->
      (* Start by updating gamma with the information provided by quantifier types.
         There's very few foralls, so it's ok to copy the gamma entirely *)
      let gamma = Hashtbl.copy gamma in
      List.iter
        (fun (x, ty) ->
          match ty with
          | None -> Hashtbl.remove gamma x
          | Some ty -> Hashtbl.replace gamma x ty)
        quantified_vars;
      (* Not the same gamma now!*)
      let encoded_assertion =
        match encode_assertion ~gamma ~llen_lvars assertion with
        | { kind = Native BooleanType; expr } -> expr
        | _ -> failwith "the thing inside forall is not boolean!"
      in
      let quantified_vars =
        List.map
          (fun (x, t) ->
            let sort =
              match t with
              | None -> extended_literal_sort
              | Some ty -> Encoding.native_sort_of_type ty
            in
            ZExpr.mk_const_s ctx x sort)
          quantified_vars
      in
      let quantifier =
        Quantifier.mk_forall_const ctx quantified_vars encoded_assertion None []
          [] None None
      in
      let quantifier_expr = Quantifier.expr_of_quantifier quantifier in
      ZExpr.simplify quantifier_expr None >- BooleanType

and encode_assertion ~(gamma : tyenv) ~(llen_lvars : SS.t) (a : Formula.t) :
    Encoding.t =
  let f = encode_assertion ~gamma ~llen_lvars in
  let fe = encode_logical_expression ~gamma ~llen_lvars in
  let open Encoding in
  match a with
  | Not a -> Boolean.mk_not ctx (get_bool (f a)) >- BooleanType
  | Eq (le1, le2) -> encode_equality (fe le1) (fe le2)
  | FLess (le1, le2) ->
      mk_lt (get_num (fe le1)) (get_num (fe le2)) >- BooleanType
  | FLessEq (le1, le2) ->
      mk_le (get_num (fe le1)) (get_num (fe le2)) >- BooleanType
  | ILess (le1, le2) ->
      mk_lt (get_int (fe le1)) (get_int (fe le2)) >- BooleanType
  | ILessEq (le1, le2) ->
      mk_le (get_int (fe le1)) (get_int (fe le2)) >- BooleanType
  | StrLess (_, _) -> raise (Failure "Z3 encoding does not support STRLESS")
  | True -> Boolean.mk_true ctx >- BooleanType
  | False -> Boolean.mk_false ctx >- BooleanType
  | Or (a1, a2) ->
      Boolean.mk_or ctx [ get_bool (f a1); get_bool (f a2) ] >- BooleanType
  | And (a1, a2) ->
      Boolean.mk_and ctx [ get_bool (f a1); get_bool (f a2) ] >- BooleanType
  | SetMem (le1, le2) ->
      let le1' = simple_wrap (fe le1) in
      let le2' = get_set (fe le2) in
      Set.mk_membership ctx le1' le2' >- SetType
  | SetSub (le1, le2) ->
      Set.mk_subset ctx (get_set (fe le1)) (get_set (fe le2)) >- SetType
  | ForAll (bt, a) -> encode_forall ~gamma ~llen_lvars bt a
  | IsInt e -> Arithmetic.Real.mk_is_integer ctx (get_num (fe e)) >- BooleanType

(* ****************
   * SATISFIABILITY *
   * **************** *)

let encode_assertion_top_level
    ~(gamma : tyenv)
    ~(llen_lvars : SS.t)
    (a : Formula.t) : ZExpr.expr =
  try (encode_assertion ~gamma ~llen_lvars (Formula.push_in_negations a)).expr
  with Z3.Error s as exn ->
    let msg =
      Fmt.str "Failed to encode %a in gamma %a with error %s\n" Formula.pp a
        pp_tyenv gamma s
    in
    Logging.print_to_all msg;
    raise exn

(** Gets the list of lvar names that are only used in llen *)
let lvars_only_in_llen (assertions : Formula.Set.t) : SS.t =
  let inspector =
    object
      inherit [_] Visitors.iter as super
      val mutable llen_vars = SS.empty
      val mutable other_vars = SS.empty
      method get_diff = SS.diff llen_vars other_vars

      method! visit_expr () e =
        match e with
        | UnOp (UnOp.LstLen, Expr.LVar l) -> llen_vars <- SS.add l llen_vars
        | LVar l -> other_vars <- SS.add l other_vars
        | _ -> super#visit_expr () e
    end
  in
  assertions |> Formula.Set.iter (inspector#visit_formula ());
  inspector#get_diff

(** For a given set of pure formulae and its associated gamma, return the corresponding encoding *)
let encode_assertions (assertions : Formula.Set.t) (gamma : tyenv) :
    ZExpr.expr list =
  (* Check if the assertions have previously been cached *)
  match Hashtbl.find_opt encoding_cache assertions with
  | Some encoding -> encoding
  | None ->
      let llen_lvars = lvars_only_in_llen assertions in
      (* Encode assertions *)
      let encoded_assertions =
        List.map
          (encode_assertion_top_level ~gamma ~llen_lvars)
          (Formula.Set.elements assertions)
      in
      (* Cache *)
      Hashtbl.replace encoding_cache assertions encoded_assertions;
      encoded_assertions

let master_solver =
  let solver = Solver.mk_solver ctx None in
  Solver.push solver;
  solver

let dump_smt =
  let counter = ref 0 in
  let folder =
    let folder_name = "gillian_smt_queries" in
    let created = ref false in
    let create () =
      created := true;
      Unix.mkdir folder_name 0o755
    in
    fun () ->
      let () = if not !created then create () in
      folder_name
  in
  let file () =
    let ret = Printf.sprintf "query_%d.smt2" !counter in
    let () = incr counter in
    ret
  in
  fun fs gamma status ->
    let path = Filename.concat (folder ()) (file ()) in
    let c = open_out path in
    Fmt.pf
      (Format.formatter_of_out_channel c)
      "GIL query:\n\
       FS: %a\n\
       GAMMA: %a\n\
       Resulted in Status: %s\n\n\
       Encoded as Z3 Query:\n\
       %s"
      (Fmt.iter ~sep:Fmt.comma Formula.Set.iter Formula.pp)
      fs pp_tyenv gamma
      (Solver.string_of_status status)
      (Solver.to_string master_solver)

let reset_solver () =
  Solver.pop master_solver 1;
  Solver.push master_solver

let check_sat_core (fs : Formula.Set.t) (gamma : tyenv) : Model.model option =
  L.verbose (fun m ->
      m "@[<v 2>About to check SAT of:@\n%a@]@\nwith gamma:@\n@[%a@]\n"
        (Fmt.iter ~sep:(Fmt.any "@\n") Formula.Set.iter Formula.pp)
        fs pp_tyenv gamma);

  (* Step 1: Reset the solver and add the encoded formulae *)
  let encoded_assertions = encode_assertions fs gamma in

  (* Step 2: Reset the solver and add the encoded formulae *)
  Solver.add master_solver encoded_assertions;
  (* L.(
     verbose (fun m ->
         m "SAT: About to check the following:\n%s"
           (string_of_solver masterSolver))); *)
  (* Step 3: Check satisfiability *)
  (* let t = Sys.time () in *)
  L.verbose (fun fmt -> fmt "Reached Z3.");
  let ret = Solver.check master_solver [] in
  (* Utils.Statistics.update_statistics "Solver check" (Sys.time () -. t); *)
  L.(
    verbose (fun m -> m "The solver returned: %s" (Solver.string_of_status ret)));

  if !Utils.Config.dump_smt then dump_smt fs gamma ret;

  let ret_value =
    match ret with
    | Solver.UNKNOWN ->
        if !Utils.Config.under_approximation then raise Z3Unknown
        else
          Format.printf
            "FATAL ERROR: Z3 returned UNKNOWN for SAT question:\n\
             %a\n\
             with gamma:\n\
             @[%a@]\n\n\n\
             Reason: %s\n\n\
             Solver:\n\
             %a\n\
             @?"
            (Fmt.iter ~sep:(Fmt.any ", ") Formula.Set.iter Formula.pp)
            fs pp_tyenv gamma
            (Z3.Solver.get_reason_unknown master_solver)
            (Fmt.list ~sep:(Fmt.any "\n\n") Fmt.string)
            (List.map Z3.Expr.to_string encoded_assertions);
        exit 1
    | SATISFIABLE -> Solver.get_model master_solver
    | UNSATISFIABLE -> None
  in
  reset_solver ();
  ret_value

let check_sat (fs : Formula.Set.t) (gamma : tyenv) : bool =
  match Hashtbl.find_opt sat_cache fs with
  | Some result ->
      L.(verbose (fun m -> m "SAT check cached with result: %b" result));
      result
  | None ->
      L.(verbose (fun m -> m "SAT check not found in cache."));
      let ret = check_sat_core fs gamma in
      L.(
        verbose (fun m ->
            m "Adding to cache : @[%a@]" Formula.pp
              (Formula.conjunct (Formula.Set.elements fs))));
      let result = Option.is_some ret in
      Hashtbl.replace sat_cache fs (Option.is_some ret);
      result

let lift_z3_model
    (model : Model.model)
    (gamma : (string, Type.t) Hashtbl.t)
    (subst_update : string -> Expr.t -> unit)
    (target_vars : Expr.Set.t) : unit =
  let ( let* ) = Option.bind in
  let ( let+ ) x f = Option.map f x in
  let z3lv ~ty x =
    let sort = Encoding.native_sort_of_type ty in
    ZExpr.mk_const_s ctx x sort
  in
  let recover_z3_number (n : ZExpr.expr) : float option =
    if ZExpr.is_numeral n then (
      L.(verbose (fun m -> m "Z3 number: %s" (ZExpr.to_string n)));
      Some (float_of_string (Z3.Arithmetic.Real.to_decimal_string n 16)))
    else None
  in

  let recover_z3_int (n : ZExpr.expr) : Z.t option =
    if ZExpr.is_numeral n then (
      L.(verbose (fun m -> m "Z3 integer: %s" (ZExpr.to_string n)));
      Some (Z.of_string (Z3.Arithmetic.Integer.numeral_to_string n)))
    else None
  in

  let lift_z3_val (x : string) : Literal.t option =
    let* gil_type = Hashtbl.find_opt gamma x in
    let* v = Model.eval model (z3lv ~ty:gil_type x) true in
    match gil_type with
    | NumberType ->
        let+ n = recover_z3_number v in
        Literal.Num n
    | IntType ->
        let+ n = recover_z3_int v in
        Literal.Int n
    | StringType ->
        let* si = recover_z3_int v in
        let+ str_code = Hashtbl.find_opt str_codes_inv (Z.to_int si) in
        Literal.String str_code
    | _ -> None
  in

  L.(verbose (fun m -> m "Inside lift_z3_model"));
  Expr.Set.iter
    (fun x ->
      let x =
        match x with
        | LVar x -> x
        | _ ->
            raise
              (Failure "INTERNAL ERROR: Z3 lifting of a non-logical variable")
      in
      let v = lift_z3_val x in
      L.(
        verbose (fun m ->
            m "Z3 binding for %s: %s\n" x
              (Option.fold
                 ~some:(Fmt.to_to_string Literal.pp)
                 ~none:"NO BINDING!" v)));
      Option.fold ~some:(fun v -> subst_update x (Expr.Lit v)) ~none:() v)
    target_vars
