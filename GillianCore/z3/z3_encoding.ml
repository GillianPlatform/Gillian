open Gil_syntax
open Utils
open Names
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

[@@@ocaml.warning "-A"]

type tyenv = (string * Type.t) Seq.t

let pp_tyenv =
  let open Fmt in
  Dump.seq (Dump.pair string (Fmt.of_to_string Type.str))

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
    ("timeout", "16384");
  ]

let ctx : Z3.context = Z3.mk_context cfg
let booleans_sort = Boolean.mk_sort ctx
let ints_sort = Arithmetic.Integer.mk_sort ctx
let reals_sort = Arithmetic.Real.mk_sort ctx
let numbers_sort = reals_sort
let mk_string_symb s = Symbol.mk_string ctx s
let mk_int_i = Arithmetic.Integer.mk_numeral_i ctx
let mk_const = Arithmetic.Real.mk_const ctx
let mk_num_i = Arithmetic.Real.mk_numeral_i ctx
let mk_num_s = Arithmetic.Real.mk_numeral_s ctx
let mk_lt = Arithmetic.mk_lt ctx
let mk_le = Arithmetic.mk_le ctx
let mk_ge = Arithmetic.mk_ge ctx
let mk_add e1 e2 = Arithmetic.mk_add ctx [ e1; e2 ]
let mk_sub e1 e2 = Arithmetic.mk_sub ctx [ e1; e2 ]
let mk_mul e1 e2 = Arithmetic.mk_mul ctx [ e1; e2 ]
let mk_div e1 e2 = Arithmetic.mk_div ctx e1 e2
let mk_mod = Arithmetic.Integer.mk_mod ctx

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
      [ None ] [ 1 ]

  let gil_none_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "None")
      (mk_string_symb "isNone") [] [] []

  (* GIL List Type constructors *)
  let gil_list_nil_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Nil") (mk_string_symb "isNil")
      [] [] []

  let gil_list_cons_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Cons")
      (mk_string_symb "isCons")
      [ mk_string_symb "head"; mk_string_symb "tail" ]
      [ None; None ] [ 0; 1 ]

  let literal_and_literal_list_sorts =
    Datatype.mk_sorts ctx
      [ mk_string_symb "GIL_Literal"; mk_string_symb "GIL_Literal_List" ]
      [
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
        ];
        [ gil_list_nil_constructor; gil_list_cons_constructor ];
      ]

  let z3_gil_literal_sort = List.nth literal_and_literal_list_sorts 0
  let z3_gil_list_sort = List.nth literal_and_literal_list_sorts 1
  let gil_list_constructors = Datatype.get_constructors z3_gil_list_sort
  let nil_constructor = List.nth gil_list_constructors 0
  let cons_constructor = List.nth gil_list_constructors 1
  let gil_list_accessors = Datatype.get_accessors z3_gil_list_sort
  let head_accessor = List.nth (List.nth gil_list_accessors 1) 0
  let tail_accessor = List.nth (List.nth gil_list_accessors 1) 1
  let gil_list_recognizers = Datatype.get_recognizers z3_gil_list_sort
  let nil_recognizer = List.nth gil_list_recognizers 0
  let cons_recognizer = List.nth gil_list_recognizers 1
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
  let loc_accessor = List.nth (List.nth gil_literal_accessors 7) 0
  let type_accessor = List.nth (List.nth gil_literal_accessors 8) 0
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
end

let z3_gil_literal_sort = Lit_operations.z3_gil_literal_sort
let z3_gil_list_sort = Lit_operations.z3_gil_list_sort

module List_operations = struct
  open Lit_operations

  let nil_constructor = nil_constructor
  let cons_constructor = cons_constructor
  let head_accessor = head_accessor
  let tail_accessor = tail_accessor
  let nil_recognizer = nil_recognizer
  let cons_recognizer = cons_recognizer
end

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

  let singular_elem_recognizer = List.nth gil_extended_literal_recognizers 0
  let set_recognizer = List.nth gil_extended_literal_recognizers 1
end

let extended_literal_sort = Extended_literal_operations.extended_literal_sort

let mk_singleton_elem ele =
  ZExpr.mk_app ctx Extended_literal_operations.singular_constructor [ ele ]

let mk_singleton_access ele =
  ZExpr.mk_app ctx Extended_literal_operations.singular_elem_accessor [ ele ]

module Axiomatised_operations = struct
  let slen_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "s-len") [ numbers_sort ]
      numbers_sort

  let llen_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-len")
      [ Lit_operations.z3_gil_list_sort ]
      ints_sort

  let num2str_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "num2str") [ numbers_sort ]
      numbers_sort

  let str2num_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "str2num") [ numbers_sort ]
      numbers_sort

  let num2int_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "num2int") [ numbers_sort ]
      numbers_sort

  let snth_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "s-nth")
      [ numbers_sort; numbers_sort ]
      numbers_sort

  let lnth_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-nth")
      [ z3_gil_list_sort; ints_sort ]
      z3_gil_literal_sort

  let lcat_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-cat")
      [ z3_gil_list_sort; z3_gil_list_sort ]
      z3_gil_list_sort

  let lrev_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-rev") [ z3_gil_list_sort ]
      z3_gil_list_sort
end

let mk_z3_list_core les list_nil list_cons =
  let empty_list = ZExpr.mk_app ctx list_nil [] in
  let rec loop les cur_list =
    match les with
    | [] -> cur_list
    | le :: rest_les ->
        let new_cur_list = ZExpr.mk_app ctx list_cons [ le; cur_list ] in
        loop rest_les new_cur_list
  in
  let result = loop les empty_list in
  result

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

let mk_z3_list les nil_constructor cons_constructor =
  try mk_z3_list_core (List.rev les) nil_constructor cons_constructor
  with _ -> raise (Failure "DEATH: mk_z3_list")

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

let typeof_expression x =
  let set_guard =
    ZExpr.mk_app ctx Extended_literal_operations.set_recognizer [ x ]
  in
  let sing_elem_guard =
    ZExpr.mk_app ctx Extended_literal_operations.singular_elem_recognizer [ x ]
  in

  let elem_x = mk_singleton_access x in
  let undefined_guard =
    ZExpr.mk_app ctx Lit_operations.undefined_recognizer [ elem_x ]
  in
  let null_guard = ZExpr.mk_app ctx Lit_operations.null_recognizer [ elem_x ] in
  let empty_guard =
    ZExpr.mk_app ctx Lit_operations.empty_recognizer [ elem_x ]
  in
  let boolean_guard =
    ZExpr.mk_app ctx Lit_operations.boolean_recognizer [ elem_x ]
  in
  let number_guard =
    ZExpr.mk_app ctx Lit_operations.number_recognizer [ elem_x ]
  in
  let string_guard =
    ZExpr.mk_app ctx Lit_operations.string_recognizer [ elem_x ]
  in
  let loc_guard = ZExpr.mk_app ctx Lit_operations.loc_recognizer [ elem_x ] in
  let type_guard = ZExpr.mk_app ctx Lit_operations.type_recognizer [ elem_x ] in
  let list_guard = ZExpr.mk_app ctx Lit_operations.list_recognizer [ elem_x ] in
  let none_guard = ZExpr.mk_app ctx Lit_operations.none_recognizer [ elem_x ] in

  let sing_elem_types_guards =
    [
      undefined_guard;
      null_guard;
      empty_guard;
      boolean_guard;
      number_guard;
      string_guard;
      loc_guard;
      type_guard;
      list_guard;
      none_guard;
    ]
  in

  let sing_elem_types_guards =
    List.map
      (fun a -> Boolean.mk_and ctx [ sing_elem_guard; a ])
      sing_elem_types_guards
  in

  let guards = set_guard :: sing_elem_types_guards in
  let results =
    List.map encode_type
      [
        SetType;
        UndefinedType;
        NullType;
        EmptyType;
        BooleanType;
        NumberType;
        StringType;
        ObjectType;
        TypeType;
        ListType;
        NoneType;
      ]
  in

  let rec loop guards results =
    match (guards, results) with
    | [], _ | _, [] -> raise (Failure "DEATH: typeof_expression")
    | [ _ ], res :: _ -> res
    | guard :: rest_guards, res :: rest_results ->
        Boolean.mk_ite ctx guard res (loop rest_guards rest_results)
  in

  loop guards results

let rec encode_lit (lit : Literal.t) =
  let mk_singleton_elem ele =
    ZExpr.mk_app ctx Extended_literal_operations.singular_constructor [ ele ]
  in

  try
    match lit with
    | Undefined ->
        mk_singleton_elem
          (ZExpr.mk_app ctx Lit_operations.undefined_constructor [])
    | Null ->
        mk_singleton_elem (ZExpr.mk_app ctx Lit_operations.null_constructor [])
    | Empty ->
        mk_singleton_elem (ZExpr.mk_app ctx Lit_operations.empty_constructor [])
    | Bool b ->
        let b_arg =
          match b with
          | true -> Boolean.mk_true ctx
          | false -> Boolean.mk_false ctx
        in
        mk_singleton_elem
          (ZExpr.mk_app ctx Lit_operations.boolean_constructor [ b_arg ])
    | Int i ->
        let i_arg = mk_int_i i in
        mk_singleton_elem
          (ZExpr.mk_app ctx Lit_operations.int_constructor [ i_arg ])
    | Num n ->
        let sfn = Float.to_string n in
        let n_arg = mk_num_s sfn in
        mk_singleton_elem
          (ZExpr.mk_app ctx Lit_operations.number_constructor [ n_arg ])
    | String s ->
        let s_arg = encode_string s in
        mk_singleton_elem
          (ZExpr.mk_app ctx Lit_operations.string_constructor [ s_arg ])
    | Loc l ->
        let l_arg = encode_string l in
        mk_singleton_elem
          (ZExpr.mk_app ctx Lit_operations.loc_constructor [ l_arg ])
    | Type t ->
        let t_arg = encode_type t in
        mk_singleton_elem
          (ZExpr.mk_app ctx Lit_operations.type_constructor [ t_arg ])
    | LList lits ->
        let args =
          List.map (fun lit -> mk_singleton_access (encode_lit lit)) lits
        in
        let arg_list =
          mk_z3_list args List_operations.nil_constructor
            List_operations.cons_constructor
        in
        mk_singleton_elem
          (ZExpr.mk_app ctx Lit_operations.list_constructor [ arg_list ])
    | Nono ->
        mk_singleton_elem (ZExpr.mk_app ctx Lit_operations.none_constructor [])
    | Constant _ -> raise (Exceptions.Unsupported "Z3 encoding: constants")
  with Failure msg ->
    raise
      (Failure
         (Printf.sprintf "DEATH: encode_lit %s. %s"
            ((Fmt.to_to_string Literal.pp) lit)
            msg))

(** Encode GIL binary operators *)
let encode_binop (op : BinOp.t) le1 le2 =
  let binop_numbers_to_numbers mk_op le1 le2 =
    let n_le1 =
      ZExpr.mk_app ctx Lit_operations.number_accessor
        [ mk_singleton_access le1 ]
    in
    let n_le2 =
      ZExpr.mk_app ctx Lit_operations.number_accessor
        [ mk_singleton_access le2 ]
    in
    let nle1_op_nle2 = mk_op n_le1 n_le2 in
    mk_singleton_elem
      (ZExpr.mk_app ctx Lit_operations.number_constructor [ nle1_op_nle2 ])
  in

  let binop_ints_to_ints mk_op le1 le2 =
    let n_le1 =
      ZExpr.mk_app ctx Lit_operations.int_accessor [ mk_singleton_access le1 ]
    in
    let n_le2 =
      ZExpr.mk_app ctx Lit_operations.int_accessor [ mk_singleton_access le2 ]
    in
    let nle1_op_nle2 = mk_op n_le1 n_le2 in
    mk_singleton_elem
      (ZExpr.mk_app ctx Lit_operations.int_constructor [ nle1_op_nle2 ])
  in

  let binop_ints_to_booleans mk_op le1 le2 =
    let n_le1 =
      ZExpr.mk_app ctx Lit_operations.int_accessor [ mk_singleton_access le1 ]
    in
    let n_le2 =
      ZExpr.mk_app ctx Lit_operations.int_accessor [ mk_singleton_access le2 ]
    in
    let nle1_op_nle2 = mk_op n_le1 n_le2 in
    mk_singleton_elem
      (ZExpr.mk_app ctx Lit_operations.boolean_constructor [ nle1_op_nle2 ])
  in

  let binop_numbers_to_booleans mk_op le1 le2 =
    let n_le1 =
      ZExpr.mk_app ctx Lit_operations.number_accessor
        [ mk_singleton_access le1 ]
    in
    let n_le2 =
      ZExpr.mk_app ctx Lit_operations.number_accessor
        [ mk_singleton_access le2 ]
    in
    let nle1_op_nle2 = mk_op n_le1 n_le2 in
    mk_singleton_elem
      (ZExpr.mk_app ctx Lit_operations.boolean_constructor [ nle1_op_nle2 ])
  in

  match op with
  | IPlus -> binop_ints_to_ints mk_add le1 le2
  | IMinus -> binop_ints_to_ints mk_sub le1 le2
  | ITimes -> binop_ints_to_ints mk_mul le1 le2
  | IDiv -> binop_ints_to_ints mk_div le1 le2
  | IMod -> binop_ints_to_ints mk_mod le1 le2
  | ILessThan -> binop_ints_to_booleans mk_lt le1 le2
  | ILessThanEqual -> binop_ints_to_booleans mk_le le1 le2
  | FPlus -> binop_numbers_to_numbers mk_add le1 le2
  | FMinus -> binop_numbers_to_numbers mk_sub le1 le2
  | FTimes -> binop_numbers_to_numbers mk_mul le1 le2
  | FDiv -> binop_numbers_to_numbers mk_div le1 le2
  | FLessThan -> binop_numbers_to_booleans mk_lt le1 le2
  | FLessThanEqual -> binop_numbers_to_booleans mk_le le1 le2
  | Equal ->
      ZExpr.mk_app ctx Lit_operations.boolean_constructor
        [ Boolean.mk_eq ctx le1 le2 ]
  | BOr ->
      let le1_b =
        ZExpr.mk_app ctx Lit_operations.boolean_accessor
          [ mk_singleton_access le1 ]
      in
      let le2_b =
        ZExpr.mk_app ctx Lit_operations.boolean_accessor
          [ mk_singleton_access le2 ]
      in
      let le = Boolean.mk_or ctx [ le1_b; le2_b ] in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.boolean_constructor [ le ])
  | BAnd ->
      let le1_b =
        ZExpr.mk_app ctx Lit_operations.boolean_accessor
          [ mk_singleton_access le1 ]
      in
      let le2_b =
        ZExpr.mk_app ctx Lit_operations.boolean_accessor
          [ mk_singleton_access le2 ]
      in
      let le = Boolean.mk_and ctx [ le1_b; le2_b ] in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.boolean_constructor [ le ])
  | BSetMem ->
      let le1_mem = mk_singleton_access le1 in
      let le2_set =
        ZExpr.mk_app ctx Extended_literal_operations.set_accessor [ le2 ]
      in
      let le = Set.mk_membership ctx le1_mem le2_set in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.boolean_constructor [ le ])
  | SetDiff ->
      let le1_set =
        ZExpr.mk_app ctx Extended_literal_operations.set_accessor [ le1 ]
      in
      let le2_set =
        ZExpr.mk_app ctx Extended_literal_operations.set_accessor [ le2 ]
      in
      let le = Set.mk_difference ctx le1_set le2_set in
      ZExpr.mk_app ctx Extended_literal_operations.set_constructor [ le ]
  | BSetSub ->
      let le1_set =
        ZExpr.mk_app ctx Extended_literal_operations.set_accessor [ le1 ]
      in
      let le2_set =
        ZExpr.mk_app ctx Extended_literal_operations.set_accessor [ le2 ]
      in
      let le = Set.mk_subset ctx le1_set le2_set in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.boolean_constructor [ le ])
  | LstNth ->
      let lst' =
        ZExpr.mk_app ctx Lit_operations.list_accessor
          [ mk_singleton_access le1 ]
      in
      let index' =
        ZExpr.mk_app ctx Lit_operations.int_accessor [ mk_singleton_access le2 ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Axiomatised_operations.lnth_fun [ lst'; index' ])
  | StrNth ->
      let str' =
        ZExpr.mk_app ctx Lit_operations.string_accessor
          [ mk_singleton_access le1 ]
      in
      let index' =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access le2 ]
      in
      let res =
        ZExpr.mk_app ctx Axiomatised_operations.snth_fun [ str'; index' ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.string_constructor [ res ])
  (* FIXME: Specify which *)
  | _ ->
      raise
        (Failure
           (Printf.sprintf
              "SMT encoding: Construct not supported yet - binop: %s"
              (BinOp.str op)))

let encode_unop (op : UnOp.t) le =
  match op with
  | IUnaryMinus ->
      let le_n =
        ZExpr.mk_app ctx Lit_operations.int_accessor [ mk_singleton_access le ]
      in
      let op_le_n = Arithmetic.mk_unary_minus ctx le_n in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.int_constructor [ op_le_n ])
  | FUnaryMinus ->
      let le_n =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access le ]
      in
      let op_le_n = Arithmetic.mk_unary_minus ctx le_n in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.number_constructor [ op_le_n ])
  | LstLen ->
      let le_lst =
        ZExpr.mk_app ctx Lit_operations.list_accessor [ mk_singleton_access le ]
      in
      let op_le_lst =
        ZExpr.mk_app ctx Axiomatised_operations.llen_fun [ le_lst ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.int_constructor [ op_le_lst ])
  | StrLen ->
      let le_s =
        ZExpr.mk_app ctx Lit_operations.string_accessor
          [ mk_singleton_access le ]
      in
      let op_le_s = ZExpr.mk_app ctx Axiomatised_operations.slen_fun [ le_s ] in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.number_constructor [ op_le_s ])
  | ToStringOp ->
      let le_n =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access le ]
      in
      let op_le_n =
        ZExpr.mk_app ctx Axiomatised_operations.num2str_fun [ le_n ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.string_constructor [ op_le_n ])
  | ToNumberOp ->
      let le_s =
        ZExpr.mk_app ctx Lit_operations.string_accessor
          [ mk_singleton_access le ]
      in
      let op_le_s =
        ZExpr.mk_app ctx Axiomatised_operations.str2num_fun [ le_s ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.number_constructor [ op_le_s ])
  | ToIntOp ->
      let le_n =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access le ]
      in
      let op_le_n =
        ZExpr.mk_app ctx Axiomatised_operations.num2int_fun [ le_n ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.number_constructor [ op_le_n ])
  | UNot ->
      let le_b =
        ZExpr.mk_app ctx Lit_operations.boolean_accessor
          [ mk_singleton_access le ]
      in
      let op_le_b = Boolean.mk_not ctx le_b in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.boolean_constructor [ op_le_b ])
  | Cdr ->
      let le_lst =
        ZExpr.mk_app ctx Lit_operations.list_accessor [ mk_singleton_access le ]
      in
      let op_le_lst =
        ZExpr.mk_app ctx List_operations.tail_accessor [ le_lst ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.list_constructor [ op_le_lst ])
  | Car ->
      let le_lst =
        ZExpr.mk_app ctx Lit_operations.list_accessor [ mk_singleton_access le ]
      in
      let op_le = ZExpr.mk_app ctx List_operations.head_accessor [ le_lst ] in
      mk_singleton_elem op_le
  | TypeOf ->
      let res = typeof_expression le in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.type_constructor [ res ])
  | ToUint32Op ->
      let le_n =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access le ]
      in
      let op_le_n =
        Arithmetic.Integer.mk_int2real ctx
          (Arithmetic.Real.mk_real2int ctx le_n)
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.number_constructor [ op_le_n ])
  | LstRev ->
      let le_lst =
        ZExpr.mk_app ctx Lit_operations.list_accessor [ mk_singleton_access le ]
      in
      let n_le = ZExpr.mk_app ctx Axiomatised_operations.lrev_fun [ le_lst ] in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.list_constructor [ n_le ])
  | _ ->
      Printf.printf "SMT encoding: Construct not supported yet - unop - %s!\n"
        (UnOp.str op);
      let msg =
        Printf.sprintf "SMT encoding: Construct not supported yet - unop - %s!"
          (UnOp.str op)
      in
      raise (Failure msg)

let encode_nop (op : NOp.t) les =
  match op with
  | SetUnion ->
      let le = Set.mk_union ctx les in
      ZExpr.mk_app ctx Extended_literal_operations.set_constructor [ le ]
  | SetInter ->
      let le = Set.mk_intersection ctx les in
      ZExpr.mk_app ctx Extended_literal_operations.set_constructor [ le ]
  | LstCat ->
      List.fold_left
        (fun ac next ->
          (* Unpack ac *)
          let ac =
            ZExpr.mk_app ctx Lit_operations.list_accessor
              [ mk_singleton_access ac ]
          in
          (* Unpack next one *)
          let next =
            ZExpr.mk_app ctx Lit_operations.list_accessor
              [ mk_singleton_access next ]
          in
          let new_ac =
            ZExpr.mk_app ctx Axiomatised_operations.lcat_fun [ ac; next ]
          in
          let new_ac =
            mk_singleton_elem
              (ZExpr.mk_app ctx Lit_operations.list_constructor [ new_ac ])
          in
          new_ac)
        (List.hd les) (List.tl les)

let rec encode_logical_expression (le : Expr.t) : ZExpr.expr =
  let f = encode_logical_expression in

  match le with
  | Lit lit -> encode_lit lit
  | LVar var -> ZExpr.mk_const ctx (mk_string_symb var) extended_literal_sort
  | ALoc var -> ZExpr.mk_const ctx (mk_string_symb var) extended_literal_sort
  | PVar _ -> raise (Failure "Program variable in pure formula: FIRE")
  | UnOp (op, le) -> encode_unop op (f le)
  | BinOp (le1, op, le2) -> encode_binop op (f le1) (f le2)
  | NOp (op, les) ->
      let les =
        match op with
        | SetInter | SetUnion ->
            List.map
              (fun le ->
                ZExpr.mk_app ctx Extended_literal_operations.set_accessor
                  [ f le ])
              les
        | LstCat -> List.map f les
      in
      encode_nop op les
  | EList les ->
      let args = List.map (fun le -> mk_singleton_access (f le)) les in
      let arg_list =
        mk_z3_list args List_operations.nil_constructor
          List_operations.cons_constructor
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx Lit_operations.list_constructor [ arg_list ])
  | ESet les ->
      let args = List.map (fun le -> mk_singleton_access (f le)) les in
      let arg_list = mk_z3_set args in
      ZExpr.mk_app ctx Extended_literal_operations.set_constructor [ arg_list ]
  | _ ->
      let msg =
        Printf.sprintf
          "Failure - z3 encoding: Unsupported logical expression: %s"
          ((Fmt.to_to_string Expr.pp) le)
      in
      raise (Failure msg)

let encode_quantifier quantifier_type ctx quantified_vars var_sorts assertion =
  if List.length quantified_vars > 0 then
    let quantified_assertion =
      Quantifier.mk_quantifier_const ctx quantifier_type
        (List.map2
           (fun v s -> ZExpr.mk_const_s ctx v s)
           quantified_vars var_sorts)
        assertion None [] [] None None
    in
    let quantified_assertion =
      Quantifier.expr_of_quantifier quantified_assertion
    in
    let quantified_assertion = ZExpr.simplify quantified_assertion None in
    quantified_assertion
  else assertion

let make_recognizer_assertion x (t_x : Type.t) =
  let le_x = ZExpr.mk_const ctx (mk_string_symb x) extended_literal_sort in

  let non_set_type_recognizer f =
    let a1 =
      ZExpr.mk_app ctx Extended_literal_operations.singular_elem_recognizer
        [ le_x ]
    in
    let a2 = ZExpr.mk_app ctx f [ mk_singleton_access le_x ] in
    Boolean.mk_and ctx [ a1; a2 ]
  in

  match t_x with
  | UndefinedType -> non_set_type_recognizer Lit_operations.undefined_recognizer
  | NullType -> non_set_type_recognizer Lit_operations.null_recognizer
  | EmptyType -> non_set_type_recognizer Lit_operations.empty_recognizer
  | NoneType -> non_set_type_recognizer Lit_operations.none_recognizer
  | BooleanType -> non_set_type_recognizer Lit_operations.boolean_recognizer
  | IntType -> non_set_type_recognizer Lit_operations.int_recognizer
  | NumberType -> non_set_type_recognizer Lit_operations.number_recognizer
  | StringType -> non_set_type_recognizer Lit_operations.string_recognizer
  | ObjectType -> non_set_type_recognizer Lit_operations.loc_recognizer
  | ListType -> non_set_type_recognizer Lit_operations.list_recognizer
  | TypeType -> non_set_type_recognizer Lit_operations.type_recognizer
  | SetType ->
      ZExpr.mk_app ctx Extended_literal_operations.set_recognizer [ le_x ]

let rec encode_assertion (a : Formula.t) : ZExpr.expr =
  let f = encode_assertion in
  let fe = encode_logical_expression in

  match a with
  | Not a -> Boolean.mk_not ctx (f a)
  | Eq (le1, le2) -> Boolean.mk_eq ctx (fe le1) (fe le2)
  | FLess (le1, le2) ->
      let le1' =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access (fe le1) ]
      in
      let le2' =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access (fe le2) ]
      in
      mk_lt le1' le2'
  | FLessEq (le1, le2) ->
      let le1' =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access (fe le1) ]
      in
      let le2' =
        ZExpr.mk_app ctx Lit_operations.number_accessor
          [ mk_singleton_access (fe le2) ]
      in
      mk_le le1' le2'
  | ILess (le1, le2) ->
      let le1' =
        ZExpr.mk_app ctx Lit_operations.int_accessor
          [ mk_singleton_access (fe le1) ]
      in
      let le2' =
        ZExpr.mk_app ctx Lit_operations.int_accessor
          [ mk_singleton_access (fe le2) ]
      in
      mk_lt le1' le2'
  | ILessEq (le1, le2) ->
      let le1' =
        ZExpr.mk_app ctx Lit_operations.int_accessor
          [ mk_singleton_access (fe le1) ]
      in
      let le2' =
        ZExpr.mk_app ctx Lit_operations.int_accessor
          [ mk_singleton_access (fe le2) ]
      in
      mk_le le1' le2'
  | StrLess (_, _) -> raise (Failure "Z3 encoding does not support STRLESS")
  | True -> Boolean.mk_true ctx
  | False -> Boolean.mk_false ctx
  | Or (a1, a2) -> Boolean.mk_or ctx [ f a1; f a2 ]
  | And (a1, a2) -> Boolean.mk_and ctx [ f a1; f a2 ]
  | SetMem (le1, le2) ->
      let le1' = mk_singleton_access (fe le1) in
      let le2' =
        ZExpr.mk_app ctx Extended_literal_operations.set_accessor [ fe le2 ]
      in
      Set.mk_membership ctx le1' le2'
  | SetSub (le1, le2) ->
      let le1' =
        ZExpr.mk_app ctx Extended_literal_operations.set_accessor [ fe le1 ]
      in
      let le2' =
        ZExpr.mk_app ctx Extended_literal_operations.set_accessor [ fe le2 ]
      in
      Set.mk_subset ctx le1' le2'
  | ForAll (bt, a) ->
      let z3_sorts = List.map (fun _ -> extended_literal_sort) bt in
      let z3_types_assertions =
        List.filter_map
          (fun (x, t_x) ->
            match t_x with
            | Some t_x -> Some (make_recognizer_assertion x t_x)
            | None -> None)
          bt
      in
      let binders, _ = List.split bt in
      let z3_types_assertion = Boolean.mk_and ctx z3_types_assertions in
      let z3_a = Boolean.mk_implies ctx z3_types_assertion (f a) in
      encode_quantifier true ctx binders z3_sorts z3_a

(* ****************
   * SATISFIABILITY *
   * **************** *)

let encode_assertion_top_level (a : Formula.t) : ZExpr.expr =
  encode_assertion (Formula.push_in_negations a)

let encode_gamma (gamma : tyenv) =
  Seq.filter_map
    (fun (x, t_x) ->
      if is_lvar_name x || is_aloc_name x then
        Some (make_recognizer_assertion x t_x)
      else None)
    gamma

(** For a given set of pure formulae and its associated gamma, return the corresponding encoding *)
let encode_assertions
    (assertions : Formula.Set.t)
    (gamma : (string * Gil_syntax.Type.t) Seq.t) : ZExpr.expr list =
  (* Check if the assertions have previously been cached *)
  match Hashtbl.find_opt encoding_cache assertions with
  | Some encoding -> encoding
  | None ->
      (* Encode assertions *)
      let encoded_assertions =
        Seq.map encode_assertion_top_level (Formula.Set.to_seq assertions)
      in
      (* Encode gamma *)
      let encoded_assertions =
        Seq.append encoded_assertions (encode_gamma gamma) |> List.of_seq
      in
      (* Cache *)
      Hashtbl.replace encoding_cache assertions encoded_assertions;
      encoded_assertions

let master_solver =
  let solver = Solver.mk_solver ctx None in
  Solver.push solver;
  solver

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

  let ret_value =
    match ret with
    | Solver.UNKNOWN ->
        Format.printf
          "FATAL ERROR: Z3 returned UNKNOWN for SAT question:\n\
           %a\n\
           with gamma:\n\
           @[%a@]\n\n\n\
           Z3 EXPRS:\n\
           %a\n\
          \       @?"
          (Fmt.iter ~sep:(Fmt.any ", ") Formula.Set.iter Formula.pp)
          fs pp_tyenv gamma
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
  let recover_z3_number (n : ZExpr.expr) : float option =
    if ZExpr.is_numeral n then (
      L.(verbose (fun m -> m "Z3 number: %s" (ZExpr.to_string n)));
      Some (float_of_string (Z3.Arithmetic.Real.to_decimal_string n 16)))
    else None
  in

  let recover_z3_int (zn : ZExpr.expr) : int option =
    let+ n = recover_z3_number zn in
    int_of_float n
  in

  let lift_z3_val (x : string) : Expr.t option =
    let* gil_type = Hashtbl.find_opt gamma x in
    match gil_type with
    | NumberType ->
        let x' = encode_logical_expression (LVar x) in
        let x'' =
          ZExpr.mk_app ctx Lit_operations.number_accessor
            [ mk_singleton_access x' ]
        in
        let* v = Model.eval model x'' true in
        let+ n = recover_z3_number v in
        Expr.num n
    | StringType ->
        let x' = encode_logical_expression (LVar x) in
        let x'' =
          ZExpr.mk_app ctx Lit_operations.string_accessor
            [ mk_singleton_access x' ]
        in
        let* v = Model.eval model x'' true in
        let* si = recover_z3_int v in
        let+ str_code = Hashtbl.find_opt str_codes_inv si in
        Expr.string str_code
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
              (Option.fold ~some:(Fmt.to_to_string Expr.pp) ~none:"NO BINDING!"
                 v)));
      Option.fold ~some:(subst_update x) ~none:() v)
    target_vars
