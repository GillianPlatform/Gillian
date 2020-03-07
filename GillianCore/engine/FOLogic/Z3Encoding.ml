open SVal
open Names
open Containers
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

let encoding_cache : (Formula.Set.t, ZExpr.expr list) Hashtbl.t =
  Hashtbl.create Config.big_tbl_size

let sat_cache : (Formula.Set.t, bool) Hashtbl.t =
  Hashtbl.create Config.big_tbl_size

type encoding = WithReals | WithFPA

let string_of_enc enc =
  match enc with
  | WithReals -> "REAL"
  | WithFPA   -> "FPA"

let encoding = ref WithReals

let match_enc msg x y =
  match !encoding with
  | WithReals -> x
  | WithFPA   -> y

type gil_axiomatized_operations = {
  llen_fun : FuncDecl.func_decl;
  slen_fun : FuncDecl.func_decl;
  num2str_fun : FuncDecl.func_decl;
  str2num_fun : FuncDecl.func_decl;
  num2int_fun : FuncDecl.func_decl;
  lnth_fun : FuncDecl.func_decl;
  snth_fun : FuncDecl.func_decl;
  lcat_fun : FuncDecl.func_decl;
  lrev_fun : FuncDecl.func_decl;
}

type gil_type_constructors = {
  undefined_type_constructor : FuncDecl.func_decl;
  null_type_constructor : FuncDecl.func_decl;
  empty_type_constructor : FuncDecl.func_decl;
  none_type_constructor : FuncDecl.func_decl;
  boolean_type_constructor : FuncDecl.func_decl;
  int_type_constructor : FuncDecl.func_decl;
  number_type_constructor : FuncDecl.func_decl;
  string_type_constructor : FuncDecl.func_decl;
  object_type_constructor : FuncDecl.func_decl;
  list_type_constructor : FuncDecl.func_decl;
  type_type_constructor : FuncDecl.func_decl;
  set_type_constructor : FuncDecl.func_decl;
}

type z3_basic_gil_value = {
  (****************)
  (* constructors *)
  (****************)
  undefined_constructor : FuncDecl.func_decl;
  null_constructor : FuncDecl.func_decl;
  empty_constructor : FuncDecl.func_decl;
  boolean_constructor : FuncDecl.func_decl;
  int_constructor : FuncDecl.func_decl;
  number_constructor : FuncDecl.func_decl;
  string_constructor : FuncDecl.func_decl;
  loc_constructor : FuncDecl.func_decl;
  type_constructor : FuncDecl.func_decl;
  list_constructor : FuncDecl.func_decl;
  none_constructor : FuncDecl.func_decl;
  (*************)
  (* accessors *)
  (*************)
  boolean_accessor : FuncDecl.func_decl;
  int_accessor : FuncDecl.func_decl;
  number_accessor : FuncDecl.func_decl;
  string_accessor : FuncDecl.func_decl;
  loc_accessor : FuncDecl.func_decl;
  type_accessor : FuncDecl.func_decl;
  list_accessor : FuncDecl.func_decl;
  (***************)
  (* recognizers *)
  (***************)
  undefined_recognizer : FuncDecl.func_decl;
  null_recognizer : FuncDecl.func_decl;
  empty_recognizer : FuncDecl.func_decl;
  boolean_recognizer : FuncDecl.func_decl;
  int_recognizer : FuncDecl.func_decl;
  number_recognizer : FuncDecl.func_decl;
  string_recognizer : FuncDecl.func_decl;
  loc_recognizer : FuncDecl.func_decl;
  type_recognizer : FuncDecl.func_decl;
  list_recognizer : FuncDecl.func_decl;
  none_recognizer : FuncDecl.func_decl;
}

type z3_gil_list = {
  nil_constructor : FuncDecl.func_decl;
  cons_constructor : FuncDecl.func_decl;
  head_accessor : FuncDecl.func_decl;
  tail_accessor : FuncDecl.func_decl;
  nil_recognizer : FuncDecl.func_decl;
  cons_recognizer : FuncDecl.func_decl;
}

type extended_gil_value_constructor = {
  singular_constructor : FuncDecl.func_decl;
  set_constructor : FuncDecl.func_decl;
  singular_elem_accessor : FuncDecl.func_decl;
  set_accessor : FuncDecl.func_decl;
  singular_elem_recognizer : FuncDecl.func_decl;
  set_recognizer : FuncDecl.func_decl;
}

let cfg =
  [
    ("model", "true");
    ("proof", "true");
    ("unsat_core", "true");
    ("auto_config", "true");
    ("timeout", "16384");
  ]

let ctx : Z3.context = Z3.mk_context cfg

let booleans_sort = Boolean.mk_sort ctx

let ints_sort = Arithmetic.Integer.mk_sort ctx

let reals_sort = Arithmetic.Real.mk_sort ctx

let fp_sort = FloatingPoint.mk_sort_64 ctx

let numbers_sort = match_enc "mk_sort" reals_sort fp_sort

let rm =
  FloatingPoint.mk_const ctx
    (Symbol.mk_string ctx "rm")
    (FloatingPoint.RoundingMode.mk_sort ctx)

let mk_string_symb s = Symbol.mk_string ctx s

let mk_int_i = Arithmetic.Integer.mk_numeral_i ctx

let mk_const =
  match_enc "mk_const" (Arithmetic.Real.mk_const ctx)
    (fun (s : Symbol.symbol) -> FloatingPoint.mk_const ctx s fp_sort)

let mk_num_i =
  match_enc "mk_num_i" (Arithmetic.Real.mk_numeral_i ctx) (fun i ->
      FloatingPoint.mk_numeral_i ctx i fp_sort)

let mk_num_s =
  match_enc "mk_num_s" (Arithmetic.Real.mk_numeral_s ctx) (fun s ->
      FloatingPoint.mk_numeral_s ctx s fp_sort)

let mk_lt = match_enc "mk_lt" Arithmetic.mk_lt FloatingPoint.mk_lt

let mk_le = match_enc "mk_le" Arithmetic.mk_le FloatingPoint.mk_leq

let mk_ge = match_enc "mk_ge" Arithmetic.mk_ge FloatingPoint.mk_geq

let mk_add =
  match_enc "mk_add"
    (fun e1 e2 -> Arithmetic.mk_add ctx [ e1; e2 ])
    (fun e1 e2 -> FloatingPoint.mk_add ctx rm e1 e2)

let mk_sub =
  match_enc "mk_sub"
    (fun e1 e2 -> Arithmetic.mk_sub ctx [ e1; e2 ])
    (fun e1 e2 -> FloatingPoint.mk_sub ctx rm e1 e2)

let mk_mul =
  match_enc "mk_mul"
    (fun e1 e2 -> Arithmetic.mk_mul ctx [ e1; e2 ])
    (fun e1 e2 -> FloatingPoint.mk_mul ctx rm e1 e2)

let mk_div =
  match_enc "mk_div"
    (fun e1 e2 -> Arithmetic.mk_div ctx e1 e2)
    (fun e1 e2 -> FloatingPoint.mk_div ctx rm e1 e2)

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

let type_operations =
  try
    let z3_gil_type_constructors = Datatype.get_constructors z3_gil_type_sort in
    let undefined_type_constructor = List.nth z3_gil_type_constructors 0 in
    let null_type_constructor = List.nth z3_gil_type_constructors 1 in
    let empty_type_constructor = List.nth z3_gil_type_constructors 2 in
    let none_type_constructor = List.nth z3_gil_type_constructors 3 in
    let boolean_type_constructor = List.nth z3_gil_type_constructors 4 in
    let int_type_constructor = List.nth z3_gil_type_constructors 5 in
    let number_type_constructor = List.nth z3_gil_type_constructors 6 in
    let string_type_constructor = List.nth z3_gil_type_constructors 7 in
    let object_type_constructor = List.nth z3_gil_type_constructors 8 in
    let list_type_constructor = List.nth z3_gil_type_constructors 9 in
    let type_type_constructor = List.nth z3_gil_type_constructors 10 in
    let set_type_constructor = List.nth z3_gil_type_constructors 11 in
    {
      undefined_type_constructor;
      null_type_constructor;
      empty_type_constructor;
      none_type_constructor;
      boolean_type_constructor;
      int_type_constructor;
      number_type_constructor;
      string_type_constructor;
      object_type_constructor;
      list_type_constructor;
      type_type_constructor;
      set_type_constructor;
    }
  with _ -> raise (Failure "DEATH: type_operations")

let z3_gil_literal_sort, z3_gil_list_sort, lit_operations, list_operations =
  (* GIL type constructors *)
  let gil_undefined_constructor =
    Datatype.mk_constructor ctx
      (mk_string_symb "Undefined")
      (mk_string_symb "isUndefined")
      [] [] []
  in
  let gil_null_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Null")
      (mk_string_symb "isNull") [] [] []
  in
  let gil_empty_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Empty")
      (mk_string_symb "isEmpty") [] [] []
  in
  let gil_bool_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Bool")
      (mk_string_symb "isBool")
      [ mk_string_symb "bValue" ]
      [ Some booleans_sort ] [ 0 ]
  in
  let gil_int_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Int") (mk_string_symb "isInt")
      [ mk_string_symb "iValue" ]
      [ Some ints_sort ] [ 0 ]
  in
  let gil_num_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Num") (mk_string_symb "isNum")
      [ mk_string_symb "nValue" ]
      [ Some numbers_sort ] [ 0 ]
  in
  let gil_string_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "String")
      (mk_string_symb "isString")
      [ mk_string_symb "sValue" ]
      [ Some ints_sort ] [ 0 ]
  in
  let gil_loc_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Loc") (mk_string_symb "isLoc")
      [ mk_string_symb "locValue" ]
      [ Some ints_sort ] [ 0 ]
  in
  let gil_type_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Type")
      (mk_string_symb "isType")
      [ mk_string_symb "tValue" ]
      [ Some z3_gil_type_sort ] [ 0 ]
  in
  let gil_list_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "List")
      (mk_string_symb "isList")
      [ mk_string_symb "listValue" ]
      [ None ] [ 1 ]
  in
  let gil_none_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "None")
      (mk_string_symb "isNone") [] [] []
  in

  (* GIL List Type constructors *)
  let gil_list_nil_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Nil") (mk_string_symb "isNil")
      [] [] []
  in
  let gil_list_cons_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Cons")
      (mk_string_symb "isCons")
      [ mk_string_symb "head"; mk_string_symb "tail" ]
      [ None; None ] [ 0; 1 ]
  in

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
  in

  try
    let z3_gil_literal_sort = List.nth literal_and_literal_list_sorts 0 in
    let z3_gil_list_sort = List.nth literal_and_literal_list_sorts 1 in

    let gil_list_constructors = Datatype.get_constructors z3_gil_list_sort in
    let nil_constructor = List.nth gil_list_constructors 0 in
    let cons_constructor = List.nth gil_list_constructors 1 in

    let gil_list_accessors = Datatype.get_accessors z3_gil_list_sort in
    let head_accessor = List.nth (List.nth gil_list_accessors 1) 0 in
    let tail_accessor = List.nth (List.nth gil_list_accessors 1) 1 in

    let gil_list_recognizers = Datatype.get_recognizers z3_gil_list_sort in
    let nil_recognizer = List.nth gil_list_recognizers 0 in
    let cons_recognizer = List.nth gil_list_recognizers 1 in

    let z3_literal_constructors =
      Datatype.get_constructors z3_gil_literal_sort
    in
    let undefined_constructor = List.nth z3_literal_constructors 0 in
    let null_constructor = List.nth z3_literal_constructors 1 in
    let empty_constructor = List.nth z3_literal_constructors 2 in
    let boolean_constructor = List.nth z3_literal_constructors 3 in
    let int_constructor = List.nth z3_literal_constructors 4 in
    let number_constructor = List.nth z3_literal_constructors 5 in
    let string_constructor = List.nth z3_literal_constructors 6 in
    let loc_constructor = List.nth z3_literal_constructors 7 in
    let type_constructor = List.nth z3_literal_constructors 8 in
    let list_constructor = List.nth z3_literal_constructors 9 in
    let none_constructor = List.nth z3_literal_constructors 10 in

    let gil_literal_accessors = Datatype.get_accessors z3_gil_literal_sort in
    let boolean_accessor = List.nth (List.nth gil_literal_accessors 3) 0 in
    let int_accessor = List.nth (List.nth gil_literal_accessors 4) 0 in
    let number_accessor = List.nth (List.nth gil_literal_accessors 5) 0 in
    let string_accessor = List.nth (List.nth gil_literal_accessors 6) 0 in
    let loc_accessor = List.nth (List.nth gil_literal_accessors 7) 0 in
    let type_accessor = List.nth (List.nth gil_literal_accessors 8) 0 in
    let list_accessor = List.nth (List.nth gil_literal_accessors 9) 0 in

    let gil_literal_recognizers =
      Datatype.get_recognizers z3_gil_literal_sort
    in
    let undefined_recognizer = List.nth gil_literal_recognizers 0 in
    let null_recognizer = List.nth gil_literal_recognizers 1 in
    let empty_recognizer = List.nth gil_literal_recognizers 2 in
    let boolean_recognizer = List.nth gil_literal_recognizers 3 in
    let int_recognizer = List.nth gil_literal_recognizers 4 in
    let number_recognizer = List.nth gil_literal_recognizers 5 in
    let string_recognizer = List.nth gil_literal_recognizers 6 in
    let loc_recognizer = List.nth gil_literal_recognizers 7 in
    let type_recognizer = List.nth gil_literal_recognizers 8 in
    let list_recognizer = List.nth gil_literal_recognizers 9 in
    let none_recognizer = List.nth gil_literal_recognizers 10 in

    let gil_literal_operations =
      {
        undefined_constructor;
        null_constructor;
        empty_constructor;
        boolean_constructor;
        int_constructor;
        number_constructor;
        string_constructor;
        loc_constructor;
        type_constructor;
        list_constructor;
        none_constructor;
        boolean_accessor;
        int_accessor;
        number_accessor;
        string_accessor;
        loc_accessor;
        type_accessor;
        list_accessor;
        undefined_recognizer;
        null_recognizer;
        empty_recognizer;
        boolean_recognizer;
        int_recognizer;
        number_recognizer;
        string_recognizer;
        loc_recognizer;
        type_recognizer;
        list_recognizer;
        none_recognizer;
      }
    in
    let gil_list_operations =
      {
        nil_constructor;
        cons_constructor;
        head_accessor;
        tail_accessor;
        nil_recognizer;
        cons_recognizer;
      }
    in
    ( z3_gil_literal_sort,
      z3_gil_list_sort,
      gil_literal_operations,
      gil_list_operations )
  with _ -> raise (Failure "DEATH: construction of z3_gil_value_sort")

let extended_literal_sort, extended_literal_operations, z3_gil_set_sort =
  let z3_gil_set_sort = Set.mk_sort ctx z3_gil_literal_sort in

  let gil_sing_elem_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Elem")
      (mk_string_symb "isSingular")
      [ mk_string_symb "singElem" ]
      [ Some z3_gil_literal_sort ]
      [ 0 ]
  in

  let gil_set_elem_constructor =
    Datatype.mk_constructor ctx (mk_string_symb "Set") (mk_string_symb "isSet")
      [ mk_string_symb "setElem" ]
      [ Some z3_gil_set_sort ] [ 0 ]
  in

  let extended_literal_sort =
    Datatype.mk_sort ctx
      (mk_string_symb "Extended_GIL_Literal")
      [ gil_sing_elem_constructor; gil_set_elem_constructor ]
  in

  try
    let gil_extended_literal_constructors =
      Datatype.get_constructors extended_literal_sort
    in
    let singular_constructor = List.nth gil_extended_literal_constructors 0 in
    let set_constructor = List.nth gil_extended_literal_constructors 1 in

    let gil_extended_literal_accessors =
      Datatype.get_accessors extended_literal_sort
    in
    let singular_elem_accessor =
      List.nth (List.nth gil_extended_literal_accessors 0) 0
    in
    let set_accessor = List.nth (List.nth gil_extended_literal_accessors 1) 0 in

    let gil_extended_literal_recognizers =
      Datatype.get_recognizers extended_literal_sort
    in
    let singular_elem_recognizer =
      List.nth gil_extended_literal_recognizers 0
    in
    let set_recognizer = List.nth gil_extended_literal_recognizers 1 in

    let extended_literal_operations =
      {
        singular_constructor;
        set_constructor;
        singular_elem_accessor;
        set_accessor;
        singular_elem_recognizer;
        set_recognizer;
      }
    in
    (extended_literal_sort, extended_literal_operations, z3_gil_set_sort)
  with _ -> raise (Failure "DEATH: construction of z3_gil_value_sort")

let mk_singleton_elem ele =
  ZExpr.mk_app ctx extended_literal_operations.singular_constructor [ ele ]

let mk_singleton_access ele =
  ZExpr.mk_app ctx extended_literal_operations.singular_elem_accessor [ ele ]

let axiomatised_operations =
  let slen_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "s-len") [ numbers_sort ]
      numbers_sort
  in
  let llen_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-len") [ z3_gil_list_sort ]
      numbers_sort
  in
  let num2str_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "num2str") [ numbers_sort ]
      numbers_sort
  in
  let str2num_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "str2num") [ numbers_sort ]
      numbers_sort
  in
  let num2int_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "num2int") [ numbers_sort ]
      numbers_sort
  in
  let snth_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "s-nth")
      [ numbers_sort; numbers_sort ]
      numbers_sort
  in
  let lnth_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-nth")
      [ z3_gil_list_sort; numbers_sort ]
      z3_gil_literal_sort
  in
  let lcat_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-cat")
      [ z3_gil_list_sort; z3_gil_list_sort ]
      z3_gil_list_sort
  in
  let lrev_fun =
    FuncDecl.mk_func_decl ctx (mk_string_symb "l-rev") [ z3_gil_list_sort ]
      z3_gil_list_sort
  in

  {
    slen_fun;
    llen_fun;
    num2str_fun;
    str2num_fun;
    num2int_fun;
    snth_fun;
    lnth_fun;
    lcat_fun;
    lrev_fun;
  }

let mk_z3_list_core les list_nil list_cons =
  let empty_list = ZExpr.mk_app ctx list_nil [] in
  let rec loop les cur_list =
    match les with
    | []             -> cur_list
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
    | []             -> cur_set
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
        ZExpr.mk_app ctx type_operations.undefined_type_constructor []
    | NullType      -> ZExpr.mk_app ctx type_operations.null_type_constructor []
    | EmptyType     -> ZExpr.mk_app ctx type_operations.empty_type_constructor []
    | NoneType      -> ZExpr.mk_app ctx type_operations.none_type_constructor []
    | BooleanType   ->
        ZExpr.mk_app ctx type_operations.boolean_type_constructor []
    | IntType       -> ZExpr.mk_app ctx type_operations.int_type_constructor []
    | NumberType    -> ZExpr.mk_app ctx type_operations.number_type_constructor
                         []
    | StringType    -> ZExpr.mk_app ctx type_operations.string_type_constructor
                         []
    | ObjectType    -> ZExpr.mk_app ctx type_operations.object_type_constructor
                         []
    | ListType      -> ZExpr.mk_app ctx type_operations.list_type_constructor []
    | TypeType      -> ZExpr.mk_app ctx type_operations.type_type_constructor []
    | SetType       -> ZExpr.mk_app ctx type_operations.set_type_constructor []
  with _ ->
    raise
      (Failure (Printf.sprintf "DEATH: encode_type with arg: %s" (Type.str t)))

let typeof_expression x =
  let set_guard =
    ZExpr.mk_app ctx extended_literal_operations.set_recognizer [ x ]
  in
  let sing_elem_guard =
    ZExpr.mk_app ctx extended_literal_operations.singular_elem_recognizer [ x ]
  in

  let elem_x = mk_singleton_access x in
  let undefined_guard =
    ZExpr.mk_app ctx lit_operations.undefined_recognizer [ elem_x ]
  in
  let null_guard = ZExpr.mk_app ctx lit_operations.null_recognizer [ elem_x ] in
  let empty_guard =
    ZExpr.mk_app ctx lit_operations.empty_recognizer [ elem_x ]
  in
  let boolean_guard =
    ZExpr.mk_app ctx lit_operations.boolean_recognizer [ elem_x ]
  in
  let number_guard =
    ZExpr.mk_app ctx lit_operations.number_recognizer [ elem_x ]
  in
  let string_guard =
    ZExpr.mk_app ctx lit_operations.string_recognizer [ elem_x ]
  in
  let loc_guard = ZExpr.mk_app ctx lit_operations.loc_recognizer [ elem_x ] in
  let type_guard = ZExpr.mk_app ctx lit_operations.type_recognizer [ elem_x ] in
  let list_guard = ZExpr.mk_app ctx lit_operations.list_recognizer [ elem_x ] in
  let none_guard = ZExpr.mk_app ctx lit_operations.none_recognizer [ elem_x ] in

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
    ZExpr.mk_app ctx extended_literal_operations.singular_constructor [ ele ]
  in

  try
    match lit with
    | Undefined  ->
        mk_singleton_elem
          (ZExpr.mk_app ctx lit_operations.undefined_constructor [])
    | Null       ->
        mk_singleton_elem (ZExpr.mk_app ctx lit_operations.null_constructor [])
    | Empty      ->
        mk_singleton_elem (ZExpr.mk_app ctx lit_operations.empty_constructor [])
    | Bool b     ->
        let b_arg =
          match b with
          | true  -> Boolean.mk_true ctx
          | false -> Boolean.mk_false ctx
        in
        mk_singleton_elem
          (ZExpr.mk_app ctx lit_operations.boolean_constructor [ b_arg ])
    | Int i      ->
        let i_arg = mk_int_i i in
        mk_singleton_elem
          (ZExpr.mk_app ctx lit_operations.int_constructor [ i_arg ])
    | Num n      ->
        let sfn = Float.to_string n in
        let n_arg = mk_num_s sfn in
        mk_singleton_elem
          (ZExpr.mk_app ctx lit_operations.number_constructor [ n_arg ])
    | String s   ->
        let s_arg = encode_string s in
        mk_singleton_elem
          (ZExpr.mk_app ctx lit_operations.string_constructor [ s_arg ])
    | Loc l      ->
        let l_arg = encode_string l in
        mk_singleton_elem
          (ZExpr.mk_app ctx lit_operations.loc_constructor [ l_arg ])
    | Type t     ->
        let t_arg = encode_type t in
        mk_singleton_elem
          (ZExpr.mk_app ctx lit_operations.type_constructor [ t_arg ])
    | LList lits ->
        let args =
          List.map (fun lit -> mk_singleton_access (encode_lit lit)) lits
        in
        let arg_list =
          mk_z3_list args list_operations.nil_constructor
            list_operations.cons_constructor
        in
        mk_singleton_elem
          (ZExpr.mk_app ctx lit_operations.list_constructor [ arg_list ])
    | Nono       ->
        mk_singleton_elem (ZExpr.mk_app ctx lit_operations.none_constructor [])
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
      ZExpr.mk_app ctx lit_operations.number_accessor
        [ mk_singleton_access le1 ]
    in
    let n_le2 =
      ZExpr.mk_app ctx lit_operations.number_accessor
        [ mk_singleton_access le2 ]
    in
    let nle1_op_nle2 = mk_op n_le1 n_le2 in
    mk_singleton_elem
      (ZExpr.mk_app ctx lit_operations.number_constructor [ nle1_op_nle2 ])
  in

  let binop_ints_to_ints mk_op le1 le2 =
    let n_le1 =
      ZExpr.mk_app ctx lit_operations.int_accessor [ mk_singleton_access le1 ]
    in
    let n_le2 =
      ZExpr.mk_app ctx lit_operations.int_accessor [ mk_singleton_access le2 ]
    in
    let nle1_op_nle2 = mk_op n_le1 n_le2 in
    mk_singleton_elem
      (ZExpr.mk_app ctx lit_operations.int_constructor [ nle1_op_nle2 ])
  in

  let binop_ints_to_booleans mk_op le1 le2 =
    let n_le1 =
      ZExpr.mk_app ctx lit_operations.int_accessor [ mk_singleton_access le1 ]
    in
    let n_le2 =
      ZExpr.mk_app ctx lit_operations.int_accessor [ mk_singleton_access le2 ]
    in
    let nle1_op_nle2 = mk_op n_le1 n_le2 in
    mk_singleton_elem
      (ZExpr.mk_app ctx lit_operations.boolean_constructor [ nle1_op_nle2 ])
  in

  let binop_numbers_to_booleans mk_op le1 le2 =
    let n_le1 =
      ZExpr.mk_app ctx lit_operations.number_accessor
        [ mk_singleton_access le1 ]
    in
    let n_le2 =
      ZExpr.mk_app ctx lit_operations.number_accessor
        [ mk_singleton_access le2 ]
    in
    let nle1_op_nle2 = mk_op n_le1 n_le2 in
    mk_singleton_elem
      (ZExpr.mk_app ctx lit_operations.boolean_constructor [ nle1_op_nle2 ])
  in

  match op with
  | IPlus -> binop_ints_to_ints mk_add le1 le2
  | IMinus -> binop_ints_to_ints mk_sub le1 le2
  | ITimes -> binop_ints_to_ints mk_mul le1 le2
  | IDiv -> binop_ints_to_ints mk_div le1 le2
  | IMod -> binop_ints_to_ints mk_mod le1 le2
  | ILessThan -> binop_ints_to_booleans (mk_lt ctx) le1 le2
  | ILessThanEqual -> binop_ints_to_booleans (mk_le ctx) le1 le2
  | FPlus -> binop_numbers_to_numbers mk_add le1 le2
  | FMinus -> binop_numbers_to_numbers mk_sub le1 le2
  | FTimes -> binop_numbers_to_numbers mk_mul le1 le2
  | FDiv -> binop_numbers_to_numbers mk_div le1 le2
  | FLessThan -> binop_numbers_to_booleans (mk_lt ctx) le1 le2
  | FLessThanEqual -> binop_numbers_to_booleans (mk_le ctx) le1 le2
  | Equal ->
      ZExpr.mk_app ctx lit_operations.boolean_constructor
        [ Boolean.mk_eq ctx le1 le2 ]
  | BOr ->
      let le1_b =
        ZExpr.mk_app ctx lit_operations.boolean_accessor
          [ mk_singleton_access le1 ]
      in
      let le2_b =
        ZExpr.mk_app ctx lit_operations.boolean_accessor
          [ mk_singleton_access le2 ]
      in
      let le = Boolean.mk_or ctx [ le1_b; le2_b ] in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.boolean_constructor [ le ])
  | BAnd ->
      let le1_b =
        ZExpr.mk_app ctx lit_operations.boolean_accessor
          [ mk_singleton_access le1 ]
      in
      let le2_b =
        ZExpr.mk_app ctx lit_operations.boolean_accessor
          [ mk_singleton_access le2 ]
      in
      let le = Boolean.mk_and ctx [ le1_b; le2_b ] in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.boolean_constructor [ le ])
  | BSetMem ->
      let le1_mem = mk_singleton_access le1 in
      let le2_set =
        ZExpr.mk_app ctx extended_literal_operations.set_accessor [ le2 ]
      in
      let le = Set.mk_membership ctx le1_mem le2_set in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.boolean_constructor [ le ])
  | SetDiff ->
      let le1_set =
        ZExpr.mk_app ctx extended_literal_operations.set_accessor [ le1 ]
      in
      let le2_set =
        ZExpr.mk_app ctx extended_literal_operations.set_accessor [ le2 ]
      in
      let le = Set.mk_difference ctx le1_set le2_set in
      ZExpr.mk_app ctx extended_literal_operations.set_constructor [ le ]
  | BSetSub ->
      let le1_set =
        ZExpr.mk_app ctx extended_literal_operations.set_accessor [ le1 ]
      in
      let le2_set =
        ZExpr.mk_app ctx extended_literal_operations.set_accessor [ le2 ]
      in
      let le = Set.mk_subset ctx le1_set le2_set in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.boolean_constructor [ le ])
  | LstNth ->
      let lst' =
        ZExpr.mk_app ctx lit_operations.list_accessor
          [ mk_singleton_access le1 ]
      in
      let index' =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access le2 ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx axiomatised_operations.lnth_fun [ lst'; index' ])
  | StrNth ->
      let str' =
        ZExpr.mk_app ctx lit_operations.string_accessor
          [ mk_singleton_access le1 ]
      in
      let index' =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access le2 ]
      in
      let res =
        ZExpr.mk_app ctx axiomatised_operations.snth_fun [ str'; index' ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.string_constructor [ res ])
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
        ZExpr.mk_app ctx lit_operations.int_accessor [ mk_singleton_access le ]
      in
      let op_le_n = Arithmetic.mk_unary_minus ctx le_n in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.int_constructor [ op_le_n ])
  | FUnaryMinus ->
      let le_n =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access le ]
      in
      let op_le_n = Arithmetic.mk_unary_minus ctx le_n in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.number_constructor [ op_le_n ])
  | LstLen      ->
      let le_lst =
        ZExpr.mk_app ctx lit_operations.list_accessor [ mk_singleton_access le ]
      in
      let op_le_lst =
        ZExpr.mk_app ctx axiomatised_operations.llen_fun [ le_lst ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.number_constructor [ op_le_lst ])
  | StrLen      ->
      let le_s =
        ZExpr.mk_app ctx lit_operations.string_accessor
          [ mk_singleton_access le ]
      in
      let op_le_s = ZExpr.mk_app ctx axiomatised_operations.slen_fun [ le_s ] in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.number_constructor [ op_le_s ])
  | ToStringOp  ->
      let le_n =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access le ]
      in
      let op_le_n =
        ZExpr.mk_app ctx axiomatised_operations.num2str_fun [ le_n ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.string_constructor [ op_le_n ])
  | ToNumberOp  ->
      let le_s =
        ZExpr.mk_app ctx lit_operations.string_accessor
          [ mk_singleton_access le ]
      in
      let op_le_s =
        ZExpr.mk_app ctx axiomatised_operations.str2num_fun [ le_s ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.number_constructor [ op_le_s ])
  | ToIntOp     ->
      let le_n =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access le ]
      in
      let op_le_n =
        ZExpr.mk_app ctx axiomatised_operations.num2int_fun [ le_n ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.number_constructor [ op_le_n ])
  | UNot        ->
      let le_b =
        ZExpr.mk_app ctx lit_operations.boolean_accessor
          [ mk_singleton_access le ]
      in
      let op_le_b = Boolean.mk_not ctx le_b in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.boolean_constructor [ op_le_b ])
  | Cdr         ->
      let le_lst =
        ZExpr.mk_app ctx lit_operations.list_accessor [ mk_singleton_access le ]
      in
      let op_le_lst =
        ZExpr.mk_app ctx list_operations.tail_accessor [ le_lst ]
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.list_constructor [ op_le_lst ])
  | Car         ->
      let le_lst =
        ZExpr.mk_app ctx lit_operations.list_accessor [ mk_singleton_access le ]
      in
      let op_le = ZExpr.mk_app ctx list_operations.head_accessor [ le_lst ] in
      mk_singleton_elem op_le
  | TypeOf      ->
      let res = typeof_expression le in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.type_constructor [ res ])
  | ToUint32Op  ->
      let le_n =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access le ]
      in
      let op_le_n =
        Arithmetic.Integer.mk_int2real ctx
          (Arithmetic.Real.mk_real2int ctx le_n)
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.number_constructor [ op_le_n ])
  | LstRev      ->
      let le_lst =
        ZExpr.mk_app ctx lit_operations.list_accessor [ mk_singleton_access le ]
      in
      let n_le = ZExpr.mk_app ctx axiomatised_operations.lrev_fun [ le_lst ] in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.list_constructor [ n_le ])
  | _           ->
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
      ZExpr.mk_app ctx extended_literal_operations.set_constructor [ le ]
  | SetInter ->
      let le = Set.mk_intersection ctx les in
      ZExpr.mk_app ctx extended_literal_operations.set_constructor [ le ]
  | LstCat   ->
      let n_le =
        List.fold_left
          (fun ac next ->
            (* Unpack ac *)
            let ac = ZExpr.mk_app ctx lit_operations.list_accessor [ ac ] in
            (* Unpack next one *)
            let next = ZExpr.mk_app ctx lit_operations.list_accessor [ next ] in
            ZExpr.mk_app ctx axiomatised_operations.lcat_fun [ ac; next ])
          (List.hd les) (List.tl les)
      in
      (* Repack *)
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.list_constructor [ n_le ])

let rec encode_logical_expression (le : Expr.t) : ZExpr.expr =
  let f = encode_logical_expression in

  match le with
  | Lit lit              -> encode_lit lit
  | LVar var             -> ZExpr.mk_const ctx (mk_string_symb var)
                              extended_literal_sort
  | ALoc var             -> ZExpr.mk_const ctx (mk_string_symb var)
                              extended_literal_sort
  | PVar _               -> raise
                              (Failure "Program variable in pure formula: FIRE")
  | UnOp (op, le)        -> encode_unop op (f le)
  | BinOp (le1, op, le2) -> encode_binop op (f le1) (f le2)
  | NOp (op, les)        ->
      let les =
        match op with
        | SetInter | SetUnion ->
            List.map
              (fun le ->
                ZExpr.mk_app ctx extended_literal_operations.set_accessor
                  [ f le ])
              les
        | LstCat              -> List.map
                                   (fun le -> mk_singleton_access (f le))
                                   les
      in
      encode_nop op les
  | EList les            ->
      let args = List.map (fun le -> mk_singleton_access (f le)) les in
      let arg_list =
        mk_z3_list args list_operations.nil_constructor
          list_operations.cons_constructor
      in
      mk_singleton_elem
        (ZExpr.mk_app ctx lit_operations.list_constructor [ arg_list ])
  | ESet les             ->
      let args = List.map (fun le -> mk_singleton_access (f le)) les in
      let arg_list = mk_z3_set args in
      ZExpr.mk_app ctx extended_literal_operations.set_constructor [ arg_list ]
  | _                    ->
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
      ZExpr.mk_app ctx extended_literal_operations.singular_elem_recognizer
        [ le_x ]
    in
    let a2 = ZExpr.mk_app ctx f [ mk_singleton_access le_x ] in
    Boolean.mk_and ctx [ a1; a2 ]
  in

  match t_x with
  | UndefinedType -> non_set_type_recognizer lit_operations.undefined_recognizer
  | NullType      -> non_set_type_recognizer lit_operations.null_recognizer
  | EmptyType     -> non_set_type_recognizer lit_operations.empty_recognizer
  | NoneType      -> non_set_type_recognizer lit_operations.none_recognizer
  | BooleanType   -> non_set_type_recognizer lit_operations.boolean_recognizer
  | IntType       -> non_set_type_recognizer lit_operations.int_recognizer
  | NumberType    -> non_set_type_recognizer lit_operations.number_recognizer
  | StringType    -> non_set_type_recognizer lit_operations.string_recognizer
  | ObjectType    -> non_set_type_recognizer lit_operations.loc_recognizer
  | ListType      -> non_set_type_recognizer lit_operations.list_recognizer
  | TypeType      -> non_set_type_recognizer lit_operations.type_recognizer
  | SetType       ->
      ZExpr.mk_app ctx extended_literal_operations.set_recognizer [ le_x ]

let rec encode_assertion (a : Formula.t) : ZExpr.expr =
  let f = encode_assertion in
  let fe = encode_logical_expression in

  match a with
  | Not a             -> Boolean.mk_not ctx (f a)
  | Eq (le1, le2)     -> Boolean.mk_eq ctx (fe le1) (fe le2)
  | Less (le1, le2)   ->
      let le1' =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access (fe le1) ]
      in
      let le2' =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access (fe le2) ]
      in
      mk_lt ctx le1' le2'
  | LessEq (le1, le2) ->
      let le1' =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access (fe le1) ]
      in
      let le2' =
        ZExpr.mk_app ctx lit_operations.number_accessor
          [ mk_singleton_access (fe le2) ]
      in
      mk_le ctx le1' le2'
  | StrLess (_, _)    -> raise (Failure "Z3 encoding does not support STRLESS")
  | True              -> Boolean.mk_true ctx
  | False             -> Boolean.mk_false ctx
  | Or (a1, a2)       -> Boolean.mk_or ctx [ f a1; f a2 ]
  | And (a1, a2)      -> Boolean.mk_and ctx [ f a1; f a2 ]
  | SetMem (le1, le2) ->
      let le1' = mk_singleton_access (fe le1) in
      let le2' =
        ZExpr.mk_app ctx extended_literal_operations.set_accessor [ fe le2 ]
      in
      Set.mk_membership ctx le1' le2'
  | SetSub (le1, le2) ->
      let le1' =
        ZExpr.mk_app ctx extended_literal_operations.set_accessor [ fe le1 ]
      in
      let le2' =
        ZExpr.mk_app ctx extended_literal_operations.set_accessor [ fe le2 ]
      in
      Set.mk_subset ctx le1' le2'
  | ForAll (bt, a)    ->
      let z3_sorts = List.map (fun x -> extended_literal_sort) bt in
      let bt_with_some = List.filter (fun (x, t_x) -> t_x <> None) bt in
      let z3_types_assertions =
        List.map
          (fun (x, t_x) -> make_recognizer_assertion x (Option.get t_x))
          bt_with_some
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

let string_of_z3_expr_list exprs =
  List.fold_left
    (fun ac e ->
      let e_str = ZExpr.to_string e in
      if ac = "" then e_str else ac ^ ",\n" ^ e_str)
    "" exprs

let print_model solver =
  let model = Solver.get_model solver in
  match model with
  | Some model ->
      let str_model = Model.to_string model in
      L.(verboser (fun m -> m "I found the model: \n\n%s" str_model))
  | None       -> L.(verboser (fun m -> m "No model found."))

let string_of_solver solver =
  let exprs = Solver.get_assertions solver in
  string_of_z3_expr_list exprs

let encode_gamma gamma =
  let gamma_var_type_pairs = TypEnv.get_var_type_pairs gamma in
  let encoded_gamma =
    List.filter
      (fun x -> x <> Boolean.mk_true ctx)
      (List.map
         (fun (x, t_x) ->
           if is_lvar_name x || is_aloc_name x then
             make_recognizer_assertion x t_x
           else Boolean.mk_true ctx)
         gamma_var_type_pairs)
  in
  encoded_gamma

(** For a given set of pure formulae and its associated gamma, return the corresponding encoding *)
let encode_assertions (assertions : Formula.Set.t) (gamma : TypEnv.t) :
    ZExpr.expr list =
  (* Check if the assertions have previously been cached *)
  let cached = Hashtbl.mem encoding_cache assertions in
  let result =
    match cached with
    (* Cached, return from cache *)
    | true -> Hashtbl.find encoding_cache assertions
    (* Not cached *)
    | false ->
        (* Encode assertions *)
        let encoded_assertions =
          List.map encode_assertion_top_level (Formula.Set.elements assertions)
        in
        (* Encode gamma *)
        let encoded_assertions = encode_gamma gamma @ encoded_assertions in
        (* Cache *)
        Hashtbl.replace encoding_cache assertions encoded_assertions;
        encoded_assertions
  in
  (* Return *)
  result

let check_sat_core (fs : Formula.Set.t) (gamma : TypEnv.t) : Model.model option
    =
  L.(
    verboser (fun m ->
        m "@[<v 2>About to check SAT of:@\n%a@]@\nwith gamma: @[%a@]\n"
          (Fmt.iter ~sep:(Fmt.any "@\n") Formula.Set.iter Formula.pp)
          fs TypEnv.pp gamma));

  (* Step 1: Reset the solver and add the encoded formulae *)
  let encoded_assertions = encode_assertions fs gamma in

  (* Step 2: Reset the solver and add the encoded formulae *)
  let masterSolver = Solver.mk_solver ctx None in
  Solver.add masterSolver encoded_assertions;
  L.(
    verboser (fun m ->
        m "SAT: About to check the following:\n%s"
          (string_of_solver masterSolver)));

  (* Step 3: Check satisfiability *)
  (* let t = Sys.time () in *)
  L.verboser (fun x -> x "Aqui!");
  let ret = Solver.check masterSolver [] in
  (* Utils.Statistics.update_statistics "Solver check" (Sys.time () -. t); *)
  L.(
    verbose (fun m -> m "The solver returned: %s" (Solver.string_of_status ret)));

  (* Step 4: BREAK if ret = UNKNOWN *)
  if ret = Solver.UNKNOWN then (
    Format.printf
      "FATAL ERROR: Z3 returned UNKNOWN for SAT question:\n\
       %a\n\
       with gamma:\n\
       @[%a@]@?"
      (Fmt.iter ~sep:(Fmt.any ", ") Formula.Set.iter Formula.pp)
      fs TypEnv.pp gamma;
    exit 1 );

  (* Step 5: RETURN *)
  let ret = ret = Solver.SATISFIABLE in

  if ret then Solver.get_model masterSolver else None

let check_sat (fs : Formula.Set.t) (gamma : TypEnv.t) : bool =
  let cached = Hashtbl.mem sat_cache fs in
  let ret =
    if cached then (
      let result = Hashtbl.find sat_cache fs in
      L.(verboser (fun m -> m "SAT check cached with result: %b" result));
      result )
    else (
      L.(verboser (fun m -> m "SAT check not found in cache."));
      let ret = check_sat_core fs gamma in

      L.(
        verboser (fun m ->
            m "Adding to cache : @[%a@]" Formula.pp
              (Formula.conjunct (Formula.Set.elements fs))));

      let result =
        match ret with
        | None   -> false
        | Some _ -> true
      in
      Hashtbl.replace sat_cache fs result;
      result )
  in

  ret

let lift_z3_model
    (model : Model.model)
    (gamma : TypEnv.t)
    (subst : SSubst.t)
    (target_vars : SS.t) : unit =
  let recover_z3_number (n : ZExpr.expr) : float option =
    if ZExpr.is_numeral n then (
      L.(verboser (fun m -> m "Z3 number: %s" (ZExpr.to_string n)));
      Some (float_of_string (Z3.Arithmetic.Real.to_decimal_string n 16)) )
    else None
  in

  let recover_z3_int (zn : ZExpr.expr) : int option =
    let n = recover_z3_number zn in
    Option.map int_of_float n
  in

  let lift_z3_val (x : string) : Expr.t option =
    match TypEnv.get gamma x with
    | None            -> None
    | Some NumberType ->
        let x' = encode_logical_expression (LVar x) in
        let x'' =
          ZExpr.mk_app ctx lit_operations.number_accessor
            [ mk_singleton_access x' ]
        in
        let v = Model.eval model x'' true in
        let n = Option.fold ~some:recover_z3_number ~none:None v in
        Option.map (fun x -> Expr.Lit (Num x)) n
    | Some StringType -> (
        let x' = encode_logical_expression (LVar x) in
        let x'' =
          ZExpr.mk_app ctx lit_operations.string_accessor
            [ mk_singleton_access x' ]
        in
        let v = Model.eval model x'' true in
        let si = Option.fold ~some:recover_z3_int ~none:None v in
        match
          Option.fold ~some:(Hashtbl.find_opt str_codes_inv) ~none:None si
        with
        | Some s -> Some (Expr.Lit (String s))
        | _      -> None )
    | _               -> None
  in

  L.(verboser (fun m -> m "Inside lift_z3_model"));
  SS.iter
    (fun x ->
      let v = lift_z3_val x in
      L.(
        verboser (fun m ->
            m "Z3 binding for %s: %s\n" x
              (Option.fold ~some:(Fmt.to_to_string Expr.pp) ~none:"NO BINDING!"
                 v)));
      Option.fold ~some:(SSubst.put subst x) ~none:() v)
    target_vars
