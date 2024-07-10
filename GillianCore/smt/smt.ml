open Gil_syntax
open Utils
open Simple_smt
open Syntaxes.Option
open Ctx
module L = Logging

exception SMT_unknown

let is_true = function
  | Sexplib.Sexp.Atom "true" -> true
  | _ -> false

type typenv = (string, Type.t) Hashtbl.t

let pp_typenv = Fmt.(Dump.hashtbl string (Fmt.of_to_string Type.str))

let encoding_cache : (Formula.Set.t, sexp list) Hashtbl.t =
  Hashtbl.create Config.big_tbl_size

let sat_cache : (Formula.Set.t, sexp option) Hashtbl.t =
  Hashtbl.create Config.big_tbl_size

let ctx = Ctx.make ()
let ( <| ) constr e = app constr [ e ]
let ( $$ ) constr l = app constr l

let rec substitute ~from ~to_ s =
  let open Sexplib.Sexp in
  let found : found option =
    match search_physical s ~contained:from with
    | `Not_found -> None
    | `Found -> Some `Found
    | `Pos x -> Some (`Pos x)
  in
  match found with
  | None -> s
  | Some found ->
      let s' = subst_found s ~subst:to_ found in
      substitute ~from ~to_ s'

let rec substitute_all s = function
  | [] -> s
  | (from, to_) :: rest ->
      let s' = substitute ~from ~to_ s in
      substitute_all s' rest

let quant q (vars : (sexp * sexp) list) (s : sexp) : sexp =
  let vars = vars |> List.map (fun (v, t) -> list [ v; t ]) in
  app (atom q) [ list vars; s ]

let forall' = quant "forall"

let forall (vars : (string * sexp) list) (s : sexp) : sexp =
  let vars = vars |> List.map (fun (v, t) -> (atom v, t)) in
  forall' vars s

let exists' = quant "exists"

let exists (vars : (string * sexp) list) (s : sexp) : sexp =
  let vars = vars |> List.map (fun (v, t) -> (atom v, t)) in
  exists' vars s

let t_seq t = list [ atom "Seq"; t ]
let seq_len s = atom "seq.len" <| s
let seq_extract s offset length = atom "seq.extract" $$ [ s; offset; length ]
let seq_nth s offset = atom "seq.nth" $$ [ s; offset ]
let seq_concat ss = atom "seq.++" $$ ss
let seq_unit s = atom "seq.unit" <| s

let set_union' ext xs =
  let f =
    match ext with
    | CVC5 -> atom "set.union"
    | _ -> atom "union"
  in
  f $$ xs

let set_intersection' ext xs =
  let f =
    match ext with
    | CVC5 -> atom "set.inter"
    | _ -> atom "intersection"
  in
  f $$ xs

module Variant = struct
  module type Nullary = sig
    val constructor : string * (string * sexp) list
    val construct : sexp
    val recognize : sexp -> sexp
  end

  module type Unary = sig
    include Nullary

    val construct : sexp -> sexp
    val access : sexp -> sexp
  end

  let nul ?recognizer name =
    let recognizer = Option.value recognizer ~default:("is" ^ name) in
    let module M = struct
      let constructor = (name, [])
      let construct = atom name $$ []

      (* let recognizer_decl =
         let pat = PCon (name, [ "_" ]) in
         define_fun recognizer [ ("x", typ) ] t_bool
           (match_datatype (atom "x") [ pat, (bool_k true) ]) *)
      let recognize x = atom recognizer <| x
    end in
    (module M : Nullary)

  let un ?recognizer name param param_typ =
    let module N = (val nul ?recognizer name : Nullary) in
    let module M = struct
      include N

      let constructor = (name, [ (param, param_typ) ])
      let construct x = atom name <| x
      let accessor = atom param
      let access x = accessor <| x
    end in
    (module M : Unary)
end

module Type_operations = struct
  open Variant
  module Undefined = (val nul "UndefinedType" : Nullary)
  module Null = (val nul "NullType" : Nullary)
  module Empty = (val nul "EmptyType" : Nullary)
  module None = (val nul "NoneType" : Nullary)
  module Boolean = (val nul "BooleanType" : Nullary)
  module Int = (val nul "IntType" : Nullary)
  module Number = (val nul "NumberType" : Nullary)
  module String = (val nul "StringType" : Nullary)
  module Object = (val nul "ObjectType" : Nullary)
  module List = (val nul "ListType" : Nullary)
  module Type = (val nul "TypeType" : Nullary)
  module Set = (val nul "SetType" : Nullary)

  let t_gil_type =
    declare_datatype ctx "GIL_Type" []
      [
        Undefined.constructor;
        Null.constructor;
        Empty.constructor;
        None.constructor;
        Boolean.constructor;
        Int.constructor;
        Number.constructor;
        String.constructor;
        Object.constructor;
        List.constructor;
        Type.constructor;
        Set.constructor;
      ]
end

let t_gil_type = Type_operations.t_gil_type

module Lit_operations = struct
  open Variant

  let gil_literal_name = "GIL_Literal"
  let t_gil_literal = atom gil_literal_name

  module Undefined = (val nul "Undefined" : Nullary)
  module Null = (val nul "Null" : Nullary)
  module Empty = (val nul "Empty" : Nullary)
  module Bool = (val un "Bool" "bValue" t_bool : Unary)
  module Int = (val un "Int" "iValue" t_int : Unary)
  module Num = (val un "Num" "nValue" t_real : Unary)
  module String = (val un "String" "sValue" t_int : Unary)
  module Loc = (val un "Loc" "locValue" t_int : Unary)
  module Type = (val un "Type" "tValue" t_gil_type : Unary)
  module List = (val un "List" "listValue" (t_seq t_gil_literal) : Unary)
  module None = (val nul "None" : Nullary)

  let () =
    declare_datatype' ctx gil_literal_name []
      [
        Undefined.constructor;
        Null.constructor;
        Empty.constructor;
        Bool.constructor;
        Int.constructor;
        Num.constructor;
        String.constructor;
        Loc.constructor;
        Type.constructor;
        List.constructor;
        None.constructor;
      ]
end

let t_gil_literal = Lit_operations.t_gil_literal
let t_gil_literal_list = t_seq t_gil_literal
let t_gil_literal_set = t_set t_gil_literal

let seq_of = function
  | [] -> atom "seq.empty"
  | xs -> xs |> List.map seq_unit |> seq_concat

let set_of xs =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (set_insert Z3 x acc) xs
  in
  aux (set_empty Z3 t_gil_literal) xs

module Ext_lit_operations = struct
  open Variant

  module Gil_sing_elem =
    (val un "Elem" ~recognizer:"isSingular" "singElem" t_gil_literal : Unary)

  module Gil_set = (val un "Set" "setElem" t_gil_literal_set : Unary)

  let t_gil_ext_literal =
    declare_datatype ctx "Extended_GIL_Literal" []
      [ Gil_sing_elem.constructor; Gil_set.constructor ]
end

module Axiomatised_operations = struct
  let slen = declare_fun ctx "s-len" [ t_int ] t_real
  let llen = declare_fun ctx "l-len" [ t_gil_literal_list ] t_int
  let num2str = declare_fun ctx "num2str" [ t_real ] t_int
  let str2num = declare_fun ctx "str2num" [ t_int ] t_real
  let num2int = declare_fun ctx "num2int" [ t_real ] t_real
  let snth = declare_fun ctx "s-nth" [ t_int; t_real ] t_int
  let lrev = declare_fun ctx "l-rev" [ t_gil_literal_list ] t_gil_literal_list
end

let t_gil_ext_literal = Ext_lit_operations.t_gil_ext_literal
let str_codes = Hashtbl.create 1000
let str_codes_inv = Hashtbl.create 1000
let str_counter = ref 0

(** We only check for string equality; each unique string is assigned a code,
    and the solver can check for equality by checking equality of the codes. *)
let encode_string s =
  match Hashtbl.find_opt str_codes s with
  | Some code -> int_k code
  | None ->
      let code = int_k !str_counter in
      let () = Hashtbl.add str_codes s !str_counter in
      let () = Hashtbl.add str_codes_inv !str_counter s in
      let () = incr str_counter in
      code

let encode_type (t : Type.t) =
  try
    match t with
    | UndefinedType -> Type_operations.Undefined.construct
    | NullType -> Type_operations.Null.construct
    | EmptyType -> Type_operations.Empty.construct
    | NoneType -> Type_operations.None.construct
    | BooleanType -> Type_operations.Boolean.construct
    | IntType -> Type_operations.Int.construct
    | NumberType -> Type_operations.Number.construct
    | StringType -> Type_operations.String.construct
    | ObjectType -> Type_operations.Object.construct
    | ListType -> Type_operations.List.construct
    | TypeType -> Type_operations.Type.construct
    | SetType -> Type_operations.Set.construct
  with _ -> Fmt.failwith "DEATH: encode_type with arg: %a" Type.pp t

module Encoding = struct
  type kind =
    | Native of Type.t
        (** This value encodes to an SMTLIB native type, like Int or Seq *)
    | Simple_wrapped  (** Cannot be a set *)
    | Extended_wrapped  (** Can be a set *)

  let native_sort_of_type =
    let open Type in
    function
    | IntType | StringType | ObjectType -> t_int
    | ListType -> t_gil_literal_list
    | BooleanType -> t_bool
    | NumberType -> t_real
    | UndefinedType | NoneType | EmptyType | NullType -> t_gil_literal
    | SetType -> t_gil_literal_set
    | TypeType -> t_gil_type

  type t = { kind : kind; expr : sexp [@main] } [@@deriving make]

  let undefined_encoding =
    make ~kind:Simple_wrapped Lit_operations.Undefined.construct

  let null_encoding = make ~kind:Simple_wrapped Lit_operations.Null.construct
  let empty_encoding = make ~kind:Simple_wrapped Lit_operations.Empty.construct
  let none_encoding = make ~kind:Simple_wrapped Lit_operations.None.construct
  let native typ = make ~kind:(Native typ)
  let ( >- ) expr typ = native typ expr

  let get_native ~accessor { expr; kind; _ } =
    (* No additional check is performed on native type,
       it should be already type checked *)
    match kind with
    | Native _ -> expr
    | Simple_wrapped -> accessor expr
    | Extended_wrapped ->
        accessor (Ext_lit_operations.Gil_sing_elem.access expr)

  let simply_wrapped = make ~kind:Simple_wrapped
  let extended_wrapped = make ~kind:Extended_wrapped

  (** Takes a value either natively encoded or simply wrapped
    and returns a value simply wrapped.
    Careful: do not use wrap with a a set, as they cannot be simply wrapped *)
  let simple_wrap { expr; kind; _ } =
    let open Lit_operations in
    match kind with
    | Simple_wrapped -> expr
    | Native typ ->
        let construct =
          match typ with
          | IntType -> Int.construct
          | NumberType -> Num.construct
          | StringType -> String.construct
          | ObjectType -> Loc.construct
          | TypeType -> Type.construct
          | BooleanType -> Bool.construct
          | ListType -> List.construct
          | UndefinedType | NullType | EmptyType | NoneType | SetType ->
              Fmt.failwith "Cannot simple-wrap value of type %s"
                (Gil_syntax.Type.str typ)
        in
        construct expr
    | Extended_wrapped -> Ext_lit_operations.Gil_sing_elem.access expr

  let extend_wrap e =
    match e.kind with
    | Extended_wrapped -> e.expr
    | Native SetType -> Ext_lit_operations.Gil_set.construct (simple_wrap e)
    | _ -> Ext_lit_operations.Gil_sing_elem.construct (simple_wrap e)

  let get_num = get_native ~accessor:Lit_operations.Num.access
  let get_int = get_native ~accessor:Lit_operations.Int.access
  let get_bool = get_native ~accessor:Lit_operations.Bool.access
  let get_list = get_native ~accessor:Lit_operations.List.access

  let get_set { kind; expr; _ } =
    match kind with
    | Native SetType -> expr
    | Extended_wrapped -> Ext_lit_operations.Gil_set.access expr
    | _ -> failwith "wrong encoding of set"

  let get_string = get_native ~accessor:Lit_operations.String.access
end

(* TODO: just use a function to replace. It looks like placeholders are unnecessary? *)
let placeholder_sw = declare ctx "placeholder" t_gil_literal
let placeholder_ew = declare ctx "placeholder" t_gil_ext_literal
let placeholder_else = declare ctx "placeholder" t_gil_type

let ready_to_subst_expr_for_simply_wrapped_typeof =
  let open Type in
  let guards =
    Lit_operations.
      [
        (Null.recognize placeholder_sw, NullType);
        (Empty.recognize placeholder_sw, EmptyType);
        (Undefined.recognize placeholder_sw, UndefinedType);
        (None.recognize placeholder_sw, NoneType);
        (Bool.recognize placeholder_sw, BooleanType);
        (Int.recognize placeholder_sw, IntType);
        (Num.recognize placeholder_sw, NumberType);
        (String.recognize placeholder_sw, StringType);
        (Loc.recognize placeholder_sw, ObjectType);
        (Type.recognize placeholder_sw, TypeType);
        (List.recognize placeholder_sw, ListType);
      ]
  in
  List.fold_left
    (fun acc (guard, typ) -> ite guard (encode_type typ) acc)
    (encode_type UndefinedType)
    guards

let ready_to_subst_expr_for_extended_wrapped_typeof =
  let set_guard = Ext_lit_operations.Gil_set.recognize placeholder_ew in
  ite set_guard (encode_type SetType) placeholder_else

let typeof_expression ({ kind; expr; _ } : Encoding.t) =
  match kind with
  | Native typ -> encode_type typ
  | Simple_wrapped ->
      substitute ~from:placeholder_sw ~to_:expr
        ready_to_subst_expr_for_simply_wrapped_typeof
  | Extended_wrapped ->
      let else_subst =
        substitute ~from:placeholder_sw
          ~to_:(Ext_lit_operations.Gil_sing_elem.access expr)
          ready_to_subst_expr_for_simply_wrapped_typeof
      in
      substitute_all ready_to_subst_expr_for_extended_wrapped_typeof
        [ (placeholder_ew, expr); (placeholder_else, else_subst) ]

module RepeatCache = struct
  let cache = Hashtbl.create 0
  let var_counter = ref 0

  let make_var counter =
    declare ctx ("__repeat_var_" ^ string_of_int counter) t_gil_literal_list

  let next_var () =
    let ret = !var_counter in
    let () = incr var_counter in
    make_var ret

  let clear_var_counter () = var_counter := 0
  let index = atom "__index__"

  let clear () =
    let () = Hashtbl.clear cache in
    clear_var_counter ()

  let add_constraints var x n =
    let at_index_is_x = eq (seq_nth var index) x in
    let length = seq_len var in
    let all_eq_x = forall' [ (index, t_int) ] at_index_is_x in
    let length_is_n = eq length n in
    let () = assume ctx all_eq_x in
    let () = assume ctx length_is_n in
    ()

  let get x n =
    let- () = Hashtbl.find_opt cache (x, n) in
    let var = next_var () in
    let () = add_constraints var x n in
    let () = Hashtbl.add cache (x, n) var in
    var
end

let rec encode_lit (lit : Literal.t) : Encoding.t =
  let open Encoding in
  try
    match lit with
    | Undefined -> undefined_encoding
    | Null -> null_encoding
    | Empty -> empty_encoding
    | Nono -> none_encoding
    | Bool b -> bool_k b >- BooleanType
    | Int i -> int_zk i >- IntType
    | Num n -> real_k (Q.of_float n) >- NumberType
    | String s -> encode_string s >- StringType
    | Loc l -> encode_string l >- ObjectType
    | Type t -> encode_type t >- TypeType
    | LList lits ->
        let args = List.map (fun lit -> simple_wrap (encode_lit lit)) lits in
        list args >- ListType
    | Constant _ -> raise (Exceptions.Unsupported "Z3 encoding: constants")
  with Failure msg ->
    Fmt.failwith "DEATH: encode_lit %a. %s" Literal.pp lit msg

let encode_equality (p1 : Encoding.t) (p2 : Encoding.t) : Encoding.t =
  let open Encoding in
  let res =
    match (p1.kind, p2.kind) with
    | Native t1, Native t2 when Type.equal t1 t2 ->
        if Type.equal t1 BooleanType then
          if is_true p1.expr then p2.expr
          else if is_true p2.expr then p1.expr
          else eq p1.expr p2.expr
        else eq p1.expr p2.expr
    | Simple_wrapped, Simple_wrapped | Extended_wrapped, Extended_wrapped ->
        eq p1.expr p2.expr
    | Native _, Native _ -> failwith "incompatible equality, type error!"
    | Simple_wrapped, Native _ | Native _, Simple_wrapped ->
        eq (simple_wrap p1) (simple_wrap p2)
    | Extended_wrapped, _ | _, Extended_wrapped ->
        eq (extend_wrap p1) (extend_wrap p2)
  in
  res >- BooleanType

let encode_binop (op : BinOp.t) (p1 : Encoding.t) (p2 : Encoding.t) : Encoding.t
    =
  let open Encoding in
  (* In the case of strongly typed operations, we do not perform any check.
     Type checking has happened before reaching z3, and therefore, isn't required here again.
     An unknown type is represented by the [None] variant of the option type.
     It is expected that values of unknown type are already wrapped into their constructors.
  *)
  match op with
  | IPlus -> num_add (get_int p1) (get_int p2) >- IntType
  | IMinus -> num_sub (get_int p1) (get_int p2) >- IntType
  | ITimes -> num_mul (get_int p1) (get_int p2) >- IntType
  | IDiv -> num_div (get_int p1) (get_int p2) >- IntType
  | IMod -> num_mod (get_int p1) (get_int p2) >- IntType
  | ILessThan -> num_lt (get_int p1) (get_int p2) >- BooleanType
  | ILessThanEqual -> num_leq (get_int p1) (get_int p2) >- BooleanType
  | FPlus -> num_add (get_num p1) (get_num p2) >- NumberType
  | FMinus -> num_sub (get_num p1) (get_num p2) >- NumberType
  | FTimes -> num_mul (get_num p1) (get_num p2) >- NumberType
  | FDiv -> num_div (get_num p1) (get_num p2) >- NumberType
  | FLessThan -> num_lt (get_num p1) (get_num p2) >- BooleanType
  | FLessThanEqual -> num_leq (get_num p1) (get_num p2) >- BooleanType
  | Equal -> encode_equality p1 p2
  | BOr -> bool_or (get_bool p1) (get_bool p2) >- BooleanType
  | BAnd -> bool_and (get_bool p1) (get_bool p2) >- BooleanType
  | BSetMem ->
      (* p2 has to be already wrapped *)
      set_member Z3 (simple_wrap p1) (get_set p2) >- BooleanType
  | SetDiff -> set_difference Z3 (get_set p1) (get_set p2) >- SetType
  | BSetSub -> set_subset Z3 (get_set p1) (get_set p2) >- BooleanType
  | LstNth -> seq_nth (get_list p1) (get_int p2) |> simply_wrapped
  | LstRepeat ->
      let x = simple_wrap p1 in
      let n = get_int p2 in
      RepeatCache.get x n >- ListType
  | StrNth ->
      let str' = get_string p1 in
      let index' = get_num p2 in
      let res = Axiomatised_operations.snth $$ [ str'; index' ] in
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
      Fmt.failwith "SMT encoding: Costruct not supported yet - binop: %s"
        (BinOp.str op)

let encode_unop ~llen_lvars ~e (op : UnOp.t) le =
  let open Encoding in
  let open Axiomatised_operations in
  match op with
  | IUnaryMinus -> num_neg (get_int le) >- IntType
  | FUnaryMinus -> num_neg (get_num le) >- NumberType
  | LstLen ->
      (* If we only use an LVar as an argument to llen, then encode it as an uninterpreted function. *)
      let enc =
        match e with
        | Expr.LVar l when SS.mem l llen_lvars -> llen <| get_list le
        | _ -> seq_len (get_list le)
      in
      enc >- IntType
  | StrLen -> slen <| get_string le >- NumberType
  | ToStringOp -> Axiomatised_operations.num2str <| get_num le >- StringType
  | ToNumberOp -> Axiomatised_operations.str2num <| get_string le >- NumberType
  | ToIntOp -> Axiomatised_operations.num2int <| get_num le >- NumberType
  | UNot -> bool_not (get_bool le) >- BooleanType
  | Cdr ->
      let list = get_list le in
      seq_extract list (int_k 1) (seq_len list) >- ListType
  | Car -> seq_nth (get_list le) (int_k 0) |> simply_wrapped
  | TypeOf -> typeof_expression le >- TypeType
  | ToUint32Op -> get_num le |> real_to_int |> int_to_real >- NumberType
  | LstRev -> Axiomatised_operations.lrev <| get_list le >- ListType
  | NumToInt -> get_num le |> real_to_int >- IntType
  | IntToNum -> get_int le |> int_to_real >- NumberType
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
        Fmt.str "SMT encoding: Construct not supported yet - unop - %s!"
          (UnOp.str op)
      in
      let () = L.print_to_all msg in
      raise (Failure msg)

let encode_quantified_expr
    ~(encode_expr : gamma:typenv -> llen_lvars:SS.t -> 'a -> Encoding.t)
    ~mk_quant
    ~gamma
    ~llen_lvars
    quantified_vars
    (assertion : 'a) : Encoding.t =
  let open Encoding in
  let- () =
    match quantified_vars with
    | [] ->
        (* A quantified assertion with no quantified variables is just the assertion *)
        Some (encode_expr ~gamma ~llen_lvars assertion)
    | _ -> None
  in
  (* Start by updating gamma with the information provided by quantifier types.
     There's very few foralls, so it's ok to copy the gamma entirely *)
  let gamma = Hashtbl.copy gamma in
  let () =
    quantified_vars
    |> List.iter (fun (x, typ) ->
           match typ with
           | None -> Hashtbl.remove gamma x
           | Some typ -> Hashtbl.replace gamma x typ)
  in
  (* Not the same gamma now!*)
  let encoded_assertion =
    match encode_expr ~gamma ~llen_lvars assertion with
    | { kind = Native BooleanType; expr; _ } -> expr
    | _ -> failwith "the thing inside forall is not boolean!"
  in
  let quantified_vars =
    quantified_vars
    |> List.map (fun (x, t) ->
           let sort =
             match t with
             | None -> t_gil_ext_literal
             | Some typ -> Encoding.native_sort_of_type typ
           in
           (x, sort))
  in
  mk_quant quantified_vars encoded_assertion >- BooleanType

let rec encode_logical_expression
    ~(gamma : typenv)
    ~(llen_lvars : SS.t)
    (le : Expr.t) : Encoding.t =
  let open Encoding in
  let f = encode_logical_expression ~gamma ~llen_lvars in

  match le with
  | Lit lit -> encode_lit lit
  | LVar var -> (
      match Hashtbl.find_opt gamma var with
      | Some typ -> declare ctx var (native_sort_of_type typ) >- typ
      | None -> declare ctx var t_gil_ext_literal |> extended_wrapped)
  | ALoc var -> declare ctx var t_int >- ObjectType
  | PVar _ -> failwith "HORROR: Program variable in pure formula"
  | UnOp (op, le) -> encode_unop ~llen_lvars ~e:le op (f le)
  | BinOp (le1, op, le2) -> encode_binop op (f le1) (f le2)
  | NOp (SetUnion, les) ->
      let les = les |> List.map (fun le -> get_set (f le)) in
      set_union' Z3 les >- SetType
  | NOp (SetInter, les) ->
      let les = les |> List.map (fun le -> get_set (f le)) in
      set_intersection' Z3 les >- SetType
  | NOp (LstCat, les) ->
      let les = les |> List.map (fun le -> get_list (f le)) in
      seq_concat les >- ListType
  | EList les ->
      let args = List.map (fun le -> simple_wrap (f le)) les in
      seq_of args >- ListType
  | ESet les ->
      let args = List.map (fun le -> simple_wrap (f le)) les in
      set_of args >- SetType
  | LstSub (lst, start, len) ->
      let lst = get_list (f lst) in
      let start = get_int (f start) in
      let len = get_int (f len) in
      seq_extract lst start len >- ListType
  | Exists (bt, e) ->
      encode_quantified_expr ~encode_expr:encode_logical_expression
        ~mk_quant:exists ~gamma ~llen_lvars bt e
  | EForall (bt, e) ->
      encode_quantified_expr ~encode_expr:encode_logical_expression
        ~mk_quant:forall ~gamma ~llen_lvars bt e

and encode_assertion ~(gamma : typenv) ~(llen_lvars : SS.t) (a : Formula.t) :
    Encoding.t =
  let f = encode_assertion ~gamma ~llen_lvars in
  let fe = encode_logical_expression ~gamma ~llen_lvars in
  let open Encoding in
  match a with
  | Not a -> get_bool (f a) |> bool_not >- BooleanType
  | Eq (le1, le2) -> encode_equality (fe le1) (fe le2)
  | FLess (le1, le2) ->
      num_lt (get_num (fe le1)) (get_num (fe le2)) >- BooleanType
  | FLessEq (le1, le2) ->
      num_leq (get_num (fe le1)) (get_num (fe le2)) >- BooleanType
  | ILess (le1, le2) ->
      num_lt (get_int (fe le1)) (get_int (fe le2)) >- BooleanType
  | ILessEq (le1, le2) ->
      num_leq (get_int (fe le1)) (get_int (fe le2)) >- BooleanType
  | Impl (a1, a2) ->
      bool_implies (get_bool (f a1)) (get_bool (f a2)) >- BooleanType
  | StrLess (_, _) -> failwith "SMT encoding does not support STRLESS"
  | True -> bool_k true >- BooleanType
  | False -> bool_k false >- BooleanType
  | Or (a1, a2) -> bool_or (get_bool (f a1)) (get_bool (f a2)) >- BooleanType
  | And (a1, a2) -> bool_and (get_bool (f a1)) (get_bool (f a2)) >- BooleanType
  | SetMem (le1, le2) ->
      let le1' = simple_wrap (fe le1) in
      let le2' = get_set (fe le2) in
      set_member Z3 le1' le2' >- BooleanType
  | SetSub (le1, le2) ->
      set_subset Z3 (get_set (fe le1)) (get_set (fe le2)) >- BooleanType
  | ForAll (bt, a) ->
      encode_quantified_expr ~encode_expr:encode_assertion ~mk_quant:forall
        ~gamma ~llen_lvars bt a
  | IsInt e -> num_divisible (get_num (fe e)) 1 >- BooleanType

let encode_assertion_top_level
    ~(gamma : typenv)
    ~(llen_lvars : SS.t)
    (a : Formula.t) : sexp =
  try (encode_assertion ~gamma ~llen_lvars (Formula.push_in_negations a)).expr
  with e ->
    let s = Printexc.to_string e in
    let msg =
      Fmt.str "Failed to encode %a in gamma %a with error %s\n" Formula.pp a
        pp_typenv gamma s
    in
    let () = Logging.print_to_all msg in
    raise e

let lvars_only_in_llen (fs : Formula.Set.t) : SS.t =
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
  fs |> Formula.Set.iter (inspector#visit_formula ());
  inspector#get_diff

let encode_assertions (fs : Formula.Set.t) (gamma : typenv) : sexp list =
  let- () = Hashtbl.find_opt encoding_cache fs in
  let llen_lvars = lvars_only_in_llen fs in
  let encoded =
    Formula.Set.elements fs
    |> List.map (encode_assertion_top_level ~gamma ~llen_lvars)
  in
  let () = Hashtbl.replace encoding_cache fs encoded in
  encoded

let dump_smt =
  let counter = ref 0 in
  let folder =
    let folder_name = "gillian_smt_queries" in
    let created = ref false in
    let create () =
      created := true;
      try Unix.mkdir folder_name 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
      | e -> raise e
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
  fun fs gamma status ctx ->
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
      fs pp_typenv gamma (show_result status) (Ctx.dump ctx)

let reset_solver () =
  let () = pop ctx in
  let () = RepeatCache.clear () in
  let () = push ctx in
  ()

let exec_sat (fs : Formula.Set.t) (gamma : typenv) : sexp option =
  let () =
    L.verbose (fun m ->
        m "@[<v 2>About to check SAT of:@\n%a@]@\nwith gamma:@\n@[%a@]\n"
          (Fmt.iter ~sep:(Fmt.any "@\n") Formula.Set.iter Formula.pp)
          fs pp_typenv gamma)
  in
  let encoded_assertions = encode_assertions fs gamma in
  let () = encoded_assertions |> List.iter (fun a -> assume ctx a) in
  let result = check ctx.solver in
  let () = if !Utils.Config.dump_smt then dump_smt fs gamma result ctx in
  let ret =
    match result with
    | Unknown ->
        if !Utils.Config.under_approximation then raise SMT_unknown
        else
          let msg =
            Fmt.str
              "FATAL ERROR: SMT returned UNKNOWN for SAT question:\n\
               %a\n\
               with gamma:\n\
               @[%a@]\n\n\n\
               Solver:\n\
               %a\n\
               @?"
              (Fmt.iter ~sep:(Fmt.any ", ") Formula.Set.iter Formula.pp)
              fs pp_typenv gamma
              (Fmt.list ~sep:(Fmt.any "\n\n") Sexplib.Sexp.pp_hum)
              encoded_assertions
          in
          let () = L.print_to_all msg in
          exit 1
    | Sat -> Some (get_model ctx.solver)
    | Unsat -> None
  in
  let () = reset_solver () in
  ret

let check_sat (fs : Formula.Set.t) (gamma : typenv) : sexp option =
  match Hashtbl.find_opt sat_cache fs with
  | Some result ->
      let () =
        L.verbose (fun m ->
            m "SAT check cached with result: %b" (Option.is_some result))
      in
      result
  | None ->
      let () = L.verbose (fun m -> m "SAT check not found in cache") in
      let ret = exec_sat fs gamma in
      let () =
        L.verbose (fun m ->
            let f = Formula.conjunct (Formula.Set.elements fs) in
            m "Adding to cache : @[%a@]" Formula.pp f)
      in
      let () = Hashtbl.replace sat_cache fs ret in
      ret

let is_sat (fs : Formula.Set.t) (gamma : typenv) : bool =
  check_sat fs gamma |> Option.is_some

let lift_model
    (model : sexp)
    (gamma : typenv)
    (subst_update : string -> Expr.t -> unit)
    (target_vars : Expr.Set.t) : unit =
  let model_eval = (model_eval z3 model).eval [] in

  let get_val x =
    try model_eval (atom "get-value" <| atom x) |> Option.some
    with UnexpectedSolverResponse _ -> None
  in

  let recover_number (n : sexp) : float option =
    try Some (to_q n |> Q.to_float) with UnexpectedSolverResponse _ -> None
  in

  let recover_int (n : sexp) : Z.t option =
    try Some (to_z n) with UnexpectedSolverResponse _ -> None
  in

  let lift_val (x : string) : Literal.t option =
    let* gil_type = Hashtbl.find_opt gamma x in
    let* v = get_val x in
    match gil_type with
    | NumberType ->
        let+ n = recover_number v in
        Literal.Num n
    | IntType ->
        let+ n = recover_int v in
        Literal.Int n
    | StringType ->
        let* si = recover_int v in
        let+ str_code = Hashtbl.find_opt str_codes_inv (Z.to_int si) in
        Literal.String str_code
    | _ -> None
  in

  let () = L.verbose (fun m -> m "Inside lift_model") in
  target_vars
  |> Expr.Set.iter (fun x ->
         let x =
           match x with
           | LVar x -> x
           | _ ->
               failwith "INTERNAL ERROR: SMT lifting of a non-logical variable"
         in
         let v = lift_val x in
         let () =
           L.verbose (fun m ->
               let binding =
                 v
                 |> Option.fold
                      ~some:(Fmt.to_to_string Literal.pp)
                      ~none:"NO BINDING!"
               in
               m "SMT binding for %s: %s\n" x binding)
         in
         v |> Option.iter (fun v -> subst_update x (Expr.Lit v)))

let () = push ctx
