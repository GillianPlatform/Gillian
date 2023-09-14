type t = Typedefs__.type_ =
  | Array of t * int
  | Bool
  | CInteger of IntType.t
  | Float
  | Double
  | Signedbv of { width : int }
  | Unsignedbv of { width : int }
  | Code of { params : Typedefs__.param list; return_type : t }
  | Pointer of t
  | Struct of { components : Typedefs__.datatype_component list; tag : string }
  | IncompleteStruct of string
  | StructTag of string
  | Union of { components : Typedefs__.datatype_component list; tag : string }
  | UnionTag of string
  | Constructor
  | Empty
  | Vector of { type_ : t; size : int }

let show t = Typedefs__.show_type_ t

let show_simple = function
  | Array _ -> "Array"
  | Bool -> "Bool"
  | CInteger t -> "CInteger::" ^ IntType.show t
  | Float -> "Float"
  | Double -> "Double"
  | Signedbv { width } -> "i" ^ string_of_int width
  | Unsignedbv { width } -> "u" ^ string_of_int width
  | Code _ -> "Code"
  | Pointer _ -> "Pointer"
  | Struct _ -> "Struct"
  | IncompleteStruct _ -> "IncompleteStruct"
  | StructTag _ -> "StructTag"
  | Union _ -> "Union"
  | UnionTag _ -> "UnionTag"
  | Constructor -> "Constructor"
  | Empty -> "Empty"
  | Vector _ -> "Vector"

let pp fmt t = Typedefs__.pp_type_ fmt t
let equal ta tb = Typedefs__.equal_type_ ta tb

let is_function = function
  | Code _ -> true (* Will also be true for variadic code if ever *)
  | _ -> false

open Irep.Infix

(** This feels a bit hacky, maybe the constant-deserialization
    should be factored out somewhere else,
    and used both here and in [Expr]
    For example, here, there are no check that it's of the right type *)
let size_of_irep irep =
  try
    irep $ Value |> Irep.as_just_bitpattern ~signed:false ~width:64 |> Z.to_int
  with Not_found -> Gerror.unexpected ~irep "No size!"

let rec of_irep ~(machine : Machine_model.t) (irep : Irep.t) : t =
  let of_irep = of_irep ~machine in
  let datatype_component_of_irep = datatype_component_of_irep ~machine in
  let unexpected = Gerror.unexpected ~irep in
  let ( $ ) irep name =
    match irep $? name with
    | Some e -> e
    | None -> unexpected ("Couldn't find " ^ Id.to_string name)
  in
  match irep.id with
  | Array ->
      let elem_ty = List.hd irep.sub |> of_irep in
      let sz = size_of_irep (irep $ Size) in
      Array (elem_ty, sz)
  | Vector ->
      let type_ = List.hd irep.sub |> of_irep in
      let size = size_of_irep (irep $ Size) in
      Vector { type_; size }
  | Bool -> Bool
  | Floatbv -> (
      match
        (irep $ Width |> Irep.as_just_int, irep $ F |> Irep.as_just_int)
      with
      | 32, 23 -> Float
      | 64, 52 -> Double
      | _ ->
          unexpected
            "Float bitvector doesn't correspond to standard 32bit or 64bit \
             float")
  | CBool -> CInteger I_bool
  | Unsignedbv -> (
      let width = irep $ Width |> Irep.as_just_int in
      let int_ty = IntType.which_int_type_opt ~machine ~signed:false ~width in
      match int_ty with
      | Some int_ty -> CInteger int_ty
      | None -> Unsignedbv { width })
  | Signedbv -> (
      let width = irep $ Width |> Irep.as_just_int in
      let int_ty = IntType.which_int_type_opt ~machine ~signed:true ~width in
      match int_ty with
      | Some int_ty -> CInteger int_ty
      | None -> Signedbv { width })
  | Code ->
      let param_ireps = irep $ Parameters in
      let params = List.map (param_of_irep ~machine) param_ireps.sub in
      let return_type = of_irep (irep $ ReturnType) in
      Code { params; return_type }
  | Pointer ->
      let points_to = Lift_utils.exactly_one ~msg:"Pointer type" irep in
      let points_to = of_irep points_to in
      Pointer points_to
  | Struct ->
      let incomplete =
        match irep $? Incomplete with
        | Some { id = Id1; _ } -> true
        | _ -> false
      in
      let tag = irep $ Tag |> Irep.as_just_string in
      if incomplete then IncompleteStruct tag
      else
        let components =
          (irep $ Components).sub |> List.map datatype_component_of_irep
        in
        Struct { components; tag }
  | StructTag ->
      let identifier = irep $ Identifier |> Irep.as_just_string in
      StructTag identifier
  | Union ->
      let tag = irep $ Tag |> Irep.as_just_string in
      let components =
        (irep $ Components).sub |> List.map datatype_component_of_irep
      in
      Union { tag; components }
  | UnionTag ->
      let identifier = irep $ Identifier |> Irep.as_just_string in
      UnionTag identifier
  | Constructor -> Constructor
  | Empty -> Empty
  | other -> failwith ("unhandled type: " ^ Id.to_string other)

and param_of_irep ~machine irep =
  let identifier = Option.map Irep.as_just_string (irep $? CIdentifier) in
  let base_name = Option.map Irep.as_just_string (irep $? CBaseName) in
  let type_ = of_irep ~machine (irep $ Type) in
  Typedefs__.{ identifier; base_name; type_ }

and datatype_component_of_irep ~machine irep : Datatype_component.t =
  let is_padding =
    match irep $? CIsPadding with
    | Some { id = Id1; _ } -> true
    | _ -> false
  in
  if is_padding then
    let name = irep $ Name |> Irep.as_just_string in
    let bits = irep $ Type $ Width |> Irep.as_just_int in
    Padding { name; bits }
  else
    let name = irep $ Name |> Irep.as_just_string in
    let type_ = irep $ Type |> of_irep ~machine in
    Field { name; type_ }

let type_in_irep ~machine irep = of_irep ~machine (irep $ Type)

let is_integer = function
  | CInteger _ | Signedbv _ | Unsignedbv _ -> true
  | _ -> false

module Overflow_result = struct
  let rec is_overflow_result ~tag_lookup = function
    | Struct
        {
          components =
            [
              Field { name = "result"; type_ };
              Field { name = "overflowed"; type_ = Bool };
            ];
          _;
        } -> is_integer type_
    | StructTag t -> is_overflow_result ~tag_lookup (tag_lookup t)
    | _ -> false

  type field = Result | Overflowed

  let field s =
    match s with
    | "result" -> Result
    | "overflowed" -> Overflowed
    | _ -> failwith "invalid field for overflow_result access"
end

(** Returns the size of a type in bits *)
let rec bit_size_of ~(machine : Machine_model.t) ~(tag_lookup : string -> t) t =
  let dc_bit_size = bit_size_of_datatype_component ~machine ~tag_lookup in
  let bit_size_of = bit_size_of ~tag_lookup ~machine in
  (* Fmt.pr "bit_size_of: %a\n@?" pp t; *)
  match t with
  | Array (ty, sz) | Vector { type_ = ty; size = sz } -> sz * bit_size_of ty
  | CInteger I_bool -> machine.bool_width
  | CInteger I_int -> machine.int_width
  | CInteger I_char -> machine.char_width
  | CInteger (I_size_t | I_ssize_t) | Pointer _ -> machine.pointer_width
  | Float -> 32
  | Double -> 64
  | Signedbv { width } | Unsignedbv { width } -> width
  | Empty -> 0
  | StructTag x | UnionTag x -> bit_size_of (tag_lookup x)
  | Struct { components; _ } ->
      List.fold_left (fun x y -> x + dc_bit_size y) 0 components
  | Union { components; _ } ->
      (* I don't have to think about aligning everything on the biggest alignment,
         because Kani sends padding fields when necessary *)
      List.fold_left (fun x y -> max x (dc_bit_size y)) 0 components
  | Bool -> Gerror.code_error "bit_size_of Bool"
  | Code _ -> Gerror.code_error "bit_size_of Code"
  | Constructor -> Gerror.code_error "bit_size_of Constructor"
  | IncompleteStruct _ -> Gerror.code_error "bit_size_of IncompleteStruct"

and bit_size_of_datatype_component
    ~machine
    ~tag_lookup
    (dc : Typedefs__.datatype_component) =
  match dc with
  | Field { type_; _ } -> bit_size_of ~machine ~tag_lookup type_
  | Padding { bits; _ } -> bits

(** Returns the size of a type in bytes *)
let size_of ~machine ~tag_lookup t = bit_size_of ~machine ~tag_lookup t / 8

let rec bit_offset_struct_field ~machine ~tag_lookup ty field =
  let rec aux acc (components : Typedefs__.datatype_component list) =
    match components with
    | [] -> Gerror.unexpected "missing field for structure!"
    | Field { name; type_ } :: r ->
        if String.equal name field then acc
        else
          let acc = acc + bit_size_of ~machine ~tag_lookup type_ in
          aux acc r
    | Padding { bits; _ } :: r ->
        let acc = bits + acc in
        aux acc r
  in
  match ty with
  | Struct { components; _ } -> aux 0 components
  | Union { components; _ } ->
      if
        List.exists
          (function
            | Typedefs__.Field { name; _ } -> String.equal name field
            | _ -> false)
          components
      then 0
      else Gerror.unexpected "missing field for union!"
  | StructTag s | UnionTag s ->
      bit_offset_struct_field ~machine ~tag_lookup (tag_lookup s) field
  | _ ->
      Gerror.code_error
        ("bit_offset_struct_field for non-struct-or-union but: " ^ show ty)

let offset_struct_field ~machine ~tag_lookup ty field =
  bit_offset_struct_field ~machine ~tag_lookup ty field / 8
