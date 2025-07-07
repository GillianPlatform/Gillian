open Gil_syntax
open Monadic
module DO = Delayed_option
module DR = Delayed_result

type t = Int of int | F32 | F64 | Ptr

let type_to_chunk = function
  | Int w -> Chunk.IntegerChunk w
  | F32 -> Chunk.F32
  | F64 -> Chunk.F64
  | Ptr -> Chunk.IntegerOrPtrChunk

let chunk_to_type = function
  | Chunk.IntegerChunk w -> [ Int w ]
  | Chunk.F32 -> [ F32 ]
  | Chunk.F64 -> [ F64 ]
  | Chunk.IntegerOrPtrChunk -> [ Ptr; Int (Llvmconfig.ptr_width ()) ]

let type_to_string = function
  | Int w -> "int-" ^ string_of_int w
  | F32 -> "float"
  | F64 -> "double"
  | Ptr -> "ptr"

let string_to_type = function
  | "float" -> F32
  | "double" -> F64
  | "ptr" -> Ptr
  | st ->
      let sp = String.split_on_char '-' st in
      if List.length sp = 2 && List.hd sp = "int" then
        Int (int_of_string (List.nth sp 1))
      else failwith ("Invalid runtime type: " ^ st)

let make_expr_of_type_unsafe (expr : Expr.t) (typ : t) : Expr.t =
  Expr.list [ Expr.string (type_to_string typ); expr ]

let rtype_to_gil_type = function
  | Int w -> Type.BvType w
  | F32 -> Type.NumberType
  | F64 -> Type.NumberType
  | Ptr -> Type.ListType

let make_type_check_expr (expr : Expr.t) (typ : t) : Expr.t =
  let open Expr.Infix in
  Expr.list_nth expr 0 == Expr.string (type_to_string typ)
  && Expr.typeof (Expr.list_nth expr 1) == Expr.type_ (rtype_to_gil_type typ)

let ptr_pat expr = make_type_check_expr expr Ptr
let float_pat expr = make_type_check_expr expr F32
let double_pat expr = make_type_check_expr expr F64

(* TODO(Ian): This is hack and potentially incomplete i think
   to do this appropriately we would need to either: solve for the string
   or a-priori know all available integer types and branch on each *)
let get_integer_type (expr : Expr.t) : t DO.t =
  match expr with
  | Expr.EList [ Expr.Lit (Literal.String w); _ ] ->
      let open DO.Syntax in
      let split = String.split_on_char '-' w in
      let width =
        if List.length split = 2 && List.hd split = "int" then
          int_of_string_opt (List.nth split 1)
        else None
      in
      DO.map (DO.of_option width) (fun w -> Int w)
  | _ -> DO.none ()

(* Determines the type of an expression within a context *)
let type_of_expr (expr : Expr.t) : t DO.t =
  let open Delayed.Syntax in
  let* reduced = Delayed.reduce expr in
  let* ity = get_integer_type reduced in
  match ity with
  | Some ity -> Delayed.return (Some ity)
  | None -> (
      match%sat reduced with
      | ptr_pat -> DO.some Ptr
      | float_pat -> DO.some F32
      | double_pat -> DO.some F64
      | _ -> DO.none ())
