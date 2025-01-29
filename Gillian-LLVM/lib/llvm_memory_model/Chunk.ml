open Gillian.Gil_syntax

type t = IntegerChunk of int | F32 | F64 [@@deriving eq, yojson]
type components = Float of { bit_width : int } | Int of { bit_width : int }

let to_string = function
  | IntegerChunk i -> "i-" ^ Int.to_string i
  | F32 -> "f32"
  | F64 -> "f64"

let of_string = function
  | "f32" -> F32
  | "f64" -> F64
  | x ->
      let lst = String.split_on_char '-' x in
      if List.length lst = 2 && String.equal (List.hd lst) "i" then
        let st = List.nth lst 1 in
        IntegerChunk (int_of_string st)
      else failwith ("invalid chunk " ^ x)

let size = function
  | IntegerChunk i -> i / 8
  | F32 -> 4
  | F64 -> 8

let align = function
  | IntegerChunk i -> i / 8
  | F32 -> 4
  | F64 -> 8

let to_components chunk =
  match chunk with
  | IntegerChunk w -> Int { bit_width = w }
  | F32 -> Float { bit_width = 32 }
  | F64 -> Float { bit_width = 64 }

let is_int = function
  | IntegerChunk _ -> true
  | F32 | F64 -> false

let i8 = IntegerChunk 8

(* TODO(Ian): should we somehow know if this is a pointer chunk? *)
let type_of curr_chunk =
  match curr_chunk with
  | IntegerChunk i ->
      if Llvmconfig.ptr_width () = i then None else Some (Type.BvType i)
  | F32 -> Some Type.NumberType
  | F64 -> Some Type.NumberType
