type t = Compcert.AST.memory_chunk =
  | Mint8signed
  | Mint8unsigned
  | Mint16signed
  | Mint16unsigned
  | Mint32
  | Mint64
  | Mfloat32
  | Mfloat64
  | Many32
  | Many64
[@@deriving yojson]

let of_string = function
  | "int8signed" -> Mint8signed
  | "int8unsigned" -> Mint8unsigned
  | "int16signed" -> Mint16signed
  | "int16unsigned" -> Mint16unsigned
  | "int32" -> Mint32
  | "int64" -> Mint64
  | "float32" -> Mfloat32
  | "float64" -> Mfloat64
  | "any32" -> Many32
  | "any64" -> Many64
  | str -> failwith ("unknown chunk : " ^ str)

let to_string = function
  | Mint8signed -> "int8signed"
  | Mint8unsigned -> "int8unsigned"
  | Mint16signed -> "int16signed"
  | Mint16unsigned -> "int16unsigned"
  | Mint32 -> "int32"
  | Mint64 -> "int64"
  | Mfloat32 -> "float32"
  | Mfloat64 -> "float64"
  | Many32 -> "any32"
  | Many64 -> "any64"

let pp fmt chunk = Fmt.pf fmt "%s" (to_string chunk)
let type_of = Compcert.AST.type_of_chunk

let size chunk =
  let open Compcert in
  Camlcoq.Z.to_int (Memdata.size_chunk chunk)

let size_expr chunk = Gil_syntax.Expr.int (size chunk)

let align chunk =
  let open Compcert in
  Camlcoq.Z.to_int (Memdata.align_chunk chunk)

let equal = Compcert.AST.chunk_eq
let ptr = Compcert.AST.coq_Mptr
