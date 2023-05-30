type t =
  | Mint8signed
  | Mint8unsigned
  | Mint16signed
  | Mint16unsigned
  | Mint32
  | Mint64
  | Mfloat32
  | Mfloat64
  | Mptr
[@@deriving eq, yojson]

let of_compcert : Compcert.AST.memory_chunk -> t = function
  | Mint8signed -> Mint8signed
  | Mint8unsigned -> Mint8unsigned
  | Mint16signed -> Mint16signed
  | Mint16unsigned -> Mint16unsigned
  | Mint32 -> Mint32
  | Mint64 -> Mint64
  | Mfloat32 -> Mfloat32
  | Mfloat64 -> Mfloat64
  | Many32 | Many64 -> failwith "Unsupported Concert Chunk Many32 or Many64"

let to_compcert : t -> Compcert.AST.memory_chunk = function
  | Mint8signed -> Mint8signed
  | Mint8unsigned -> Mint8unsigned
  | Mint16signed -> Mint16signed
  | Mint16unsigned -> Mint16unsigned
  | Mint32 -> Mint32
  | Mint64 -> Mint64
  | Mfloat32 -> Mfloat32
  | Mfloat64 -> Mfloat64
  | Mptr -> if Compcert.Archi.ptr64 then Mint64 else Mint32

let of_string = function
  | "int8signed" -> Mint8signed
  | "int8unsigned" -> Mint8unsigned
  | "int16signed" -> Mint16signed
  | "int16unsigned" -> Mint16unsigned
  | "int32" -> Mint32
  | "int64" -> Mint64
  | "float32" -> Mfloat32
  | "float64" -> Mfloat64
  | "ptr" -> Mptr
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
  | Mptr -> "ptr"

let pp fmt chunk = Fmt.pf fmt "%s" (to_string chunk)

let type_of = function
  | Mint64 -> Compcert.AST.Tlong
  | Mfloat32 -> Compcert.AST.Tsingle
  | Mfloat64 -> Compcert.AST.Tfloat
  | Mptr ->
      if Compcert.Archi.ptr64 then Compcert.AST.Tlong else Compcert.AST.Tint
  | _ -> Tint

let size chunk =
  let open Compcert in
  to_compcert chunk |> Memdata.size_chunk |> Camlcoq.Z.to_int

let size_expr chunk = Gil_syntax.Expr.int (size chunk)

let align chunk =
  let open Compcert in
  to_compcert chunk |> Memdata.align_chunk |> Camlcoq.Z.to_int

let ptr = Mptr
