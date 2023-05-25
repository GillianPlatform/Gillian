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
[@@deriving yojson]

let of_compcert_ast_chunk : Compcert.AST.memory_chunk -> t = function
  | Mint8signed -> Mint8signed
  | Mint8unsigned -> Mint8unsigned
  | Mint16signed -> Mint16signed
  | Mint16unsigned -> Mint16unsigned
  | Mint32 -> Mint32
  | Mint64 -> Mint64
  | Mfloat32 -> Mfloat32
  | Mfloat64 -> Mfloat64
  | Many32 | Many64 -> Mptr (* Should we fail with error here instead? *)

let to_compcert_ast_chunk : t -> Compcert.AST.memory_chunk = function
  | Mint8signed -> Mint8signed
  | Mint8unsigned -> Mint8unsigned
  | Mint16signed -> Mint16signed
  | Mint16unsigned -> Mint16unsigned
  | Mint32 -> Mint32
  | Mint64 -> Mint64
  | Mfloat32 -> Mfloat32
  | Mfloat64 -> Mfloat64
  | Mptr -> if Compcert.Archi.ptr64 then Many32 else Many64

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
      if Compcert.Archi.ptr64 then Compcert.AST.Tany64 else Compcert.AST.Tany32
  | _ -> Tint

let size chunk =
  let open Compcert in
  let open BinNums in
  let value =
    match chunk with
    | Mint8signed -> Memdata.size_chunk Mint8signed
    | Mint8unsigned -> Memdata.size_chunk Mint8unsigned
    | Mint16signed -> Memdata.size_chunk Mint16signed
    | Mint16unsigned -> Memdata.size_chunk Mint16unsigned
    | Mint32 -> Memdata.size_chunk Mint32
    | Mint64 -> Memdata.size_chunk Mint64
    | Mfloat32 -> Memdata.size_chunk Mfloat32
    | Mfloat64 -> Memdata.size_chunk Mfloat64
    | Mptr ->
        if Compcert.Archi.ptr64 then Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
        else Zpos (Coq_xO (Coq_xO Coq_xH))
  in
  Camlcoq.Z.to_int value

let size_expr chunk = Gil_syntax.Expr.int (size chunk)

let align chunk =
  let open Compcert in
  let open BinNums in
  let value =
    match chunk with
    | Mint8signed -> Memdata.align_chunk Mint8signed
    | Mint8unsigned -> Memdata.align_chunk Mint8unsigned
    | Mint16signed -> Memdata.align_chunk Mint16signed
    | Mint16unsigned -> Memdata.align_chunk Mint16unsigned
    | Mint32 -> Memdata.align_chunk Mint32
    | Mint64 -> Memdata.align_chunk Mint64
    | Mfloat32 -> Memdata.align_chunk Mfloat32
    | Mfloat64 -> Memdata.align_chunk Mfloat64
    | Mptr -> Zpos (Coq_xO (Coq_xO Coq_xH))
  in
  Camlcoq.Z.to_int value

let equal chunk1 chunk2 = chunk1 = chunk2
let ptr = Mptr
