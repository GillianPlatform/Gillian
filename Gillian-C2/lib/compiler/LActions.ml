(* First, type definitions *)

type mem_ac =
  | Alloc
  | DropPerm
  | GetCurPerm
  | WeakValidPointer
  | Store
  | Load
  | Free
  | Move
  | SetZeros

type genv_ac = GetDef
type ac = AGEnv of genv_ac | AMem of mem_ac

type ga = Single | Array | Hole | Zeros | Bounds | Freed
[@@deriving yojson, show]

(* Some things about the semantics of these Actions *)

let is_overlapping_asrt _ = false

(* Then serialization and deserialization functions *)

let mem_prefix = "mem"
let genv_prefix = "genv"

let str_mem_ac = function
  | Alloc -> "alloc"
  | DropPerm -> "dropperm"
  | WeakValidPointer -> "weakvalidpointer"
  | GetCurPerm -> "getperm"
  | Store -> "store"
  | Load -> "load"
  | Move -> "move"
  | Free -> "free"
  | SetZeros -> "setZeros"

let mem_ac_from_str = function
  | "alloc" -> Alloc
  | "dropperm" -> DropPerm
  | "weakvalidpointer" -> WeakValidPointer
  | "getcurperm" -> GetCurPerm
  | "store" -> Store
  | "load" -> Load
  | "free" -> Free
  | "move" -> Move
  | "setZeros" -> SetZeros
  | s -> failwith ("Unkown Memory Action : " ^ s)

let str_genv_ac = function
  | GetDef -> "getdef"

let genv_ac_from_str = function
  | "getdef" -> GetDef
  | s -> failwith ("Unkown Global Env Action : " ^ s)

let separator_char = '_'
let separator_string = String.make 1 separator_char

let str_ac = function
  | AMem mem_ac -> mem_prefix ^ separator_string ^ str_mem_ac mem_ac
  | AGEnv genv_ac -> genv_prefix ^ separator_string ^ str_genv_ac genv_ac

let ac_from_str str =
  match String.split_on_char separator_char str with
  | [ pref; ac ] when String.equal pref mem_prefix -> AMem (mem_ac_from_str ac)
  | [ pref; ac ] when String.equal pref genv_prefix ->
      AGEnv (genv_ac_from_str ac)
  | _ -> failwith ("Unkown action : " ^ str)

let str_ga = function
  | Single -> "mem_single"
  | Array -> "mem_array"
  | Hole -> "mem_hole"
  | Zeros -> "mem_zeros"
  | Bounds -> "mem_bounds"
  | Freed -> "mem_freed"

let ga_from_str = function
  | "mem_single" -> Single
  | "mem_array" -> Array
  | "mem_bounds" -> Bounds
  | "mem_zeros" -> Zeros
  | "mem_hole" -> Hole
  | "mem_freed" -> Freed
  | str -> failwith ("Unkown memory assertion : " ^ str)

(** Additional stuff *)

let is_overlapping_asrt_str str = ga_from_str str |> is_overlapping_asrt
