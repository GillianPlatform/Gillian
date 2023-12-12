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
  | Poison
  | ZeroInit
  | GetArray
  | GetBounds

type genv_ac = GetSymbol | SetSymbol | GetDef | SetDef
type ac = AGEnv of genv_ac | AMem of mem_ac
type mem_ga = Single | Array | Hole | Zeros | Bounds | Freed
type genv_ga = Symbol | Definition
type ga = GMem of mem_ga | GGenv of genv_ga

(* Some things about the semantics of these Actions *)

let is_overlapping_asrt = function
  | GGenv _ -> true
  | _ -> false

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
  | Poison -> "poison"
  | ZeroInit -> "zeroinit"
  | GetArray -> "getArray"
  | GetBounds -> "getBounds"

let mem_ac_from_str = function
  | "alloc" -> Alloc
  | "dropperm" -> DropPerm
  | "weakvalidpointer" -> WeakValidPointer
  | "getcurperm" -> GetCurPerm
  | "store" -> Store
  | "load" -> Load
  | "free" -> Free
  | "move" -> Move
  | "poison" -> Poison
  | "zeroinit" -> ZeroInit
  | "getArray" -> GetArray
  | "getBounds" -> GetBounds
  | s -> failwith ("Unknown Memory Action : " ^ s)

let str_genv_ac = function
  | GetSymbol -> "getsymbol"
  | SetSymbol -> "setsymbol"
  | GetDef -> "getdef"
  | SetDef -> "setdef"

let genv_ac_from_str = function
  | "getsymbol" -> GetSymbol
  | "setsymbol" -> SetSymbol
  | "getdef" -> GetDef
  | "setdef" -> SetDef
  | s -> failwith ("Unknown Global Env Action : " ^ s)

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
  | _ -> failwith ("Unknown action : " ^ str)

let str_mem_ga = function
  | Single -> "single"
  | Array -> "array"
  | Hole -> "hole"
  | Zeros -> "zeros"
  | Bounds -> "bounds"
  | Freed -> "freed"

let str_genv_ga = function
  | Definition -> "def"
  | Symbol -> "symb"

let mem_ga_from_str = function
  | "single" -> Single
  | "array" -> Array
  | "bounds" -> Bounds
  | "zeros" -> Zeros
  | "hole" -> Hole
  | "freed" -> Freed
  | str -> failwith ("Unknown memory assertion : " ^ str)

let genv_ga_from_str = function
  | "symb" -> Symbol
  | "def" -> Definition
  | str -> failwith ("Unknown global assertion : " ^ str)

let str_ga = function
  | GMem mem_ga -> mem_prefix ^ separator_string ^ str_mem_ga mem_ga
  | GGenv genv_ga -> genv_prefix ^ separator_string ^ str_genv_ga genv_ga

let ga_from_str str =
  match String.split_on_char separator_char str with
  | [ pref; ga ] when String.equal pref mem_prefix -> GMem (mem_ga_from_str ga)
  | [ pref; ga ] when String.equal pref genv_prefix ->
      GGenv (genv_ga_from_str ga)
  | _ -> failwith ("Unknown GA : " ^ str)

let ga_to_action_str action str = ga_from_str str |> action |> str_ac

(** Additional stuff *)

let is_overlapping_asrt_str str = ga_from_str str |> is_overlapping_asrt
