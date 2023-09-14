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
  | GetSingle
  | SetSingle
  | RemSingle
  | GetArray
  | SetArray
  | RemArray
  | GetHole
  | SetHole
  | RemHole
  | GetZeros
  | SetZeros
  | RemZeros
  | GetBounds
  | SetBounds
  | RemBounds
  | GetFreed
  | SetFreed
  | RemFreed

type genv_ac = GetSymbol | SetSymbol | RemSymbol | GetDef | SetDef | RemDef
type ac = AGEnv of genv_ac | AMem of mem_ac
type mem_ga = Single | Array | Hole | Zeros | Bounds | Freed
type genv_ga = Symbol | Definition
type ga = GMem of mem_ga | GGenv of genv_ga

(* Some things about the semantics of these Actions *)

let is_overlapping_asrt = function
  | GGenv _ -> true
  | _ -> false

let mem_ga_to_setter = function
  | Single -> SetSingle
  | Array -> SetArray
  | Hole -> SetHole
  | Zeros -> SetZeros
  | Bounds -> SetBounds
  | Freed -> SetFreed

let mem_ga_to_getter = function
  | Single -> GetSingle
  | Array -> GetArray
  | Hole -> GetHole
  | Zeros -> GetZeros
  | Bounds -> GetBounds
  | Freed -> GetFreed

let mem_ga_to_deleter = function
  | Single -> RemSingle
  | Array -> RemArray
  | Hole -> RemHole
  | Zeros -> RemZeros
  | Bounds -> RemBounds
  | Freed -> RemFreed

let genv_ga_to_getter = function
  | Definition -> GetDef
  | Symbol -> GetSymbol

let genv_ga_to_setter = function
  | Definition -> SetDef
  | Symbol -> SetSymbol

let genv_ga_to_deleter = function
  | Definition -> RemDef
  | Symbol -> RemSymbol

let make_map_act tr_mem tr_genv = function
  | GMem mga -> AMem (tr_mem mga)
  | GGenv gge -> AGEnv (tr_genv gge)

let ga_to_getter = make_map_act mem_ga_to_getter genv_ga_to_getter
let ga_to_setter = make_map_act mem_ga_to_setter genv_ga_to_setter
let ga_to_deleter = make_map_act mem_ga_to_deleter genv_ga_to_deleter

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
  | GetSingle -> "getSingle"
  | SetSingle -> "setSingle"
  | RemSingle -> "remSingle"
  | GetArray -> "getArray"
  | SetArray -> "setArray"
  | RemArray -> "remArray"
  | GetBounds -> "getBounds"
  | SetBounds -> "setBounds"
  | RemBounds -> "remBounds"
  | GetHole -> "getHole"
  | SetHole -> "setHole"
  | RemHole -> "remHole"
  | GetZeros -> "getZeros"
  | SetZeros -> "setZeros"
  | RemZeros -> "remZeros"
  | GetFreed -> "getFreed"
  | SetFreed -> "setFreed"
  | RemFreed -> "remFreed"

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
  | "getSingle" -> GetSingle
  | "setSingle" -> SetSingle
  | "remSingle" -> RemSingle
  | "getArray" -> GetArray
  | "setArray" -> SetArray
  | "remArray" -> RemArray
  | "getBounds" -> GetBounds
  | "setBounds" -> SetBounds
  | "remBounds" -> RemBounds
  | "getHole" -> GetHole
  | "setHole" -> SetHole
  | "remHole" -> RemHole
  | "getZeros" -> GetZeros
  | "setZeros" -> SetZeros
  | "remZeros" -> RemZeros
  | "getFreed" -> GetFreed
  | "setFreed" -> SetFreed
  | "remFreed" -> RemFreed
  | s -> failwith ("Unknown Memory Action : " ^ s)

let str_genv_ac = function
  | GetSymbol -> "getsymbol"
  | SetSymbol -> "setsymbol"
  | RemSymbol -> "remsymbol"
  | GetDef -> "getdef"
  | SetDef -> "setdef"
  | RemDef -> "remdef"

let genv_ac_from_str = function
  | "getsymbol" -> GetSymbol
  | "setsymbol" -> SetSymbol
  | "remsymbol" -> RemSymbol
  | "getdef" -> GetDef
  | "setdef" -> SetDef
  | "remdef" -> RemDef
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
let ga_to_setter_str = ga_to_action_str ga_to_setter
let ga_to_getter_str = ga_to_action_str ga_to_getter
let ga_to_deleter_str = ga_to_action_str ga_to_deleter

(** Additional stuff *)

let is_overlapping_asrt_str str = ga_from_str str |> is_overlapping_asrt
