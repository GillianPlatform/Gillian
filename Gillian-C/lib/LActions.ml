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

type genv_ac = GetDef
type ac = AGEnv of genv_ac | AMem of mem_ac

type ga = Single | Array | Hole | Zeros | Bounds | Freed
[@@deriving yojson, show]

(* Some things about the semantics of these Actions *)

let is_overlapping_asrt _ = false

let ga_to_setter ga =
  AMem
    (match ga with
    | Single -> SetSingle
    | Array -> SetArray
    | Hole -> SetHole
    | Zeros -> SetZeros
    | Bounds -> SetBounds
    | Freed -> SetFreed)

let ga_to_getter ga =
  AMem
    (match ga with
    | Single -> GetSingle
    | Array -> GetArray
    | Hole -> GetHole
    | Zeros -> GetZeros
    | Bounds -> GetBounds
    | Freed -> GetFreed)

let ga_to_deleter ga =
  AMem
    (match ga with
    | Single -> RemSingle
    | Array -> RemArray
    | Hole -> RemHole
    | Zeros -> RemZeros
    | Bounds -> RemBounds
    | Freed -> RemFreed)

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

let ga_to_action_str action str = ga_from_str str |> action |> str_ac
let ga_to_setter_str = ga_to_action_str ga_to_setter
let ga_to_getter_str = ga_to_action_str ga_to_getter
let ga_to_deleter_str = ga_to_action_str ga_to_deleter

(** Additional stuff *)

let is_overlapping_asrt_str str = ga_from_str str |> is_overlapping_asrt
