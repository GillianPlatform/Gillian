(* First, type definitions *)

type mem_ac =
  | Alloc
  | DropPerm
  | GetCurPerm
  | Store
  | Load
  | Free
  | Move
  | GetSingle
  | SetSingle
  | RemSingle
  | GetHole
  | SetHole
  | RemHole
  | GetBounds
  | SetBounds
  | RemBounds
  | GetPerm
  | SetPerm
  | RemPerm
  | GetFreed
  | SetFreed
  | RemFreed

type genv_ac = GetSymbol | SetSymbol | RemSymbol | GetDef | SetDef | RemDef

type ac = AGEnv of genv_ac | AMem of mem_ac

type mem_ga = Single | Hole | Bounds | Perm | Freed

type genv_ga = Symbol | Definition

type ga = GMem of mem_ga | GGenv of genv_ga

(* Some things about the semantics of these Actions *)

let is_overlapping_asrt = function
  | GMem Perm | GGenv _ -> true
  | _                   -> false

let mem_ga_to_setter = function
  | Single -> SetSingle
  | Hole   -> SetHole
  | Bounds -> SetBounds
  | Perm   -> SetPerm
  | Freed  -> SetFreed

let mem_ga_to_getter = function
  | Single -> GetSingle
  | Hole   -> GetHole
  | Bounds -> GetBounds
  | Perm   -> GetPerm
  | Freed  -> GetFreed

let mem_ga_to_deleter = function
  | Single -> RemSingle
  | Hole   -> RemHole
  | Bounds -> RemBounds
  | Perm   -> RemPerm
  | Freed  -> RemFreed

let genv_ga_to_getter = function
  | Definition -> GetDef
  | Symbol     -> GetSymbol

let genv_ga_to_setter = function
  | Definition -> SetDef
  | Symbol     -> SetSymbol

let genv_ga_to_deleter = function
  | Definition -> RemDef
  | Symbol     -> RemSymbol

let make_map_act tr_mem tr_genv = function
  | GMem mga  -> AMem (tr_mem mga)
  | GGenv gge -> AGEnv (tr_genv gge)

let ga_to_getter = make_map_act mem_ga_to_getter genv_ga_to_getter

let ga_to_setter = make_map_act mem_ga_to_setter genv_ga_to_setter

let ga_to_deleter = make_map_act mem_ga_to_deleter genv_ga_to_deleter

(* Then serialization and deserialization functions *)

let mem_prefix = "mem"

let genv_prefix = "genv"

let str_mem_ac = function
  | Alloc      -> "alloc"
  | DropPerm   -> "dropperm"
  | GetCurPerm -> "getperm"
  | Store      -> "store"
  | Load       -> "load"
  | Move       -> "move"
  | Free       -> "free"
  | GetSingle  -> "get_single"
  | SetSingle  -> "set_single"
  | RemSingle  -> "rem_single"
  | GetBounds  -> "get_bounds"
  | SetBounds  -> "set_bounds"
  | RemBounds  -> "rem_bounds"
  | GetHole    -> "get_hole"
  | SetHole    -> "set_hole"
  | RemHole    -> "rem_hole"
  | GetPerm    -> "get_perm"
  | SetPerm    -> "set_perm"
  | RemPerm    -> "rem_perm"
  | GetFreed   -> "get_freed"
  | SetFreed   -> "set_freed"
  | RemFreed   -> "rem_freed"

let mem_ac_from_str = function
  | "alloc"      -> Alloc
  | "dropperm"   -> DropPerm
  | "getcurperm" -> GetCurPerm
  | "store"      -> Store
  | "load"       -> Load
  | "free"       -> Free
  | "move"       -> Move
  | "get_single" -> GetSingle
  | "set_single" -> SetSingle
  | "rem_single" -> RemSingle
  | "get_bounds" -> GetBounds
  | "set_bounds" -> SetBounds
  | "rem_bounds" -> RemBounds
  | "get_hole"   -> GetHole
  | "set_hole"   -> SetHole
  | "rem_hole"   -> RemHole
  | "get_perm"   -> GetPerm
  | "set_perm"   -> SetPerm
  | "rem_perm"   -> RemPerm
  | "get_freed"  -> GetFreed
  | "set_freed"  -> SetFreed
  | "rem_freed"  -> RemFreed
  | s            -> failwith ("Unkown Memory Action : " ^ s)

let str_genv_ac = function
  | GetSymbol -> "getsymbol"
  | SetSymbol -> "setsymbol"
  | RemSymbol -> "remsymbol"
  | GetDef    -> "getdef"
  | SetDef    -> "setdef"
  | RemDef    -> "remdef"

let genv_ac_from_str = function
  | "getsymbol" -> GetSymbol
  | "setsymbol" -> SetSymbol
  | "remsymbol" -> RemSymbol
  | "getdef"    -> GetDef
  | "setdef"    -> SetDef
  | "remdef"    -> RemDef
  | s           -> failwith ("Unkown Global Env Action : " ^ s)

let separator_char = '_'

let separator_string = String.make 1 separator_char

let str_ac = function
  | AMem mem_ac   -> mem_prefix ^ separator_string ^ str_mem_ac mem_ac
  | AGEnv genv_ac -> genv_prefix ^ separator_string ^ str_genv_ac genv_ac

let ac_from_str str =
  match String.split_on_char separator_char str with
  | [ pref; ac ] when String.equal pref mem_prefix -> AMem (mem_ac_from_str ac)
  | [ pref; ac ] when String.equal pref genv_prefix ->
      AGEnv (genv_ac_from_str ac)
  | _ -> failwith ("Unkown action : " ^ str)

let str_mem_ga = function
  | Single -> "single"
  | Hole   -> "hole"
  | Bounds -> "bounds"
  | Perm   -> "perm"
  | Freed  -> "freed"

let str_genv_ga = function
  | Definition -> "def"
  | Symbol     -> "symb"

let mem_ga_from_str = function
  | "single" -> Single
  | "bounds" -> Bounds
  | "hole"   -> Hole
  | "freed"  -> Freed
  | str      -> failwith ("Unkown memory assertion : " ^ str)

let genv_ga_from_str = function
  | "symb" -> Symbol
  | "def"  -> Definition
  | str    -> failwith ("Unkown global assertion : " ^ str)

let str_ga = function
  | GMem mem_ga   -> mem_prefix ^ separator_string ^ str_mem_ga mem_ga
  | GGenv genv_ga -> genv_prefix ^ separator_string ^ str_genv_ga genv_ga

let ga_from_str str =
  match String.split_on_char separator_char str with
  | [ pref; ga ] when String.equal pref mem_prefix -> GMem (mem_ga_from_str ga)
  | [ pref; ga ] when String.equal pref genv_prefix ->
      GGenv (genv_ga_from_str ga)
  | _ -> failwith ("Unkown GA : " ^ str)

let ga_to_action_str action str = ga_from_str str |> action |> str_ac

let ga_to_setter_str = ga_to_action_str ga_to_setter

let ga_to_getter_str = ga_to_action_str ga_to_getter

let ga_to_deleter_str = ga_to_action_str ga_to_deleter

(** Additional stuff *)

let is_overlapping_asrt_str str = ga_from_str str |> is_overlapping_asrt

let ga_loc_indexes ga =
  match ga with
  | GMem Single      -> [ 0 ]
  | GMem Hole        -> [ 0 ]
  | GMem Bounds      -> [ 0 ]
  | GMem Perm        -> [ 0 ]
  | GMem Freed       -> [ 0 ]
  | GGenv Definition -> [ 0 ]
  | GGenv Symbol     -> []

let ga_loc_indexes_str ga_str = ga_from_str ga_str |> ga_loc_indexes
