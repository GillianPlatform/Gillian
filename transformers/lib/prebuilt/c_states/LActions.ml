type ac = DropPerm | GetCurPerm | WeakValidPointer | Store | Load
type ga = Single | Array | Hole | Zeros | Bounds

let str_ac = function
  | DropPerm -> "dropperm"
  | WeakValidPointer -> "weakvalidpointer"
  | GetCurPerm -> "getcurperm"
  | Store -> "store"
  | Load -> "load"

let ac_from_str = function
  | "dropperm" -> DropPerm
  | "weakvalidpointer" -> WeakValidPointer
  | "getcurperm" -> GetCurPerm
  | "store" -> Store
  | "load" -> Load
  | _ -> failwith "Unrecognized action"

let str_ga = function
  | Single -> "single"
  | Array -> "array"
  | Hole -> "hole"
  | Zeros -> "zeros"
  | Bounds -> "bounds"

let ga_from_str = function
  | "single" -> Single
  | "array" -> Array
  | "bounds" -> Bounds
  | "zeros" -> Zeros
  | "hole" -> Hole
  | _ -> failwith "Unrecognized predicate"
