type ac =
  | Store
  | Load
  | SetCell
  | GetCell
  | RemCell
  | GetFreed
  | SetFreed
  | RemFreed
  | GetBound
  | SetBound
  | RemBound
  | Alloc
  | Dispose

type ga = Cell | Bound | Freed [@@deriving yojson, show]

let str_ac = function
  | Store -> "store"
  | Load -> "load"
  | SetCell -> "setcell"
  | GetCell -> "getcell"
  | RemCell -> "remcell"
  | Alloc -> "alloc"
  | Dispose -> "dispose"
  | GetFreed -> "getfreed"
  | SetFreed -> "setfreed"
  | RemFreed -> "remfreed"
  | GetBound -> "getbound"
  | SetBound -> "setbound"
  | RemBound -> "rembound"

let ac_from_str = function
  | "store" -> Some Store
  | "load" -> Some Load
  | "setcell" -> Some SetCell
  | "getcell" -> Some GetCell
  | "remcell" -> Some RemCell
  | "getfreed" -> Some GetFreed
  | "setfreed" -> Some SetFreed
  | "remfreed" -> Some RemFreed
  | "getbound" -> Some GetBound
  | "setbound" -> Some SetBound
  | "rembound" -> Some RemBound
  | "alloc" -> Some Alloc
  | "dispose" -> Some Dispose
  | _ -> None

let str_ga = function
  | Cell -> "cell"
  | Bound -> "bound"
  | Freed -> "freed"

let ga_from_str = function
  | "cell" -> Some Cell
  | "bound" -> Some Bound
  | "freed" -> Some Freed
  | _ -> None

let ga_from_str_exn ga =
  match ga_from_str ga with
  | Some ga -> ga
  | None -> failwith ("Unknown general assertion for wisl : " ^ ga)

let ga_to_setter = function
  | Cell -> SetCell
  | Bound -> SetBound
  | Freed -> SetFreed

let ga_to_getter = function
  | Cell -> GetCell
  | Bound -> GetBound
  | Freed -> GetFreed

let ga_to_deleter = function
  | Cell -> RemCell
  | Bound -> RemBound
  | Freed -> RemFreed

let ga_to_yojson x = `String (str_ga x)

let ga_of_yojson x =
  match x with
  | `String s -> Ok (ga_from_str_exn s)
  | _ -> Error "Invalid json representing wisl core predicate"
