type ac =
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
  | "setcell" -> SetCell
  | "getcell" -> GetCell
  | "remcell" -> RemCell
  | "getfreed" -> GetFreed
  | "setfreed" -> SetFreed
  | "remfreed" -> RemFreed
  | "getbound" -> GetBound
  | "setbound" -> SetBound
  | "rembound" -> RemBound
  | "alloc" -> Alloc
  | "dispose" -> Dispose
  | ac -> failwith ("Unknown local action for wisl : " ^ ac)

let str_ga = function
  | Cell -> "cell"
  | Bound -> "bound"
  | Freed -> "freed"

let ga_from_str = function
  | "cell" -> Cell
  | "bound" -> Bound
  | "freed" -> Freed
  | ga -> failwith ("Unknown general assertion for wisl : " ^ ga)

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

let ga_to_action_str action str = ga_from_str str |> action |> str_ac
let ga_to_setter_str = ga_to_action_str ga_to_setter
let ga_to_getter_str = ga_to_action_str ga_to_getter
let ga_to_deleter_str = ga_to_action_str ga_to_deleter
let ga_to_yojson x = `String (str_ga x)

let ga_of_yojson x =
  match x with
  | `String s -> Ok (ga_from_str s)
  | _ -> Error "Invalid json representing wisl core predicate"
