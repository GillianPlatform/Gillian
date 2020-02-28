type ac = SetCell | GetCell | RemCell | Alloc | Dispose

type ga = Cell

let str_ac = function
  | SetCell -> "setcell"
  | GetCell -> "getcell"
  | RemCell -> "remcell"
  | Alloc   -> "alloc"
  | Dispose -> "dispose"

let ac_from_str = function
  | "setcell" -> SetCell
  | "getcell" -> GetCell
  | "remcell" -> RemCell
  | "alloc"   -> Alloc
  | "dispose" -> Dispose
  | ac        -> failwith ("Unknown local action for wisl : " ^ ac)

let str_ga = function
  | Cell -> "cell"

let ga_from_str = function
  | "cell" -> Cell
  | ga     -> failwith ("Unkown general assertion for wisl : " ^ ga)

let ga_to_setter = function
  | Cell -> SetCell

let ga_to_getter = function
  | Cell -> GetCell

let ga_to_deleter = function
  | Cell -> RemCell

let ga_to_action_str action str = ga_from_str str |> action |> str_ac

let ga_to_setter_str = ga_to_action_str ga_to_setter

let ga_to_getter_str = ga_to_action_str ga_to_getter

let ga_to_deleter_str = ga_to_action_str ga_to_deleter
