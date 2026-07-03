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

val str_ac : ac -> string
val ac_from_str : string -> ac
val str_ga : ga -> string
val ga_from_str : string -> ga option
val ga_from_str_exn : string -> ga
val ga_to_setter_str : string -> string
val ga_to_getter_str : string -> string
val ga_to_deleter_str : string -> string
