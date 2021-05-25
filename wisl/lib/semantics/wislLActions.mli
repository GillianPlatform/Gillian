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

type ga = Cell | Bound | Freed

val str_ac : ac -> string

val ac_from_str : string -> ac

val str_ga : ga -> string

val ga_from_str : string -> ga

val ga_to_setter_str : string -> string

val ga_to_getter_str : string -> string

val ga_to_deleter_str : string -> string
