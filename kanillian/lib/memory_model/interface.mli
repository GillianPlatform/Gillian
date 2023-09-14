(** {3 Type definitions} *)

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

type ac =
  | AGEnv of genv_ac  (** Actions related to the memory *)
  | AMem of mem_ac  (** Actions related to the global environment *)

type mem_ga = Single | Array | Hole | Zeros | Bounds | Freed
type genv_ga = Symbol | Definition
type ga = GMem of mem_ga | GGenv of genv_ga

(** {3 Serialization of actions} *)

(** Serializes an action into a string *)
val str_ac : ac -> string

(** Deserializes a string into an action *)
val ac_from_str : string -> ac

(** {3 Global assertion and their actions} *)

val ga_to_setter : ga -> ac
val ga_to_getter : ga -> ac
val ga_to_deleter : ga -> ac

(** {3 Global assertion serialization } *)

val str_ga : ga -> string
val ga_from_str : string -> ga
val ga_to_setter_str : string -> string
val ga_to_getter_str : string -> string
val ga_to_deleter_str : string -> string

(** {3 Gillian-related things} *)

val is_overlapping_asrt_str : string -> bool
