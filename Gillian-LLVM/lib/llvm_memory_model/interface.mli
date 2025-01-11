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
  | GetArray
  | GetBounds

type genv_ac = GetSymbol | SetSymbol | GetDef | SetDef

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

(** {3 Global assertion serialization } *)

val str_ga : ga -> string
val ga_from_str : string -> ga

(** {3 Gillian-related things} *)

val is_overlapping_asrt_str : string -> bool
