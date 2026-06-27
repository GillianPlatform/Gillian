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
  | SetZeros

type genv_ac = GetDef

type ac =
  | AGEnv of genv_ac  (** Actions related to the memory *)
  | AMem of mem_ac  (** Actions related to the global environment *)

type ga = Single | Array | Hole | Zeros | Bounds | Freed
[@@deriving yojson, show]

(** {3 Serialization of actions} *)

(** Serializes an action into a string *)
val str_ac : ac -> string

(** Deserializes a string into an action *)
val ac_from_str : string -> ac

(** {3 Global assertion serialization} *)

val str_ga : ga -> string
val ga_from_str : string -> ga

(** {3 Gillian-related things} *)

val is_overlapping_asrt_str : string -> bool
