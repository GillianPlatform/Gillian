(**
	GIL Constants
*)

type t = TypeDef__.constant =
  | Min_float  (** The smallest float *)
  | Max_float  (** The largest float *)
  | MaxSafeInteger  (** {% 2^53 - 1 %} **)
  | Epsilon  (** Smallest positive number *)
  | Random  (** A random number between 0 and 1 *)
  | Pi  (** The number {% \pi %} *)
  | UTCTime  (** Current UTC time *)
  | LocalTime  (** Current local time *)

(** Print *)
let str (x : t) =
  match x with
  | Min_float      -> "$$min_value"
  | Max_float      -> "$$max_value"
  | MaxSafeInteger -> "$$max_safe_integer"
  | Epsilon        -> "$$epsilon"
  | Random         -> "$$random"
  | Pi             -> "$$pi"
  | UTCTime        -> "$$utctime"
  | LocalTime      -> "$$localtime"
