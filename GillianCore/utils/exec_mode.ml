(** @canonical Gillian.Utils.Exec_mode

  Gillian's execution modes *)

type t = Concrete | Verification | Symbolic | BiAbduction

let is_concrete_exec = function
  | Concrete -> true
  | _ -> false

let is_symbolic_exec = function
  | Symbolic -> true
  | _ -> false

let is_verification_exec = function
  | Verification -> true
  | _ -> false

let is_biabduction_exec = function
  | BiAbduction -> true
  | _ -> false

(** {2 [Exec_mode]s that satisfy conditions }*)

(** All modes *)
let all_exec = [ Concrete; Verification; Symbolic; BiAbduction ]

(** Modes that make use of predicates *)
let exec_with_preds = [ Verification; BiAbduction ]

(** Modes that use bi-abduction *)
let bi_exec = [ BiAbduction ]

(** Modes that {i don't} use bi-abduction*)
let non_bi_exec = [ Concrete; Verification; Symbolic ]

(** Modes that use verification*)
let ver_exec = [ Verification ]

(** Modes that {i don't} use verification*)
let non_ver_exec = [ Concrete; Symbolic; BiAbduction ]

let concrete_exec = [ Concrete ]
let non_concrete_exec = [ Verification; Symbolic; BiAbduction ]

(** {2 String converters} *)

let to_string = function
  | Concrete -> "concrete"
  | Verification -> "verif"
  | Symbolic -> "wpst"
  | BiAbduction -> "act"

let of_string = function
  | "concrete" -> Concrete
  | "verif" -> Verification
  | "wpst" -> Symbolic
  | "act" -> BiAbduction
  | other -> failwith (Printf.sprintf "unknown exec mode \"%s\"" other)
