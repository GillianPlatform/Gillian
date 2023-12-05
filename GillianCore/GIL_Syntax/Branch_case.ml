type t =
  | GuardedGoto of bool
  | LCmd of int
  | SpecExec of Flag.t * int
  | LAction of int
  | LActionFail of int
[@@deriving show, yojson]

type path = t list [@@deriving yojson]
