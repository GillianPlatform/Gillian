type t =
  | GuardedGoto of bool
  | LCmd of int
  | SpecExec of Flag.t
  | LAction of Yojson.Safe.t list
  | LActionFail of int
[@@deriving show, yojson]

type path = t list [@@deriving yojson]
