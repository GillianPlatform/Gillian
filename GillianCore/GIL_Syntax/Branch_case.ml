type t =
  | GuardedGoto of bool
  | LCmd of int
  | SpecExec of Flag.t * int
  | LAction of int
  | LActionFail of int
[@@deriving show, yojson]

type path = t list [@@deriving yojson, show]

let pp_short fmt = function
  | GuardedGoto b -> Fmt.pf fmt "%B" b
  | LCmd x -> Fmt.pf fmt "%d" x
  | SpecExec (fl, i) -> Fmt.pf fmt "%a-%d" Flag.pp fl i
  | LAction x -> Fmt.pf fmt "L%d" x
  | LActionFail x -> Fmt.pf fmt "LF%d" x
