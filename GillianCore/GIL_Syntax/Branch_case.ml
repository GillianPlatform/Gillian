type t' =
  | GuardedGoto of bool
  | LCmd
  | SpecExec of Flag.t
  | LAction
  | LActionFail
[@@deriving show, yojson]

type t = t' * int [@@deriving show, yojson]
type path = t list [@@deriving yojson, show]

let pp_short fmt (case, i) =
  match case with
  | GuardedGoto b -> Fmt.pf fmt "%B" b
  | LCmd -> Fmt.pf fmt "%d" i
  | SpecExec fl -> Fmt.pf fmt "%a-%d" Flag.pp fl i
  | LAction -> Fmt.pf fmt "L%d" i
  | LActionFail -> Fmt.pf fmt "LF%d" i
