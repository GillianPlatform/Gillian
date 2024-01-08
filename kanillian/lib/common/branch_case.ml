type kind = If_else_kind | For_loop_kind | While_loop_kind
[@@deriving yojson, eq]

type case =
  | If_else of bool
  | For_loop of bool
  | While_loop of bool
  | Func_exit of string
  | Unknown
[@@deriving yojson]

type t = Case of case * int | Func_exit_placeholder [@@deriving yojson]

let display = function
  | Case (Unknown, i) -> Fmt.str "%d" i
  | Case ((If_else b | For_loop b | While_loop b), -1) -> Fmt.str "%B" b
  | Case ((If_else b | For_loop b | While_loop b), i) -> Fmt.str "%B - %d" b i
  | Case (Func_exit label, i) -> Fmt.str "%s-%d" label i
  | Func_exit_placeholder -> "<step in>"

let bool_kind_to_case kind b =
  match kind with
  | If_else_kind -> If_else b
  | For_loop_kind -> For_loop b
  | While_loop_kind -> While_loop b
