type kind = If_else_kind [@@deriving yojson, eq]

type case = If_else of bool | Func_exit of string | Unknown
[@@deriving yojson]

type t = Case of case * int | Func_exit_placeholder [@@deriving yojson]

let display = function
  | Case (Unknown, i) -> Fmt.str "%d" i
  | Case (If_else b, -1) -> Fmt.str "%B" b
  | Case (If_else b, i) -> Fmt.str "%B - %d" b i
  | Case (Func_exit label, i) -> Fmt.str "%s-%d" label i
  | Func_exit_placeholder -> "<step in>"
