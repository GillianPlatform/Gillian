type kind = IfElseKind | WhileLoopKind [@@deriving yojson]

type case = IfElse of bool | WhileLoop of bool | FuncExit of string | Unknown
[@@deriving yojson]

type t = Case of case * int | FuncExitPlaceholder [@@deriving yojson]

let pp fmt = function
  | Case (Unknown, i) -> Fmt.pf fmt "%d" i
  | Case ((IfElse b | WhileLoop b), -1) -> Fmt.pf fmt "%B" b
  | Case ((IfElse b | WhileLoop b), i) -> Fmt.pf fmt "%B - %d" b i
  | Case (FuncExit label, i) -> Fmt.pf fmt "%s-%d" label i
  | FuncExitPlaceholder -> Fmt.pf fmt "<step in>"

let display = Fmt.str "%a" pp
