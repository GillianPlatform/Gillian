type t =
  | IfElse of bool
  | LCmd of int
  | Gil of int
  | FuncExitPlaceholder
  | FuncExit of (string * int)
[@@deriving yojson]

let is_hidden_when_single = function
  | Gil _ | FuncExit _ -> true
  | _ -> false
