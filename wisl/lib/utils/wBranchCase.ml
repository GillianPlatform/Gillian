type kind = IfElseKind [@@deriving yojson]
type case = IfElse of bool | FuncExit of string | Unknown [@@deriving yojson]
type t = Case of case * int | FuncExitPlaceholder [@@deriving yojson]

let pp fmt = function
  | Case (Unknown, i) -> Fmt.pf fmt "%d" i
  | Case (IfElse b, -1) -> Fmt.pf fmt "%B" b
  | Case (IfElse b, i) -> Fmt.pf fmt "%B - %d" b i
  | Case (FuncExit label, i) -> Fmt.pf fmt "%s-%d" label i
  | FuncExitPlaceholder -> Fmt.pf fmt "<step in>"

let display = Fmt.str "%a" pp

let compare a b =
  let compare_pair (a1, a2) (b1, b2) =
    let cmp = Int.compare a1 b1 in
    if cmp = 0 then String.compare a2 b2 else cmp
  in
  let get_ix = function
    | FuncExitPlaceholder -> (0, "")
    | Case (IfElse false, _) -> (1, "")
    | Case (IfElse true, _) -> (2, "")
    | Case (FuncExit l, _) -> (3, l)
    | Case (Unknown, _) -> (4, "")
  in
  let cmp = compare_pair (get_ix a) (get_ix b) in
  match (cmp, a, b) with
  | 0, FuncExitPlaceholder, _ | 0, _, FuncExitPlaceholder ->
      failwith "impossible"
  | 0, Case (_, a), Case (_, b) -> Int.compare a b
  | cmp, _, _ -> cmp
