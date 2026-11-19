open Gillian.Gil_syntax

let pp_comma_sep pp = Fmt.list ~sep:(Fmt.any ", ") pp

let rec pp_lit ft =
  let open Literal in
  function
  | Int i -> Fmt.pf ft "%a" Z.pp_print i
  | LList ll -> Fmt.pf ft "[%a]" (pp_comma_sep pp_lit) ll
  | l -> Fmt.pf ft "%a" Literal.pp l

let pp_binop ft =
  let open BinOp in
  function
  | ILessThanEqual -> Fmt.string ft "<="
  | ILessThan -> Fmt.string ft "<"
  | IPlus -> Fmt.string ft "+"
  | IMinus -> Fmt.string ft "-"
  | ITimes -> Fmt.string ft "*"
  | IDiv -> Fmt.string ft "/"
  | IMod -> Fmt.string ft "%"
  | StrLess -> Fmt.string ft "<"
  | op -> Fmt.pf ft "%s" (BinOp.str op)

let rec pp_expr' ~outermost ft =
  ignore outermost;
  let open Expr in
  let pp = pp_expr' ~outermost:false in
  let pp_o = pp_expr' ~outermost:true in
  function
  | Lit l -> Fmt.pf ft "%a" pp_lit l
  | UnOp (LstLen, e) -> Fmt.pf ft "len(%a)" pp_o e
  | BinOp (e1, LstNth, e2) -> Fmt.pf ft "%a[%a]" pp e1 pp_o e2
  | BinOp (Lit (Int x), IPlus, e2) ->
      if Z.sign x = -1 then Fmt.pf ft "%a - %a" pp e2 Z.pp_print (Z.neg x)
      else Fmt.pf ft "%a + %a" pp e2 Z.pp_print x
  | BinOp (e1, op, e2) when outermost ->
      Fmt.pf ft "%a %a %a" pp e1 pp_binop op pp e2
  | BinOp (e1, op, e2) -> Fmt.pf ft "(%a %a %a)" pp e1 pp_binop op pp e2
  | EList ll -> Fmt.pf ft "[%a]" (pp_comma_sep pp_o) ll
  | ESet ll -> Fmt.pf ft "{%a}" (pp_comma_sep pp_o) ll
  | NOp (LstCat, ll) -> Fmt.pf ft "(%a)" (Fmt.list ~sep:(Fmt.any " %@ ") pp) ll
  | e -> Fmt.pf ft "%a" (pp_custom ~pp) e

let pp = Fmt.hbox (pp_expr' ~outermost:true)
