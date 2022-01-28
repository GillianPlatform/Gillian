open Alcotest
open Gil_syntax

let constant = of_pp (Fmt.of_to_string Constant.str)
let typ = of_pp (Fmt.of_to_string Type.str)
let literal = of_pp Literal.pp
let expr = of_pp Expr.full_pp
