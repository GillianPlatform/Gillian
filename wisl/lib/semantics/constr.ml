open WislLActions
open Gil_syntax
open WUtils.WConfig

let cell ~loc ~offset ~value ?(permission = Expr.num 1.) () =
  let cell = str_ga Cell in
  let ins =
    if !fractional_permissions then [ loc; offset; permission ]
    else [ loc; offset ]
  in
  Asrt.CorePred (cell, ins, [ value ])

let bound ~loc ~bound ?(permission = Expr.num 1.) () =
  let bound_ga = str_ga Bound in
  let bound = Expr.int bound in
  let ins = if !fractional_permissions then [ loc; permission ] else [ loc ] in
  Asrt.CorePred (bound_ga, ins, [ bound ])

let freed ~loc =
  let freed = str_ga Freed in
  Asrt.CorePred (freed, [ loc ], [])
