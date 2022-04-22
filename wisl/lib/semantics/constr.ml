open WislLActions
open Gil_syntax

let cell ~loc ~offset ~value =
  let cell = str_ga Cell in
  Asrt.GA (cell, [ loc; offset ], [ value ])

let bound ~loc ~bound =
  let bound_ga = str_ga Bound in
  let bound = Expr.int bound in
  Asrt.GA (bound_ga, [ loc ], [ bound ])

let freed ~loc =
  let freed = str_ga Freed in
  Asrt.GA (freed, [ loc ], [])
