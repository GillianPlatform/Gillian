open Gil_syntax

let pred ga ins outs =
  let ga_name = LActions.str_ga ga in
  Asrt.GA (ga_name, ins, outs)

let single ~loc ~ofs ~chunk ~sval =
  let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
  pred (GMem Single) [ loc; ofs; chunk ] [ sval ]

let hole ~loc ~low ~high = pred (GMem Hole) [ loc; low; high ] []

let bounds ~loc ~low ~high =
  let bounds = Expr.EList [ low; high ] in
  pred (GMem Bounds) [ loc ] [ bounds ]

let no_bounds ~loc = pred (GMem Bounds) [ loc ] [ Lit Null ]

let bounds_opt ~loc ~bounds:b =
  match b with
  | None             -> no_bounds ~loc
  | Some (low, high) -> bounds ~loc ~low ~high

let freed ~loc = pred (GMem Freed) [ loc ] []

let perm ~loc ~perm =
  let perm = Expr.Lit (String (ValueTranslation.string_of_permission perm)) in
  pred (GMem Perm) [ loc ] [ perm ]
