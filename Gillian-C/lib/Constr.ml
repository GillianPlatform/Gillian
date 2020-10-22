open Gil_syntax

let pred ga ins outs =
  let ga_name = LActions.str_ga ga in
  Asrt.GA (ga_name, ins, outs)

let single ~loc ~ofs ~chunk ~sval ~perm =
  let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
  let perm =
    Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
  in
  pred (GMem Single) [ loc; ofs; chunk ] [ sval; perm ]

let hole ~loc ~low ~high ~perm =
  let perm =
    Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
  in
  pred (GMem Hole) [ loc; low; high ] [ perm ]

let bounds ~loc ~low ~high =
  let bounds = Expr.EList [ low; high ] in
  pred (GMem Bounds) [ loc ] [ bounds ]

let no_bounds ~loc = pred (GMem Bounds) [ loc ] [ Lit Null ]

let bounds_opt ~loc ~bounds:b =
  match b with
  | None             -> no_bounds ~loc
  | Some (low, high) -> bounds ~loc ~low ~high

let freed ~loc = pred (GMem Freed) [ loc ] []
