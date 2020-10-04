open Gil_syntax

let pred ga ins outs =
  let ga_name = LActions.str_ga ga in
  Asrt.GA (ga_name, ins, outs)

let single ~loc ~ofs ~chunk ~sval =
  let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
  pred (GMem Single) [ loc; ofs; chunk ] [ sval ]

let hole ~loc ~low ~high = pred (GMem Hole) [ loc; low; high ] []

let bounds ~loc ~low ~high = pred (GMem Bounds) [ loc ] [ low; high ]
