open Gil_syntax
module ValueTranslation = Cgil_lib.ValueTranslation
module Others = Cgil_lib.Constr.Others

(* Redefine Constr, to remove the loc parameter (since that is handled by the PMap), and have
   core predicates be a simpler tuple *)
module Core = struct
  open LActions

  let pred ga ins outs = (ga, ins, outs)

  let single ~ofs ~chunk ~sval ~perm =
    let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred Single [ ofs; chunk ] [ sval; perm ]

  let array ~ofs ~chunk ~size ~sval_arr ~perm =
    let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred Array [ ofs; size; chunk ] [ sval_arr; perm ]

  let hole ~low ~high ~perm =
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred Hole [ low; high ] [ perm ]

  let zeros ~low ~high ~perm =
    let perm = Expr.string (ValueTranslation.string_of_permission_opt perm) in
    pred Zeros [ low; high ] [ perm ]

  let bounds ~low ~high =
    let bounds = Expr.EList [ low; high ] in
    pred Bounds [] [ bounds ]
end
