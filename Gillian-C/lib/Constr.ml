open Gil_syntax

module Core = struct
  let pred ga ins outs =
    let ga_name = LActions.str_ga ga in
    Asrt.GA (ga_name, ins, outs)

  let single ~loc ~ofs ~chunk ~sval ~perm =
    let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred (GMem Single) [ loc; ofs; chunk ] [ sval; perm ]

  let array ~loc ~ofs ~chunk ~size ~sval_arr ~perm =
    let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred (GMem Array) [ loc; ofs; size; chunk ] [ sval_arr; perm ]

  let hole ~loc ~low ~high ~perm =
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred (GMem Hole) [ loc; low; high ] [ perm ]

  let zeros ~loc ~low ~high ~perm =
    let perm = Expr.string (ValueTranslation.string_of_permission_opt perm) in
    pred (GMem Zeros) [ loc; low; high ] [ perm ]

  let bounds ~loc ~low ~high =
    let bounds = Expr.EList [ low; high ] in
    pred (GMem Bounds) [ loc ] [ bounds ]

  let no_bounds ~loc = pred (GMem Bounds) [ loc ] [ Lit Null ]

  let bounds_opt ~loc ~bounds:b =
    match b with
    | None             -> no_bounds ~loc
    | Some (low, high) -> bounds ~loc ~low ~high

  let freed ~loc = pred (GMem Freed) [ loc ] []
end

module Others = struct
  let pred name params = Asrt.Pred (name, params)

  let malloced_abst ~ptr ~total_size =
    pred CConstants.Internal_Predicates.malloced [ ptr; total_size ]

  let malloced ~ptr ~total_size =
    let loc, ofs = ptr in
    let size = Expr.num (float_of_int total_size) in
    pred CConstants.Internal_Predicates.malloced
      [ Expr.list [ loc; ofs ]; size ]

  let zeros_ptr_size ~ptr ~size =
    pred CConstants.Internal_Predicates.zeros_ptr_size [ ptr; size ]

  let array_ptr ~ptr ~chunk ~size ~content =
    let chunk_str = Expr.string (Chunk.to_string chunk) in
    pred CConstants.Internal_Predicates.array_ptr
      [ ptr; size; chunk_str; content ]

  let ptr_add ~ptr ~to_add ~res =
    pred CConstants.Internal_Predicates.ptr_add [ ptr; to_add; res ]
end
