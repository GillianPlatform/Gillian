open Gil_syntax

module Core = struct
  let pred ga ins outs =
    let ga_name = Interface.str_ga ga in
    Asrt.GA (ga_name, ins, outs)

  let single ~loc ~ofs ~chunk ~sval ~perm =
    let chunk = Expr.Lit (String (Chunk.to_string chunk)) in
    let perm = Expr.Lit (String (Perm.opt_to_string perm)) in
    pred (GMem Single) [ loc; ofs; chunk ] [ sval; perm ]

  let array ~loc ~ofs ~chunk ~size ~sval_arr ~perm =
    let chunk = Expr.Lit (String (Chunk.to_string chunk)) in
    let perm = Expr.Lit (String (Perm.opt_to_string perm)) in
    pred (GMem Array) [ loc; ofs; size; chunk ] [ sval_arr; perm ]

  let hole ~loc ~low ~high ~perm =
    let perm = Expr.Lit (String (Perm.opt_to_string perm)) in
    pred (GMem Hole) [ loc; low; high ] [ perm ]

  let symbol ~symb ~loc = pred (GGenv Symbol) [ Expr.string symb ] [ loc ]

  let zeros ~loc ~low ~high ~perm =
    let perm = Expr.string (Perm.opt_to_string perm) in
    pred (GMem Zeros) [ loc; low; high ] [ perm ]

  let bounds ~loc ~low ~high =
    let bounds = Expr.EList [ low; high ] in
    pred (GMem Bounds) [ loc ] [ bounds ]

  let freed ~loc = pred (GMem Freed) [ loc ] []
end

module Others = struct
  open Kcommons.Constants

  let pred name params = Asrt.Pred (name, params)

  let malloced_abst ~ptr ~total_size =
    pred Internal_Predicates.malloced [ ptr; total_size ]

  let malloced ~ptr ~total_size =
    let loc, ofs = ptr in
    let size = Expr.int_z total_size in
    pred Internal_Predicates.malloced [ Expr.list [ loc; ofs ]; size ]

  let zeros_ptr_size ~ptr ~size =
    pred Internal_Predicates.zeros_ptr_size [ ptr; size ]

  let undefs_ptr_size ~ptr ~size =
    pred Internal_Predicates.undefs_ptr_size [ ptr; size ]

  let array_ptr ~ptr ~chunk ~size ~content =
    let chunk_str = Expr.string (Chunk.to_string chunk) in
    pred Internal_Predicates.array_ptr [ ptr; size; chunk_str; content ]

  let ptr_add ~ptr ~to_add ~res =
    pred Internal_Predicates.ptr_add [ ptr; to_add; res ]

  let fun_ptr ~ptr ~symb =
    pred Internal_Predicates.fun_ptr [ Lit (String symb); ptr ]

  let glob_fun ~symb ~fname =
    pred Internal_Predicates.glob_fun [ Lit (String symb); fname ]

  let glob_var_unallocated ~symb ~vname =
    pred Internal_Predicates.glob_var_unallocated [ Expr.string symb; vname ]

  let glob_var_unallocated_loc ~symb ~loc ~vname =
    pred Internal_Predicates.glob_var_unallocated_loc
      [ Expr.string symb; loc; vname ]
end
