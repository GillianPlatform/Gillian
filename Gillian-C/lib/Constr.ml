open Gil_syntax

module Core = struct
  let pred ga ins outs =
    let ga_name = LActions.str_ga ga in
    Asrt.CorePred (ga_name, ins, outs)

  let single ~loc ~ofs ~chunk ~sval ~perm =
    let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred Single [ loc; ofs; chunk ] [ sval; perm ]

  let array ~loc ~ofs ~chunk ~size ~sval_arr ~perm =
    let chunk = Expr.Lit (String (ValueTranslation.string_of_chunk chunk)) in
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred Array [ loc; ofs; size; chunk ] [ sval_arr; perm ]

  let hole ~loc ~low ~high ~perm =
    let perm =
      Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
    in
    pred Hole [ loc; low; high ] [ perm ]

  let zeros ~loc ~low ~high ~perm =
    let perm = Expr.string (ValueTranslation.string_of_permission_opt perm) in
    pred Zeros [ loc; low; high ] [ perm ]

  let bounds ~loc ~low ~high =
    let bounds = Expr.EList [ low; high ] in
    pred Bounds [ loc ] [ bounds ]

  let freed ~loc = pred Freed [ loc ] []
end

module Others = struct
  open CConstants

  (* The in/out split below must match the runtime predicate definitions, since
     the engine trusts the split carried by the assertion. All-ins predicates
     use [pred]; predicates with out-parameters spell out [ins] and [outs]. *)
  let pred name ins outs = Asrt.pred name ins outs
  let pred_in name ins = Asrt.pred name ins []

  (* [i__malloced] is, unfortunately, declared with a different in/out split
     depending on the architecture: [(p; bytes)] in 64-bit, [(p, bytes;)] in
     32-bit. *)
  let malloced_pred p bytes =
    if Compcert.Archi.ptr64 then
      pred Internal_Predicates.malloced [ p ] [ bytes ]
    else pred_in Internal_Predicates.malloced [ p; bytes ]

  let malloced_abst ~ptr ~total_size = malloced_pred ptr total_size

  let malloced ~ptr ~total_size =
    let loc, ofs = ptr in
    let size = Expr.int_z total_size in
    malloced_pred (Expr.list [ loc; ofs ]) size

  let zeros_ptr_size ~ptr ~size =
    pred_in Internal_Predicates.zeros_ptr_size [ ptr; size ]

  let undefs_ptr_size ~ptr ~size =
    pred_in Internal_Predicates.undefs_ptr_size [ ptr; size ]

  let array_ptr ~ptr ~chunk ~size ~content =
    let chunk_str = Expr.string (Chunk.to_string chunk) in
    pred Internal_Predicates.array_ptr [ ptr; size; chunk_str ] [ content ]

  let ptr_add ~ptr ~to_add ~res =
    pred Internal_Predicates.ptr_add [ ptr; to_add ] [ res ]
end
