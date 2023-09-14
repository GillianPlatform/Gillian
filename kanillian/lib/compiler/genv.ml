open Gil_syntax

let lookup_symbol ~ctx sym =
  let genvlookup = Memory_model.Interface.(str_ac (AGEnv GetSymbol)) in
  let sym_and_loc = Ctx.fresh_v ctx in
  let act = Cmd.LAction (sym_and_loc, genvlookup, [ Lit (String sym) ]) in
  let ptr = Ctx.fresh_v ctx in
  let assign =
    Cmd.Assignment
      (ptr, EList [ Expr.list_nth (PVar sym_and_loc) 1; Expr.zero_i ])
  in
  let ptr = Expr.PVar ptr in
  Cs.return ~app:[ act; assign ] ptr
