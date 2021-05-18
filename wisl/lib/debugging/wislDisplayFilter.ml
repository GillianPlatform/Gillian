let filter_map_store store =
  List.filter_map
    (fun (var, (value : Gil_syntax.Expr.t)) ->
      if Str.string_match (Str.regexp "gvar") var 0 then None
      else
        let value =
          match value with
          | Gil_syntax.Expr.EList [ Lit (Loc loc); Lit (Num offset) ] ->
              (* Displaying of a pointer in WISL *)
              Fmt.str "-> (%a, %.0f)"
                (Fmt.hbox Gil_syntax.Expr.pp)
                (Gil_syntax.Expr.Lit (Loc loc)) offset
          | value -> Fmt.to_to_string (Fmt.hbox Gil_syntax.Expr.pp) value
        in
        Some (var, value))
    store
