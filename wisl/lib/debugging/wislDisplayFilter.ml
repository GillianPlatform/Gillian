open Gil_syntax

let filter_map_store store =
  List.filter_map
    (fun (var, (value : Gil_syntax.Expr.t)) ->
      if Str.string_match (Str.regexp "gvar") var 0 then None
      else
        let match_offset lst loc loc_pp =
          match lst with
          | [ Expr.Lit (Num offset) ] ->
              Fmt.str "-> (%a, %.0f)" (Fmt.hbox loc_pp) loc offset
          | [ offset ]                ->
              Fmt.str "-> (%a, %a)" (Fmt.hbox loc_pp) loc (Fmt.hbox Expr.pp)
                offset
          | _                         -> Fmt.to_to_string (Fmt.hbox Expr.pp)
                                           value
        in
        let value =
          match value with
          | Expr.EList (Lit (Loc loc) :: rest) | Expr.EList (LVar loc :: rest)
            -> match_offset rest loc Fmt.string
          | _ -> Fmt.to_to_string (Fmt.hbox Expr.pp) value
        in
        Some (var, value))
    store
