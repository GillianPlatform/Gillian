module List_utils = Gillian.Utils.List_utils

let assert_unhandled ~feature args =
  let open Gil_syntax in
  let open Stats.Unhandled in
  let () = signal feature in
  let feature_string = Expr.string (feature_to_string feature) in
  Cmd.Fail ("unhandled", feature_string :: args)

let set_first_label_opt ~annot:b label stmts =
  let open Gil_syntax in
  match stmts with
  | [] -> [ b ?label Cmd.Skip ]
  | (a, None, cmd) :: r -> (a, label, cmd) :: r
  | (_, Some _, _) :: _ -> b ?label Cmd.Skip :: stmts

let set_first_label ~annot label stmts =
  set_first_label_opt ~annot (Some label) stmts

let set_end ?(is_end = true) =
  List_utils.map_last (fun (annot, label, cmd) ->
      let annot = C2_annot.set_end ~is_end annot in
      (annot, label, cmd))
