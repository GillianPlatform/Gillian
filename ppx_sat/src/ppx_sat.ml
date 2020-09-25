open Ppxlib

let ext ext =
  Extension.declare_with_path_arg
    (Ppx_sat_expander.Extension_name.to_string ext)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg:_ expr -> Ppx_sat_expander.expand ~ext expr)

let () = Driver.register_transformation "sat" ~extensions:[ ext Sat; ext Ent ]
