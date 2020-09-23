open Ppxlib

let ext =
  Extension.declare_with_path_arg Ppx_sat_expander.ext_name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg:_ expr -> Ppx_sat_expander.expand expr)

let () = Driver.register_transformation "sat" ~extensions:[ ext ]
