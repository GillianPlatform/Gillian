open Ppxlib
open Ast_builder.Default

let ppx_sat_runtime = Lident "Ppx_sat_runtime"

module Extension_name = struct
  type t = Sat | Ent

  let to_string = function
    | Sat -> "sat"
    | Ent -> "ent"

  let if_then_else = Ldot (ppx_sat_runtime, "if_then_else")

  let if_sure_then_else = Ldot (ppx_sat_runtime, "if_sure_then_else")

  let if_fun_fexpr_of_ext = function
    | Sat -> if_then_else
    | Ent -> if_sure_then_else
end

let fexpr ~ext loc =
  let if_fun = Extension_name.if_fun_fexpr_of_ext ext in
  pexp_ident ~loc (Located.mk ~loc if_fun)

let to_thunk ~loc exp =
  let any_pattern = ppat_any ~loc in
  pexp_fun ~loc Nolabel None any_pattern exp

let expand_if ~ext ~loc expr then_ else_ =
  let fexpr = fexpr ~ext loc in
  pexp_apply ~loc fexpr
    [
      (Nolabel, expr);
      (Labelled "then_branch", to_thunk ~loc then_);
      (Labelled "else_branch", to_thunk ~loc else_);
    ]

let expand ~ext expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let expansion =
    match expr.pexp_desc with
    | Pexp_ifthenelse (expr, then_, else_) ->
        let else_ =
          match else_ with
          | Some else_ -> else_
          | None       ->
              Location.raise_errorf ~loc "'if%%%s' must include an else branch"
                (Extension_name.to_string ext)
        in
        expand_if ~ext ~loc expr then_ else_
    | _ ->
        Location.raise_errorf ~loc "%%%s can only be used with 'if'"
          (Extension_name.to_string ext)
  in
  {
    expansion with
    pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes;
  }
