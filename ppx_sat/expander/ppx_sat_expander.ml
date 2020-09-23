open Ppxlib
open Ast_builder.Default

let ext_name = "sat"

let ppx_sat_runtime = Lident "Ppx_sat_runtime"

let if_then_else = Ldot (ppx_sat_runtime, "if_then_else")

let fexpr loc = pexp_ident ~loc (Located.mk ~loc if_then_else)

let to_thunk ~loc exp =
  let any_pattern = ppat_any ~loc in
  pexp_fun ~loc Nolabel None any_pattern exp

let expand_if ~loc expr then_ else_ =
  let fexpr = fexpr loc in
  pexp_apply ~loc fexpr
    [
      (Nolabel, expr);
      (Labelled "then_branch", to_thunk ~loc then_);
      (Labelled "else_branch", to_thunk ~loc else_);
    ]

let expand expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let expansion =
    match expr.pexp_desc with
    | Pexp_ifthenelse (expr, then_, else_) ->
        let else_ =
          match else_ with
          | Some else_ -> else_
          | None       ->
              Location.raise_errorf ~loc "'if%%%s' must include an else branch"
                ext_name
        in
        expand_if ~loc expr then_ else_
    | _ -> Location.raise_errorf ~loc "%%%s can only be used with 'if'" ext_name
  in
  {
    expansion with
    pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes;
  }
