open GJS_syntax

module Constants = struct
  let module_obj = "Module"

  let module_var = "_module"

  let cache_var = "_cache"

  let require_f = "require"

  let module_params = [ "exports"; "module"; "__filename"; "__dirname" ]

  let not_loaded = "NOT_LOADED"

  let loading = "LOADING"

  let loaded = "LOADED"
end

let mk_exp syntax : exp = mk_exp syntax Flow_parser.Loc.none []

let expr_of_ident_str (str : string) : exp = mk_exp (Var str)

let expr_of_lit_str str : exp = mk_exp (String str)

let assign_expr left right : exp = mk_exp (Assign (left, right))

let access_expr obj prop : exp = mk_exp (Access (expr_of_ident_str obj, prop))

(** Generates: _module = new Module(<filename>, <dirname>) *)
let module_init_assign filename dirname : exp =
  let left = expr_of_ident_str Constants.module_var in
  let callee = expr_of_ident_str Constants.module_obj in
  let call_args = List.map expr_of_lit_str [ filename; dirname ] in
  let right = mk_exp (New (callee, call_args)) in
  assign_expr left right

(** Generates: _cache[<filename>] = _module *)
let module_cache_assign filename : exp =
  let cache_obj = expr_of_ident_str Constants.cache_var in
  let module_id = expr_of_lit_str filename in
  let left = mk_exp (CAccess (cache_obj, module_id)) in
  let right = expr_of_ident_str Constants.module_var in
  assign_expr left right

(** Generates: module.status = <status> *)
let module_status_assign status : exp =
  assign_expr (access_expr "module" "status") (expr_of_lit_str status)

let set_start_load_status : exp = module_status_assign Constants.loading

let set_end_load_satus : exp = module_status_assign Constants.loaded

let make_func_expr use_strict expr_list param_list : exp =
  let body = mk_exp (Block expr_list) in
  mk_exp (FunctionExp (use_strict, None, param_list, body))

(** Generates an IIFE:
    (function (exports, module, __filename, __dirname) {
        <module_body>
    })(_module.exports, _module, _module.filename, _module.dirname) *)
let immediate_module_load use_strict module_body : exp =
  let func = make_func_expr use_strict module_body Constants.module_params in
  let call_args =
    [
      access_expr Constants.module_var "exports";
      expr_of_ident_str Constants.module_var;
      access_expr Constants.module_var "filename";
      access_expr Constants.module_var "dirname";
    ]
  in
  mk_exp (Call (func, call_args))

(** Generates:
    _module.load = function (exports, module, __filename, __dirname) {
        <module_body>
    } *)
let module_load_func use_strict module_body : exp =
  let left = access_expr Constants.module_var "load" in
  let func = make_func_expr use_strict module_body Constants.module_params in
  assign_expr left func
