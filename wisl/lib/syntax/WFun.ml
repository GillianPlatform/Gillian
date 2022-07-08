open VisitorUtils

type t = {
  name : string;
  params : string list;
  body : WStmt.t list;
  spec : WSpec.t option;
  return_expr : WExpr.t;
  floc : CodeLoc.t;
  fid : int;
}

let get_id f = f.fid
let get_loc f = f.floc
let get_name f = f.name
let get_spec f = f.spec

let add_spec f pre post variant loc =
  let spec = WSpec.make pre post variant f.name f.params loc in
  { f with spec = Some spec; floc = loc }

let functions_called f = WStmt.functions_called_by_list f.body
let has_spec f = Option.is_some f.spec

let get_by_id id f =
  let stmt_list_visitor = list_visitor_builder WStmt.get_by_id id in
  let aux_spec = Option.fold ~some:(WSpec.get_by_id id) ~none:`None in
  let expr_getter = WExpr.get_by_id id in
  let self_or_none = if f.fid = id then `WFun f else `None in
  let return_getter (ret_exp : WExpr.t) =
    if WExpr.get_id ret_exp = id then `Return ret_exp else `None
  in
  self_or_none
  |>> (return_getter, f.return_expr)
  |>> (expr_getter, f.return_expr)
  |>> (stmt_list_visitor, f.body)
  |>> (aux_spec, f.spec)

let pp fmt f =
  let pp_list_stmt = WStmt.pp_list in
  match f.spec with
  | None ->
      Format.fprintf fmt
        "@[<v 2>@[<h 0>function %s(%a)@] {@,%a;@,@[<h 0>return@ %a@]@]@\n}"
        f.name
        (WPrettyUtils.pp_list Format.pp_print_string)
        f.params pp_list_stmt f.body WExpr.pp f.return_expr
  | Some spec ->
      Format.fprintf fmt
        "@[{ %a }@]@[<v 2>@[<h 0>function %s(%a)@] {@,\
         %a;@,\
         @[<h 0>return@ %a@]@]@\n\
         }@\n\
         @[{ %a }@]" WLAssert.pp (WSpec.get_pre spec) f.name
        (WPrettyUtils.pp_list Format.pp_print_string)
        f.params pp_list_stmt f.body WExpr.pp f.return_expr WLAssert.pp
        (WSpec.get_post spec)
