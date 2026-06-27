open VisitorUtils

type tt =
  | Val of WVal.t
  | Var of string
  | BinOp of t * WBinOp.t * t
  | UnOp of WUnOp.t * t
  | List of t list

and t = { weid : int; weloc : CodeLoc.t; wenode : tt }

let get e = e.wenode
let get_loc e = e.weloc
let get_id e = e.weid

let make bare_expr loc =
  { weid = Generators.gen_id (); weloc = loc; wenode = bare_expr }

let rec get_by_id id e =
  let getter = get_by_id id in
  let aux ep =
    match get ep with
    | BinOp (e1, _, e2) -> getter e1 |>> (getter, e2)
    | UnOp (_, epp) -> getter epp
    | _ -> `None
  in
  let self_or_none = if get_id e = id then `WExpr e else `None in
  self_or_none |>> (aux, e)

let rec pp fmt e =
  match get e with
  | Val v -> WVal.pp fmt v
  | Var s -> Format.fprintf fmt "@[%s@]" s
  | List el ->
      WPrettyUtils.pp_list ~pre:(format_of_string "@[[")
        ~suf:(format_of_string "]@]")
        ~empty:(format_of_string "@[nil@]")
        pp fmt el
  | BinOp (e1, b, e2) ->
      Format.fprintf fmt "@[(%a@ %a@ %a)@]" pp e1 WBinOp.pp b pp e2
  | UnOp (u, e) -> Format.fprintf fmt "@[%a%a@]" WUnOp.pp u pp e

let str = Format.asprintf "%a" pp
