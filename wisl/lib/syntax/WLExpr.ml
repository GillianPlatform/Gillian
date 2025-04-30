open VisitorUtils

type tt =
  | LVal of WVal.t
  | LVar of string
  | PVar of string
  | LBinOp of t * WBinOp.t * t
  | LUnOp of WUnOp.t * t
  | LLSub of t * t * t
  | LEList of t list
  | LESet of t list
  | LFuncApp of string * t list (* Function application *)
  | LConstructorApp of string * t list (* Constructor application *)
  | LCases of t * case list

and case = { constructor : string; binders : string list; lexpr : t }
and t = { wleid : int; wleloc : CodeLoc.t; wlenode : tt }

let get le = le.wlenode
let get_loc le = le.wleloc
let get_id le = le.wleid

let make bare_lexpr loc =
  { wlenode = bare_lexpr; wleloc = loc; wleid = Generators.gen_id () }

let rec from_expr expr =
  let wleid = Generators.gen_id () in
  let wleloc = WExpr.get_loc expr in
  let wlenode =
    match WExpr.get expr with
    | Val v -> LVal v
    | Var x -> LVar x
    | BinOp (e1, b, e2) -> LBinOp (from_expr e1, b, from_expr e2)
    | UnOp (u, e) -> LUnOp (u, from_expr e)
    | List el -> LEList (List.map from_expr el)
  in
  { wleid; wleloc; wlenode }

let get_vars_and_lvars le =
  let module SS = Set.Make (String) in
  match get le with
  | LVar v -> (SS.empty, SS.singleton v)
  | PVar v -> (SS.singleton v, SS.empty)
  | _ -> (SS.empty, SS.empty)

let rec get_by_id id lexpr =
  let getter = get_by_id id in
  let list_visitor = list_visitor_builder get_by_id id in
  let aux le =
    match get le with
    | LBinOp (le1, _, le2) -> getter le1 |>> (getter, le2)
    | LUnOp (_, lep) -> getter lep
    | LEList lel -> list_visitor lel
    | LESet lel -> list_visitor lel
    | LFuncApp (_, lel) | LConstructorApp (_, lel) -> list_visitor lel
    | _ -> `None
  in
  let self_or_none = if get_id lexpr = id then `WLExpr lexpr else `None in
  self_or_none |>> (aux, lexpr)

let rec pp fmt lexpr =
  match get lexpr with
  | LVal v -> WVal.pp fmt v
  | LVar lx -> Format.fprintf fmt "@[%s@]" lx
  | PVar x -> Format.fprintf fmt "@[%s@]" x
  | LBinOp (le1, b, le2) ->
      Format.fprintf fmt "@[(%a %a %a)@]" pp le1 WBinOp.pp b pp le2
  | LUnOp (u, le) -> Format.fprintf fmt "@[(%a %a)@]" WUnOp.pp u pp le
  | LLSub (le1, le2, le3) ->
      Format.fprintf fmt "@[sub(%a, %a, %a)@]" pp le1 pp le2 pp le3
  | LEList lel ->
      WPrettyUtils.pp_list ~pre:(format_of_string "@[[")
        ~suf:(format_of_string "]@]")
        ~empty:(format_of_string "@[nil@]")
        pp fmt lel
  | LESet lel ->
      WPrettyUtils.pp_list ~pre:(format_of_string "@[-{")
        ~suf:(format_of_string "}-@]") pp fmt lel
  | LFuncApp (name, lel) ->
      Format.fprintf fmt "@[%s" name;
      WPrettyUtils.pp_list ~pre:(format_of_string "(")
        ~suf:(format_of_string ")@]") ~empty:(format_of_string "@]") pp fmt lel
  | LConstructorApp (name, lel) ->
      Format.fprintf fmt "@['%s" name;
      WPrettyUtils.pp_list ~pre:(format_of_string "(")
        ~suf:(format_of_string ")@]") ~empty:(format_of_string "@]") pp fmt lel
  | LCases (le, cs) ->
      Format.fprintf fmt "@[<v>case %a {@," pp le;
      List.iter
        (fun { constructor; binders; lexpr } ->
          Format.fprintf fmt "  %s" constructor;
          WPrettyUtils.pp_list ~pre:(format_of_string "(")
            ~suf:(format_of_string ")") ~empty:(format_of_string "")
            (fun fmt s -> Format.fprintf fmt "%s" s)
            fmt binders;
          Format.fprintf fmt " -> %a;@," pp lexpr)
        cs;
      Format.fprintf fmt "}@]"

let str = Format.asprintf "%a" pp

let rec substitution (subst : (string, tt) Hashtbl.t) (e : t) : t =
  let { wleid; wleloc; wlenode } = e in
  let f = substitution subst in
  let wlenode =
    match wlenode with
    | LVal _ -> wlenode
    | LVar x -> Option.value ~default:(LVar x) (Hashtbl.find_opt subst x)
    | PVar x -> Option.value ~default:(PVar x) (Hashtbl.find_opt subst x)
    | LBinOp (e1, binop, e2) -> LBinOp (f e1, binop, f e2)
    | LUnOp (unop, e1) -> LUnOp (unop, f e1)
    | LLSub (e1, e2, e3) -> LLSub (f e1, f e2, f e3)
    | LEList le -> LEList (List.map f le)
    | LESet le -> LESet (List.map f le)
    | LFuncApp (name, le) | LConstructorApp (name, le) ->
        LFuncApp (name, List.map f le)
    | LCases (e, cs) ->
        let cs = List.map (fun c -> { c with lexpr = f c.lexpr }) cs in
        LCases (e, cs)
  in
  { wleid; wleloc; wlenode }
