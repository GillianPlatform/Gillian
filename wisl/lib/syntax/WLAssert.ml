open VisitorUtils
module SS = Set.Make (String)

type tt =
  | LEmp
  | LStar of t * t
  | LWand of { lhs : string * WLExpr.t list; rhs : string * WLExpr.t list }
  | LPred of string * WLExpr.t list
  | LPointsTo of WLExpr.t * WLExpr.t list
  | LBlockPointsTo of WLExpr.t * WLExpr.t list
  | LPure of WLExpr.t

and t = { wlaid : int; wlaloc : CodeLoc.t; wlanode : tt }

let get la = la.wlanode
let get_id la = la.wlaid
let get_loc la = la.wlaloc

let make bare_assert loc =
  { wlanode = bare_assert; wlaloc = loc; wlaid = Generators.gen_id () }

let copy la = make (get la) (get_loc la)

let rec get_vars_and_lvars asrt =
  let double_union (sa1, sb1) (sa2, sb2) =
    (SS.union sa1 sa2, SS.union sb1 sb2)
  in
  let from_wlexpr_list lel =
    List.fold_left double_union (SS.empty, SS.empty)
      (List.map WLExpr.get_vars_and_lvars lel)
  in
  match get asrt with
  | LEmp -> (SS.empty, SS.empty)
  | LStar (a1, a2) ->
      double_union (get_vars_and_lvars a1) (get_vars_and_lvars a2)
  | LPred (_, lel) -> from_wlexpr_list lel
  | LPointsTo (el, lel) ->
      double_union (WLExpr.get_vars_and_lvars el) (from_wlexpr_list lel)
  | LBlockPointsTo (el, lel) ->
      double_union (WLExpr.get_vars_and_lvars el) (from_wlexpr_list lel)
  | LPure lf -> WLExpr.get_vars_and_lvars lf
  | LWand { lhs = _, largs; rhs = _, rargs } ->
      double_union (from_wlexpr_list largs) (from_wlexpr_list rargs)

let rec get_by_id id la =
  let getter = get_by_id id in
  let lexpr_getter = WLExpr.get_by_id id in
  let lexpr_list_visitor = list_visitor_builder WLExpr.get_by_id id in
  let lform_getter = WLExpr.get_by_id id in
  let aux lap =
    match get lap with
    | LStar (la1, la2) -> getter la1 |>> (getter, la2)
    | LPred (_, lel) -> lexpr_list_visitor lel
    | LPointsTo (le1, lle2) -> lexpr_getter le1 |>> (lexpr_list_visitor, lle2)
    | LBlockPointsTo (le1, lle2) ->
        lexpr_getter le1 |>> (lexpr_list_visitor, lle2)
    | LPure lf -> lform_getter lf
    | _ -> `None
  in
  let self_or_none = if get_id la = id then `WLAssert la else `None in
  self_or_none |>> (aux, la)

let rec pp fmt asser =
  let pp_params = WPrettyUtils.pp_list WLExpr.pp in
  match get asser with
  | LEmp -> Format.pp_print_string fmt "emp"
  | LStar (a1, a2) -> Format.fprintf fmt "@[(%a) * (%a)@]" pp a1 pp a2
  | LPred (pname, lel) -> Format.fprintf fmt "@[%s(%a)@]" pname pp_params lel
  | LWand { lhs = lname, largs; rhs = rname, rargs } ->
      Format.fprintf fmt "@[%s(%a) -* %s(%a)]" lname pp_params largs rname
        pp_params rargs
  | LPointsTo (le1, le2) ->
      Format.fprintf fmt "@[(%a) -> %a@]" WLExpr.pp le1 pp_params le2
  | LBlockPointsTo (le1, le2) ->
      Format.fprintf fmt "@[(%a) -b-> %a@]" WLExpr.pp le1 pp_params le2
  | LPure f -> Format.fprintf fmt "@[(%a)@]" WLExpr.pp f

let str = Format.asprintf "%a" pp

let rec substitution (subst : (string, WLExpr.tt) Hashtbl.t) (a : t) : t =
  let { wlaid; wlaloc; wlanode } = a in
  let f = substitution subst in
  let fe = WLExpr.substitution subst in
  let wlanode =
    match wlanode with
    | LEmp -> LEmp
    | LStar (a1, a2) -> LStar (f a1, f a2)
    | LPred (name, le) -> LPred (name, List.map fe le)
    | LWand { lhs = lname, largs; rhs = rname, rargs } ->
        LWand
          { lhs = (lname, List.map fe largs); rhs = (rname, List.map fe rargs) }
    | LPointsTo (e1, le) -> LPointsTo (fe e1, List.map fe le)
    | LBlockPointsTo (e1, le) -> LBlockPointsTo (fe e1, List.map fe le)
    | LPure frm -> LPure (WLExpr.substitution subst frm)
  in
  { wlaid; wlaloc; wlanode }
