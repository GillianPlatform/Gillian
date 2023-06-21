open VisitorUtils
module SS = Set.Make (String)

type tt =
  | LEmp
  | LStar of t * t
  | LPred of string * WLExpr.t list
  | LPointsTo of WLExpr.t * (WLExpr.t option * WLExpr.t) list
  | LBlockPointsTo of WLExpr.t * (WLExpr.t option * WLExpr.t) list
  | LPure of WLFormula.t

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
  let from_wlexpr_option_list lel =
    let lel = List.map Option.get @@ List.filter Option.is_some lel in
    List.fold_left double_union (SS.empty, SS.empty)
      (List.map WLExpr.get_vars_and_lvars lel)
  in

  let resolve_points_to el lel =
    let lel1 = List.map fst lel in
    let lel2 = List.map snd lel in
    let aux =
      double_union (WLExpr.get_vars_and_lvars el) (from_wlexpr_list lel2)
    in
    double_union aux (from_wlexpr_option_list lel1)
  in

  match get asrt with
  | LEmp -> (SS.empty, SS.empty)
  | LStar (a1, a2) ->
      double_union (get_vars_and_lvars a1) (get_vars_and_lvars a2)
  | LPred (_, lel) -> from_wlexpr_list lel
  | LPointsTo (el, lel) -> resolve_points_to el lel
  | LBlockPointsTo (el, lel) -> resolve_points_to el lel
  | LPure lf -> WLFormula.get_vars_and_lvars lf

let rec get_by_id id la =
  let getter = get_by_id id in
  let lexpr_getter = WLExpr.get_by_id id in
  let lexpr_list_visitor = list_visitor_builder WLExpr.get_by_id id in
  let lform_getter = WLFormula.get_by_id id in
  let resolve_points_to le1 lle2 =
    let lle1 = List.map fst lle2 in
    let lle2 = List.map snd lle2 in
    let lle1 = List.map Option.get @@ List.filter Option.is_some lle1 in
    lexpr_getter le1 |>> (lexpr_list_visitor, lle1)
    |>> (lexpr_list_visitor, lle2)
  in
  let aux lap =
    match get lap with
    | LStar (la1, la2) -> getter la1 |>> (getter, la2)
    | LPred (_, lel) -> lexpr_list_visitor lel
    | LPointsTo (le1, lle2) -> resolve_points_to le1 lle2
    | LBlockPointsTo (le1, lle2) -> resolve_points_to le1 lle2
    | LPure lf -> lform_getter lf
    | _ -> `None
  in
  let self_or_none = if get_id la = id then `WLAssert la else `None in
  self_or_none |>> (aux, la)

let rec pp fmt asser =
  let pair_pp fmt (perm, expr) =
    match perm with
    | None -> WLExpr.pp fmt expr
    | Some perm ->
        Format.fprintf fmt "@[(%a: %a)]@" WLExpr.pp perm WLExpr.pp expr
  in
  let pp_params = WPrettyUtils.pp_list WLExpr.pp in
  let pp_values_with_perm = WPrettyUtils.pp_list pair_pp in
  match get asser with
  | LEmp -> Format.pp_print_string fmt "emp"
  | LStar (a1, a2) -> Format.fprintf fmt "@[(%a) * (%a)@]" pp a1 pp a2
  | LPred (pname, lel) -> Format.fprintf fmt "@[%s(%a)@]" pname pp_params lel
  | LPointsTo (le1, le2) ->
      Format.fprintf fmt "@[(%a) -> %a@]" WLExpr.pp le1 pp_values_with_perm le2
  | LBlockPointsTo (le1, le2) ->
      Format.fprintf fmt "@[(%a) -b-> %a@]" WLExpr.pp le1 pp_values_with_perm
        le2
  | LPure f -> Format.fprintf fmt "@[%a@]" WLFormula.pp f

let str = Format.asprintf "%a" pp

let rec substitution (subst : (string, WLExpr.tt) Hashtbl.t) (a : t) : t =
  let { wlaid; wlaloc; wlanode } = a in
  let f = substitution subst in
  let fe = WLExpr.substitution subst in
  let f_pair (perm, expr) =
    let perm = Option.map fe perm in
    (perm, fe expr)
  in
  let wlanode =
    match wlanode with
    | LEmp -> LEmp
    | LStar (a1, a2) -> LStar (f a1, f a2)
    | LPred (name, le) -> LPred (name, List.map fe le)
    | LPointsTo (e1, le) -> LPointsTo (fe e1, List.map f_pair le)
    | LBlockPointsTo (e1, le) -> LBlockPointsTo (fe e1, List.map f_pair le)
    | LPure frm -> LPure (WLFormula.substitution subst frm)
  in
  { wlaid; wlaloc; wlanode }
