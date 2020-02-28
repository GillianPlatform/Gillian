open VisitorUtils

type tt =
  | LTrue
  | LFalse
  | LNot       of t
  | LAnd       of t * t
  | LOr        of t * t
  | LEq        of WLExpr.t * WLExpr.t
  | LLess      of WLExpr.t * WLExpr.t
  | LGreater   of WLExpr.t * WLExpr.t
  | LLessEq    of WLExpr.t * WLExpr.t
  | LGreaterEq of WLExpr.t * WLExpr.t

and t = { wlfid : int; wlfloc : CodeLoc.t; wlfnode : tt }

let get lf = lf.wlfnode

let get_loc e = e.wlfloc

let get_id e = e.wlfid

let make bare_form loc =
  { wlfid = Generators.gen_id (); wlfloc = loc; wlfnode = bare_form }

let rec not f =
  let make fp = make fp (get_loc f) in
  match get f with
  | LTrue               -> make LFalse
  | LFalse              -> make LTrue
  | LNot fp             -> fp
  | LAnd (f1, f2)       -> make (LOr (not f1, not f2))
  | LOr (f1, f2)        -> make (LAnd (not f1, not f2))
  | LLess (e1, e2)      -> make (LGreaterEq (e1, e2))
  | LLessEq (e1, e2)    -> make (LGreater (e1, e2))
  | LGreater (e1, e2)   -> make (LLessEq (e1, e2))
  | LGreaterEq (e1, e2) -> make (LLess (e1, e2))
  | LEq (e1, e2)        -> make (LNot f)

let rec lexpr_is_true ?(codeloc = CodeLoc.dummy) lexpr =
  let f = lexpr_is_true ~codeloc in
  let bare =
    match WLExpr.get lexpr with
    | LVal (Bool true) -> LTrue
    | LVal _ -> LFalse
    | LBinOp (e1, EQUAL, e2) -> LEq (e1, e2)
    | LBinOp (e1, NEQ, e2) ->
        let inner = make (LEq (e1, e2)) codeloc in
        LNot inner
    | LBinOp (e1, AND, e2) -> LAnd (f e1, f e2)
    | LBinOp (e1, OR, e2) -> LOr (f e1, f e2)
    | LBinOp (e1, LESSTHAN, e2) -> LLess (e1, e2)
    | LBinOp (e1, LESSEQUAL, e2) -> LLessEq (e1, e2)
    | LBinOp (e1, GREATERTHAN, e2) -> LGreater (e1, e2)
    | LBinOp (e1, GREATEREQUAL, e2) -> LGreaterEq (e1, e2)
    | LUnOp (NOT, e) -> LNot (f e)
    | PVar _ ->
        let ttrue = WLExpr.make (LVal (Bool true)) codeloc in
        LEq (lexpr, ttrue)
    | _ -> LFalse
  in
  make bare codeloc

let rec get_vars_and_lvars f =
  let module SS = Set.Make (String) in
  let double_union (sa1, sb1) (sa2, sb2) =
    (SS.union sa1 sa2, SS.union sb1 sb2)
  in
  match get f with
  | LNot f -> get_vars_and_lvars f
  | LAnd (f1, f2) | LOr (f1, f2) ->
      double_union (get_vars_and_lvars f1) (get_vars_and_lvars f2)
  | LEq (e1, e2)
  | LLess (e1, e2)
  | LGreater (e1, e2)
  | LLessEq (e1, e2)
  | LGreaterEq (e1, e2) ->
      double_union (WLExpr.get_vars_and_lvars e1) (WLExpr.get_vars_and_lvars e2)
  | _ -> (SS.empty, SS.empty)

let rec get_by_id id lf =
  let getter = get_by_id id in
  let lexpr_getter = WLExpr.get_by_id id in
  let aux lfp =
    match get lfp with
    | LNot lfpp -> getter lfpp
    | LAnd (lf1, lf2) | LOr (lf1, lf2) -> getter lf1 |>> (getter, lf2)
    | LEq (le1, le2)
    | LLess (le1, le2)
    | LGreater (le1, le2)
    | LLessEq (le1, le2)
    | LGreaterEq (le1, le2) -> lexpr_getter le1 |>> (lexpr_getter, le2)
    | _ -> `None
  in
  let self_or_none = if get_id lf = id then `WLFormula lf else `None in
  self_or_none |>> (aux, lf)

let rec pp fmt formula =
  match get formula with
  | LTrue                 -> Format.pp_print_string fmt "True"
  | LFalse                -> Format.pp_print_string fmt "False"
  | LNot f                -> Format.fprintf fmt "@[!(%a)@]" pp f
  | LAnd (f1, f2)         -> Format.fprintf fmt "@[(%a) /\\ (%a)@]" pp f1 pp f2
  | LOr (f1, f2)          -> Format.fprintf fmt "@[(%a) \\/ (%a)@]" pp f1 pp f2
  | LEq (le1, le2)        ->
      Format.fprintf fmt "@[(%a) == (%a)@]" WLExpr.pp le1 WLExpr.pp le2
  | LLess (le1, le2)      ->
      Format.fprintf fmt "@[(%a) <# (%a)@]" WLExpr.pp le1 WLExpr.pp le2
  | LGreater (le1, le2)   ->
      Format.fprintf fmt "@[(%a) ># (%a)@]" WLExpr.pp le1 WLExpr.pp le2
  | LLessEq (le1, le2)    ->
      Format.fprintf fmt "@[(%a) <=# (%a)@]" WLExpr.pp le1 WLExpr.pp le2
  | LGreaterEq (le1, le2) ->
      Format.fprintf fmt "@[(%a) >=# (%a)@]" WLExpr.pp le1 WLExpr.pp le2

let str = Format.asprintf "%a" pp

let rec substitution (subst : (string, WLExpr.tt) Hashtbl.t) (frm : t) : t =
  let { wlfid; wlfloc; wlfnode } = frm in
  let f = substitution subst in
  let fe = WLExpr.substitution subst in
  let wlfnode =
    match wlfnode with
    | LTrue               -> LTrue
    | LFalse              -> LFalse
    | LNot frm            -> LNot (f frm)
    | LAnd (frm1, frm2)   -> LAnd (f frm1, f frm2)
    | LOr (frm1, frm2)    -> LOr (f frm1, f frm2)
    | LEq (e1, e2)        -> LEq (fe e1, fe e2)
    | LLess (e1, e2)      -> LLess (fe e1, fe e2)
    | LGreater (e1, e2)   -> LGreater (fe e1, fe e2)
    | LLessEq (e1, e2)    -> LLessEq (fe e1, fe e2)
    | LGreaterEq (e1, e2) -> LGreaterEq (fe e1, fe e2)
  in
  { wlfid; wlfloc; wlfnode }
