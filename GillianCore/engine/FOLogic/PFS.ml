open SVal
module L = Logging

type t = Expr.t Ext_list.t [@@deriving yojson]

let init () : t = Ext_list.make ()
let equal (pfs1 : t) (pfs2 : t) : bool = Ext_list.for_all2 Expr.equal pfs1 pfs2
let to_list : t -> Expr.t list = Ext_list.to_list
let of_list : Expr.t list -> t = Ext_list.of_list

let to_set pfs =
  Ext_list.fold_left (fun acc el -> Expr.Set.add el acc) Expr.Set.empty pfs

let mem (pfs : t) (f : Expr.t) = Ext_list.mem ~equal:Expr.equal f pfs

let extend (pfs : t) (a : Expr.t) : unit =
  if not (mem pfs a) then Ext_list.add a pfs

let clear (pfs : t) : unit = Ext_list.clear pfs
let length (pfs : t) = Ext_list.length pfs
let copy (pfs : t) : t = Ext_list.copy pfs
let merge_into_left (pfs_l : t) (pfs_r : t) : unit = Ext_list.concat pfs_l pfs_r

let set (pfs : t) (reset : Expr.t list) : unit =
  clear pfs;
  merge_into_left pfs (of_list reset)

let substitution (subst : SESubst.t) (pfs : t) : unit =
  Ext_list.map_inplace (SESubst.subst_in_expr ~partial:true subst) pfs

let subst_expr_for_expr (to_subst : Expr.t) (subst_with : Expr.t) (pfs : t) :
    unit =
  Ext_list.map_inplace (Expr.subst_expr_for_expr ~to_subst ~subst_with) pfs

let lvars (pfs : t) : LVar.Set.t =
  Ext_list.fold_left
    (fun ac a -> LVar.Set.union ac (Expr.lvars a))
    LVar.Set.empty pfs

let alocs (pfs : t) : ALoc.Set.t =
  Ext_list.fold_left
    (fun ac a -> ALoc.Set.union ac (Expr.alocs a))
    ALoc.Set.empty pfs

let clocs (pfs : t) : Loc.Set.t =
  Ext_list.fold_left
    (fun ac a -> Loc.Set.union ac (Expr.clocs a))
    Loc.Set.empty pfs

let pp = Fmt.vbox (Ext_list.pp ~sep:Fmt.cut Expr.pp)

let sort (p_formulae : t) : unit =
  let pfl = to_list p_formulae in
  let var_eqs, llen_eqs, others =
    List.fold_left
      (fun (var_eqs, llen_eqs, others) (pf : Expr.t) ->
        match pf with
        | BinOp (LVar _, Equal, _) | BinOp (_, Equal, LVar _) ->
            (pf :: var_eqs, llen_eqs, others)
        | BinOp (UnOp (LstLen, _), Equal, _) | BinOp (_, Equal, UnOp (LstLen, _))
          -> (var_eqs, pf :: llen_eqs, others)
        | _ -> (var_eqs, llen_eqs, pf :: others))
      ([], [], []) pfl
  in
  let var_eqs, llen_eqs, others =
    (List.rev var_eqs, List.rev llen_eqs, List.rev others)
  in
  set p_formulae (var_eqs @ llen_eqs @ others)

let iter = Ext_list.iter
let fold_left = Ext_list.fold_left
let map_inplace = Ext_list.map_inplace
let remove_duplicates pfs = Ext_list.remove_duplicates pfs
let filter_map_stop = Ext_list.filter_map_stop
let filter_stop_cond = Ext_list.filter_stop_cond
let filter = Ext_list.filter
let filter_map = Ext_list.filter_map
let exists = Ext_list.exists
let get_nth = Ext_list.nth

let clean_up pfs =
  Ext_list.filter
    (fun (pf : Expr.t) ->
      match pf with
      | Expr.BinOp (Lit (Int x), BinOp.ILessThanEqual, UnOp (LstLen, _))
        when x = Z.zero -> false
      | _ -> true)
    pfs

let rec get_relevant_info
    (_ : Var.Set.t)
    (lvars : LVar.Set.t)
    (locs : Id.Sets.LocSet.t)
    (pfs : t) : Var.Set.t * LVar.Set.t * Id.Sets.LocSet.t =
  let new_pvars, new_lvars, new_locs =
    fold_left
      (fun (new_pvars, new_lvars, new_locs) pf ->
        let pf_pvars = Expr.pvars pf in
        let pf_lvars = Expr.lvars pf in
        let pf_locs = Expr.locs pf in
        if
          (LVar.Set.is_empty @@ LVar.Set.inter lvars pf_lvars)
          || (Id.Sets.LocSet.is_empty @@ Id.Sets.LocSet.inter locs pf_locs)
        then (new_pvars, new_lvars, new_locs)
        else
          ( Var.Set.union new_pvars pf_pvars,
            LVar.Set.union new_lvars pf_lvars,
            Id.Sets.LocSet.union new_locs pf_locs ))
      (Var.Set.empty, LVar.Set.empty, Id.Sets.LocSet.empty)
      pfs
  in
  if new_lvars = lvars && new_locs = locs then (new_pvars, new_lvars, new_locs)
  else get_relevant_info new_pvars new_lvars new_locs pfs

let filter_with_info relevant_info (pfs : t) : t =
  let pvars, lvars, alocs = relevant_info in

  let _, lvars, alocs = get_relevant_info pvars lvars alocs pfs in

  let filtered_pfs = copy pfs in
  let () =
    filter
      (fun pf ->
        LVar.Set.(not @@ is_empty @@ inter (Expr.lvars pf) lvars)
        || Id.Sets.LocSet.(not @@ is_empty @@ inter (Expr.locs pf) alocs))
      filtered_pfs
  in
  filtered_pfs

let pp_by_need relevant_info fmt pfs =
  let filtered_pfs = filter_with_info relevant_info pfs in
  pp fmt filtered_pfs
