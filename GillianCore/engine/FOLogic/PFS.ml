(** GIL Pure Formulae *)

open Containers
open SVal
module L = Logging

type t = Formula.t ExtList.t [@@deriving yojson]

(**************************************)
(** Pure formulae functions          **)

(**************************************)

let init () : t = ExtList.make ()

let equal (pfs1 : t) (pfs2 : t) : bool = ExtList.for_all2 ( = ) pfs1 pfs2

let to_list : t -> Formula.t list = ExtList.to_list

let of_list : Formula.t list -> t = ExtList.of_list

let to_set pfs =
  ExtList.fold_left (fun acc el -> Formula.Set.add el acc) Formula.Set.empty pfs

let mem (pfs : t) (f : Formula.t) = ExtList.mem f pfs

let extend (pfs : t) (a : Formula.t) : unit =
  if not (mem pfs a) then ExtList.add a pfs

let clear (pfs : t) : unit = ExtList.clear pfs

let length (pfs : t) = ExtList.length pfs

let copy (pfs : t) : t = ExtList.copy pfs

let merge_into_left (pfs_l : t) (pfs_r : t) : unit = ExtList.concat pfs_l pfs_r

let set (pfs : t) (reset : Formula.t list) : unit =
  clear pfs;
  merge_into_left pfs (of_list reset)

let substitution (subst : SESubst.t) (pfs : t) : unit =
  ExtList.map_inplace (SESubst.substitute_formula ~partial:true subst) pfs

let subst_expr_for_expr (to_subst : Expr.t) (subst_with : Expr.t) (pfs : t) :
    unit =
  ExtList.map_inplace (Formula.subst_expr_for_expr ~to_subst ~subst_with) pfs

let lvars (pfs : t) : SS.t =
  ExtList.fold_left (fun ac a -> SS.union ac (Formula.lvars a)) SS.empty pfs

let alocs (pfs : t) : SS.t =
  ExtList.fold_left (fun ac a -> SS.union ac (Formula.alocs a)) SS.empty pfs

let clocs (pfs : t) : SS.t =
  ExtList.fold_left (fun ac a -> SS.union ac (Formula.clocs a)) SS.empty pfs

let pp = Fmt.vbox (ExtList.pp ~sep:Fmt.cut Formula.pp)

let sort (p_formulae : t) : unit =
  let pfl = to_list p_formulae in
  let var_eqs, llen_eqs, others =
    List.fold_left
      (fun (var_eqs, llen_eqs, others) (pf : Formula.t) ->
        match pf with
        | Eq (LVar _, _) | Eq (_, LVar _) -> (pf :: var_eqs, llen_eqs, others)
        | Eq (UnOp (LstLen, _), _) | Eq (_, UnOp (LstLen, _)) ->
            (var_eqs, pf :: llen_eqs, others)
        | _ -> (var_eqs, llen_eqs, pf :: others))
      ([], [], []) pfl
  in
  let var_eqs, llen_eqs, others =
    (List.rev var_eqs, List.rev llen_eqs, List.rev others)
  in
  set p_formulae (var_eqs @ llen_eqs @ others)

let iter = ExtList.iter

let fold_left = ExtList.fold_left

let map_inplace = ExtList.map_inplace

let remove_duplicates pfs = ExtList.remove_duplicates pfs

let filter_map_stop = ExtList.filter_map_stop

let filter_stop_cond = ExtList.filter_stop_cond

let filter = ExtList.filter

let filter_map = ExtList.filter_map

let exists = ExtList.exists

let get_nth = ExtList.nth

let rec get_relevant_info (_ : SS.t) (lvars : SS.t) (locs : SS.t) (pfs : t) :
    SS.t * SS.t * SS.t =
  let relevant = SS.union lvars locs in
  let new_pvars, new_lvars, new_locs =
    fold_left
      (fun (new_pvars, new_lvars, new_locs) pf ->
        let pf_pvars, pf_lvars, pf_locs = Formula.get_print_info pf in
        let pf_relevant =
          List.fold_left SS.union SS.empty [ pf_pvars; pf_lvars; pf_locs ]
        in
        if SS.inter relevant pf_relevant = SS.empty then
          (new_pvars, new_lvars, new_locs)
        else
          ( SS.union new_pvars pf_pvars,
            SS.union new_lvars pf_lvars,
            SS.union new_locs pf_locs ))
      (SS.empty, SS.empty, SS.empty)
      pfs
  in
  if new_lvars = lvars && new_locs = locs then (new_pvars, new_lvars, new_locs)
  else get_relevant_info new_pvars new_lvars new_locs pfs

let filter_with_info relevant_info (pfs : t) : t =
  let pvars, lvars, locs = relevant_info in

  let _, lvars, locs = get_relevant_info pvars lvars locs pfs in

  let relevant = List.fold_left SS.union SS.empty [ lvars; locs ] in
  let filtered_pfs = copy pfs in
  let () =
    filter
      (fun pf ->
        not
          (SS.is_empty
             (SS.inter relevant (SS.union (Formula.lvars pf) (Formula.locs pf)))))
      filtered_pfs
  in
  filtered_pfs

let pp_by_need relevant_info fmt pfs =
  let filtered_pfs = filter_with_info relevant_info pfs in
  pp fmt filtered_pfs
