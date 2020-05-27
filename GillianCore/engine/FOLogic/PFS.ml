(** GIL Pure Formulae *)

open Containers
open SVal
module L = Logging

type t = Formula.t ExtList.t

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

(* 
let nth_get (pfs : t) (n : int) = DynArray.get pfs n

let nth_set (pfs : t) (n : int) = DynArray.set pfs n *)
(* 
let nth_delete (pfs : t) (n : int) : unit = DynArray.delete pfs n *)

let clear (pfs : t) : unit = ExtList.clear pfs

let length (pfs : t) = ExtList.length pfs

let copy (pfs : t) : t = ExtList.copy pfs

let merge_into_left (pfs_l : t) (pfs_r : t) : unit = ExtList.concat pfs_r pfs_l

let set (pfs : t) (reset : Formula.t list) : unit =
  clear pfs;
  merge_into_left pfs (of_list reset)

let substitution (subst : SSubst.t) (pfs : t) : unit =
  ExtList.map_inplace (SSubst.substitute_formula ~partial:true subst) pfs

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
        | Eq (LVar x, _) | Eq (_, LVar x) -> (pf :: var_eqs, llen_eqs, others)
        | Eq (UnOp (LstLen, x), _) | Eq (_, UnOp (LstLen, x)) ->
            (var_eqs, pf :: llen_eqs, others)
        | _ -> (var_eqs, llen_eqs, pf :: others))
      ([], [], []) pfl
  in
  let var_eqs, llen_eqs, others =
    (List.rev var_eqs, List.rev llen_eqs, List.rev others)
  in
  set p_formulae (var_eqs @ llen_eqs @ others)

let iter = ExtList.iter

let iteri = ExtList.iteri

let fold_left = ExtList.fold_left

let map_inplace = ExtList.map_inplace

let remove_duplicates pfs = ExtList.remove_duplicates pfs

let filter_map_stop = ExtList.filter_map_stop

let filter_stop_cond = ExtList.filter_stop_cond

let filter = ExtList.filter

let filter_map = ExtList.filter_map

let exists = ExtList.exists

let get_nth = ExtList.nth
