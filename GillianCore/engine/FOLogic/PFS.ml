(** GIL Pure Formulae *)
open Containers

open SVal
module L = Logging

type t = Formula.t DynArray.t

(**************************************)
(** Pure formulae functions          **)

(**************************************)

let init () : t = DynArray.make Config.medium_tbl_size

let equal (pfs1 : t) (pfs2 : t) : bool =
  DynArray.length pfs1 = DynArray.length pfs2
  && List.for_all
       (fun (a, b) -> a = b)
       (List.combine (DynArray.to_list pfs1) (DynArray.to_list pfs2))

let to_list (pfs : t) : Formula.t list = DynArray.to_list pfs

let of_list (pfs : Formula.t list) : t = DynArray.of_list pfs

let mem (pfs : t) (a : Formula.t) : bool = Array.mem a (DynArray.to_array pfs)

let extend (pfs : t) (a : Formula.t) : unit =
  if not (mem pfs a) then DynArray.add pfs a

let nth_get (pfs : t) (n : int) = DynArray.get pfs n

let nth_set (pfs : t) (n : int) = DynArray.set pfs n

let nth_delete (pfs : t) (n : int) : unit = DynArray.delete pfs n

let clear (pfs : t) : unit = DynArray.clear pfs

let length (pfs : t) = DynArray.length pfs

let copy (pfs : t) : t = DynArray.copy pfs

let merge_into_left (pfs_l : t) (pfs_r : t) : unit = DynArray.append pfs_r pfs_l

let set (pfs : t) (reset : Formula.t list) : unit =
  clear pfs;
  merge_into_left pfs (of_list reset)

let substitution (subst : SSubst.t) (pfs : t) : unit =
  DynArray.iteri
    (fun i a ->
      let s_a = SSubst.substitute_formula subst true a in
      DynArray.set pfs i s_a)
    pfs

let subst_expr_for_expr (to_subst : Expr.t) (subst_with : Expr.t) (pfs : t) :
    unit =
  DynArray.iteri
    (fun i a ->
      let s_a = Formula.subst_expr_for_expr ~to_subst ~subst_with a in
      DynArray.set pfs i s_a)
    pfs

let lvars (pfs : t) : SS.t =
  DynArray.fold_left (fun ac a -> SS.union ac (Formula.lvars a)) SS.empty pfs

let alocs (pfs : t) : SS.t =
  DynArray.fold_left (fun ac a -> SS.union ac (Formula.alocs a)) SS.empty pfs

let clocs (pfs : t) : SS.t =
  DynArray.fold_left (fun ac a -> SS.union ac (Formula.clocs a)) SS.empty pfs

let count_lvar (p_formulae : t) (x : string) : int =
  let count = ref 0 in
  DynArray.iter
    (fun pf -> if SS.mem x (Formula.lvars pf) then count := !count + 1)
    p_formulae;
  !count

let pp fmt pfs =
  (Fmt.iter ~sep:(Fmt.any "@\n") DynArray.iter Formula.pp) fmt pfs

let sort (p_formulae : t) : unit =
  let pfl = to_list p_formulae in
  let var_eqs, llen_eqs, others =
    List.fold_left
      (fun (var_eqs, llen_eqs, others) (pf : Formula.t) ->
        match pf with
        | Eq (LVar x, _) | Eq (_, LVar x) -> (var_eqs @ [ pf ], llen_eqs, others)
        | Eq (UnOp (LstLen, x), _) | Eq (_, UnOp (LstLen, x)) ->
            (var_eqs, llen_eqs @ [ pf ], others)
        | _ -> (var_eqs, llen_eqs, others @ [ pf ]))
      ([], [], []) pfl
  in

  List.iteri (fun i pf -> nth_set p_formulae i pf) (var_eqs @ llen_eqs @ others)

let iter (f : Formula.t -> unit) (pfs : t) : unit = DynArray.iter f pfs

let iteri (f : int -> Formula.t -> unit) (pfs : t) : unit = DynArray.iteri f pfs

let fold_left (f : 'a -> Formula.t -> 'a) (ac : 'a) (pfs : t) : 'a =
  DynArray.fold_left f ac pfs
