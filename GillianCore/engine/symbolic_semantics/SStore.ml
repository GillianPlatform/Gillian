(** GIL Symbolic Store *)

open SVal
include Store.Make (SVal.M)

(** Updates --store-- to subst(store) *)
let substitution_in_place ?(subst_all = false) (subst : SESubst.t) (x : t) :
    unit =
  if not (SESubst.is_empty subst) then (
    (* Do not substitute spec vars for spec vars *)
    let store_subst = SESubst.copy subst in
    SESubst.filter_in_place store_subst (fun u le ->
        match (u, le) with
        | LVar x, LVar _ when (not subst_all) && Names.is_spec_var_name x ->
            Some (LVar x)
        | _ -> Some le);

    filter_map_inplace x (fun _ value ->
        let substed = SESubst.subst_in_expr store_subst ~partial:true value in
        Some (Reduction.reduce_lexpr substed)))

(** Returns the set containing all the vars occurring in --x-- *)
let vars (x : t) : SS.t =
  fold x (fun x le ac -> SS.union ac (SS.add x (Expr.vars le))) SS.empty

(** Returns the set containing all the alocs occurring in --x-- *)
let alocs (x : t) : SS.t =
  fold x (fun _ le ac -> SS.union ac (Expr.alocs le)) SS.empty

(** Returns the set containing all the alocs occurring in --x-- *)
let clocs (x : t) : SS.t =
  fold x (fun _ le ac -> SS.union ac (Expr.clocs le)) SS.empty

(** conversts a symbolic store to a list of assertions *)
let assertions (x : t) : Formula.t list =
  fold x
    (fun x le (assertions : Formula.t list) -> Eq (PVar x, le) :: assertions)
    []

let is_well_formed (_ : t) : bool = true
