(** GIL Symbolic Store *)

open SVal
include Store.Make (SVal.M)

(** Updates --store-- to subst(store) *)
let substitution_in_place ?(subst_all = false) (subst : SESubst.t) (x : t) :
    unit =
  if not (SESubst.is_empty subst) then (
    (* Do not substitute spec vars for spec vars *)
    let store_subst = SESubst.copy subst in
    if not subst_all then
      SESubst.filter_in_place store_subst (fun u le ->
          match (u, le) with
          | LVar x, LVar _ when Names.is_spec_var_name @@ LVar.str x ->
              Some (LVar x)
          | _ -> Some le);

    filter_map_inplace x (fun _ value ->
        let substed = SESubst.subst_in_expr store_subst ~partial:true value in
        Some (Reduction.reduce_lexpr substed)))

(** Returns the set containing all the alocs occurring in --x-- *)
let alocs (x : t) : ALoc.Set.t =
  fold x (fun _ le ac -> ALoc.Set.union ac (Expr.alocs le)) ALoc.Set.empty

(** Returns the set containing all the alocs occurring in --x-- *)
let clocs (x : t) : Loc.Set.t =
  fold x (fun _ le ac -> Loc.Set.union ac (Expr.clocs le)) Loc.Set.empty

(** conversts a symbolic store to a list of assertions *)
let assertions (x : t) : Expr.t list =
  fold x
    (fun x le (assertions : Expr.t list) ->
      Expr.BinOp (PVar x, Equal, le) :: assertions)
    []

let is_well_formed (_ : t) : bool = true
