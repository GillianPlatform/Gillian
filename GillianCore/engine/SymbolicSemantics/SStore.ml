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

    let symbolics = symbolics x in
    Var.Set.iter
      (fun v ->
        let le = Option.get (get x v) in
        let s_le = SESubst.subst_in_expr store_subst ~partial:true le in
        let s_le = if le <> s_le then Reduction.reduce_lexpr s_le else s_le in
        if le <> s_le then put x v s_le)
      symbolics)

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

let is_in (store : t) (e : Expr.t) =
  List.exists (fun (_, v) -> Expr.sub_expr e v) (bindings store)
