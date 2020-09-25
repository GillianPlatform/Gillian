module FOSolver = Engine.FOSolver
module PFS = Engine.PFS
module Formula = Gil_syntax.Formula

(** FIXME: optimization? *)
let build_full_pfs (pc : Pc.t) =
  let copied = PFS.copy pc.pfs in
  Formula.Set.iter (PFS.extend copied) pc.learned;
  copied

let sat ~(pc : Pc.t) formula =
  FOSolver.sat ~pfs:(build_full_pfs pc) ~gamma:pc.gamma [ formula ]

let check_entailment ~(pc : Pc.t) formula =
  let pfs = build_full_pfs pc in
  let gamma = pc.gamma in
  let f =
    Engine.Reduction.reduce_formula ~gamma:pc.gamma ~pfs:(build_full_pfs pc)
      formula
  in
  match f with
  | True  -> true
  | False -> false
  | _     ->
      FOSolver.check_entailment Utils.Containers.SS.empty (PFS.to_list pfs)
        [ f ] gamma

let of_comp_fun comp ~(pc : Pc.t) e1 e2 =
  comp ~pfs:(build_full_pfs pc) ~gamma:pc.gamma e1 e2

let is_equal = of_comp_fun FOSolver.is_equal

let is_different = of_comp_fun FOSolver.is_different

let is_less_or_equal = of_comp_fun FOSolver.is_less_or_equal

let resolve_loc_name ~pc loc =
  FOSolver.resolve_loc_name ~pfs:(build_full_pfs pc) ~gamma:pc.gamma loc
