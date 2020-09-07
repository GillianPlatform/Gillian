module FOSolver = Gillian.Logic.FOSolver
module PFS = Gillian.Symbolic.PureContext
module Formula = Gillian.Gil_syntax.Formula

(** FIXME: optimization? *)
let build_full_pfs (pc : Pc.t) =
  let copied = PFS.copy pc.pfs in
  Formula.Set.iter (PFS.extend copied) pc.learned;
  copied

let sat ~(pc : Pc.t) formula =
  FOSolver.sat ~pfs:(build_full_pfs pc) ~gamma:pc.gamma [ formula ]

let of_comp_fun comp ~(pc : Pc.t) e1 e2 =
  comp ~pfs:(build_full_pfs pc) ~gamma:pc.gamma e1 e2

let is_equal = of_comp_fun FOSolver.is_equal

let is_different = of_comp_fun FOSolver.is_different

let is_less_or_equal = of_comp_fun FOSolver.is_less_or_equal

let resolve_loc_name ~pc loc =
  FOSolver.resolve_loc_name ~pfs:(build_full_pfs pc) ~gamma:pc.gamma loc
