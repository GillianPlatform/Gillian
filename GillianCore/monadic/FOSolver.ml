module FOSolver = Engine.FOSolver
module PFS = Engine.PFS
module TypEnv = Engine.TypEnv
module Formula = Gil_syntax.Formula

(** FIXME: optimization? *)
let build_full_pfs (pc : Pc.t) =
  let copied = PFS.copy pc.pfs in
  Formula.Set.iter (PFS.extend copied) pc.learned;
  copied

let build_full_gamma (pc : Pc.t) =
  let copied = TypEnv.copy pc.gamma in
  List.iter (fun (x, t) -> TypEnv.update copied x t) pc.learned_types;
  copied

let sat ~(pc : Pc.t) formula =
  FOSolver.sat ~pfs:(build_full_pfs pc) ~gamma:(build_full_gamma pc) [ formula ]

let check_entailment ~(pc : Pc.t) formula =
  let pfs = build_full_pfs pc in
  let gamma = build_full_gamma pc in
  let f = Engine.Reduction.reduce_formula ~gamma ~pfs formula in
  match f with
  | True  -> true
  | False -> false
  | _     ->
      FOSolver.check_entailment Utils.Containers.SS.empty (PFS.to_list pfs)
        [ f ] gamma

let of_comp_fun comp ~(pc : Pc.t) e1 e2 =
  comp ~pfs:(build_full_pfs pc) ~gamma:(build_full_gamma pc) e1 e2

let is_equal = of_comp_fun FOSolver.is_equal

let is_different = of_comp_fun FOSolver.is_different

let is_less_or_equal = of_comp_fun FOSolver.is_less_or_equal

let resolve_loc_name ~pc loc =
  FOSolver.resolve_loc_name ~pfs:(build_full_pfs pc)
    ~gamma:(build_full_gamma pc) loc
