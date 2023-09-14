module FOSolver = Engine.FOSolver
module PFS = Engine.PFS
module Type_env = Engine.Type_env
module Reduction = Engine.Reduction
module Formula = Gil_syntax.Formula
module Typing = Engine.Typing

(** FIXME: optimization? *)
let build_full_pfs (pc : Pc.t) =
  if Formula.Set.is_empty pc.learned then pc.pfs
  else
    let copied = PFS.copy pc.pfs in
    Formula.Set.iter (PFS.extend copied) pc.learned;
    copied

let build_full_gamma (pc : Pc.t) =
  if pc.learned_types = [] then pc.gamma
  else
    let copied = Type_env.copy pc.gamma in
    List.iter (fun (x, t) -> Type_env.update copied x t) pc.learned_types;
    copied

let sat ~(pc : Pc.t) formula =
  Logging.tmi (fun m ->
      m "Monadic about to check sat of this new formula:@[<hov>%a@]" Formula.pp
        formula);
  let pfs, gamma = (build_full_pfs pc, build_full_gamma pc) in

  Logging.tmi (fun m -> m "WITH PFS : %a" PFS.pp pfs);
  match
    Engine.Reduction.reduce_formula ~unification:pc.unification ~pfs ~gamma
      formula
  with
  | True ->
      Logging.verbose (fun fmt -> fmt "Discharged sat before Z3");
      true
  | False ->
      Logging.verbose (fun fmt -> fmt "Discharged sat before Z3");
      false
  | formula -> FOSolver.sat ~unification:pc.unification ~pfs ~gamma formula

let check_entailment ~(pc : Pc.t) formula =
  let pfs, gamma = (build_full_pfs pc, build_full_gamma pc) in
  try
    let f =
      Engine.Reduction.reduce_formula ~unification:pc.unification ~gamma ~pfs
        formula
    in
    match f with
    | True -> true
    | False -> false
    | _ ->
        FOSolver.check_entailment ~unification:pc.unification
          Utils.Containers.SS.empty pfs [ f ] gamma
  with Engine.Reduction.ReductionException (e, msg) ->
    Logging.verbose (fun m ->
        m
          "check_entailment: couldn't check due to an error reducing %a - %s\n\
           Formula:%a"
          Gil_syntax.Expr.pp e msg Formula.pp formula);
    false

let of_comp_fun comp ~(pc : Pc.t) e1 e2 =
  comp ~pfs:(build_full_pfs pc) ~gamma:(build_full_gamma pc) e1 e2

let is_equal = of_comp_fun FOSolver.is_equal
let is_different = of_comp_fun FOSolver.is_different
let num_is_less_or_equal = of_comp_fun FOSolver.num_is_less_or_equal

let resolve_loc_name ~pc loc =
  FOSolver.resolve_loc_name ~pfs:(build_full_pfs pc)
    ~gamma:(build_full_gamma pc) loc

let reduce_expr ~pc expr =
  Reduction.reduce_lexpr ~unification:pc.Pc.unification ~pfs:(build_full_pfs pc)
    ~gamma:(build_full_gamma pc) expr

let resolve_type ~(pc : Pc.t) expr =
  (* TODO: I don't know what that how parameter means.
     I'm copying what Reduction does.
     Typing is not documented - ask Petar. *)
  let t, how, _ = Typing.type_lexpr pc.gamma expr in
  if how then t else None
