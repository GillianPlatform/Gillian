open Gil_syntax

(** A fix is a list of atoms to produce onto a state in order to repair a
    missing-resource error. Besides core predicates, an atom may carry the
    types of the logical variables the fix introduces: without them, every
    later use of an abduced value has to ask the solver what its type is. *)
type 'cp atom =
  | CorePred of 'cp * Expr.t list * Expr.t list
  | Types of (Expr.t * Type.t) list
[@@deriving yojson, show]

type 'cp t = 'cp atom list [@@deriving yojson, show]

let core_pred cp ins outs = CorePred (cp, ins, outs)
let types tys = Types tys

(** Map over the core predicates of a fix, leaving any other atom untouched. *)
let map_corepred
    (f : 'cp * Expr.t list * Expr.t list -> 'dp * Expr.t list * Expr.t list) :
    'cp t list -> 'dp t list =
  List.map
    (List.map (function
      | CorePred (cp, ins, outs) ->
          let cp', ins', outs' = f (cp, ins, outs) in
          CorePred (cp', ins', outs')
      | Types tys -> Types tys))

let exprs_of_atom = function
  | CorePred (_, ins, outs) -> ins @ outs
  | Types tys -> List.map fst tys

let vars_of ~f (fix : 'cp t) =
  let open Utils.Containers in
  List.fold_left
    (fun acc atom ->
      List.fold_left (fun acc e -> SS.union acc (f e)) acc (exprs_of_atom atom))
    SS.empty fix

let lvars (fix : 'cp t) = vars_of ~f:Expr.lvars fix
let alocs (fix : 'cp t) = vars_of ~f:Expr.alocs fix

let subst (subst : Engine.Symbolic.Subst.t) (fix : 'a t) : 'a t =
  let le_subst = Engine.Symbolic.Subst.subst_in_expr subst ~partial:true in
  let subst_atom = function
    | CorePred (cp, ins, outs) ->
        CorePred (cp, List.map le_subst ins, List.map le_subst outs)
    | Types tys -> Types (List.map (fun (e, t) -> (le_subst e, t)) tys)
  in
  List.map subst_atom fix

let to_asrt ~pred_to_str fix : Asrt.t =
  ListLabels.map fix ~f:(function
    | CorePred (cp, ins, outs) -> Asrt.CorePred (pred_to_str cp, ins, outs)
    | Types tys -> Asrt.Types tys)
