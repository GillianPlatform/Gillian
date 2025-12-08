open Gillian.Utils
open Gillian.Monadic
open Gillian.Symbolic
open Gil_syntax
module DR = Delayed_result
module Recovery_tactic = Gillian.General.Recovery_tactic

type t = Expr.t option [@@deriving show, yojson]
type err_t = MissingState [@@deriving show, yojson]
type action = Load | Store
type pred = Ex

let pp = Fmt.(option ~none:(any "None") Expr.pp)

let action_from_str = function
  | "load" -> Some Load
  | "store" -> Some Store
  | _ -> None

let action_to_str = function
  | Load -> "load"
  | Store -> "store"

let list_actions () = [ (Load, [], [ "value" ]); (Store, [ "value" ], []) ]

let pred_from_str = function
  | "ex" -> Some Ex
  | _ -> None

let pred_to_str = function
  | Ex -> "ex"

let list_preds () = [ (Ex, [], [ "value" ]) ]
let empty () : t = None

let[@inline] execute_action action s args =
  match (action, s, args) with
  | _, None, _ -> DR.error MissingState
  | Load, Some v, [] -> DR.ok (Some v, [ v ])
  | Store, Some _, [ v' ] -> DR.ok (Some v', [])
  | a, _, args ->
      Fmt.failwith "Invalid action %s with state %a and args %a"
        (action_to_str a) pp s (Fmt.Dump.list Expr.pp) args

let[@inline] consume core_pred s args =
  match (core_pred, s, args) with
  | Ex, Some v, [] -> DR.ok (None, [ v ])
  | Ex, None, _ -> DR.error MissingState
  | Ex, _, _ -> failwith "Invalid PointsTo consume"

let[@inline] produce core_pred s args =
  match (core_pred, s, args) with
  | Ex, None, [ v ] -> Delayed.return (Some v)
  | Ex, Some _, _ -> Delayed.vanish ()
  | Ex, _, _ -> failwith "Invalid PointsTo produce"

let substitution_in_place subst s =
  Option.map (Subst.subst_in_expr ~partial:true subst) s |> Delayed.return

let compose s1 s2 =
  match (s1, s2) with
  | None, s | s, None -> Delayed.return s
  | _ -> Delayed.vanish ()

let is_exclusively_owned s _ =
  match s with
  | None -> Delayed.return false
  | Some _ -> Delayed.return true

let is_empty = Option.is_none

let is_concrete = function
  | None -> true
  | Some v -> Expr.is_concrete v

let instantiate = function
  | [] -> (Some (Expr.Lit Undefined), [])
  | [ v ] -> (Some v, []) (* maybe we don't want two options *)
  | _ -> failwith "Invalid Excl instantiation"

let lvars = function
  | None -> Containers.SS.empty
  | Some v -> Expr.lvars v

let alocs = function
  | None -> Containers.SS.empty
  | Some v -> Expr.alocs v

let assertions = function
  | None -> []
  | Some v -> [ (Ex, [], [ v ]) ]

let assertions_others _ = []
let get_recovery_tactic _ = Recovery_tactic.none
let can_fix MissingState = true

let get_fixes MissingState =
  [ [ MyAsrt.CorePred (Ex, [], [ LVar (Generators.fresh_svar ()) ]) ] ]
