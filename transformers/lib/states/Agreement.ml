open Gillian.Utils
open Gillian.Monadic
open Gillian.Symbolic
open Gil_syntax
module DR = Delayed_result
module Recovery_tactic = Gillian.General.Recovery_tactic

type t = Expr.t option [@@deriving yojson]
type err_t = MissingState [@@deriving show, yojson]
type action = Load
type pred = Ag

let pp = Fmt.(option ~none:(any "None") Expr.pp)

let action_from_str = function
  | "load" -> Some Load
  | _ -> None

let action_to_str = function
  | Load -> "load"

let list_actions () = [ (Load, [], [ "value" ]) ]

let pred_from_str = function
  | "ag" -> Some Ag
  | _ -> None

let pred_to_str = function
  | Ag -> "ag"

let list_preds () = [ (Ag, [], [ "value" ]) ]
let empty () : t = None

let[@inline] execute_action action s args =
  match (action, s, args) with
  | Load, None, _ -> DR.error MissingState
  | Load, Some v, [] -> DR.ok (Some v, [ v ])
  | Load, _, _ -> failwith "Invalid Load action"

let[@inline] consume core_pred s args =
  match (core_pred, s, args) with
  | Ag, Some v, [] -> DR.ok (Some v, [ v ])
  | Ag, None, _ -> DR.error MissingState
  | Ag, _, _ -> failwith "Invalid Agree consume"

let[@inline] produce core_pred s args =
  let open Expr.Infix in
  match (core_pred, s, args) with
  | Ag, None, [ v' ] -> Delayed.return (Some v')
  | Ag, Some v, [ v' ] -> Delayed.return ~learned:[ v == v' ] (Some v)
  | Ag, _, _ ->
      Fmt.failwith "Invalid Agree produce, got args [%a]"
        (Fmt.list ~sep:Fmt.comma Expr.pp)
        args

let substitution_in_place subst s =
  Option.map (Subst.subst_in_expr ~partial:true subst) s |> Delayed.return

let compose s1 s2 =
  let open Expr.Infix in
  match (s1, s2) with
  | None, _ -> Delayed.return s2
  | _, None -> Delayed.return s1
  | Some v1, Some v2 -> Delayed.return ~learned:[ v1 == v2 ] (Some v1)

let is_exclusively_owned _ _ = Delayed.return false

let is_empty = function
  | None -> true
  | Some _ -> false

let is_concrete = function
  | None -> true
  | Some v -> Expr.is_concrete v

let instantiate = function
  | [ v ] -> (Some v, [])
  | args ->
      Fmt.failwith "Invalid Agreement instantiation: %a"
        (Fmt.list ~sep:Fmt.comma Expr.pp)
        args

let lvars = function
  | None -> Containers.SS.empty
  | Some v -> Expr.lvars v

let alocs = function
  | None -> Containers.SS.empty
  | Some v -> Expr.alocs v

let assertions = function
  | None -> []
  | Some v -> [ (Ag, [], [ v ]) ]

let assertions_others _ = []

let get_recovery_tactic (e : err_t) : Values.t Recovery_tactic.t =
  match e with
  (* | MissingState -> Recovery_tactic.try_unfold ??? *)
  | _ -> Recovery_tactic.none

let can_fix = function
  | MissingState -> true

let get_fixes = function
  | MissingState ->
      [ [ MyAsrt.CorePred (Ag, [], [ LVar (Generators.fresh_svar ()) ]) ] ]
