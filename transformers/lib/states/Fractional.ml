open Gillian.Utils
open Gillian.Monadic
open Gillian.Symbolic
open Gil_syntax
module DR = Delayed_result
module Recovery_tactic = Gillian.General.Recovery_tactic

(** Value * Fraction *)
type t = (Expr.t * Expr.t) option [@@deriving show, yojson]

type err_t = MissingState | NotEnoughPermission [@@deriving show, yojson]
type action = Load | Store
type pred = Frac

let action_from_str = function
  | "load" -> Some Load
  | "store" -> Some Store
  | _ -> None

let action_to_str = function
  | Load -> "load"
  | Store -> "store"

let list_actions () = [ (Load, [], [ "value" ]); (Store, [ "value" ], []) ]

let pred_from_str = function
  | "frac" -> Some Frac
  | _ -> None

let pred_to_str = function
  | Frac -> "frac"

let list_preds () = [ (Frac, [ "fraction" ], [ "value" ]) ]
let empty () : t = None

let execute_action action s args =
  let open Formula.Infix in
  match (action, s, args) with
  | _, None, _ -> DR.error MissingState
  | Load, Some (v, q), [] -> DR.ok (Some (v, q), [ v ])
  | Store, Some (_, q), [ v' ] ->
      if%sat q #== (Expr.num 1.) then DR.ok (Some (v', q), [])
      else DR.error NotEnoughPermission
  | a, _, args ->
      failwith
        (Fmt.str "Invalid action %s with state %a and args %a" (action_to_str a)
           pp s (Fmt.Dump.list Expr.pp) args)

let consume core_pred s args =
  let open Formula.Infix in
  let open Expr.Infix in
  match (core_pred, s, args) with
  | Frac, Some (v, q), [ q' ] ->
      if%sat q #== q' then DR.ok (None, [ v ])
      else
        DR.ok
          ~learned:[ q' #>. (Expr.num 0.); (q -. q') #>. (Expr.num 0.) ]
          (Some (v, q -. q'), [ v ])
  | Frac, None, _ -> DR.error MissingState
  | Frac, _, _ -> failwith "Invalid Agree consume"

let produce core_pred s args =
  let open Formula.Infix in
  let open Expr.Infix in
  match (core_pred, s, args) with
  | Frac, None, [ q'; v' ] -> Delayed.return (Some (v', q'))
  | Frac, Some (v, q), [ q'; v' ] ->
      Delayed.return
        ~learned:[ v #== v'; (q +. q') #<=. (Expr.num 1.) ]
        (Some (v, q +. q'))
  | Frac, _, _ -> failwith "Invalid Frac produce"

let substitution_in_place subst s =
  match s with
  | None -> Delayed.return None
  | Some (v, q) ->
      let v' = Subst.subst_in_expr ~partial:true subst v in
      let q' = Subst.subst_in_expr ~partial:true subst q in
      Delayed.return (Some (v', q'))

let compose (s1 : t) (s2 : t) =
  let open Formula.Infix in
  let open Expr.Infix in
  match (s1, s2) with
  | None, _ -> Delayed.return s2
  | _, None -> Delayed.return s1
  | Some (v, q), Some (v', q') ->
      Delayed.return
        ~learned:[ v #== v'; (q +. q') #<=. (Expr.num 1.) ]
        (Some (v, q +. q'))

let is_exclusively_owned s _ =
  match s with
  | None -> Delayed.return false
  | Some (_, q) -> Delayed.check_sat Formula.Infix.(q #== (Expr.num 1.))

let is_empty = function
  | None -> true
  | Some _ -> false

let is_concrete = function
  | None -> true
  | Some (v, q) -> Expr.is_concrete v && Expr.is_concrete q

let instantiate = function
  | [] -> (Some (Expr.int 0, Expr.num 1.), [])
  | _ -> failwith "Invalid Fractional instantiation"

let lvars = function
  | None -> Containers.SS.empty
  | Some (v, _) -> Expr.lvars v

let alocs = function
  | None -> Containers.SS.empty
  | Some (v, _) -> Expr.alocs v

let assertions = function
  | None -> []
  | Some (v, q) -> [ (Frac, [ q ], [ v ]) ]

let assertions_others _ = []
let get_recovery_tactic _ = Recovery_tactic.none

let can_fix = function
  | _ -> false

let get_fixes = function
  | _ -> []
