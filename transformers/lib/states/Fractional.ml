open Gillian.Utils
open Gillian.Monadic
open Gillian.Symbolic
open Gil_syntax
module DR = Delayed_result
module Recovery_tactic = Gillian.General.Recovery_tactic

type err = MissingState | NotEnoughPermission [@@deriving show, yojson]

module type FracA_S = sig
  type t [@@deriving yojson, show]

  val of_expr : Expr.t -> t
  val to_expr : t -> Expr.t
  val subst : Values.et -> t -> t
  val subtract : t -> t -> (t option, err) result Delayed.t
  val add : t -> t -> t Delayed.t
  val _1 : t
  val is_1 : t -> Expr.t
  val is_concrete : t -> bool
end

module Classic_fracA = struct
  let _0 = Expr.num 0.
  let _1 = Expr.num 1.

  type t = Expr.t [@@deriving show, yojson]

  let is_concrete = Expr.is_concrete
  let of_expr x = x
  let to_expr x = x
  let subst s = Subst.subst_in_expr s ~partial:true

  open Expr.Infix

  let subtract left right =
    if%sat left == right then DR.ok None
    else
      if%sat left >=. right then DR.ok (Some (left -. right))
      else DR.error NotEnoughPermission

  let add left right =
    Delayed.return ~learned:[ left +. right <=. _1 ] (left +. right)

  let is_1 q = q == _1
end

module Frac_with_wildcard : FracA_S = struct
  type t = Q of Expr.t | Wildcard [@@deriving show, yojson]

  let _1 = Q (Expr.num 1.)

  let is_concrete = function
    | Wildcard -> false
    | Q q -> Expr.is_concrete q

  let of_expr = function
    | Expr.Lit (String "_") -> Wildcard
    | x -> Q x

  let to_expr = function
    | Wildcard -> Expr.Lit (String "_")
    | Q q -> q

  let subst s = function
    | Wildcard -> Wildcard
    | Q q -> Q (Classic_fracA.subst s q)

  let subtract left right =
    let open DR.Syntax in
    match (left, right) with
    | Wildcard, Wildcard -> DR.ok (Some Wildcard)
    | Wildcard, Q _ | Q _, Wildcard -> DR.error NotEnoughPermission
    | Q left, Q right ->
        let++ q = Classic_fracA.subtract left right in
        Option.map (fun q -> Q q) q

  let add left right =
    let open Delayed.Syntax in
    match (left, right) with
    (* Note: the following is over-approximating, because two wildcard could get bigger than 1. *)
    | Wildcard, _ | _, Wildcard -> Delayed.return Wildcard
    | Q x, Q y ->
        let+ r = Classic_fracA.add x y in
        Q r

  let is_1 = function
    | Wildcard -> Expr.false_
    | Q q -> Classic_fracA.is_1 q
end

let assertions_others _ = []
let get_recovery_tactic _ _ = Recovery_tactic.none

module Make (FracA : FracA_S) = struct
  (** Value * Fraction *)
  type t = (Expr.t * FracA.t) option [@@deriving show, yojson]

  type err_t = err [@@deriving show, yojson]
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

  let[@inline] execute_action action (s : t) args =
    match (action, s, args) with
    | _, None, _ -> DR.error MissingState
    | Load, Some (v, q), [] -> DR.ok (Some (v, q), [ v ])
    | Store, Some (_, q), [ v' ] ->
        if%sat FracA.is_1 q then DR.ok (Some (v', q), [])
        else DR.error NotEnoughPermission
    | a, _, args ->
        Fmt.failwith "Invalid action %s with state %a and args %a"
          (action_to_str a) pp s (Fmt.Dump.list Expr.pp) args

  let consume core_pred s args =
    let open DR.Syntax in
    match (core_pred, s, args) with
    | Frac, Some (v, q), [ q' ] ->
        let q' = FracA.of_expr q' in
        let++ new_q = FracA.subtract q q' in
        let new_state = Option.map (fun new_q -> (v, new_q)) new_q in
        (new_state, [ v ])
    | Frac, None, _ -> DR.error MissingState
    | Frac, _, _ -> failwith "Invalid Agree consume"

  let produce core_pred s args =
    let open Expr.Infix in
    let open Delayed.Syntax in
    match (core_pred, s, args) with
    | Frac, None, [ q'; v' ] -> Delayed.return (Some (v', FracA.of_expr q'))
    | Frac, Some (v, q), [ q'; v' ] ->
        let* new_q = FracA.add q (FracA.of_expr q') in
        Delayed.return ~learned:[ v == v' ] (Some (v, new_q))
    | Frac, _, _ -> failwith "Invalid Frac produce"

  let substitution_in_place subst s =
    match s with
    | None -> Delayed.return None
    | Some (v, q) ->
        let v' = Subst.subst_in_expr ~partial:true subst v in
        let q' = FracA.subst subst q in
        Delayed.return (Some (v', q'))

  let compose (s1 : t) (s2 : t) =
    let open Expr.Infix in
    let open Delayed.Syntax in
    match (s1, s2) with
    | None, _ -> Delayed.return s2
    | _, None -> Delayed.return s1
    | Some (v, q), Some (v', q') ->
        let* new_q = FracA.add q q' in
        Delayed.return ~learned:[ v == v' ] (Some (v, new_q))

  let is_exclusively_owned s _ =
    match s with
    | None -> Delayed.return false
    | Some (_, q) -> Delayed.check_sat (FracA.is_1 q)

  let is_empty = function
    | None -> true
    | Some _ -> false

  let is_concrete = function
    | None -> true
    | Some (v, q) -> Expr.is_concrete v && FracA.is_concrete q

  let instantiate : 'a list -> t * Expr.t list = function
    | [] -> (Some (Expr.int 0, FracA._1), [])
    | _ -> failwith "Invalid Fractional instantiation"

  let lvars = function
    | None -> Containers.SS.empty
    | Some (v, _) -> Expr.lvars v

  let alocs = function
    | None -> Containers.SS.empty
    | Some (v, _) -> Expr.alocs v

  let assertions = function
    | None -> []
    | Some (v, q) -> [ (Frac, [ FracA.to_expr q ], [ v ]) ]

  let assertions_others _ = []
  let get_recovery_tactic _ _ = Recovery_tactic.none

  let can_fix = function
    | _ -> false

  let get_fixes = function
    | _ -> []
end
