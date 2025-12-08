open Gillian.Monadic
module Containers = Gillian.Utils.Containers
open MyUtils

module Make (IDs : IDs) (S1 : MyMonadicSMemory.S) (S2 : MyMonadicSMemory.S) :
  MyMonadicSMemory.S with type t = S1.t * S2.t = struct
  type t = S1.t * S2.t [@@deriving yojson]

  let pp fmt (s1, s2) =
    Fmt.pf fmt "Product (@[<v>%a@]) × (@[<v>%a@])" S1.pp s1 S2.pp s2

  module IDer = Identifier (IDs)

  type action = A1 of S1.action | A2 of S2.action

  let action_from_str s =
    match IDer.get_ided s with
    | ID1 s -> Option.map (fun a -> A1 a) (S1.action_from_str s)
    | ID2 s -> Option.map (fun a -> A2 a) (S2.action_from_str s)
    | NotIDed _ -> None

  let action_to_str = function
    | A1 a -> IDs.id1 ^ S1.action_to_str a
    | A2 a -> IDs.id2 ^ S2.action_to_str a

  let list_actions () =
    List.map (fun (a, args, ret) -> (A1 a, args, ret)) (S1.list_actions ())
    @ List.map (fun (a, args, ret) -> (A2 a, args, ret)) (S2.list_actions ())

  type pred = P1 of S1.pred | P2 of S2.pred

  let pred_from_str s =
    match IDer.get_ided s with
    | ID1 s -> Option.map (fun p -> P1 p) (S1.pred_from_str s)
    | ID2 s -> Option.map (fun p -> P2 p) (S2.pred_from_str s)
    | NotIDed _ -> None

  let pred_to_str = function
    | P1 p -> IDs.id1 ^ S1.pred_to_str p
    | P2 p -> IDs.id2 ^ S2.pred_to_str p

  let list_preds () =
    List.map (fun (p, ins, outs) -> (P1 p, ins, outs)) (S1.list_preds ())
    @ List.map (fun (p, ins, outs) -> (P2 p, ins, outs)) (S2.list_preds ())

  type err_t = E1 of S1.err_t | E2 of S2.err_t [@@deriving show, yojson]

  let empty () : t = (S1.empty (), S2.empty ())

  let[@inline] execute_action action (s1, s2) args =
    let open Delayed.Syntax in
    match action with
    | A1 action -> (
        let+ r1 = S1.execute_action action s1 args in
        match r1 with
        | Ok (s1', v) -> Ok ((s1', s2), v)
        | Error e -> Error (E1 e))
    | A2 action -> (
        let+ r2 = S2.execute_action action s2 args in
        match r2 with
        | Ok (s2', v) -> Ok ((s1, s2'), v)
        | Error e -> Error (E2 e))

  let[@inline] consume pred (s1, s2) args =
    let open Delayed.Syntax in
    match pred with
    | P1 pred -> (
        let+ r1 = S1.consume pred s1 args in
        match r1 with
        | Ok (s1', v) -> Ok ((s1', s2), v)
        | Error e -> Error (E1 e))
    | P2 pred -> (
        let+ r2 = S2.consume pred s2 args in
        match r2 with
        | Ok (s2', v) -> Ok ((s1, s2'), v)
        | Error e -> Error (E2 e))

  let[@inline] produce pred (s1, s2) args =
    let open Delayed.Syntax in
    match pred with
    | P1 pred ->
        let+ s1' = S1.produce pred s1 args in
        (s1', s2)
    | P2 pred ->
        let+ s2' = S2.produce pred s2 args in
        (s1, s2')

  let compose (s1a, s2a) (s1b, s2b) =
    let open Delayed.Syntax in
    let* s1' = S1.compose s1a s1b in
    let+ s2' = S2.compose s2a s2b in
    (s1', s2')

  let is_exclusively_owned (s1, s2) e =
    let open Delayed.Syntax in
    let* owned1 = S1.is_exclusively_owned s1 e in
    let+ owned2 = S2.is_exclusively_owned s2 e in
    owned1 && owned2

  let is_empty (s1, s2) = S1.is_empty s1 && S2.is_empty s2
  let is_concrete (s1, s2) = S1.is_concrete s1 && S2.is_concrete s2

  let instantiate v =
    let s1, v1 = S1.instantiate v in
    let s2, v2 = S2.instantiate v in
    ((s1, s2), v1 @ v2)
  (* Maybe forbid it? *)

  let substitution_in_place st (s1, s2) =
    let open Delayed.Syntax in
    let* s1' = S1.substitution_in_place st s1 in
    let+ s2' = S2.substitution_in_place st s2 in
    (s1', s2')

  let lvars (s1, s2) = Containers.SS.union (S1.lvars s1) (S2.lvars s2)
  let alocs (s1, s2) = Containers.SS.union (S1.alocs s1) (S2.alocs s2)
  let lift_corepred_1 (p, i, o) = (P1 p, i, o)
  let lift_corepred_2 (p, i, o) = (P2 p, i, o)

  let assertions (s1, s2) =
    (S1.assertions s1 |> List.map lift_corepred_1)
    @ (S2.assertions s2 |> List.map lift_corepred_2)

  let assertions_others (s1, s2) =
    S1.assertions_others s1 @ S2.assertions_others s2

  let get_recovery_tactic = function
    | E1 e -> S1.get_recovery_tactic e
    | E2 e -> S2.get_recovery_tactic e

  let can_fix = function
    | E1 e -> S1.can_fix e
    | E2 e -> S2.can_fix e

  let get_fixes = function
    | E1 e -> S1.get_fixes e |> MyUtils.deep_map (MyAsrt.map_cp lift_corepred_1)
    | E2 e -> S2.get_fixes e |> MyUtils.deep_map (MyAsrt.map_cp lift_corepred_2)
end
