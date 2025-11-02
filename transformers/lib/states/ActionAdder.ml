open Gil_syntax
open Gillian.Monadic

module type ActionAddition = sig
  type t
  type action
  type err_t [@@deriving show, yojson]

  val list_actions : unit -> (action * string list * string list) list
  val action_from_str : string -> action option
  val action_to_str : action -> string

  val execute_action :
    action -> t -> Expr.t list -> (t * Expr.t list, err_t) result Delayed.t

  val can_fix : err_t -> bool
  val get_fixes : err_t -> string MyAsrt.t list list
  val get_recovery_tactic : err_t -> Expr.t Gillian.General.Recovery_tactic.t
end

module Make (A : ActionAddition) (S : MyMonadicSMemory.S with type t = A.t) =
struct
  include S

  type action = BaseAct of S.action | AddedAct of A.action

  type err_t = BaseErr of S.err_t | AddedErr of A.err_t
  [@@deriving show, yojson]

  let list_actions () =
    (S.list_actions () |> List.map (fun (a, b, c) -> (BaseAct a, b, c)))
    @ (A.list_actions () |> List.map (fun (a, b, c) -> (AddedAct a, b, c)))

  let action_from_str s =
    match S.action_from_str s with
    | Some a -> Some (BaseAct a)
    | None -> Option.map (fun a -> AddedAct a) (A.action_from_str s)

  let action_to_str = function
    | BaseAct a -> S.action_to_str a
    | AddedAct a -> A.action_to_str a

  let map_base_err x =
    Delayed.map x (function
      | Ok x -> Ok x
      | Error e -> Error (BaseErr e))

  let map_added_err x =
    Delayed.map x (function
      | Ok x -> Ok x
      | Error e -> Error (AddedErr e))

  let[@inline] execute_action a s args =
    match a with
    | BaseAct a -> S.execute_action a s args |> map_base_err
    | AddedAct a -> A.execute_action a s args |> map_added_err

  let[@inline] consume p s ins = S.consume p s ins |> map_base_err

  let can_fix = function
    | BaseErr e -> S.can_fix e
    | AddedErr e -> A.can_fix e

  let get_fixes = function
    | BaseErr e -> S.get_fixes e
    | AddedErr e ->
        A.get_fixes e
        |> MyUtils.deep_map
           @@ MyAsrt.map_cp (fun (p, i, o) ->
                  (S.pred_from_str p |> Option.get, i, o))

  let get_recovery_tactic = function
    | BaseErr e -> S.get_recovery_tactic e
    | AddedErr e -> A.get_recovery_tactic e
end
