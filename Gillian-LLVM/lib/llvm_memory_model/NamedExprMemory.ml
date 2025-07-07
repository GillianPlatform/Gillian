open Gil_syntax
open Gillian.Monadic
open States
module DR = Delayed_result
module SMap = MyUtils.SMap

module StringEnv = struct
  type t = Expr.t MyUtils.SMap.t [@@deriving yojson]

  (* TODO(Ian): Implement this*)
  let pp fmt t = ()
end

module M : States.MyMonadicSMemory.S with type t = StringEnv.t = struct
  type t = StringEnv.t [@@deriving yojson]
  type err_t = InvalidLocation [@@deriving show, yojson]
  type action = GetDef
  type pred = unit

  let pp = StringEnv.pp
  let action_from_str _ = None
  let action_to_str GetDef = "getdef"
  let pred_from_str _ = None
  let pred_to_str () = failwith "No pred in GEnv"
  let empty () = SMap.empty

  (* Execute action *)
  let execute_action GetDef s = function
    | [ loc ] ->
        let open DR.Syntax in
        let** expr =
          Delayed_result.of_do ~none:InvalidLocation (Delayed.resolve_loc loc)
        in
        let vl = SMap.find_opt expr s in
        let** res = Delayed_result.of_option ~none:InvalidLocation vl in
        DR.ok (s, [ res ])
    | _ -> failwith "Invalid arguments for GetDef"

  let consume () _ _ = failwith "Invalid C GEnv consume"
  let produce () _ _ = failwith "Invalid C GEnv produce"
  let compose _ _ = Delayed.vanish () (* TODO *)
  let is_exclusively_owned _ _ = Delayed.return false
  let is_empty _ = false
  let is_concrete _ = false
  let instantiate _ = (SMap.empty, [])

  (* Core predicates: pred * ins * outs, converted to Asrt.CorePred *)
  let assertions _ = []
  let assertions_others _ = []
  let can_fix _ = false
  let get_fixes _ = []
  let lvars _ = Gillian.Utils.Containers.SS.empty
  let alocs _ = Gillian.Utils.Containers.SS.empty
  let substitution_in_place _ s = Delayed.return s
  let get_recovery_tactic _ = Gillian.General.Recovery_tactic.none
  let list_actions () = [ (GetDef, [], []) ]
  let list_preds () = []
end
