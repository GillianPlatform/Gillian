open Gil_syntax
open Gillian.Monadic
module DR = Delayed_result
module Global_env = Cgil_lib.Global_env

let init_data = ref Global_env.empty
let set_init_data d = init_data := d

module M : States.MyMonadicSMemory.S with type t = Global_env.t = struct
  type t = Global_env.t [@@deriving yojson]
  type err_t = unit [@@deriving show, yojson]
  type action = GetDef
  type pred = unit

  let pp = Global_env.pp

  let action_from_str = function
    | "getdef" -> Some GetDef
    | _ -> None

  let action_to_str GetDef = "getdef"
  let pred_from_str _ = None
  let pred_to_str () = failwith "No pred in GEnv"
  let empty () = !init_data

  (* Execute action *)
  let[@inline] execute_action GetDef s args =
    match args with
    | [ (Expr.Lit (Loc loc) | Expr.ALoc loc | Expr.LVar loc) ] -> (
        match Global_env.find_def_opt s loc with
        | Some def ->
            let v = Global_env.serialize_def def in
            DR.ok (s, [ Expr.Lit (Loc loc); Expr.Lit v ])
        | None ->
            (* If we can't find a function, in UX mode we give up, while in OX mode we
               signal. *)
            if !Gillian.Utils.Config.under_approximation then Delayed.vanish ()
            else
              Fmt.failwith "execute_genvgetdef: couldn't find %s\nGENV:\n%a" loc
                Global_env.pp s)
    | _ -> failwith "Invalid arguments for GetDef"

  let[@inline] consume () _ _ = failwith "Invalid C GEnv consume"
  let[@inline] produce () _ _ = failwith "Invalid C GEnv produce"
  let compose _ _ = Delayed.vanish () (* TODO *)
  let is_exclusively_owned _ _ = Delayed.return false
  let is_empty _ = false
  let is_concrete _ = false
  let instantiate _ = (Global_env.empty, [])

  (* Core predicates: pred * ins * outs, converted to Asrt.CorePred *)
  let assertions _ = []
  let assertions_others _ = []
  let can_fix () = false
  let get_fixes () = []
  let lvars _ = Gillian.Utils.Containers.SS.empty
  let alocs _ = Gillian.Utils.Containers.SS.empty
  let substitution_in_place _ s = Delayed.return s
  let get_recovery_tactic _ = Gillian.General.Recovery_tactic.none
  let list_actions () = [ (GetDef, [], []) ]
  let list_preds () = []
end
