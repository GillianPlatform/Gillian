open Gillian.Monadic
module DR = Delayed_result

type ('a, 'pred) bi_state = { state : 'a; anti_frame : 'pred Fix.t }
[@@deriving show, yojson]

module Make (Mem : MyMonadicSMemory.S) :
  MyMonadicSMemory.S with type t = (Mem.t, Mem.pred) bi_state = struct
  type pred = Mem.pred [@@deriving yojson]

  let pred_from_str = Mem.pred_from_str
  let pred_to_str = Mem.pred_to_str
  let pp_pred = Fmt.of_to_string pred_to_str
  let list_preds = Mem.list_preds

  type t = (Mem.t, pred) bi_state [@@deriving show, yojson]
  type err_t = Mem.err_t [@@deriving show, yojson]
  type action = Mem.action

  let action_from_str = Mem.action_from_str
  let action_to_str = Mem.action_to_str
  let list_actions = Mem.list_actions
  let empty () : t = { state = Mem.empty (); anti_frame = [] }

  let with_bi_abduction ~f state args =
    let open Delayed.Syntax in
    let rec aux ~fuel state =
      (* If fuel is exhausted we give up *)
      let* () = if fuel <= 0 then Delayed.vanish () else Delayed.return () in
      let* r = f state.state args in
      match r with
      | Ok (state', v) -> DR.ok ({ state with state = state' }, v)
      | Error e when not (Mem.can_fix e) ->
          (* can_fix is true when the error is a `miss` *)
          DR.error e
      | Error err ->
          (* This is the missing error case *)
          (* We take each fix one by one *)
          let* cp_list =
            let fixes = Mem.get_fixes err in
            Logging.verbose (fun m ->
                m "Attempting to fix %a with candidates: %a" Mem.pp_err_t err
                  (Fmt.Dump.list (Fix.pp (Fmt.of_to_string Mem.pred_to_str)))
                  fixes);
            List.map Delayed.return fixes |> Delayed.branches
          in
          let* state' =
            List.fold_left
              (fun state (pred, ins, outs) ->
                let* state = state in
                Mem.produce pred state (ins @ outs))
              (Delayed.return state.state)
              cp_list
          in
          let anti_frame = cp_list @ state.anti_frame in
          (* We produce that fix in our current state *)
          let new_state = { state = state'; anti_frame } in
          aux ~fuel:(fuel - 1) new_state
    in
    (* We try to fix twice *)
    aux ~fuel:2 state

  let execute_action action = with_bi_abduction ~f:(Mem.execute_action action)

  let produce pred { state; anti_frame } args =
    let open Delayed.Syntax in
    let+ state = Mem.produce pred state args in
    { state; anti_frame }

  let consume pred state args =
    with_bi_abduction ~f:(Mem.consume pred) state args

  let is_empty s = Mem.is_empty s.state

  (* Variables *)

  let lvars s =
    let open Gillian.Utils.Containers in
    Mem.lvars s.state |> SS.union (Fix.lvars s.anti_frame)

  let alocs s =
    let open Gillian.Utils.Containers in
    Mem.alocs s.state |> SS.union (Fix.alocs s.anti_frame)

  let substitution_in_place subst s =
    let open Monadic.Delayed.Syntax in
    let+ state = Mem.substitution_in_place subst s.state in
    let anti_frame = Fix.subst subst s.anti_frame in
    { state; anti_frame }

  (* Error handling *)

  let get_recovery_tactic = Mem.get_recovery_tactic
  let can_fix e = Mem.can_fix e

  (* The rest of the functions remain unimplemented as Bi_abd makes no sense being an input to other transformers. *)

  let get_fixes _ =
    failwith
      "Bi_abd state should not be used as an input to the bi-abduction state \
       module. That one is legacy."

  let assertions_others _ =
    failwith
      "Should not be transforming bi-abd state to assertions, only its first \
       component"

  let assertions _ =
    failwith
      "Should not be transforming bi-abd state to assertions, only its first \
       component"

  let instantiate _ =
    failwith "Bi_abd state should not be used as an input state"

  let is_exclusively_owned _ =
    failwith "Bi_abd state should not be used as an input state"

  let is_concrete _ =
    failwith "Bi_abd state should not be used as an input state"

  let compose _ _ = failwith "Bi_abd state should not be used as an input state"
end
