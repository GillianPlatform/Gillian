module L = Logging

let pp_str_list = Fmt.(brackets (list ~sep:comma string))

module Make (Impl : Impl.S) = struct
  open Impl
  module Eval_lcmd = Eval_lcmd.Make (Impl)
  module Eval_proc_call = Eval_proc_call.Make (Impl)

  let vtrue = Val.from_literal (Bool true)
  let vfalse = Val.from_literal (Bool false)
  let eval_lcmd = Eval_lcmd.eval_lcmd
  let eval_proc_call = Eval_proc_call.eval_proc_call

  let check_loop_ids actual expected =
    match actual = expected with
    | false ->
        Fmt.failwith
          "Malformed loop structure: current loops: %a; expected loops: %a"
          pp_str_list actual pp_str_list expected
    | true -> ()

  let get_predecessor ctx =
    let { callstack; ix; prev_ix; _ } = ctx.conf in
    let predecessors = ctx.prog.prog.predecessors in
    let pid = Call_stack.get_cur_proc_id callstack in
    match Hashtbl.find_opt predecessors (pid, prev_ix, ix) with
    | Some p -> p
    | None -> Fmt.failwith "Undefined predecessor: %s %d %d" pid prev_ix ix

  let rec loop_ids_to_frame_on_at_the_end end_ids start_ids =
    if end_ids = start_ids then []
    else
      match end_ids with
      | [] ->
          Fmt.failwith
            "Malformed loop structure (at return): current loops: %a; expected \
             loops: %a"
            pp_str_list end_ids pp_str_list start_ids
      | x :: r -> x :: loop_ids_to_frame_on_at_the_end r start_ids

  let eval_assignment x e ctx =
    let state = ctx.conf.state in
    let@* v = eval_expr ctx e in
    let state = update_store state x v in
    [ make_cont ~state ctx ]

  (* Try recovering from an LAction failure *)
  let try_recovering_laction v_es errors ctx =
    let { state; laction_fuel; _ } : step_cont = ctx.conf in
    let tactic_from_params =
      let recovery_params =
        let open Syntaxes.List in
        let* v = v_es in
        let e = Val.to_expr v in
        let+ base_elem = Expr.base_elements e in
        Option.get (Val.from_expr base_elem)
      in
      Recovery_tactic.try_unfold recovery_params
    in
    let recovery_vals =
      State.get_recovery_tactic state errors
      |> Recovery_tactic.merge tactic_from_params
    in
    let recovery_states =
      if laction_fuel <= 0 then Error "out of fuel"
      else State.try_recovering state recovery_vals
    in
    match recovery_states with
    | Ok states -> Some states
    | Error msg ->
        L.normal ~title:"failure" ~severity:Error (fun m ->
            m "Action call failed with:@.%a" (Fmt.Dump.list State.pp_err) errors);
        L.verbose (fun m -> m "Couldn't recover because: %s" msg);
        None

  let eval_laction x a es ctx =
    (* TODO: AnnotatedAction log? *)
    let open Syntaxes.Result in
    let { state; callstack; laction_fuel; _ } : step_cont = ctx.conf in
    let@* v_es = eval_exprs ctx es in
    let oks, errors = State.execute_action a state v_es |> Res_list.split in
    let@* ok_states =
      oks
      |> List_utils.map_results @@ fun (state, vs) ->
         let e' = Expr.EList (List.map Val.to_expr vs) in
         let+ v' = eval_expr ctx e' in
         let state = update_store state x v' in
         (state, laction_fuel)
    in
    let get_callstack = callstack_copier callstack in
    let recovery_states, error_steps =
      let recovery_states =
        if Exec_mode.is_verification_exec !Config.current_exec_mode then
          try_recovering_laction v_es errors ctx
        else None
      in
      match recovery_states with
      | Some states ->
          let states = states |> List.map (fun s -> (s, laction_fuel - 1)) in
          (states, [])
      | None ->
          let errors = Exec_err.estates errors in
          let callstack = get_callstack () in
          ([], [ make_err ~errors ~callstack ctx ])
    in
    let cont_states = ok_states @ recovery_states in
    let did_branch = List.length cont_states > 1 in
    let cont_steps =
      cont_states
      |> List.map @@ fun (state, laction_fuel) ->
         let callstack = get_callstack () in
         make_cont ~state ~callstack ~did_branch ~laction_fuel ctx
    in
    cont_steps @ error_steps

  let eval_logic (lcmd : LCmd.t) ctx =
    let { prog; loop_ids; loop_action; _ } = ctx in
    let { state; callstack; prev_loop_ids; invariant_frames; _ } = ctx.conf in
    match lcmd with
    | SL SymbExec -> [ make_cont ~symb_exec_next:true ctx ]
    (* Invariant being revisited *)
    | SL (Invariant (a, binders)) when loop_ids = prev_loop_ids ->
        (* TODO: is ignoring the result of match_invariant and vanishing the right thing to do? *)
        let _ = State.match_invariant prog true state a binders in
        []
    | SL (Invariant (a, binders)) -> (
        assert (loop_action = FrameOff (List.hd loop_ids));
        State.match_invariant prog false state a binders
        |> List.map @@ function
           | Ok (frame, state) ->
               let invariant_frames =
                 (List.hd loop_ids, frame) :: invariant_frames
               in
               make_cont ~state ~invariant_frames ctx
           | Error err ->
               let errors = [ Exec_err.EState err ] in
               make_err ~errors ctx)
    | _ ->
        let all_results = Eval_lcmd.eval_lcmd lcmd prog state in
        let successes, errors = Res_list.split all_results in
        let get_callstack = callstack_copier callstack in
        let success_steps =
          let did_branch = List.length successes > 1 in
          successes
          |> List.map @@ fun state ->
             let callstack = get_callstack () in
             make_cont ~state ~callstack ~did_branch ctx
        in
        let error_steps =
          match errors with
          | [] -> []
          | _ ->
              let errors = Exec_err.estates errors in
              let callstack = get_callstack () in
              [ make_err ~errors ~callstack ctx ]
        in
        success_steps @ error_steps

  let eval_guarded_goto e t_ix f_ix ctx =
    let { state; callstack; _ } : step_cont = ctx.conf in
    let@* vt = eval_expr ctx e in
    let lvt = Val.to_literal vt in
    let@* vf =
      match lvt with
      | Some (Bool true) -> Ok vfalse
      | Some (Bool false) -> Ok vtrue
      | _ -> eval_expr ctx (Expr.Infix.not e)
    in
    L.verbose (fun fmt ->
        fmt "Evaluated expressions: %a, %a" Val.pp vt Val.pp vf);
    let t_sat, f_sat =
      match lvt with
      | Some (Bool true) -> (true, false)
      | Some (Bool false) -> (false, true)
      | _ ->
          let t_sat = State.sat_check state vt in
          let f_sat =
            match t_sat with
            | false -> true
            | true -> State.sat_check state vf
          in
          (t_sat, f_sat)
    in
    let ts, fs =
      match (t_sat, f_sat) with
      | false, false -> ([], [])
      | true, false -> (State.assume state vt, [])
      | false, true -> ([], State.assume state vf)
      | true, true ->
          let state_t = State.copy state in
          let ts_unfolded = State.assume ~unfold:true state_t vt in
          let state_f = State.copy state in
          let fs_unfolded = State.assume ~unfold:true state_f vf in
          if List.length ts_unfolded + List.length fs_unfolded <= 2 then
            (ts_unfolded, fs_unfolded)
          else
            let state' = State.copy state in
            (State.assume state vt, State.assume state' vf)
    in
    let ts = ts |> List.map @@ fun s -> (s, t_ix) in
    let fs = fs |> List.map @@ fun s -> (s, f_ix) in
    let states = ts @ fs in
    let did_branch = t_sat && f_sat && List.length states > 1 in
    let get_callstack = callstack_copier callstack in
    states
    |> List.map @@ fun (state, ix) ->
       let callstack = get_callstack () in
       make_cont ~state ~callstack ~ix ~did_branch ctx

  let eval_phi_assignment lxarr ctx =
    let j = get_predecessor ctx in
    let@* state =
      let open Syntaxes.Result in
      List.fold_left
        (fun state (x, x_arr) ->
          let* state = state in
          let e = List.nth x_arr j in
          let+ v = eval_expr ctx e in
          update_store state x v)
        (Ok ctx.conf.state) lxarr
    in
    [ make_cont ~state ctx ]

  let eval_call x e args j subst ctx =
    let@* pid = eval_expr ctx e in
    let@* v_args = eval_exprs ctx args in
    eval_proc_call x pid v_args j subst ctx

  let eval_ecall x pid args j ctx =
    let prog = ctx.prog in
    let { state; callstack; ix; _ } : step_cont = ctx.conf in
    let pid =
      match pid with
      | Expr.PVar pid -> pid
      | Lit (String pid) -> pid
      | _ -> failwith "Procedure identifier not a program variable"
    in
    let@* v_args = eval_exprs ctx args in
    let open Syntaxes.List in
    let+ state, callstack, prev_ix, ix =
      External.execute prog.prog state callstack ix x pid v_args j
    in
    make_cont ~state ~callstack ~ix ~prev_ix ctx

  let eval_apply x pid_args j ctx =
    let@* v_pid_args = eval_expr ctx pid_args in
    match Val.to_list v_pid_args with
    | Some v_pid_args_list ->
        let pid = List.hd v_pid_args_list in
        let v_args = List.tl v_pid_args_list in
        eval_proc_call x pid v_args j None ctx
    | None ->
        Fmt.failwith "Apply not called with a list: @[<h>%a@]" Val.pp v_pid_args

  let eval_arguments x ctx =
    let { state; callstack; _ } : step_cont = ctx.conf in
    let args = callstack |> Call_stack.get_cur_args |> Val.from_list in
    let state = update_store state x args in
    [ make_cont ~state ctx ]

  let eval_return ?(is_error = false) ctx =
    let loop_ids = ctx.loop_ids in
    let { state; callstack; invariant_frames; _ } : step_cont = ctx.conf in
    let store = State.get_store state in
    let ret_val =
      match Store.get store Names.return_variable with
      | Some v -> v
      | None ->
          let kind = if is_error then "error" else "normal" in
          Fmt.failwith "Return variable not in store (%s return)" kind
    in
    let top, callstack =
      match callstack with
      | t :: c -> (t, c)
      | [] -> failwith "Malformed callstack"
    in
    match top with
    | { store = None; loop_ids = start_loop_ids; _ } ->
        check_loop_ids loop_ids start_loop_ids;
        let flag =
          if is_error then
            let () = Fmt.pr "e @?" in
            Flag.Normal
          else
            let () = Fmt.pr "n @?" in
            Flag.Error
        in
        [ make_finish ~flag ~ret_val ctx ]
    | {
     store = Some old_store;
     loop_ids = start_loop_ids;
     ret_var = x;
     call_index = prev_ix;
     continue_index;
     error_index;
     _;
    } ->
        let ix =
          match (is_error, error_index) with
          | false, _ -> continue_index
          | true, Some ix -> ix
          | true, None -> failwith "Malformed callstack"
        in
        let to_frame_on =
          loop_ids_to_frame_on_at_the_end loop_ids start_loop_ids
        in
        let open Syntaxes.List in
        let+ state =
          (* Framing on should never fail *)
          if Exec_mode.is_verification_exec !Config.current_exec_mode then
            State.frame_on state invariant_frames to_frame_on
            |> List.filter_map Result.to_option
          else [ state ]
        in
        let state = State.set_store state old_store in
        let state = update_store state x ret_val in
        make_cont ~state ~callstack ~loop_ids:start_loop_ids ~ix ~prev_ix ctx

  let eval_fail fail_code fail_params ctx =
    let@* fail_params = eval_exprs ctx fail_params in
    let errors = [ Exec_err.EFailReached { fail_code; fail_params } ] in
    [ make_err ~errors ctx ]

  let eval_cmd (ctx : step_ctx) : step list =
    match ctx.cmd with
    | Skip -> [ make_cont ctx ]
    | Assignment (x, e) -> eval_assignment x e ctx
    | LAction (x, a, es) -> eval_laction x a es ctx
    | Logic lcmd -> eval_logic lcmd ctx
    | Goto ix -> [ make_cont ~ix ctx ]
    | GuardedGoto (e, j, k) -> eval_guarded_goto e j k ctx
    | PhiAssignment lxarr -> eval_phi_assignment lxarr ctx
    | Call (x, e, args, j, subst) -> eval_call x e args j subst ctx
    | ECall (x, pid, args, j) -> eval_ecall x pid args j ctx
    | Apply (x, pid_args, j) -> eval_apply x pid_args j ctx
    | Arguments x -> eval_arguments x ctx
    | ReturnNormal -> eval_return ctx
    | ReturnError -> eval_return ~is_error:true ctx
    | Fail (fail_code, fail_params) -> eval_fail fail_code fail_params ctx
end
