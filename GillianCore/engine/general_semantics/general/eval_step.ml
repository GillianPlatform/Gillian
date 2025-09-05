module L = Logging
open Choice
open Choice.Syntax

module Make (Impl : Impl.S) = struct
  open Impl
  module Eval_cmd = Eval_cmd.Make (Impl)

  let eval_cmd = Eval_cmd.eval_cmd
  let pp_str_list = Fmt.(brackets (list ~sep:comma string))

  let understand_loop_action current previous : loop_action =
    if
      (not (Exec_mode.is_verification_exec !Config.current_exec_mode))
      || current = previous (* No change in loop structure *)
    then Nothing
    else
      (* Change in loop structure *)
      let len_cur = List.length current in
      let len_prev = List.length previous in
      match len_cur - len_prev with
      (* We have entered a new loop *)
      | 1 ->
          if List.tl current <> previous then Malformed
          else FrameOff (List.hd current)
      (* We have entered more than one loop - this is not allowed *)
      | n when n > 0 -> Malformed
      (* We have exited at least one loop *)
      | n ->
          let ids = Option.get (List_utils.list_sub previous 0 (-n)) in
          let rEState =
            Option.get (List_utils.list_sub previous (-n) (len_prev + n))
          in
          if rEState <> current then Malformed else FrameOn ids

  let cont_to_error
      ({ callstack; ix; _ } : step_cont)
      errors
      state
      last_known_loc =
    Step_err { state; last_known_loc; callstack; ix; errors }

  let get_cmd (prog : annot MP.prog) ({ callstack; ix; _ } : step_cont) :
      string * (annot * cmd) =
    let pid = Call_stack.get_cur_proc_id callstack in
    let proc = Prog.get_proc prog.prog pid in
    let proc =
      match proc with
      | Some proc -> proc
      | None -> Fmt.failwith "Procedure %s does not exist." pid
    in
    let annot, _, cmd = proc.proc_body.(ix) in
    (pid, (annot, cmd))

  let simplify_state state =
    snd (State.simplify ~save:true ~kill_new_lvars:true state)
    |> Choice.Indexed.make_const

  let handle_loop_action loop_action invariant_frames state =
    match loop_action with
    | Nothing -> return state
    | FrameOff id ->
        L.verbose (fun fmt -> fmt "INFO: Expecting to frame off %s" id);
        return state
    | Malformed -> L.fail "Malformed loop identifiers"
    | FrameOn ids ->
        L.verbose (fun fmt -> fmt "INFO: Going to frame on %a" pp_str_list ids);
        (* Framing on should never fail *)
        let states =
          State.frame_on state invariant_frames ids
          |> List.filter_map (function
               | Ok x -> Some x
               | _ -> None)
        in
        let n = List.length states in
        if n == 0 then
          L.normal (fun fmt -> fmt "WARNING: FRAMING ON RESULTED IN 0 STATES !")
        else if n > 1 then
          L.verbose (fun fmt ->
              fmt
                "WARNING: FRAMING ON AFTER EXITING LOOP BRANCHED INTO %i STATES"
                n);
        Choice.Indexed.make_const states

  let is_internal (cs : Call_stack.t) (prog : annot MP.prog) =
    let pid = (List.hd cs).pid in
    let proc = Hashtbl.find prog.prog.procs pid in
    proc.proc_internal

  let make_last_known_loc annot cs prog last_known_loc =
    let loc =
      match Annot.get_origin_loc annot with
      | Some loc when not (is_internal cs prog) -> Some loc
      | _ -> last_known_loc
    in
    ref loc

  let prepare_step_ctx prog conf =
    let pid, (annot, cmd) = get_cmd prog conf in
    let {
      state;
      last_known_loc;
      callstack;
      invariant_frames;
      ix;
      prev_loop_ids;
      _;
    } =
      conf
    in
    let loop_ids =
      Annot.get_loop_info annot @ Call_stack.get_loop_ids callstack
    in
    let loop_action =
      if Exec_mode.is_verification_exec !Config.current_exec_mode then
        understand_loop_action loop_ids prev_loop_ids
      else Nothing
    in
    let last_known_loc =
      make_last_known_loc annot callstack prog last_known_loc
    in
    MP.update_coverage prog pid ix;
    let ctxs =
      (* TODO: branch cases for this? *)
      let&* state = simplify_state state in
      let&+ state = handle_loop_action loop_action invariant_frames state in
      let conf = { conf with state } in
      { conf; prog; loop_ids; loop_action; pid; cmd; annot; last_known_loc }
    in
    ctxs

  let evaluate_step (prog : annot MP.prog) (cont : step_cont) :
      (int, step) Choice.t =
    let&* ctx = prepare_step_ctx prog cont in
    try eval_cmd ctx
    with Failure msg ->
      let loc = !(ctx.last_known_loc) in
      raise (Gillian_result.Exc.analysis_failure ?loc msg)
end
