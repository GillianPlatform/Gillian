module L = Logging

module Make (Impl : Impl.S) = struct
  open Impl
  open Choice

  let get_pid pid ctx =
    match Val.to_literal pid with
    | Some (String pid) -> Ok pid
    | Some _ ->
        let errors = [ Exec_err.EProc pid ] in
        Error (make_err ~errors ctx)
    | None -> failwith "Procedure Call Error - unlifting procedure ID failed"

  let get_spec_and_params pid ctx =
    let open Syntaxes.Result in
    let prog = ctx.prog in
    let proc = Prog.get_proc prog.prog pid in
    let spec = Hashtbl.find_opt prog.specs pid in
    let+ params =
      match (proc, spec) with
      | Some proc, _ -> Ok (Proc.get_params proc)
      | None, Some spec -> Ok (Spec.get_params spec.data)
      | _ ->
          let errors = [ Exec_err.EProc (Val.from_literal (String pid)) ] in
          Error (make_err ~errors ctx)
    in
    (spec, params)

  let build_args v_args params =
    let undef = Val.from_literal Undefined in
    let rec aux = function
      | _, 0 -> []
      | [], n -> undef :: aux ([], n - 1)
      | param :: rest_params, n -> param :: aux (rest_params, n - 1)
    in
    aux (v_args, List.length params)

  let is_proc_internal pid (prog : annot MP.prog) =
    (Prog.get_proc_exn prog.prog pid).proc_internal

  let eval_subst_list substs ctx =
    let open Syntaxes.Result in
    match substs with
    | None -> Ok None
    | Some (lab, substs) ->
        let+ substs =
          substs
          |> List_utils.map_results @@ fun (x, e) ->
             let+ v = eval_expr ctx e in
             (x, v)
        in
        Some (lab, substs)

  type eval_method =
    | Exec (* Execute the proc body *)
    | Exec_explicit
      (* Execute the proc body, because {symb_exec_next} was set *)
    | Spec_applied of MP.spec * (state * Flag.t, state_err) Res_list.t
    (* Use these results of applying the proc's spec *)
    (* For bi-abduction: *)
    | Suspend (* Wait until this proc's spec is available *)
    | Vanish (* Stop executing (in case of max unroll depth) *)

  (* Special cases for bi-abduction *)
  let get_bi_eval_method spec caller pid ctx =
    let { prog; conf; _ } = ctx in
    let { callstack; _ } : step_cont = conf in
    let is_bi = Exec_mode.is_biabduction_exec !Config.current_exec_mode in
    if
      is_bi
      && Call_stack.recursive_depth callstack pid >= !Config.bi_unroll_depth
    then (* Bi-abduction: reached max depth of recursive calls *)
      Some Vanish
    else if is_bi && pid = caller && not (is_proc_internal pid prog) then
      (* Bi-abduction: recursive call *)
      Some Exec
    else if spec = None && Hashtbl.mem prog.prog.bi_specs pid then Some Suspend
    else None

  let try_apply_spec spec x pid args subst ctx =
    let open Syntaxes.Result in
    let* subst = eval_subst_list subst ctx in
    let () = L.verbose (fun m -> m "ABOUT TO USE THE SPEC OF %s" pid) in
    let res = State.run_spec spec ctx.conf.state x args subst in
    let () =
      L.verbose (fun m -> m "Run_spec returned %d results" (List.length res))
    in
    match (res, spec.data.spec_incomplete) with
    | [], true ->
        let () = L.normal (fun m -> m "Proceeding with symbolic execution.") in
        Ok Exec
    | [], false ->
        let msg =
          "Error: Unable to use specification of function "
          ^ spec.data.spec_name
        in
        let errors = [ Exec_err.EState (StateErr.EOther msg) ] in
        Error (make_err ~errors ctx)
    | _ -> Ok (Spec_applied (spec, res))

  let get_eval_method spec caller x pid args subst ctx =
    let { symb_exec_next; _ } = ctx.conf in
    let bi_method = get_bi_eval_method spec pid caller ctx in
    match (bi_method, spec, symb_exec_next) with
    | Some m, _, _ -> Ok m
    | _, None, _ -> Ok Exec
    | _, Some _, true -> Ok Exec_explicit
    | _, Some spec, false -> try_apply_spec spec x pid args subst ctx

  let exec_proc_body ?symb_exec_next x pid v_args j params args ctx =
    let { conf; loop_ids; _ } = ctx in
    let { state; callstack; ix; _ } : step_cont = conf in
    let symb_exec_next =
      Option.value ~default:conf.symb_exec_next symb_exec_next
    in
    let old_store = State.get_store state in
    let new_store = Store.init (List.combine params args) in
    let state = State.set_store state new_store in
    let callstack =
      Call_stack.push callstack ~pid ~arguments:v_args ~store:old_store
        ~loop_ids ~ret_var:x ~call_index:ix ~continue_index:(ix + 1)
        ?error_index:j ()
    in
    return (make_cont ~state ~callstack ~ix:0 ~prev_ix:(-1) ~symb_exec_next ctx)

  let use_applied_spec (spec : MP.spec) res pid j ctx =
    let { callstack; ix; _ } : step_cont = ctx.conf in
    let successes, errors =
      res
      |> List.partition_map @@ function
         | Ok x -> Left x
         | Error x -> Right (Exec_err.EState x)
    in
    let did_branch = List.length successes > 1 in
    let get_callstack = callstack_copier callstack in
    let&** success_steps =
      successes
      |> List_utils.map_results @@ fun (state, flag) ->
         let open Syntaxes.Result in
         let+ ix =
           match (flag, j) with
           | Flag.Normal, _ -> Ok (ix + 1)
           | Flag.Error, Some j -> Ok j
           | Flag.Error, None ->
               Fmt.failwith "No error label provided when calling procedure %s"
                 pid
           | Flag.Bug, _ ->
               let msg =
                 Fmt.str "Error: tried to use bug spec '%s'" spec.data.spec_name
               in
               let errors = [ Exec_err.EState (StateErr.EOther msg) ] in
               Error (make_err ~state ~errors ctx)
         in
         let callstack = get_callstack () in
         make_cont ~state ~callstack ~ix ~did_branch ctx
    in
    let error_steps =
      match errors with
      | [] -> []
      | _ ->
          let callstack = get_callstack () in
          [ make_err ~errors ~callstack ctx ]
    in
    choose_const (success_steps @ error_steps)

  (** Evaluate a call to a proc
    @param x Variable in which the return value is stored
    @param pid Callee proc name
    @param v_args Arguments to the proc
    @param j Step index in the caller proc to go to if the callee proc terminates with an error (e.g. for try/catch)
    @param subst Substitutions
    @param ctx The executing context *)
  let eval_proc_call x pid v_args j subst ctx : step Seq.t =
    let { callstack; _ } : step_cont = ctx.conf in
    let&** pid = get_pid pid ctx in
    let&** spec, params = get_spec_and_params pid ctx in
    let caller = Call_stack.get_cur_proc_id callstack in
    let () = Call_graph.add_proc_call call_graph caller pid in
    let args = build_args v_args params in

    let&** eval_method = get_eval_method spec caller x pid args subst ctx in
    match eval_method with
    | Exec -> exec_proc_body x pid v_args j params args ctx
    | Exec_explicit ->
        exec_proc_body ~symb_exec_next:false x pid v_args j params args ctx
    | Spec_applied (spec, res) -> use_applied_spec spec res pid j ctx
    | Suspend -> return (Step_susp (pid, ctx.conf))
    | Vanish -> vanish
end
