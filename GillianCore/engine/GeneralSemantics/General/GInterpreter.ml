open Literal
module L = Logging

module type S = sig
  module CallStack : CallStack.S

  type vt

  type st

  type store_t

  type state_t

  type state_err_t

  type state_vt

  type heap_t

  module Val : Val.S with type t = vt

  module Store : Store.S with type t = store_t and type vt = vt

  module State :
    State.S
      with type t = state_t
       and type vt = vt
       and type st = st
       and type store_t = store_t
       and type heap_t = heap_t

  type invariant_frames = (string * State.t) list

  type err_t = (vt, state_err_t) ExecErr.t [@@deriving yojson]

  type cconf_t =
    | ConfErr    of CallStack.t * int * State.t * err_t list
    | ConfCont   of
        State.t * CallStack.t * invariant_frames * int * string list * int * int
    | ConfFinish of Flag.t * State.vt * State.t
        (** Equal to Conf cont + the id of the required spec *)
    | ConfSusp   of
        string
        * State.t
        * CallStack.t
        * invariant_frames
        * int
        * string list
        * int
        * int

  type conf_t = BConfErr of err_t list | BConfCont of State.t

  type result_t = (State.t, state_vt, err_t) ExecRes.t

  type 'a cont_func =
    | Finished of 'a list
    | Continue of (string option * (unit -> 'a cont_func))

  type cmd_step = {
    call_stack : CallStack.t;
    proc_body_index : int;
    state : state_t option;
    errors : err_t list;
  }
  [@@deriving yojson]

  val pp_err : Format.formatter -> (vt, state_err_t) ExecErr.t -> unit

  val pp_result : Format.formatter -> result_t list -> unit

  val call_graph : CallGraph.t

  val reset : unit -> unit

  val evaluate_lcmds : UP.prog -> LCmd.t list -> State.t -> State.t list

  val init_evaluate_proc :
    (result_t -> 'a) ->
    UP.prog ->
    string ->
    string list ->
    State.t ->
    'a cont_func

  val evaluate_proc :
    (result_t -> 'a) -> UP.prog -> string -> string list -> State.t -> 'a list

  val evaluate_prog : UP.prog -> result_t list
end

(** General GIL Interpreter *)
module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (External : External.S) =
struct
  (* *************** *
   * Auxiliary Types *
   * *************** *)

  module CallStack = CallStack.Make (Val) (Store)
  module External = External (Val) (ESubst) (Store) (State) (CallStack)
  module Val = Val
  module State = State
  module Store = Store

  type vt = Val.t

  type st = ESubst.t

  type store_t = Store.t

  type state_t = State.t [@@deriving yojson]

  type state_err_t = State.err_t

  type state_vt = State.vt

  type heap_t = State.heap_t

  type invariant_frames = (string * State.t) list

  type err_t = (Val.t, State.err_t) ExecErr.t [@@deriving yojson]

  let pp_err = ExecErr.pp Val.pp State.pp_err

  let pp_str_list = Fmt.(brackets (list ~sep:comma string))

  (** Type of configurations: state, call stack, previous index, previous loop ids, current index, branching *)
  type cconf_t =
    | ConfErr    of CallStack.t * int * State.t * err_t list
    | ConfCont   of
        State.t * CallStack.t * invariant_frames * int * string list * int * int
    | ConfFinish of Flag.t * State.vt * State.t
        (** Equal to Conf cont + the id of the required spec *)
    | ConfSusp   of
        string
        * State.t
        * CallStack.t
        * invariant_frames
        * int
        * string list
        * int
        * int

  type conf_t = BConfErr of err_t list | BConfCont of State.t

  type result_t = (State.t, State.vt, err_t) ExecRes.t

  type 'a cont_func =
    | Finished of 'a list
    | Continue of (string option * (unit -> 'a cont_func))

  type cmd_step = {
    call_stack : CallStack.t;
    proc_body_index : int;
    state : state_t option;
    errors : err_t list;
  }
  [@@deriving yojson]

  type annotated_action = { annot : Annot.t; action_name : string }
  [@@deriving yojson]

  let max_branching = 100

  exception Interpreter_error of err_t list * State.t

  (** Internal error, carrying a string description *)
  exception Internal_error of string

  (** Syntax error, carrying a string description *)
  exception Syntax_error of string

  let pp_single_result ft res = ExecRes.pp State.pp Val.pp pp_err ft res

  let call_graph = CallGraph.make ~init_capacity:128 ()

  let reset () = CallGraph.reset call_graph

  (* Often-used values *)
  let vtrue = Val.from_literal (Bool true)

  let vfalse = Val.from_literal (Bool false)

  let symb_exec_next = ref false

  type loop_action =
    | Nothing
    | FrameOff  of string
    | FrameOn   of string list
    | Malformed

  let understand_loop_action current previous : loop_action =
    match current = previous with
    (* No change in loop structure *)
    | true -> Nothing
    (* Change in loop structure *)
    | false -> (
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
            let rest =
              Option.get (List_utils.list_sub previous (-n) (len_prev + n))
            in
            if rest <> current then Malformed else FrameOn ids)

  (* ******************* *
   * Auxiliary Functions *
   * ******************* *)

  let get_cmd (prog : UP.prog) (cs : CallStack.t) (i : int) :
      string * (Annot.t * int Cmd.t) =
    let pid = CallStack.get_cur_proc_id cs in
    let proc = Prog.get_proc prog.prog pid in
    let proc =
      match proc with
      | Some proc -> proc
      | None      -> raise (Failure ("Procedure " ^ pid ^ " does not exist."))
    in
    let annot, _, cmd = proc.proc_body.(i) in
    (pid, (annot, cmd))

  let get_predecessor (prog : UP.prog) (cs : CallStack.t) (prev : int) (i : int)
      : int =
    let pid = CallStack.get_cur_proc_id cs in
    try Hashtbl.find prog.prog.predecessors (pid, prev, i)
    with _ ->
      raise
        (Failure (Printf.sprintf "Undefined predecessor: %s %d %d" pid prev i))

  let update_store (state : State.t) (x : string) (v : Val.t) : State.t =
    let store = State.get_store state in
    let () = Store.put store x v in
    let state' = State.set_store state store in
    state'

  let eval_subst_list
      (state : State.t) (subst_lst : (string * (string * Expr.t) list) option) :
      (string * (string * Val.t) list) option =
    match subst_lst with
    | None                  -> None
    | Some (lab, subst_lst) ->
        let subst_lst' : (string * Val.t) list =
          List.map (fun (x, e) -> (x, State.eval_expr state e)) subst_lst
        in
        Some (lab, subst_lst')

  let make_eval_expr (state : State.t) : Expr.t -> Val.t =
   fun e ->
    try State.eval_expr state e
    with State.Internal_State_Error (errs, s) ->
      raise (Interpreter_error (List.map (fun x -> ExecErr.ESt x) errs, s))

  let print_configuration
      (cmd : Annot.t * int Cmd.t)
      (state : State.t)
      (cs : CallStack.t)
      (i : int)
      (b_counter : int) : unit =
    let annot, cmd = cmd in
    let state_printer =
      match !Config.pbn with
      | false -> State.pp
      | true  ->
          let pvars, lvars, locs =
            (Cmd.pvars cmd, Cmd.lvars cmd, Cmd.locs cmd)
          in
          State.pp_by_need pvars lvars locs
    in
    L.normal (fun m ->
        m
          "@[------------------------------------------------------@\n\
           --%s: %i--@\n\
           TIME: %f@\n\
           CMD: %a@\n\
           PROCS: %a@\n\
           LOOPS: %a ++ %a@\n\
           BRANCHING: %d@\n\
           @\n\
           %a@\n\
           ------------------------------------------------------@]\n"
          (CallStack.get_cur_proc_id cs)
          i (Sys.time ()) Cmd.pp_indexed cmd pp_str_list
          (CallStack.get_cur_procs cs)
          pp_str_list
          (Annot.get_loop_info annot)
          pp_str_list
          (CallStack.get_loop_ids cs)
          b_counter state_printer state)

  let cmd_step_pp fmt cmd_step =
    (* TODO: Cmd step should contain all things in a configuration
             print the same contents as print_configuration *)
    CallStack.pp fmt cmd_step.call_stack

  let annotated_action_pp fmt annotated_action =
    let origin_loc = Annot.get_origin_loc annotated_action.annot in
    Fmt.pf fmt "Executing action '%s' at %a" annotated_action.action_name
      (Fmt.option ~none:(Fmt.any "none") Location.pp)
      origin_loc

  let print_lconfiguration (lcmd : LCmd.t) (state : State.t) : unit =
    L.normal (fun m ->
        m
          "@[------------------------------------------------------@\n\
           TIME: %f@\n\
           LCMD: %a@\n\
           @\n\
           %a@\n\
           ------------------------------------------------------@]@\n"
          (Sys.time ()) LCmd.pp lcmd State.pp state)

  let check_loop_ids actual expected =
    match actual = expected with
    | false ->
        Fmt.failwith
          "Malformed loop structure: current loops: %a; expected loops: %a"
          pp_str_list actual pp_str_list expected
    | true  -> ()

  let rec loop_ids_to_frame_on_at_the_end end_ids start_ids =
    if end_ids = start_ids then []
    else
      match end_ids with
      | []     ->
          Fmt.failwith
            "Malformed loop structure (at return): current loops: %a; expected \
             loops: %a"
            pp_str_list end_ids pp_str_list start_ids
      | x :: r -> x :: loop_ids_to_frame_on_at_the_end r start_ids

  (* ************** *
   * Main Functions *
   * ************** *)

  (**
    Evaluation of logic commands

    @param prog GIL program
    @param lcmd Logic command to be evaluated
    @param state Current state
    @param preds Current predicate set
    @return List of states/predicate sets resulting from the evaluation
  *)
  let rec evaluate_lcmd (prog : UP.prog) (lcmd : LCmd.t) (state : State.t) :
      State.t list =
    print_lconfiguration lcmd state;

    let eval_expr = make_eval_expr state in
    match lcmd with
    | AssumeType (x, t) -> (
        match Val.from_expr (LVar x) with
        | Some v_x -> (
            match State.assume_t state v_x t with
            | Some state' -> [ state' ]
            | _           ->
                raise
                  (Failure
                     (Printf.sprintf
                        "ERROR: AssumeType: Cannot assume type %s for variable \
                         %s."
                        (Type.str t) x)))
        | _        ->
            raise
              (Failure
                 (Printf.sprintf
                    "ERROR: AssumeType: Variable %s cannot be turned into a \
                     value."
                    x)))
    | Assume f ->
        let store_subst = Store.to_ssubst (State.get_store state) in
        let f' = SVal.SESubst.substitute_formula store_subst ~partial:true f in
        (* Printf.printf "Assuming %s\n" (Formula.str f'); *)
        let fos =
          if ExecMode.biabduction_exec !Config.current_exec_mode then
            let fos = Formula.get_disjuncts f' in
            match fos with
            | []              -> []
            | [ f' ]          -> [ (f', state) ]
            | f' :: other_fos ->
                let new_fos_states =
                  List.map (fun f'' -> (f'', State.copy state)) other_fos
                in
                (f', state) :: new_fos_states
          else [ (f', state) ]
        in
        (* Printf.printf "Considering the following disjuncts: %s\n" *)
        (* I commented the following, because it builds a string and discards it ? *)
        (* (String.concat "; " (List.map (fun (f, _) -> Formula.str f) fos));  *)
        List.concat
          (List.map
             (fun (f'', state) ->
               match State.assume_a state [ f'' ] with
               | Some state' -> [ state' ]
               | _           -> [])
             fos)
    | SpecVar xs -> [ State.add_spec_vars state (Var.Set.of_list xs) ]
    | Assert f -> (
        let store_subst = Store.to_ssubst (State.get_store state) in
        let f' = SVal.SESubst.substitute_formula store_subst ~partial:true f in
        match State.assert_a state [ f' ] with
        | true  -> [ state ]
        | false ->
            let err = StateErr.EPure f' in
            let failing_model = State.sat_check_f state [ Not f' ] in
            let msg =
              Fmt.str
                "Assert failed with argument @[<h>%a@].@\n\
                 @[<v 2>Failing Model:@\n\
                 %a@]@\n"
                Formula.pp f'
                Fmt.(option ~none:(any "CANNOT CREATE MODEL") ESubst.pp)
                failing_model
            in
            if not (ExecMode.biabduction_exec !Config.current_exec_mode) then
              Printf.printf "%s" msg;
            L.normal (fun m -> m "%s" msg);
            raise (Interpreter_error ([ ESt err ], state)))
    | Macro (name, args) -> (
        let macro = Macro.get prog.prog.macros name in
        match macro with
        | None       ->
            L.verbose (fun m ->
                m "@[<v 2>Current MACRO TABLE:\n%a\n@]" Macro.pp_tbl
                  prog.prog.macros);
            raise
              (Failure
                 (Fmt.str "NO MACRO found when executing: @[<h>%a@]" LCmd.pp
                    lcmd))
        | Some macro ->
            let expand_macro (macro : Macro.t) (args : Expr.t list) :
                LCmd.t list =
              let params = macro.macro_params in
              let params_card = List.length params in
              let args_card = List.length args in
              if params_card <> args_card then
                raise
                  (Failure
                     (Printf.sprintf
                        "Macro %s called with incorrect number of parameters: \
                         %d instead of %d."
                        macro.macro_name args_card params_card));
              let subst = SVal.SSubst.init (List.combine params args) in
              let lcmds = macro.macro_definition in
              List.map (SVal.SSubst.substitute_lcmd subst ~partial:true) lcmds
            in
            let lcmds = expand_macro macro args in
            evaluate_lcmds prog lcmds state)
    (* We have to understand what is the intended semantics of the logic if *)
    | If (e, lcmds_t, lcmds_e) -> (
        let ve = eval_expr e in
        let e = Val.to_expr ve in
        match Formula.lift_logic_expr e with
        | Some (True, False) -> evaluate_lcmds prog lcmds_t state
        | Some (False, True) -> evaluate_lcmds prog lcmds_e state
        | Some (foe, nfoe)   ->
            let state' = State.copy state in
            let then_states =
              Option.fold
                ~some:(fun state -> evaluate_lcmds prog lcmds_t state)
                ~none:[]
                (State.assume_a state [ foe ])
            in
            let else_states =
              Option.fold
                ~some:(fun state -> evaluate_lcmds prog lcmds_e state)
                ~none:[]
                (State.assume_a state' [ nfoe ])
            in
            then_states @ else_states
        | None               ->
            raise
              (Failure
                 "Non-boolean expression in the condition of the logical if"))
    | Branch fof ->
        let state' = State.copy state in
        let state =
          Option.fold
            ~some:(fun x -> [ x ])
            ~none:[]
            (State.assume_a state [ fof ])
        in
        let state' =
          Option.fold
            ~some:(fun x -> [ x ])
            ~none:[]
            (State.assume_a state' [ Not fof ])
        in
        state @ state'
    | SL sl_cmd -> (
        match State.evaluate_slcmd prog sl_cmd state with
        | Ok result -> result
        | Error msg -> L.fail msg)

  and evaluate_lcmds (prog : UP.prog) (lcmds : LCmd.t list) (state : State.t) :
      State.t list =
    match lcmds with
    | []                 -> [ state ]
    | lcmd :: rest_lcmds ->
        let rets = evaluate_lcmd prog lcmd state in
        List.concat
          (List.map (fun state -> evaluate_lcmds prog rest_lcmds state) rets)

  (**
  Evaluation of commands

  @param prog GIL program
  @param state Current state
  @param preds Current predicate set
  @param cs Current call stack
  @param prev Previous index
  @param i Current index
  @return List of configurations resulting from the evaluation
*)

  let rec evaluate_cmd
      (prog : UP.prog)
      (state : State.t)
      (cs : CallStack.t)
      (iframes : invariant_frames)
      (prev : int)
      (prev_loop_ids : string list)
      (i : int)
      (b_counter : int) : cconf_t list =
    let _, (annot, _) = get_cmd prog cs i in

    (* The full list of loop ids is the concatenation
       of the loop ids of the current procedure plus
       the loop ids that have come from the call stack *)
    let loop_ids = Annot.get_loop_info annot @ CallStack.get_loop_ids cs in

    let loop_action : loop_action =
      if ExecMode.verification_exec !Config.current_exec_mode then
        understand_loop_action loop_ids prev_loop_ids
      else Nothing
    in
    let eval_in_state state =
      evaluate_cmd_after_frame_handling prog state cs iframes prev prev_loop_ids
        i b_counter
    in
    match loop_action with
    | Nothing     -> eval_in_state state
    | FrameOff id ->
        L.verbose (fun fmt -> fmt "INFO: Expecting to frame off %s" id);
        eval_in_state state
    | Malformed   -> L.fail "Malformed loop identifiers"
    | FrameOn ids ->
        L.verbose (fun fmt -> fmt "INFO: Going to frame on %a" pp_str_list ids);
        let states = State.frame_on state iframes ids in
        let n = List.length states in
        if n == 0 then
          L.normal (fun fmt -> fmt "WARNING: FRAMING ON RESULTED IN 0 STATES !")
        else if n > 1 then
          L.verbose (fun fmt ->
              fmt
                "WARNING: FRAMING ON AFTER EXITING LOOP BRANCHED INTO %i STATES"
                n);
        List.concat_map eval_in_state states

  and evaluate_cmd_after_frame_handling
      (prog : UP.prog)
      (state : State.t)
      (cs : CallStack.t)
      (iframes : invariant_frames)
      (prev : int)
      (prev_loop_ids : string list)
      (i : int)
      (b_counter : int) : cconf_t list =
    let store = State.get_store state in
    let eval_expr = make_eval_expr state in
    let proc_name, annot_cmd = get_cmd prog cs i in
    let annot, cmd = annot_cmd in
    let loop_ids = Annot.get_loop_info annot @ CallStack.get_loop_ids cs in
    let loop_action : loop_action =
      if ExecMode.verification_exec !Config.current_exec_mode then
        understand_loop_action loop_ids prev_loop_ids
      else Nothing
    in
    (* if !Config.stats then Statistics.exec_cmds := !Statistics.exec_cmds + 1; *)
    UP.update_coverage prog proc_name i;

    print_configuration annot_cmd state cs i b_counter;

    let evaluate_procedure_call x pid v_args j subst =
      let pid =
        match Val.to_literal pid with
        | Some (String pid) -> pid
        | Some _            ->
            let err = [ ExecErr.EProc pid ] in
            raise (Interpreter_error (err, state))
        | None              ->
            raise
              (Internal_error
                 "Procedure Call Error - unlifting procedure ID failed")
      in

      let proc = Prog.get_proc prog.prog pid in
      let spec = Hashtbl.find_opt prog.specs pid in
      let params =
        match (proc, spec) with
        | Some proc, _    -> Proc.get_params proc
        | None, Some spec -> Spec.get_params spec.spec
        | _               ->
            raise
              (Interpreter_error
                 ([ EProc (Val.from_literal (String pid)) ], state))
      in
      let caller = CallStack.get_cur_proc_id cs in
      let () = CallGraph.add_proc_call call_graph caller pid in
      let prmlen = List.length params in

      let args = Array.make prmlen (Val.from_literal Undefined) in
      let () =
        List.iteri (fun i v_arg -> if i < prmlen then args.(i) <- v_arg) v_args
      in
      let args = Array.to_list args in

      let process_ret copy_cs ret_state fl b_counter : cconf_t =
        let new_cs =
          match copy_cs with
          | true  -> CallStack.copy cs
          | false -> cs
        in

        let new_j =
          match (fl, j) with
          | Flag.Normal, _     -> i + 1
          | Flag.Error, Some j -> j
          | Flag.Error, None   ->
              let msg =
                Printf.sprintf
                  "SYNTAX ERROR: No error label provided when calling \
                   procedure %s"
                  pid
              in
              L.normal (fun fmt -> fmt "%s" msg);
              raise (Syntax_error msg)
        in

        ConfCont (ret_state, new_cs, iframes, i, loop_ids, new_j, b_counter)
      in

      let is_internal_proc proc_name =
        (Prog.get_proc_exn prog.prog proc_name).proc_internal
      in

      let symb_exec_proc () =
        let new_store = Store.init (List.combine params args) in
        let old_store = State.get_store state in
        let state' = State.set_store state new_store in
        let cs' =
          (* Note the new loop identifiers *)
          CallStack.push cs ~pid ~arguments:v_args ~store:old_store ~loop_ids
            ~ret_var:x ~call_index:i ~continue_index:(i + 1) ?error_index:j ()
        in
        [ ConfCont (state', cs', iframes, -1, loop_ids, 0, b_counter) ]
      in

      let spec_exec_proc () =
        match spec with
        | Some spec -> (
            match !symb_exec_next with
            | true  ->
                symb_exec_next := false;
                symb_exec_proc ()
            | false -> (
                let subst = eval_subst_list state subst in
                L.verbose (fun fmt -> fmt "ABOUT TO USE THE SPEC OF %s" pid);
                (* print_to_all ("\tStarting run spec: " ^ pid); *)
                let rets : (State.t * Flag.t) list =
                  State.run_spec spec state x args subst
                in
                (* print_to_all ("\tFinished run spec: " ^ pid); *)
                L.verbose (fun fmt ->
                    fmt "Run_spec returned %d Results" (List.length rets));
                let b_counter =
                  if List.length rets > 1 then b_counter + 1 else b_counter
                in
                match rets with
                | (ret_state, fl) :: rest_rets ->
                    process_ret false ret_state fl b_counter
                    :: List.map
                         (fun (ret_state, fl) ->
                           process_ret true ret_state fl b_counter)
                         rest_rets
                (* Run spec returned no results *)
                | _ -> (
                    match spec.spec.spec_incomplete with
                    | true  ->
                        L.normal (fun fmt ->
                            fmt "Proceeding with symbolic execution.");
                        symb_exec_proc ()
                    | false ->
                        L.fail
                          (Format.asprintf
                             "ERROR: Unable to use specification of function %s"
                             spec.spec.spec_name))))
        | None      ->
            if Hashtbl.mem prog.prog.bi_specs pid then
              [
                ConfSusp
                  (pid, state, cs, iframes, prev, prev_loop_ids, i, b_counter);
              ]
            else symb_exec_proc ()
      in

      match ExecMode.biabduction_exec !Config.current_exec_mode with
      | true  -> (
          match
            ( pid = caller,
              is_internal_proc pid,
              CallStack.recursive_depth cs pid >= !Config.bi_unroll_depth )
          with
          (* In bi-abduction, reached max depth of recursive calls *)
          | _, _, true -> []
          (* In bi-abduction, recursive call *)
          | true, false, _ -> symb_exec_proc ()
          (* TODO: When JS internals work
             | true, false, false
               when List.length
                      (List.filter is_internal_proc (CallStack.get_cur_procs cs))
                    < !Config.bi_no_spec_depth -> symb_exec_proc () *)
          | _ -> spec_exec_proc ())
      | false -> spec_exec_proc ()
    in

    match cmd with
    (* Skip *)
    | Skip -> [ ConfCont (state, cs, iframes, i, loop_ids, i + 1, b_counter) ]
    (* Assignment *)
    | Assignment (x, e) ->
        let v = eval_expr e in
        let state' = update_store state x v in
        [ ConfCont (state', cs, iframes, i, loop_ids, i + 1, b_counter) ]
    (* Action *)
    | LAction (x, a, es) -> (
        let _ =
          L.normal_specific
            (L.Loggable.make annotated_action_pp annotated_action_of_yojson
               annotated_action_to_yojson { annot; action_name = a })
            L.LoggingConstants.ContentType.annotated_action
        in
        let v_es = List.map eval_expr es in
        match State.execute_action a state v_es with
        | ASucc [] ->
            raise (Failure "HORROR: Successful action resulted in no states")
        | ASucc ((state', vs) :: rest_rets) -> (
            let e' = Expr.EList (List.map Val.to_expr vs) in
            let v' = eval_expr e' in
            let state'' = update_store state' x v' in
            let rest_confs =
              List.map
                (fun (r_state, r_vs) ->
                  let r_e = Expr.EList (List.map Val.to_expr r_vs) in
                  let r_v = eval_expr r_e in
                  let r_state' = update_store r_state x r_v in
                  ConfCont
                    ( r_state',
                      CallStack.copy cs,
                      iframes,
                      i,
                      loop_ids,
                      i + 1,
                      b_counter ))
                rest_rets
            in
            let ret_len = 1 + List.length rest_rets in
            let b_counter = b_counter + if ret_len > 1 then 1 else 0 in
            match
              (ret_len >= 3 && !Config.parallel, ret_len = 2 && !Config.parallel)
              (* XXX: && !Config.act_threads < !Config.max_threads ) *)
            with
            | true, _     -> (
                (* print_endline (Printf.sprintf "Action returned >=3: %d" (!Config.act_threads + 2)); *)
                let pid = Unix.fork () in
                match pid with
                | 0 -> (
                    let pid = Unix.fork () in
                    match pid with
                    | 0 -> List.tl rest_confs
                    | _ -> [ List.hd rest_confs ])
                | _ ->
                    [
                      ConfCont
                        (state'', cs, iframes, i, loop_ids, i + 1, b_counter);
                    ])
            | false, true -> (
                (* Can split into two threads *)
                let b_counter = b_counter + 1 in
                (* print_endline (Printf.sprintf "Action returned 2: %d" (!Config.act_threads + 1)); *)
                let pid = Unix.fork () in
                match pid with
                | 0 ->
                    [
                      ConfCont
                        (state'', cs, iframes, i, loop_ids, i + 1, b_counter);
                    ]
                | _ -> rest_confs)
            | _           ->
                ConfCont (state'', cs, iframes, i, loop_ids, i + 1, b_counter)
                :: rest_confs)
        | AFail errs ->
            if not (ExecMode.concrete_exec !Config.current_exec_mode) then (
              let expr_params = List.map Val.to_expr v_es in
              let recovery_params =
                List.concat_map Expr.base_elements expr_params
              in
              let recovery_params =
                List.map Option.get (List.map Val.from_expr recovery_params)
              in
              let recovery_vals =
                State.get_recovery_vals state errs @ recovery_params
              in
              let recovery_states : (State.t list, string) result =
                State.automatic_unfold state recovery_vals
              in
              match recovery_states with
              | Ok recovery_states ->
                  let b_counter =
                    b_counter + if List.length recovery_states = 1 then 0 else 1
                  in
                  List.map
                    (fun state ->
                      ConfCont
                        (state, cs, iframes, prev, prev_loop_ids, i, b_counter))
                    recovery_states
              | _                  ->
                  L.normal ~title:"failure" ~severity:Error (fun m ->
                      m "Action call failed with:@.%a"
                        (Fmt.Dump.list State.pp_err)
                        errs);
                  raise (State.Internal_State_Error (errs, state)))
            else Fmt.failwith "Local Action Failed: %a" Cmd.pp_indexed cmd)
    (* Logic command *)
    | Logic lcmd -> (
        match lcmd with
        | SL SymbExec ->
            symb_exec_next := true;
            [ ConfCont (state, cs, iframes, i, loop_ids, i + 1, b_counter) ]
        (* Invariant being revisited *)
        | SL (Invariant (a, binders)) when prev_loop_ids = loop_ids ->
            (* let () = Fmt.pr "\nRe-establishing invariant... @?" in *)
            let _ = State.unify_invariant prog true state a binders in
            let () = L.verbose (fun fmt -> fmt "Invariant re-established.") in
            (* let () = Fmt.pr "\nInvariant re-established. @?" in *)
            []
        | SL (Invariant (a, binders)) ->
            assert (loop_action = FrameOff (List.hd loop_ids));
            (* let () = Fmt.pr "\nEstablishing invariant... @?" in *)
            let frames_and_states : (State.t * State.t) list =
              State.unify_invariant prog false state a binders
            in
            (* let () = Fmt.pr "\nSuccessfully established invariant. @?" in *)
            List.map
              (fun (frame, state) ->
                let iframes = (List.hd loop_ids, frame) :: iframes in
                ConfCont (state, cs, iframes, i, loop_ids, i + 1, b_counter))
              frames_and_states
        | _ ->
            let resulting_states : State.t list =
              evaluate_lcmd prog lcmd state
            in
            let b_counter =
              if List.length resulting_states > 1 then b_counter + 1
              else b_counter
            in
            List.map
              (fun state ->
                ConfCont (state, cs, iframes, i, loop_ids, i + 1, b_counter))
              resulting_states)
    (* Unconditional goto *)
    | Goto j -> [ ConfCont (state, cs, iframes, i, loop_ids, j, b_counter) ]
    (* Conditional goto *)
    | GuardedGoto (e, j, k) -> (
        let vt = eval_expr e in
        let lvt = Val.to_literal vt in
        let vf =
          match lvt with
          | Some (Bool true)  -> vfalse
          | Some (Bool false) -> vtrue
          | _                 -> eval_expr (UnOp (UNot, e))
        in
        L.verbose (fun fmt ->
            fmt "Evaluated expressions: %a, %a" Val.pp vt Val.pp vf);
        let can_put_t, can_put_f =
          match lvt with
          | Some (Bool true)  -> (true, false)
          | Some (Bool false) -> (false, true)
          | _                 ->
              let vtx = State.sat_check state vt in
              let vfx =
                match vtx with
                | false -> true
                | true  -> State.sat_check state vf
              in
              (vtx, vfx)
        in
        let sp_t, sp_f =
          match (can_put_t, can_put_f) with
          | false, false -> ([], [])
          | true, false  ->
              (List.map (fun x -> (x, j)) (State.assume state vt), [])
          | false, true  ->
              ([], List.map (fun x -> (x, k)) (State.assume state vf))
          | true, true   ->
              let state_t = State.copy state in
              let unfolded_trues = State.assume ~unfold:true state_t vt in
              let state_f = State.copy state in
              let unfolded_falses = State.assume ~unfold:true state_f vf in
              let utlen, uflen =
                (List.length unfolded_trues, List.length unfolded_falses)
              in
              if utlen = 0 || uflen = 0 || utlen + uflen = 2 then
                ( List.map (fun x -> (x, j)) unfolded_trues,
                  List.map (fun x -> (x, k)) unfolded_falses )
              else
                let state' = State.copy state in
                ( List.map (fun x -> (x, j)) (State.assume state vt),
                  List.map (fun x -> (x, k)) (State.assume state' vf) )
        in
        let sp = sp_t @ sp_f in

        let b_counter =
          if can_put_t && can_put_f && List.length sp > 1 then b_counter + 1
          else b_counter
        in
        let result =
          List.mapi
            (fun j (state, next) ->
              ConfCont
                ( state,
                  (if j = 0 then cs else CallStack.copy cs),
                  iframes,
                  i,
                  loop_ids,
                  next,
                  b_counter ))
            sp
        in
        match
          List.length result = 2 && !Config.parallel
          (* XXX: && !Config.act_threads < !Config.max_threads *)
        with
        | true  -> (
            (* print_endline (Printf.sprintf "Conditional goto: %d" (!Config.act_threads + 1)); *)
            let pid = Unix.fork () in
            match pid with
            | 0 -> [ List.hd result ]
            | _ -> List.tl result)
        | false -> result)
    | PhiAssignment lxarr ->
        let j = get_predecessor prog cs prev i in
        let state' =
          List.fold_left
            (fun state (x, x_arr) ->
              let e = List.nth x_arr j in
              let v = eval_expr e in
              update_store state x v)
            state lxarr
        in
        [ ConfCont (state', cs, iframes, i, loop_ids, i + 1, b_counter) ]
    (* Function call *)
    | Call (x, e, args, j, subst) ->
        let pid = eval_expr e in
        let v_args = List.map eval_expr args in
        let result = evaluate_procedure_call x pid v_args j subst in
        result
    (* External function call *)
    | ECall (x, pid, args, j) ->
        let pid =
          match pid with
          | PVar pid         -> pid
          | Lit (String pid) -> pid
          | _                ->
              raise
                (Exceptions.Impossible
                   "Procedure identifier not a program variable")
        in
        let v_args = List.map eval_expr args in
        List.map
          (fun (state, cs, i, j) ->
            ConfCont (state, cs, iframes, i, loop_ids, j, b_counter))
          (External.execute prog.prog state cs i x pid v_args j)
    (* Function application *)
    | Apply (x, pid_args, j) -> (
        let v_pid_args = eval_expr pid_args in
        let v_pid_args_list = Val.to_list v_pid_args in
        match v_pid_args_list with
        | Some v_pid_args_list ->
            let pid = List.hd v_pid_args_list in
            let v_args = List.tl v_pid_args_list in
            evaluate_procedure_call x pid v_args j None
        | None                 ->
            raise
              (Failure
                 (Fmt.str "Apply not called with a list: @[<h>%a@]" Val.pp
                    v_pid_args)))
    (* Arguments *)
    | Arguments x ->
        let args = CallStack.get_cur_args cs in
        let args = Val.from_list args in
        let state' = update_store state x args in
        [ ConfCont (state', cs, iframes, i, loop_ids, i + 1, b_counter) ]
    (* Normal-mode return *)
    | ReturnNormal ->
        let v_ret = Store.get store Names.return_variable in
        let result =
          match (v_ret, cs) with
          | None, _ -> raise (Failure "nm_ret_var not in store (normal return)")
          | Some v_ret, { store = None; loop_ids = start_loop_ids; _ } :: _ ->
              check_loop_ids loop_ids start_loop_ids;
              (* TODO: Redirect stdout to a file in debugging mode, as
                   the debug adapter communicates with VSCode via stdout. This
                   particular print statement currently causes issues, but
                   should be re-added once stdout has been redirected. *)
              (* Fmt.pr "n @?"; *)
              [ ConfFinish (Normal, v_ret, state) ]
          | ( Some v_ret,
              {
                store = Some old_store;
                loop_ids = start_loop_ids;
                ret_var = x;
                call_index = prev';
                continue_index = j;
                _;
              }
              :: cs' ) ->
              let to_frame_on =
                loop_ids_to_frame_on_at_the_end loop_ids start_loop_ids
              in
              let ( let+ ) x f = List.map f x in
              let+ state =
                if ExecMode.verification_exec !Config.current_exec_mode then
                  State.frame_on state iframes to_frame_on
                else [ state ]
              in
              let state' = State.set_store state old_store in
              let state'' = update_store state' x v_ret in
              ConfCont
                (state'', cs', iframes, prev', start_loop_ids, j, b_counter)
          | _ -> raise (Failure "Malformed callstack")
        in
        L.verbose (fun m -> m "Returning.");
        result
    (* Error-mode return *)
    | ReturnError -> (
        let v_ret = Store.get store Names.return_variable in
        match (v_ret, cs) with
        | None, _ ->
            raise (Failure "Return variable not in store (error return) ")
        | Some v_ret, { store = None; loop_ids = start_loop_ids; _ } :: _ ->
            check_loop_ids loop_ids start_loop_ids;
            Fmt.pr "e @?";
            [ ConfFinish (Error, v_ret, state) ]
        | ( Some v_ret,
            {
              store = Some old_store;
              loop_ids = start_loop_ids;
              ret_var = x;
              call_index = prev';
              error_index = Some j;
              _;
            }
            :: cs' ) ->
            let to_frame_on =
              loop_ids_to_frame_on_at_the_end loop_ids start_loop_ids
            in
            let ( let+ ) x f = List.map f x in
            let+ state =
              if ExecMode.verification_exec !Config.current_exec_mode then
                State.frame_on state iframes to_frame_on
              else [ state ]
            in
            let state' = State.set_store state old_store in
            let state'' = update_store state' x v_ret in
            ConfCont (state'', cs', iframes, prev', start_loop_ids, j, b_counter)
        | _ -> raise (Failure "Malformed callstack"))
    (* Explicit failure *)
    | Fail (fname, exprs) ->
        let message =
          Fmt.(str "Fail : %s%a" fname (parens (list ~sep:comma Expr.pp)) exprs)
        in
        raise (Failure message)

  let simplify state =
    snd (State.simplify ~save:true ~kill_new_lvars:true state)

  let protected_evaluate_cmd
      (prog : UP.prog)
      (state : State.t)
      (cs : CallStack.t)
      (iframes : invariant_frames)
      (prev : int)
      (prev_loop_ids : string list)
      (i : int)
      (b_counter : int) : cconf_t list =
    let states =
      match get_cmd prog cs i with
      | _, (_, LAction _) -> simplify state
      | _                 -> [ state ]
    in
    List.concat_map
      (fun state ->
        try
          evaluate_cmd prog state cs iframes prev prev_loop_ids i b_counter
        with
        | Interpreter_error (errs, state) -> [ ConfErr (cs, i, state, errs) ]
        | State.Internal_State_Error (errs, state) ->
            (* Return: current procedure name, current command index, the state, and the associated errors *)
            [ ConfErr (cs, i, state, List.map (fun x -> ExecErr.ESt x) errs) ])
      states

  (**
  Evaluates one step of a program

  @param ret_fun Function to transform the results
  @param prog GIL program
  @param name Identifier of the procedure to be evaluated
  @param params Parameters of the procedure to be evaluated
  @state state Current state
  @preds preds Current predicate set
  @return Continuation function specifying the next step of evaluation
  *)
  let rec evaluate_cmd_step
      (ret_fun : result_t -> 'a)
      (retry : bool)
      (prog : UP.prog)
      (hold_results : 'a list)
      (on_hold : (cconf_t * string) list)
      (confs : cconf_t list)
      (results : result_t list) : 'a cont_func =
    let f = evaluate_cmd_step ret_fun retry prog hold_results on_hold in

    let continue_or_pause rest_confs cont_func =
      match rest_confs with
      | ConfCont (state, call_stack, _, _, _, proc_body_index, _) :: _ ->
          let report_id =
            L.normal_specific
              (L.Loggable.make cmd_step_pp cmd_step_of_yojson cmd_step_to_yojson
                 {
                   call_stack;
                   proc_body_index;
                   state = Some state;
                   errors = [];
                 })
              L.LoggingConstants.ContentType.cmd_step
          in
          Continue (report_id, fun () -> L.with_normal_phase cont_func)
      | ConfErr (call_stack, proc_body_index, state, errors) :: _ ->
          let report_id =
            L.normal_specific
              (L.Loggable.make cmd_step_pp cmd_step_of_yojson cmd_step_to_yojson
                 { call_stack; proc_body_index; state = Some state; errors })
              L.LoggingConstants.ContentType.cmd_step
          in
          Continue (report_id, fun () -> L.with_normal_phase cont_func)
      | _ -> cont_func ()
    in

    match confs with
    | [] ->
        let results = List.map ret_fun results in
        let results = hold_results @ results in
        if not retry then Finished results
        else (
          L.(verbose (fun m -> m "Relaunching suspended confs"));
          let hold_confs =
            List.filter_map
              (fun (conf, pid) ->
                if Hashtbl.mem prog.specs pid then Some conf else None)
              on_hold
          in
          continue_or_pause hold_confs (fun () ->
              evaluate_cmd_step ret_fun false prog results [] hold_confs []))
    | ConfCont (state, cs, iframes, prev, prev_loop_ids, i, b_counter)
      :: rest_confs
      when b_counter < max_branching ->
        let next_confs =
          protected_evaluate_cmd prog state cs iframes prev prev_loop_ids i
            b_counter
        in
        continue_or_pause (next_confs @ rest_confs) (fun () ->
            f (next_confs @ rest_confs) results)
    | ConfCont (state, cs, _, _, _, i, b_counter) :: rest_confs ->
        let _, annot_cmd = get_cmd prog cs i in
        Printf.printf "WARNING: MAX BRANCHING STOP: %d.\n" b_counter;
        L.(
          verbose (fun m ->
              m
                "Stopping Symbolic Execution due to MAX BRANCHING with %d. \
                 STOPPING CONF:\n"
                b_counter));
        print_configuration annot_cmd state cs i b_counter;
        continue_or_pause rest_confs (fun () -> f rest_confs results)
    | ConfErr (cs, i, state, errs) :: rest_confs ->
        let proc = CallStack.get_cur_proc_id cs in
        let result = ExecRes.RFail (proc, i, state, errs) in
        continue_or_pause rest_confs (fun () ->
            f rest_confs (result :: results))
    | ConfFinish (fl, v, state) :: rest_confs ->
        let result = ExecRes.RSucc (fl, v, state) in
        continue_or_pause rest_confs (fun () ->
            f rest_confs (result :: results))
    | ConfSusp (fid, state, cs, iframes, prev, prev_loop_ids, i, b_counter)
      :: rest_confs
      when retry ->
        let conf =
          ConfCont (state, cs, iframes, prev, prev_loop_ids, i, b_counter)
        in
        L.(
          verbose (fun m ->
              m "Suspending a computation that was trying to call %s" fid));
        continue_or_pause rest_confs (fun () ->
            evaluate_cmd_step ret_fun retry prog hold_results
              ((conf, fid) :: on_hold) rest_confs results)
    | _ :: rest_confs ->
        continue_or_pause rest_confs (fun () -> f rest_confs results)

  (**
  Evaluates commands iteratively

  @param init_func The initial continuation function which evaluates the first
                   step of the program
*)
  let rec evaluate_cmd_iter (init_func : 'a cont_func) : 'a list =
    match init_func with
    | Finished results        -> results
    | Continue (_, cont_func) -> evaluate_cmd_iter (cont_func ())

  (**
  Sets the initial values for evaluating a program, and returns a continuation
  function which evaluates the first step of the program

  @param ret_fun Function to transform the results
  @param prog GIL program
  @param name Identifier of the procedure to be evaluated
  @param params Parameters of the procedure to be evaluated
  @state state Current state
  @preds preds Current predicate set
  @return Continuation function which evaluates the first step of the program
  *)
  let init_evaluate_proc
      (ret_fun : result_t -> 'a)
      (prog : UP.prog)
      (name : string)
      (params : string list)
      (state : State.t) : 'a cont_func =
    let () = CallGraph.add_proc call_graph name in
    L.normal (fun m ->
        m
          ("*******************************************@\n"
         ^^ "*** Executing procedure: %s@\n"
         ^^ "*******************************************@\n")
          name);

    let store = State.get_store state in
    let arguments =
      List.map
        (fun x ->
          match Store.get store x with
          | Some v_x -> v_x
          | None     ->
              raise (Failure "Symbolic State does NOT contain formal parameter"))
        params
    in
    let cs : CallStack.t =
      CallStack.push CallStack.empty ~pid:name ~arguments ~loop_ids:[]
        ~ret_var:"out" ~call_index:(-1) ~continue_index:(-1) ~error_index:(-1)
        ()
    in
    let proc_body_index = 0 in
    let conf : cconf_t = ConfCont (state, cs, [], -1, [], proc_body_index, 0) in
    let report_id =
      L.normal_specific
        (L.Loggable.make cmd_step_pp cmd_step_of_yojson cmd_step_to_yojson
           { call_stack = cs; proc_body_index; state = Some state; errors = [] })
        L.LoggingConstants.ContentType.cmd_step
    in
    Continue
      ( report_id,
        fun () -> evaluate_cmd_step ret_fun true prog [] [] [ conf ] [] )

  (**
  Evaluation of procedures

  @param prog GIL program
  @param name Identifier of the procedure to be evaluated
  @param params Parameters of the procedure to be evaluated
  @state state Current state
  @preds preds Current predicate set
  @return List of final configurations
*)
  let evaluate_proc
      (ret_fun : result_t -> 'a)
      (prog : UP.prog)
      (name : string)
      (params : string list)
      (state : State.t) : 'a list =
    let init_func = init_evaluate_proc ret_fun prog name params state in
    evaluate_cmd_iter init_func

  (**
  Evaluation of programs

  @param prog Target GIL program
  @return Final configurations
*)
  let evaluate_prog (prog : UP.prog) : result_t list =
    Random.self_init ();
    let ret_fun x = x in
    let initial_cs =
      CallStack.push CallStack.empty ~pid:"main" ~arguments:[] ~loop_ids:[]
        ~ret_var:"out" ~call_index:(-1) ~continue_index:(-1) ~error_index:(-1)
        ()
    in
    let initial_proc_body_index = 0 in
    let initial_state = State.init (Some prog.preds) in
    let initial_conf =
      ConfCont
        (initial_state, initial_cs, [], -1, [], initial_proc_body_index, 0)
    in
    let report_id =
      L.normal_specific
        (L.Loggable.make cmd_step_pp cmd_step_of_yojson cmd_step_to_yojson
           {
             call_stack = initial_cs;
             proc_body_index = initial_proc_body_index;
             state = Some initial_state;
             errors = [];
           })
        L.LoggingConstants.ContentType.cmd_step
    in
    let init_func =
      Continue
        ( report_id,
          fun () ->
            evaluate_cmd_step ret_fun true prog [] [] [ initial_conf ] [] )
    in
    evaluate_cmd_iter init_func

  (** Configuration pretty-printer *)
  let pp_result (ft : Format.formatter) (reslt : result_t list) : unit =
    let open Fmt in
    let pp_one ft (i, res) = pf ft "RESULT: %d.@\n%a" i pp_single_result res in
    (iter_bindings List.iteri pp_one) ft reslt
end
