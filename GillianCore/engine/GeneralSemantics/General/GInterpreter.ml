open Literal
module L = Logging

(** General GIL Interpreter *)
module Make
    (Val : Val.S)
    (Subst : Subst.S with type vt = Val.t and type t = Val.st)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = Subst.t
                and type store_t = Store.t)
    (External : External.S) =
struct
  (* *************** *
   * Auxiliary Types *
   * *************** *)

  module CallStack = CallStack.Make (Val) (Store)
  module External = External (Val) (Subst) (Store) (State) (CallStack)

  type state = State.t

  type err_t = (Val.t, State.err_t) ExecErr.t

  let pp_err = ExecErr.pp Val.pp State.pp_err

  (** Type of configurations: state, optional predicate set, call stack, previous index, current index *)
  type cconf_t =
    | ConfErr    of string * int * State.t * err_t list
    | ConfCont   of State.t * CallStack.t * int * int * int
    | ConfFinish of Flag.t * State.vt * State.t
        (** Equal to Conf cont + the id of the required spec *)
    | ConfSusp   of string * State.t * CallStack.t * int * int * int

  type conf_t = BConfErr of err_t list | BConfCont of State.t

  type result_t = (State.t, State.vt, err_t) ExecRes.t

  let max_branching = 100

  exception Interpreter_error of err_t list * State.t

  (** Internal error, carrying a string description *)
  exception Internal_error of string

  (** Syntax error, carrying a string description *)
  exception Syntax_error of string

  let pp_single_result ft res = ExecRes.pp State.pp Val.pp pp_err ft res

  let call_graph = CallGraph.make ()

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
    let _ = Store.put store x v in
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
    L.normal (fun m ->
        m
          "@[------------------------------------------------------@\n\
           --%s: %i--@\n\
           TIME: %f@\n\
           CMD: %a@\n\
           BRANCHING: %d@\n\
           @\n\
           %a@\n\
           ------------------------------------------------------@]\n"
          (CallStack.get_cur_proc_id cs)
          i (Sys.time ()) Cmd.pp_indexed cmd b_counter State.pp state)

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

  let get_proc_path_opt (prog : UP.prog) proc_name =
    let proc = Option.get (Prog.get_proc prog.prog proc_name) in
    proc.proc_source_path

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
    let eval_expr = make_eval_expr state in

    print_lconfiguration lcmd state;

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
                        (Type.str t) x)) )
        | _        ->
            raise
              (Failure
                 (Printf.sprintf
                    "ERROR: AssumeType: Variable %s cannot be turned into a \
                     value."
                    x)) )
    | Assume f ->
        let store_subst = Store.to_ssubst (State.get_store state) in
        let f' = SVal.SSubst.substitute_formula store_subst ~partial:true f in
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
               | _           ->
                   (* Printf.printf "WARNING: ASSUMING FALSE\n"; *)
                   [])
             fos)
    (*
        (match State.assume_a state [ f' ] with
          | Some state' -> [ state' ]
          | _ -> (* Printf.printf "WARNING: ASSUMING FALSE\n"; *) []) *)
    | SpecVar xs -> [ State.add_spec_vars state (Var.Set.of_list xs) ]
    | Assert f -> (
        let store_subst = Store.to_ssubst (State.get_store state) in
        let f' = SVal.SSubst.substitute_formula store_subst ~partial:true f in
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
                Fmt.(option ~none:(any "CANNOT CREATE MODEL") Subst.pp)
                failing_model
            in
            if not (ExecMode.biabduction_exec !Config.current_exec_mode) then
              Printf.printf "%s" msg;
            L.normal (fun m -> m "%s" msg);
            raise (Interpreter_error ([ ESt err ], state)) )
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
            let rec expand_macro (macro : Macro.t) (args : Expr.t list) :
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
            evaluate_lcmds prog lcmds state )
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
                 "Non-boolean expression in the condition of the logical if") )
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
    | SL sl_cmd -> State.evaluate_slcmd prog sl_cmd state

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
  let evaluate_cmd
      (prog : UP.prog)
      (state : State.t)
      (cs : CallStack.t)
      (prev : int)
      (i : int)
      (b_counter : int) : cconf_t list =
    let store = State.get_store state in
    let eval_expr = make_eval_expr state in
    let proc_name, annot_cmd = get_cmd prog cs i in
    let _, cmd = annot_cmd in

    let vtrue = Val.from_literal (Bool true) in
    let vfalse = Val.from_literal (Bool false) in

    if !Config.stats then Statistics.exec_cmds := !Statistics.exec_cmds + 1;
    let first_time = UP.first_time_running prog proc_name i in
    UP.update_coverage prog proc_name i;
    print_configuration annot_cmd state cs i b_counter;

    let evaluate_procedure_call x pid v_args j subst =
      let pid =
        match Val.to_literal pid with
        | Some (String pid) -> pid
        | Some other_thing  ->
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
      let caller_name = CallStack.get_cur_proc_id cs in
      let caller_path = get_proc_path_opt prog caller_name in
      let callee_path = get_proc_path_opt prog pid in
      let () =
        if Option.is_some caller_path && Option.is_some callee_path then
          let caller_id = CallGraph.id_of_proc_name caller_name in
          let callee_id = CallGraph.id_of_proc_name pid in
          CallGraph.add_edge call_graph caller_id caller_name callee_id pid
      in
      let prmlen = List.length params in

      let args = Array.make prmlen (Val.from_literal Undefined) in
      let _ =
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

        ConfCont (ret_state, new_cs, i, new_j, b_counter)
      in

      match spec with
      | Some spec -> (
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
          | _ -> [] )
      | _         ->
          if Hashtbl.mem prog.prog.bi_specs pid then
            [ ConfSusp (pid, state, cs, prev, i, b_counter) ]
          else
            let new_store = Store.init (List.combine params args) in
            let old_store = State.get_store state in
            let state' = State.set_store state new_store in
            let cs' = (pid, v_args, Some old_store, x, i, i + 1, j) :: cs in
            [ ConfCont (state', cs', -1, 0, b_counter) ]
    in

    match cmd with
    | Skip -> [ ConfCont (state, cs, i, i + 1, b_counter) ]
    | Assignment (x, e) ->
        let v = eval_expr e in
        let state' = update_store state x v in
        [ ConfCont (state', cs, i, i + 1, b_counter) ]
    | LAction (x, a, es) -> (
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
                  ConfCont (r_state', CallStack.copy cs, i, i + 1, b_counter))
                rest_rets
            in
            let ret_len = 1 + List.length rest_rets in
            let b_counter = b_counter + if ret_len > 1 then 1 else 0 in
            match
              ( ret_len >= 3 && !Config.parallel && !Config.multi_thread,
                ret_len = 2 && !Config.parallel && !Config.multi_thread )
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
                    | n -> [ List.hd rest_confs ] )
                | n -> [ ConfCont (state'', cs, i, i + 1, b_counter) ] )
            | false, true -> (
                (* Can split into two threads *)
                let b_counter = b_counter + 1 in
                (* print_endline (Printf.sprintf "Action returned 2: %d" (!Config.act_threads + 1)); *)
                let pid = Unix.fork () in
                match pid with
                | 0 -> [ ConfCont (state'', cs, i, i + 1, b_counter) ]
                | n -> rest_confs )
            | _           -> ConfCont (state'', cs, i, i + 1, b_counter)
                             :: rest_confs )
        (* FIXME: It feels like there should be an auto-unfold here if we are in an abstract execution *)
        | AFail errs ->
            if not (ExecMode.concrete_exec !Config.current_exec_mode) then
              let recovery_vals = State.get_recovery_vals errs in
              let recovery_states : (State.t list, string) result =
                State.automatic_unfold state recovery_vals
              in
              match recovery_states with
              | Ok recovery_states ->
                  List.map
                    (fun state -> ConfCont (state, cs, prev, i, b_counter))
                    recovery_states
              | _                  ->
                  raise
                    (Fmt.failwith "Local Action Failed: %a" Cmd.pp_indexed cmd)
            else Fmt.failwith "Local Action Failed: %a" Cmd.pp_indexed cmd )
    | Logic lcmd -> (
        let resulting_states : State.t list = evaluate_lcmd prog lcmd state in
        match lcmd with
        | SL (Invariant _) when not first_time -> []
        | _ ->
            let b_counter =
              if List.length resulting_states > 1 then b_counter + 1
              else b_counter
            in
            List.map
              (fun state -> ConfCont (state, cs, i, i + 1, b_counter))
              resulting_states )
    | Goto j -> [ ConfCont (state, cs, i, j, b_counter) ]
    (* When executing the guarded goto, we copy only when needed and parallelise *)
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
          | _                 -> ( State.sat_check state vt,
                                   State.sat_check state vf )
        in
        let sp_t, sp_f =
          match (can_put_t, can_put_f) with
          | false, false -> ([], [])
          | true, false  ->
              (List.map (fun x -> (x, j)) (State.assume state vt), [])
          | false, true  ->
              ([], List.map (fun x -> (x, k)) (State.assume state vf))
          | true, true   ->
              let state' = State.copy state in
              ( List.map (fun x -> (x, j)) (State.assume ~unfold:true state vt),
                List.map (fun x -> (x, k)) (State.assume ~unfold:true state' vf)
              )
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
                  i,
                  next,
                  b_counter ))
            sp
        in
        match
          List.length result = 2 && !Config.parallel && !Config.multi_thread
          (* XXX: && !Config.act_threads < !Config.max_threads *)
        with
        | true  -> (
            (* print_endline (Printf.sprintf "Conditional goto: %d" (!Config.act_threads + 1)); *)
            let pid = Unix.fork () in
            match pid with
            | 0 -> [ List.hd result ]
            | n -> List.tl result )
        | false -> result )
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
        [ ConfCont (state', cs, i, i + 1, b_counter) ]
    | Call (x, e, args, j, subst) ->
        let pid = eval_expr e in
        let v_args = List.map eval_expr args in
        let result = evaluate_procedure_call x pid v_args j subst in
        result
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
          (fun (state, cs, i, j) -> ConfCont (state, cs, i, j, b_counter))
          (External.execute prog.prog state cs i x pid v_args j)
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
                    v_pid_args)) )
    | Arguments x ->
        let args = CallStack.get_cur_args cs in
        let args = Val.from_list args in
        let state' = update_store state x args in
        [ ConfCont (state', cs, i, i + 1, b_counter) ]
    | ReturnNormal ->
        let v_ret = Store.get store Names.return_variable in
        let result =
          match (v_ret, cs) with
          | None, _ -> raise (Failure "nm_ret_var not in store (normal return)")
          | Some v_ret, (_, _, None, _, _, _, _) :: _ ->
              [ ConfFinish (Normal, v_ret, state) ]
          | Some v_ret, (_, _, Some old_store, x, prev', j, _) :: cs' ->
              let state' = State.set_store state old_store in
              let state'' = update_store state' x v_ret in
              [ ConfCont (state'', cs', prev', j, b_counter) ]
          | _ -> raise (Failure "Malformed callstack")
        in
        L.verbose (fun m -> m "Returning.");
        result
    | ReturnError -> (
        let v_ret = Store.get store Names.return_variable in
        match (v_ret, cs) with
        | None, _ ->
            raise (Failure "Return variable not in store (error return) ")
        | Some v_ret, (_, _, None, _, _, _, _) :: _ ->
            [ ConfFinish (Error, v_ret, state) ]
        | Some v_ret, (pid, _, Some old_store, x, prev', _, Some j) :: cs' ->
            let state' = State.set_store state old_store in
            let state'' = update_store state' x v_ret in
            [ ConfCont (state'', cs', prev', j, b_counter) ]
        | _ -> raise (Failure "Malformed callstack") )
    | Fail (fname, exprs) ->
        let message =
          Fmt.(str "Fail : %s(%a)" fname (list ~sep:comma Expr.pp) exprs)
        in
        raise (Failure message)

  let protected_evaluate_cmd
      (prog : UP.prog)
      (state : State.t)
      (cs : CallStack.t)
      (prev : int)
      (i : int)
      (b_counter : int) : cconf_t list =
    try evaluate_cmd prog state cs prev i b_counter with
    | Interpreter_error (errs, state) ->
        let proc = CallStack.get_cur_proc_id cs in
        [ ConfErr (proc, i, state, errs) ]
    | State.Internal_State_Error (errs, state) ->
        (* Return: current procedure name, current command index, the state, and the associated errors *)
        let proc = CallStack.get_cur_proc_id cs in
        [ ConfErr (proc, i, state, List.map (fun x -> ExecErr.ESt x) errs) ]

  (**
  Iterative evaluation of commands

  @param prog GIL program
  @param confs Current configurations
  @param results Current evaluation outcomes
  @return List of final configurations
*)
  let rec evaluate_cmd_iter
      (ret_fun : result_t -> 'a)
      (retry : bool)
      (prog : UP.prog)
      (hold_results : 'a list)
      (on_hold : (cconf_t * string) list)
      (confs : cconf_t list)
      (results : result_t list) : 'a list =
    let f = evaluate_cmd_iter ret_fun retry prog hold_results on_hold in

    match confs with
    | [] ->
        let results = List.map ret_fun results in
        let results = hold_results @ results in
        if not retry then results
        else (
          L.(verbose (fun m -> m "Relaunching suspended confs"));
          let hold_confs =
            List.filter (fun (_, pid) -> Hashtbl.mem prog.specs pid) on_hold
          in
          let hold_confs = List.map (fun (conf, _) -> conf) hold_confs in
          evaluate_cmd_iter ret_fun false prog results [] hold_confs [] )
    | ConfCont (state, cs, prev, i, b_counter) :: rest_confs
      when b_counter < max_branching ->
        let next_confs =
          protected_evaluate_cmd prog state cs prev i b_counter
        in
        f (next_confs @ rest_confs) results
    | ConfCont (state, cs, prev, i, b_counter) :: rest_confs ->
        let _, annot_cmd = get_cmd prog cs i in
        Printf.printf "WARNING: MAX BRANCHING STOP: %d.\n" b_counter;
        L.(
          verbose (fun m ->
              m
                "Stopping Symbolic Execution due to MAX BRANCHING with %d. \
                 STOPPING CONF:\n"
                b_counter));
        print_configuration annot_cmd state cs i b_counter;
        f rest_confs results
    | ConfErr (proc, i, state, errs) :: rest_confs ->
        let result = ExecRes.RFail (proc, i, state, errs) in
        f rest_confs (result :: results)
    | ConfFinish (fl, v, state) :: rest_confs ->
        let result = ExecRes.RSucc (fl, v, state) in
        f rest_confs (result :: results)
    | ConfSusp (fid, state, cs, prev, i, b_counter) :: rest_confs when retry ->
        let conf = ConfCont (state, cs, prev, i, b_counter) in
        L.(
          verbose (fun m ->
              m "Suspending a computation that was trying to call %s" fid));
        evaluate_cmd_iter ret_fun retry prog hold_results
          ((conf, fid) :: on_hold) rest_confs results
    | _ :: rest_confs -> f rest_confs results

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
    L.normal (fun m ->
        m
          ( "*******************************************@\n"
          ^^ "*** Executing procedure: %s@\n"
          ^^ "*******************************************@\n" )
          name);

    let store = State.get_store state in
    let args =
      List.map
        (fun x ->
          match Store.get store x with
          | Some v_x -> v_x
          | None     ->
              raise (Failure "Symbolic State does NOT contain formal parameter"))
        params
    in

    let cs : CallStack.t = [ (name, args, None, "out", -1, -1, Some (-1)) ] in
    let conf : cconf_t = ConfCont (state, cs, -1, 0, 0) in
    evaluate_cmd_iter ret_fun true prog [] [] [ conf ] []

  (**
  Evaluation of programs

  @param prog Target GIL program
  @return Final configurations
*)
  let evaluate_prog (prog : UP.prog) : result_t list =
    Random.self_init ();
    let ret_fun x = x in
    let initial_cs = [ ("main", [], None, "out", -1, -1, Some (-1)) ] in
    let initial_conf =
      ConfCont (State.init (Some prog.preds), initial_cs, -1, 0, 0)
    in
    evaluate_cmd_iter ret_fun true prog [] [] [ initial_conf ] []

  (**
  Configuration pretty-printer
*)
  let pp_result (ft : Format.formatter) (reslt : result_t list) : unit =
    let open Fmt in
    let pp_one ft (i, res) = pf ft "RESULT: %d.@\n%a" i pp_single_result res in
    (iter_bindings List.iteri pp_one) ft reslt
end
