open Literal
open Branch_case
open Syntaxes.Result
module L = Logging
module DL = Debugger_log
include G_interpreter_intf

type branch_case = Branch_case.t [@@deriving yojson]

(** General GIL Interpreter *)
module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (PC : ParserAndCompiler.S)
    (External : External.T(PC.Annot).S) =
struct
  (* *************** *
   * Auxiliary Types *
   * *************** *)

  module Call_stack = Call_stack.Make (Val) (Store)
  module External = External (Val) (ESubst) (Store) (State) (Call_stack)
  module Val = Val
  module State = State
  module Store = Store
  module Annot = PC.Annot

  type vt = Val.t
  type st = ESubst.t
  type store_t = Store.t
  type state_t = State.t [@@deriving yojson, show]
  type state_err_t = State.err_t [@@deriving yojson]
  type init_data = State.init_data
  type annot = Annot.t [@@deriving yojson]

  let pp_state_err_t = State.pp_err
  let show_state_err_t = Fmt.to_to_string pp_state_err_t

  type state_vt = State.vt [@@deriving yojson, show]
  type heap_t = State.heap_t
  type invariant_frames = (string * State.t) list [@@deriving yojson]
  type err_t = (Val.t, state_err_t) Exec_err.t [@@deriving show, yojson]
  type branch_path = branch_case list [@@deriving yojson]

  (** Type of configurations: state, call stack, previous index, previous loop ids, current index, branching *)
  module CConf = struct
    type err = {
      callstack : Call_stack.t;
      proc_idx : int;
      error_state : state_t;
      errors : err_t list;
      branch_path : branch_path;
      prev_cmd_report_id : Logging.Report_id.t option;
      loc : Location.t option;
    }
    [@@deriving yojson]

    type cont = {
      state : state_t;
      callstack : Call_stack.t;
      invariant_frames : invariant_frames;
      prev_idx : int;
      next_idx : int;
      loop_ids : string list;
      branch_count : int;
      prev_cmd_report_id : Logging.Report_id.t option;
      branch_case : branch_case option;
      branch_path : branch_path;
      loc : Location.t option;
    }
    [@@deriving yojson]

    (** Equal to conf_cont + the id of the required spec *)
    type finish = {
      flag : Flag.t;
      ret_val : state_vt;
      final_state : state_t;
      branch_path : branch_path;
      prev_cmd_report_id : Logging.Report_id.t option;
      loc : Location.t option;
    }
    [@@deriving yojson]

    type susp = {
      spec_id : string;
      state : state_t;
      callstack : Call_stack.t;
      invariant_frames : invariant_frames;
      prev_idx : int;
      next_idx : int;
      loop_ids : string list;
      branch_count : int;
      branch_path : branch_path;
      prev_cmd_report_id : Logging.Report_id.t option;
      loc : Location.t option;
    }
    [@@deriving yojson]

    type t =
      | ConfErr of err
      | ConfCont of cont
      | ConfFinish of finish
      | ConfSusp of susp
    [@@deriving yojson]

    let pp_short fmt = function
      | ConfErr _ -> Fmt.pf fmt "Err"
      | ConfCont _ -> Fmt.pf fmt "Cont"
      | ConfFinish _ -> Fmt.pf fmt "Finish"
      | ConfSusp _ -> Fmt.pf fmt "Susp"

    let make_cont
        ~state
        ~callstack
        ~invariant_frames
        ~prev_idx
        ~next_idx
        ~loop_ids
        ~branch_count
        ~branch_path
        ?loc
        ?prev_cmd_report_id
        ?branch_case
        () =
      (* TODO this needs some optimising; big concrete tests like Test262 use
         way too much memory due to long branch paths.
         For now, just don't think about it when we're not debugging. *)
      let branch_case = if !Config.debug then branch_case else None in
      ConfCont
        {
          state;
          callstack;
          invariant_frames;
          prev_idx;
          next_idx;
          loop_ids;
          branch_count;
          branch_path;
          loc;
          prev_cmd_report_id;
          branch_case;
        }

    let get_prev_cmd_id = function
      | ConfErr { prev_cmd_report_id; _ } -> prev_cmd_report_id
      | ConfFinish { prev_cmd_report_id; _ } -> prev_cmd_report_id
      | ConfSusp { prev_cmd_report_id; _ } -> prev_cmd_report_id
      | ConfCont { prev_cmd_report_id; _ } -> prev_cmd_report_id

    let get_branch_case = function
      | ConfCont { branch_case; _ } -> branch_case
      | _ -> None

    let get_branch_path = function
      | ConfErr { branch_path; _ }
      | ConfFinish { branch_path; _ }
      | ConfSusp { branch_path; _ } -> branch_path
      | ConfCont { branch_path; branch_case; _ } ->
          List_utils.cons_opt branch_case branch_path
  end

  type conf_t = BConfErr of err_t list | BConfCont of State.t
  type result_t = (State.t, State.vt, err_t) Exec_res.t [@@deriving yojson]

  type conf_selector =
    | Path of Branch_case.path
    | IdCase of L.Report_id.t * Branch_case.t option
  [@@deriving to_yojson, show]

  type 'result cont_func_f =
    ?selector:conf_selector -> unit -> 'result cont_func

  and 'result cont_func =
    | Finished of 'result list
    | Continue of {
        report_id : Logging.Report_id.t option;
        branch_path : Branch_case.path;
        new_branch_cases : Branch_case.t list;
        cont_func : 'result cont_func_f;
      }
    | EndOfBranch of 'result * 'result cont_func_f

  module Logging = struct
    let pp_str_list = Fmt.(brackets (list ~sep:comma string))

    module ConfigReport = struct
      type t = {
        proc_line : int;
        time : float;
        cmd : int Cmd.t;
        callstack : Call_stack.t;
        annot : annot;
        branching : int;
        state : state_t;
        branch_case : branch_case option;
        proc_name : string;
      }
      [@@deriving yojson, make]

      let pp
          state_printer
          fmt
          {
            proc_line = i;
            time;
            cmd;
            callstack = cs;
            annot;
            branching;
            state;
            _;
          } =
        Fmt.pf fmt
          "@[------------------------------------------------------@\n\
           --%s: %i--@\n\
           TIME: %f@\n\
           CMD: %a@\n\
           LOC: %a@\n\
           PROCS: %a@\n\
           LOOPS: %a ++ %a@\n\
           BRANCHING: %d@\n\
           @\n\
           %a@\n\
           ------------------------------------------------------@]\n"
          (Call_stack.get_cur_proc_id cs)
          i time Cmd.pp_indexed cmd Location.pp_log_opt
          (Annot.get_origin_loc annot)
          pp_str_list
          (Call_stack.get_cur_procs cs)
          pp_str_list
          (Annot.get_loop_info annot)
          pp_str_list
          (Call_stack.get_loop_ids cs)
          branching state_printer state

      let to_loggable state_printer =
        L.Loggable.make (pp state_printer) of_yojson to_yojson

      let log state_printer report =
        L.Specific.normal
          (to_loggable state_printer report)
          L.Logging_constants.Content_type.cmd
    end

    module CmdResult = struct
      type t = {
        callstack : Call_stack.t;
        proc_body_index : int;
        state : state_t option;
        errors : err_t list;
        branch_case : branch_case option;
      }
      [@@deriving yojson]

      let pp fmt cmd_step =
        (* TODO: Cmd step should contain all things in a configuration
                 print the same contents as log_configuration *)
        Call_stack.pp fmt cmd_step.callstack

      let to_loggable = L.Loggable.make pp of_yojson to_yojson
      let log type_ report = L.Specific.normal (to_loggable report) type_

      open L.Logging_constants.Content_type

      let log_result = log cmd_result
      let log_init = log proc_init
    end

    module AnnotatedAction = struct
      type t = { annot : Annot.t; action_name : string } [@@deriving yojson]

      let pp fmt annotated_action =
        let origin_loc = Annot.get_origin_loc annotated_action.annot in
        Fmt.pf fmt "Executing action '%s' at %a" annotated_action.action_name
          (Fmt.option ~none:(Fmt.any "none") Location.pp)
          origin_loc

      let to_loggable = L.Loggable.make pp of_yojson to_yojson

      let log report =
        L.Specific.normal (to_loggable report)
          L.Logging_constants.Content_type.annotated_action
    end

    let log_configuration
        (cmd : Annot.t * int Cmd.t)
        (state : State.t)
        (cs : Call_stack.t)
        (i : int)
        (b_counter : int)
        (branch_case : branch_case option)
        (proc_name : string) : L.Report_id.t option =
      let annot, cmd = cmd in
      let state_printer =
        match !Config.pbn with
        | false -> State.pp
        | true ->
            let pvars, lvars, locs =
              (Cmd.pvars cmd, Cmd.lvars cmd, Cmd.locs cmd)
            in
            State.pp_by_need pvars lvars locs
      in
      ConfigReport.log state_printer
        (ConfigReport.make ~proc_name ~proc_line:i ~time:(Sys.time ()) ~cmd
           ~callstack:cs ~annot ~branching:b_counter ~state ?branch_case ())

    let print_lconfiguration
        (lcmd : LCmd.t)
        (annot : annot option)
        (state : State.t) : unit =
      let loc = Option.bind annot Annot.get_origin_loc in
      L.normal (fun m ->
          m
            "@[------------------------------------------------------@\n\
             TIME: %f@\n\
             LCMD: %a@\n\
             LOC: %a@\n\
             @\n\
             %a@\n\
             ------------------------------------------------------@]@\n"
            (Sys.time ()) LCmd.pp lcmd Location.pp_log_opt loc State.pp state)

    let pp_err = Exec_err.pp Val.pp State.pp_err
    let pp_single_result ft res = Exec_res.pp State.pp Val.pp pp_err ft res

    (** Configuration pretty-printer *)
    let pp_result (ft : Format.formatter) (reslt : result_t list) : unit =
      let open Fmt in
      let pp_one ft (i, res) =
        pf ft "RESULT: %d.@\n%a" i pp_single_result res
      in
      (iter_bindings List.iteri pp_one) ft reslt
  end

  open Logging

  exception Interpreter_error of err_t list * State.t

  let () =
    Printexc.register_printer (function
      | Interpreter_error (errs, state) ->
          let msg =
            Fmt.str
              "Interpreter error!\n\n=== Errors ===\n%a\n\n=== State ===\n%a"
              (Fmt.list ~sep:(Fmt.any "\n") pp_err_t)
              errs pp_state_t state
          in
          Some msg
      | _ -> None)

  (** Internal error, carrying a string description *)
  exception Internal_error of string

  (** Syntax error, carrying a string description *)
  exception Syntax_error of string

  let call_graph = Call_graph.make ~init_capacity:128 ()
  let reset_call_graph () = Call_graph.reset call_graph

  (* Often-used values *)
  let vtrue = Val.from_literal (Bool true)
  let vfalse = Val.from_literal (Bool false)
  let symb_exec_next = ref false

  type loop_action =
    | Nothing
    | FrameOff of string
    | FrameOn of string list
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
            let rEState =
              Option.get (List_utils.list_sub previous (-n) (len_prev + n))
            in
            if rEState <> current then Malformed else FrameOn ids)

  (* ******************* *
   * Auxiliary Functions *
   * ******************* *)

  let get_cmd (prog : annot MP.prog) (cs : Call_stack.t) (i : int) :
      string * (Annot.t * int Cmd.t) =
    let pid = Call_stack.get_cur_proc_id cs in
    let proc = Prog.get_proc prog.prog pid in
    let proc =
      match proc with
      | Some proc -> proc
      | None -> raise (Failure ("Procedure " ^ pid ^ " does not exist."))
    in
    let annot, _, cmd = proc.proc_body.(i) in
    (pid, (annot, cmd))

  let get_predecessor
      (prog : annot MP.prog)
      (cs : Call_stack.t)
      (prev : int)
      (i : int) : int =
    let pid = Call_stack.get_cur_proc_id cs in
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
      (state : State.t)
      (subst_lst : (string * (string * Expr.t) list) option) :
      (string * (string * Val.t) list) option =
    match subst_lst with
    | None -> None
    | Some (lab, subst_lst) ->
        let subst_lst' : (string * Val.t) list =
          List.map (fun (x, e) -> (x, State.eval_expr state e)) subst_lst
        in
        Some (lab, subst_lst')

  let make_eval_expr (state : State.t) : Expr.t -> Val.t =
   fun e ->
    try State.eval_expr state e
    with State.Internal_State_Error (errs, s) ->
      raise (Interpreter_error (List.map (fun x -> Exec_err.EState x) errs, s))

  let check_loop_ids actual expected =
    match actual = expected with
    | false ->
        Fmt.failwith
          "Malformed loop structure: current loops: %a; expected loops: %a"
          pp_str_list actual pp_str_list expected
    | true -> ()

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
  module Evaluate_lcmd = struct
    let eval_assumetype e t state eval_expr =
      let v_x = eval_expr e in
      match State.assume_t state v_x t with
      | Some state' -> [ Ok state' ]
      | _ ->
          L.normal (fun m ->
              m "ERROR: AssumeType: Cannot assume type %s for expression %a."
                (Type.str t) Expr.pp e);
          []

    let eval_assume f state =
      let store_subst = Store.to_ssubst (State.get_store state) in
      let f' = SVal.SESubst.subst_in_expr store_subst ~partial:true f in
      (* Printf.printf "Assuming %s\n" (Formula.str f'); *)
      let open Syntaxes.List in
      let* f'', state =
        (* Sacha: I don't know why something different is happening in bi-exec *)
        if Exec_mode.is_biabduction_exec !Config.current_exec_mode then
          let fos =
            let rec aux = function
              | Expr.BinOp (e1, Or, e2) -> aux e1 @ aux e2
              | e -> [ e ]
            in
            aux f'
          in
          match fos with
          | [] -> []
          | [ f' ] -> [ (f', state) ]
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
      match State.assume_a state [ f'' ] with
      | Some state' -> Res_list.return state'
      | _ -> Res_list.vanish

    let eval_freshsvar x state =
      let new_svar = Generators.fresh_svar () in
      let state' = State.add_spec_vars state (SS.singleton new_svar) in
      let v = Val.from_expr (LVar new_svar) |> Option.get in
      Res_list.return (update_store state' x v)

    let eval_assert f state =
      let store_subst = Store.to_ssubst (State.get_store state) in
      let f' = SVal.SESubst.subst_in_expr store_subst ~partial:true f in
      match State.assert_a state [ f' ] with
      | true -> Res_list.return state
      | false ->
          let err = StateErr.EPure f' in
          let failing_model = State.sat_check_f state [ Expr.Infix.not f' ] in
          let msg =
            Fmt.str
              "Assert failed with argument @[<h>%a@].@\n\
               @[<v 2>Failing Model:@\n\
               %a@]@\n"
              Expr.pp f'
              Fmt.(option ~none:(any "CANNOT CREATE MODEL") ESubst.pp)
              failing_model
          in
          L.normal (fun m -> m "%s" msg);
          Res_list.error_with err

    let eval_branch fof state =
      let state' = State.copy state in
      let left_states =
        match State.assume_a state [ fof ] with
        | Some state -> Res_list.return state
        | None -> Res_list.vanish
      in
      let right_states =
        match State.assume_a state' [ Expr.Infix.not fof ] with
        | Some state -> Res_list.return state
        | None -> Res_list.vanish
      in
      left_states @ right_states

    let rec eval_macro name args lcmd prog annot state =
      let macro =
        match Macro.get MP.(prog.prog.macros) name with
        | Some macro -> macro
        | None ->
            L.verbose (fun m ->
                m "@[<v 2>Current MACRO TABLE:\n%a\n@]" Macro.pp_tbl
                  MP.(prog.prog.macros));
            Fmt.failwith "NO MACRO found when executing: @[<h>%a@]" LCmd.pp lcmd
      in
      let lcmds =
        let params = macro.macro_params in
        let params_card = List.length params in
        let args_card = List.length args in
        if params_card <> args_card then
          raise
            (Failure
               (Printf.sprintf
                  "Macro %s called with incorrect number of parameters: %d \
                   instead of %d."
                  macro.macro_name args_card params_card));
        let subst = SVal.SSubst.init (List.combine params args) in
        let lcmds = macro.macro_definition in
        List.map (SVal.SSubst.substitute_lcmd subst ~partial:true) lcmds
      in
      eval_lcmds prog lcmds ~annot state

    (* We have to understand what is the intended semantics of the logic if *)
    and eval_if e lcmds_t lcmds_e prog annot state eval_expr =
      let ve = eval_expr e in
      let e = Val.to_expr ve in
      match e with
      | Expr.Lit (Bool true) -> eval_lcmds prog lcmds_t ~annot state
      | Expr.Lit (Bool false) -> eval_lcmds prog lcmds_e ~annot state
      | _ ->
          if not @@ Expr.is_boolean_expr e then
            Fmt.failwith
              "Non-boolean expression in the condition of the logical if: %a"
              Expr.pp e;
          let ne = Expr.negate e in
          let state' = State.copy state in
          let then_states =
            match State.assume_a state [ e ] with
            | Some state -> eval_lcmds prog lcmds_t ~annot state
            | None -> Res_list.vanish
          in
          let else_states =
            match State.assume_a state' [ ne ] with
            | Some state -> eval_lcmds prog lcmds_e ~annot state
            | None -> Res_list.vanish
          in
          then_states @ else_states

    and eval_lcmd
        (prog : annot MP.prog)
        (lcmd : LCmd.t)
        ?(annot : annot option)
        (state : State.t) : (State.t, state_err_t) Res_list.t =
      print_lconfiguration lcmd annot state;

      let eval_expr = make_eval_expr state in
      match lcmd with
      | AssumeType (e, t) -> eval_assumetype e t state eval_expr
      | Assume f -> eval_assume f state
      | FreshSVar x -> eval_freshsvar x state
      | Assert f -> eval_assert f state
      | Macro (name, args) -> eval_macro name args lcmd prog annot state
      | If (e, lcmds_t, lcmds_e) ->
          eval_if e lcmds_t lcmds_e prog annot state eval_expr
      | Branch fof -> eval_branch fof state
      | SL sl_cmd -> State.evaluate_slcmd prog sl_cmd state

    and eval_lcmds
        (prog : annot MP.prog)
        (lcmds : LCmd.t list)
        ?(annot : annot option = None)
        (state : State.t) : (State.t, state_err_t) Res_list.t =
      let open Res_list.Syntax in
      match lcmds with
      | [] -> Res_list.return state
      | lcmd :: rest_lcmds ->
          let** new_state = eval_lcmd prog lcmd ?annot state in
          eval_lcmds prog rest_lcmds ~annot new_state
  end

  let evaluate_lcmd = Evaluate_lcmd.eval_lcmd
  let evaluate_lcmds = Evaluate_lcmd.eval_lcmds

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

  module Evaluate_cmd = struct
    type make_confcont =
      state:state_t ->
      callstack:Call_stack.t ->
      invariant_frames:invariant_frames ->
      prev_idx:int ->
      next_idx:int ->
      loop_ids:string list ->
      branch_count:int ->
      ?branch_case:branch_case ->
      unit ->
      CConf.t

    type eval_state = {
      prog : annot MP.prog;
      store : store_t;
      annot : annot;
      i : int;
      cs : Call_stack.t;
      iframes : invariant_frames;
      loop_ids : string list;
      b_counter : int;
      state : state_t;
      prev : int;
      prev_loop_ids : string list;
      make_confcont : make_confcont;
      eval_expr : Expr.t -> Val.t;
      loop_action : loop_action;
      branch_path : branch_path;
      last_known_loc : Location.t option;
      prev_cmd_report_id : L.Report_id.t option;
    }

    module Do_eval = struct
      module Eval_proc_call = struct
        let get_pid_or_error pid state =
          match Val.to_literal pid with
          | Some (String pid) -> pid
          | Some _ ->
              let err = [ Exec_err.EProc pid ] in
              raise (Interpreter_error (err, state))
          | None ->
              raise
                (Internal_error
                   "Procedure Call Error - unlifting procedure ID failed")

        let get_spec_and_params (prog : annot MP.prog) pid state =
          let proc = Prog.get_proc prog.prog pid in
          let spec = Hashtbl.find_opt prog.specs pid in
          let params =
            match (proc, spec) with
            | Some proc, _ -> Proc.get_params proc
            | None, Some spec -> Spec.get_params spec.data
            | _ ->
                raise
                  (Interpreter_error
                     ([ EProc (Val.from_literal (String pid)) ], state))
          in
          (spec, params)

        let build_args v_args params =
          let prmlen = List.length params in
          let args = Array.make prmlen (Val.from_literal Undefined) in
          let () =
            List.iteri
              (fun i v_arg -> if i < prmlen then args.(i) <- v_arg)
              v_args
          in
          Array.to_list args

        let process_ret_cont
            new_j
            eval_state
            ix
            ret_state
            fl
            b_counter
            has_branched =
          let { i; cs; make_confcont; iframes; loop_ids; _ } = eval_state in

          let new_cs = if ix = 0 then cs else Call_stack.copy cs in
          let branch_case =
            if has_branched then Some (SpecExec (fl, ix)) else None
          in

          make_confcont ~state:ret_state ~callstack:new_cs
            ~invariant_frames:iframes ~prev_idx:i ~loop_ids ~next_idx:new_j
            ~branch_count:b_counter ?branch_case ()

        let process_ret
            pid
            j
            eval_state
            ix
            ret_state
            fl
            b_counter
            has_branched
            spec_name : CConf.t =
          let { i; cs; branch_path; prev_cmd_report_id; annot; _ } =
            eval_state
          in
          let process_ret_cont new_j =
            process_ret_cont new_j eval_state ix ret_state fl b_counter
              has_branched
          in

          match (fl, j) with
          | Flag.Normal, _ -> process_ret_cont (i + 1)
          | Flag.Error, Some j -> process_ret_cont j
          | Flag.Error, None ->
              let msg =
                Printf.sprintf
                  "SYNTAX ERROR: No error label provided when calling \
                   procedure %s"
                  pid
              in
              L.normal (fun fmt -> fmt "%s" msg);
              raise (Syntax_error msg)
          | Flag.Bug, _ ->
              let loc = Annot.get_origin_loc annot in
              ConfErr
                {
                  callstack = cs;
                  proc_idx = i;
                  error_state = ret_state;
                  errors =
                    [
                      Exec_err.EState
                        (EOther
                           (Fmt.str "Error: tried to use bug spec '%s'"
                              spec_name));
                    ];
                  branch_path;
                  prev_cmd_report_id;
                  loc;
                }

        let symb_exec_proc x pid v_args j params args eval_state () =
          let { cs; state; i; loop_ids; b_counter; iframes; make_confcont; _ } =
            eval_state
          in
          let new_store = Store.init (List.combine params args) in
          let old_store = State.get_store state in
          let state' = State.set_store state new_store in
          let cs' =
            (* Note the new loop identifiers *)
            Call_stack.push cs ~pid ~arguments:v_args ~store:old_store ~loop_ids
              ~ret_var:x ~call_index:i ~continue_index:(i + 1) ?error_index:j ()
          in
          [
            make_confcont ~state:state' ~callstack:cs' ~invariant_frames:iframes
              ~prev_idx:(-1) ~loop_ids ~next_idx:0 ~branch_count:b_counter ();
          ]

        let exec_with_spec spec x j args pid subst symb_exec_proc eval_state =
          let {
            annot;
            state;
            i;
            b_counter;
            cs;
            branch_path;
            prev_cmd_report_id;
            _;
          } =
            eval_state
          in
          let process_ret = process_ret pid j eval_state in
          match !symb_exec_next with
          | true ->
              symb_exec_next := false;
              symb_exec_proc ()
          | false ->
              let subst = eval_subst_list state subst in
              L.verbose (fun fmt -> fmt "ABOUT TO USE THE SPEC OF %s" pid);
              (* print_to_all ("\tStarting run spec: " ^ pid); *)
              let (ret : (State.t * Flag.t, state_err_t) Res_list.t) =
                State.run_spec spec state x args subst
              in
              L.verbose (fun fmt ->
                  fmt "Run_spec returned %d Results" (List.length ret));
              let loc = Annot.get_origin_loc annot in
              if ret = [] then
                if spec.data.spec_incomplete then (
                  L.normal (fun fmt ->
                      fmt "Proceeding with symbolic execution.");
                  symb_exec_proc ())
                else
                  [
                    CConf.ConfErr
                      {
                        callstack = cs;
                        proc_idx = i;
                        error_state = state;
                        errors =
                          [
                            Exec_err.EState
                              (EOther
                                 (Fmt.str
                                    "Error: Unable to use specification of \
                                     function %s"
                                    spec.data.spec_name));
                          ];
                        branch_path;
                        prev_cmd_report_id;
                        loc;
                      };
                  ]
              else
                let successes, errors =
                  List.partition_map
                    (function
                      | Ok x -> Left x
                      | Error x -> Right (Exec_err.EState x))
                    ret
                in
                let b_counter, has_branched =
                  match successes with
                  | [] -> (b_counter, false)
                  | _ -> (b_counter + 1, true)
                in
                let spec_name = spec.data.spec_name in
                let success_confs =
                  successes
                  |> List.mapi (fun ix (ret_state, fl) ->
                         process_ret ix ret_state fl b_counter has_branched
                           spec_name)
                in
                let error_confs =
                  match errors with
                  | [] -> []
                  | errors ->
                      [
                        CConf.ConfErr
                          {
                            callstack = cs;
                            proc_idx = i;
                            error_state = state;
                            errors;
                            branch_path;
                            prev_cmd_report_id;
                            loc;
                          };
                      ]
                in
                success_confs @ error_confs

        let exec_without_spec pid symb_exec_proc eval_state =
          let {
            prog;
            cs;
            state;
            i;
            b_counter;
            iframes;
            prev;
            prev_loop_ids;
            branch_path;
            last_known_loc;
            prev_cmd_report_id;
            _;
          } =
            eval_state
          in
          if Hashtbl.mem prog.prog.bi_specs pid then
            [
              CConf.ConfSusp
                {
                  spec_id = pid;
                  state;
                  callstack = cs;
                  invariant_frames = iframes;
                  prev_idx = prev;
                  loop_ids = prev_loop_ids;
                  next_idx = i;
                  branch_path;
                  branch_count = b_counter;
                  loc = last_known_loc;
                  prev_cmd_report_id;
                };
            ]
          else symb_exec_proc ()

        let f x pid v_args j subst eval_state =
          let { prog; cs; state; _ } = eval_state in
          let pid = get_pid_or_error pid state in

          let spec, params = get_spec_and_params prog pid state in
          let caller = Call_stack.get_cur_proc_id cs in
          let () = Call_graph.add_proc_call call_graph caller pid in
          let args = build_args v_args params in

          let is_internal_proc proc_name =
            (Prog.get_proc_exn prog.prog proc_name).proc_internal
          in

          let symb_exec_proc =
            symb_exec_proc x pid v_args j params args eval_state
          in

          let spec_exec_proc () =
            match spec with
            | Some spec ->
                (* Fmt.pr "Calling %s WITH SPEC\n" pid; *)
                exec_with_spec spec x j args pid subst symb_exec_proc eval_state
            | None ->
                (* Fmt.pr "Calling %s WITHOUT SPEC\n" pid; *)
                exec_without_spec pid symb_exec_proc eval_state
          in

          match Exec_mode.is_biabduction_exec !Config.current_exec_mode with
          | true -> (
              match
                ( pid = caller,
                  is_internal_proc pid,
                  Call_stack.recursive_depth cs pid >= !Config.bi_unroll_depth
                )
              with
              (* In bi-abduction, reached max depth of recursive calls *)
              | _, _, true -> []
              (* In bi-abduction, recursive call *)
              | true, false, _ -> symb_exec_proc ()
              (* TODO: When JS internals work
                 | true, false, false
                   when List.length
                          (List.filter is_internal_proc (Call_stack.get_cur_procs cs))
                        < !Config.bi_no_spec_depth -> symb_exec_proc () *)
              | _ -> spec_exec_proc ())
          | false -> spec_exec_proc ()
      end

      let eval_proc_call = Eval_proc_call.f

      let split_results results =
        let oks, errs =
          List.fold_left
            (fun (oks, errs) res ->
              match res with
              | Ok ok -> (ok :: oks, errs)
              | Error err -> (oks, err :: errs))
            ([], []) results
        in
        (List.rev oks, List.rev errs)

      (* Action *)
      let eval_laction ~branch_path x a es state =
        let {
          annot;
          i;
          cs;
          iframes;
          loop_ids;
          b_counter;
          state;
          prev;
          prev_loop_ids;
          make_confcont;
          eval_expr;
          prev_cmd_report_id;
          _;
        } =
          state
        in
        DL.log ~v:true (fun m ->
            m
              ~json:
                [
                  ("x", `String x);
                  ("a", `String a);
                  ("es", `List (List.map Expr.to_yojson es));
                ]
              "LAction");
        AnnotatedAction.log { annot; action_name = a } |> ignore;
        let open Utils.Syntaxes.List in
        let v_es = List.map eval_expr es in
        let oks, errors = State.execute_action a state v_es |> split_results in
        let oks =
          match oks with
          | [] -> []
          | (state', vs) :: rest_rets ->
              DL.log ~v:true (fun m ->
                  m
                    ~json:
                      [
                        ("state'", state_t_to_yojson state');
                        ("vs", `List (List.map state_vt_to_yojson vs));
                      ]
                    "Ok");
              let e' = Expr.EList (List.map Val.to_expr vs) in
              let v' = eval_expr e' in
              let state'' = update_store state' x v' in
              let rest_confs =
                rest_rets
                |> List.mapi (fun ix (r_state, r_vs) ->
                       let r_e = Expr.EList (List.map Val.to_expr r_vs) in
                       let r_v = eval_expr r_e in
                       let r_state' = update_store r_state x r_v in
                       let branch_case = LAction (ix + 1) in
                       make_confcont ~state:r_state'
                         ~callstack:(Call_stack.copy cs)
                         ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                         ~next_idx:(i + 1) ~branch_count:b_counter ~branch_case
                         ())
              in
              let b_counter, branch_case =
                match rest_rets with
                | [] -> (b_counter, None)
                | _ -> (b_counter + 1, Some (LAction 0))
              in
              make_confcont ~state:state'' ~callstack:cs
                ~invariant_frames:iframes ?branch_case ~prev_idx:i ~loop_ids
                ~next_idx:(i + 1) ~branch_count:b_counter ()
              :: rest_confs
        in
        let errors =
          match errors with
          | [] -> []
          | errs ->
              DL.log ~v:true (fun m ->
                  m
                    ~json:
                      [ ("errs", `List (List.map state_err_t_to_yojson errs)) ]
                    "Error");
              let loc = Annot.get_origin_loc annot in
              if Exec_mode.is_verification_exec !Config.current_exec_mode then (
                let tactic_from_params =
                  let recovery_params =
                    let* v = v_es in
                    let e = Val.to_expr v in
                    let+ base_elem = Expr.base_elements e in
                    Option.get (Val.from_expr base_elem)
                  in
                  Recovery_tactic.try_unfold recovery_params
                in
                let recovery_vals =
                  State.get_recovery_tactic state errs
                  |> Recovery_tactic.merge tactic_from_params
                in
                let recovery_states : (State.t list, string) result =
                  State.try_recovering state recovery_vals
                in
                match recovery_states with
                | Ok recovery_states ->
                    let num_states = List.length recovery_states in
                    let b_counter =
                      b_counter + if num_states = 1 then 0 else 1
                    in
                    List.mapi
                      (fun ix state ->
                        let branch_case =
                          if num_states > 1 then Some (LActionFail ix) else None
                        in
                        let cs = if ix = 0 then cs else Call_stack.copy cs in
                        make_confcont ~state ~callstack:cs
                          ~invariant_frames:iframes ~prev_idx:prev
                          ~loop_ids:prev_loop_ids ~next_idx:i
                          ~branch_count:b_counter ?branch_case ())
                      recovery_states
                | Error msg ->
                    L.verbose (fun m -> m "Couldn't recover because: %s" msg);
                    let pp_err ft (a, errs) =
                      Fmt.pf ft "FAILURE: Action %s failed with: %a" a
                        (Fmt.Dump.list State.pp_err)
                        errs
                    in
                    Fmt.pr "%a\n@?" (Fmt.styled `Red pp_err) (a, errs);
                    L.normal ~title:"failure" ~severity:Error (fun m ->
                        m "Action call failed with:@.%a"
                          (Fmt.Dump.list State.pp_err)
                          errs);

                    [
                      ConfErr
                        (* (errs, state) *)
                        {
                          callstack = cs;
                          proc_idx = i;
                          error_state = state;
                          errors = List.map (fun x -> Exec_err.EState x) errs;
                          branch_path;
                          prev_cmd_report_id;
                          loc;
                        };
                    ])
              else
                [
                  ConfErr
                    (* (errs, state) *)
                    {
                      callstack = cs;
                      proc_idx = i;
                      error_state = state;
                      errors = List.map (fun x -> Exec_err.EState x) errs;
                      branch_path;
                      prev_cmd_report_id;
                      loc;
                    };
                ]
        in
        oks @ errors

      (* Logic command *)
      let eval_logic (lcmd : LCmd.t) state =
        let {
          prog;
          i;
          cs;
          iframes;
          loop_ids;
          b_counter;
          state;
          prev_loop_ids;
          make_confcont;
          loop_action;
          branch_path;
          annot;
          prev_cmd_report_id;
          _;
        } =
          state
        in
        DL.log ~v:true (fun m -> m "LCmd");
        let loc = Annot.get_origin_loc annot in
        match lcmd with
        | SL SymbExec ->
            symb_exec_next := true;
            [
              make_confcont ~state ~callstack:cs ~invariant_frames:iframes
                ~prev_idx:i ~loop_ids ~next_idx:(i + 1) ~branch_count:b_counter
                ();
            ]
        (* Invariant being revisited *)
        | SL (Invariant (a, binders)) when prev_loop_ids = loop_ids ->
            (* let () = Fmt.pr "\nRe-establishing invariant... @?" in *)
            let _ = State.match_invariant prog true state a binders in
            let () = L.verbose (fun fmt -> fmt "Invariant re-established.") in
            (* let () = Fmt.pr "\nInvariant re-established. @?" in *)
            []
        | SL (Invariant (a, binders)) ->
            assert (loop_action = FrameOff (List.hd loop_ids));
            (* let () = Fmt.pr "\nEstablishing invariant... @?" in *)
            let frames_and_states =
              State.match_invariant prog false state a binders
            in
            (* let () = Fmt.pr "\nSuccessfully established invariant. @?" in *)
            List.map
              (fun ret ->
                match ret with
                | Ok (frame, state) ->
                    let iframes = (List.hd loop_ids, frame) :: iframes in
                    make_confcont ~state ~callstack:cs ~invariant_frames:iframes
                      ~prev_idx:i ~loop_ids ~next_idx:(i + 1)
                      ~branch_count:b_counter ()
                | Error err ->
                    let errors = [ Exec_err.EState err ] in
                    ConfErr
                      {
                        callstack = cs;
                        proc_idx = i;
                        error_state = state;
                        errors;
                        branch_path;
                        prev_cmd_report_id;
                        loc;
                      })
              frames_and_states
        | _ ->
            let all_results = evaluate_lcmd prog lcmd ~annot state in
            let successes, errors = Res_list.split all_results in
            let success_confs =
              let num_successes = List.length successes in
              let has_branched = num_successes > 1 in
              let b_counter =
                if has_branched then b_counter + 1 else b_counter
              in
              successes
              |> List.mapi (fun ix state ->
                     let branch_case =
                       if has_branched then Some (LCmd ix) else None
                     in
                     make_confcont ~state ~callstack:cs
                       ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                       ~next_idx:(i + 1) ~branch_count:b_counter ?branch_case ())
            in
            let error_conf =
              match errors with
              | [] -> []
              | errors ->
                  let errors =
                    errors |> List.map (fun e -> Exec_err.EState e)
                  in

                  [
                    CConf.ConfErr
                      {
                        callstack = cs;
                        proc_idx = i;
                        error_state = state;
                        errors;
                        branch_path;
                        prev_cmd_report_id;
                        loc;
                      };
                  ]
            in
            success_confs @ error_conf

      (* Conditional goto *)
      let eval_guarded_goto e j k state =
        let {
          i;
          cs;
          iframes;
          loop_ids;
          b_counter;
          state;
          make_confcont;
          eval_expr;
          _;
        } =
          state
        in
        let vt = eval_expr e in
        let lvt = Val.to_literal vt in
        let vf =
          match lvt with
          | Some (Bool true) -> vfalse
          | Some (Bool false) -> vtrue
          | _ -> eval_expr (Expr.Infix.not e)
        in
        L.verbose (fun fmt ->
            fmt "Evaluated expressions: %a, %a" Val.pp vt Val.pp vf);
        let can_put_t, can_put_f =
          match lvt with
          | Some (Bool true) -> (true, false)
          | Some (Bool false) -> (false, true)
          | _ ->
              let vtx = State.sat_check state vt in
              let vfx =
                match vtx with
                | false -> true
                | true -> State.sat_check state vf
              in
              (vtx, vfx)
        in
        let sp_t, sp_f =
          match (can_put_t, can_put_f) with
          | false, false -> ([], [])
          | true, false ->
              (List.map (fun x -> (x, j)) (State.assume state vt), [])
          | false, true ->
              ([], List.map (fun x -> (x, k)) (State.assume state vf))
          | true, true ->
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
        let sp_t = List.map (fun t -> (t, true)) sp_t in
        let sp_f = List.map (fun f -> (f, false)) sp_f in
        let sp = sp_t @ sp_f in

        let b_counter =
          if can_put_t && can_put_f && List.length sp > 1 then b_counter + 1
          else b_counter
        in
        List.mapi
          (fun j ((state, next), case) ->
            make_confcont ~state
              ~callstack:(if j = 0 then cs else Call_stack.copy cs)
              ~invariant_frames:iframes ~prev_idx:i ~loop_ids ~next_idx:next
              ~branch_count:b_counter ~branch_case:(GuardedGoto case) ())
          sp

      let eval_phi_assignment lxarr eval_state =
        let {
          eval_expr;
          prog;
          cs;
          prev;
          i;
          make_confcont;
          iframes;
          b_counter;
          loop_ids;
          state;
          _;
        } =
          eval_state
        in
        DL.log ~v:true (fun m -> m "PhiAssignment");
        let j = get_predecessor prog cs prev i in
        let state' =
          List.fold_left
            (fun state (x, x_arr) ->
              let e = List.nth x_arr j in
              let v = eval_expr e in
              update_store state x v)
            state lxarr
        in
        [
          make_confcont ~state:state' ~callstack:cs ~invariant_frames:iframes
            ~prev_idx:i ~loop_ids ~next_idx:(i + 1) ~branch_count:b_counter ();
        ]

      (* Function call *)
      let eval_call x e args j subst eval_state =
        let { eval_expr; _ } = eval_state in
        DL.log ~v:true (fun m -> m "Call");
        let pid = eval_expr e in
        let v_args = List.map eval_expr args in
        let result = eval_proc_call x pid v_args j subst eval_state in
        result

      (* External function call *)
      let eval_ecall x (pid : Expr.t) args j eval_state =
        let {
          eval_expr;
          make_confcont;
          iframes;
          prog;
          state;
          cs;
          i;
          b_counter;
          loop_ids;
          _;
        } =
          eval_state
        in
        DL.log ~v:true (fun m -> m "ECall");
        let pid =
          match pid with
          | PVar pid -> pid
          | Lit (String pid) -> pid
          | _ ->
              raise
                (Exceptions.Impossible
                   "Procedure identifier not a program variable")
        in
        let v_args = List.map eval_expr args in
        List.map
          (fun (state, cs, i, j) ->
            make_confcont ~state ~callstack:cs ~invariant_frames:iframes
              ~prev_idx:i ~loop_ids ~next_idx:j ~branch_count:b_counter ())
          (External.execute prog.prog state cs i x pid v_args j)

      (* Function application *)
      let eval_apply x pid_args j state =
        let { eval_expr; _ } = state in
        DL.log ~v:true (fun m -> m "Apply");
        let v_pid_args = eval_expr pid_args in
        let v_pid_args_list = Val.to_list v_pid_args in
        match v_pid_args_list with
        | Some v_pid_args_list ->
            let pid = List.hd v_pid_args_list in
            let v_args = List.tl v_pid_args_list in
            eval_proc_call x pid v_args j None state
        | None ->
            raise
              (Failure
                 (Fmt.str "Apply not called with a list: @[<h>%a@]" Val.pp
                    v_pid_args))

      let eval_return_normal eval_state =
        let {
          annot;
          store;
          cs;
          loop_ids;
          branch_path;
          make_confcont;
          state;
          iframes;
          b_counter;
          prev_cmd_report_id;
          _;
        } =
          eval_state
        in
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
              [
                CConf.ConfFinish
                  {
                    flag = Normal;
                    ret_val = v_ret;
                    final_state = state;
                    branch_path;
                    prev_cmd_report_id;
                    loc = Annot.get_origin_loc annot;
                  };
              ]
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
              let open Syntaxes.List in
              let+ state =
                (* Framing on should never fail.. *)
                if Exec_mode.is_verification_exec !Config.current_exec_mode then
                  State.frame_on state iframes to_frame_on
                  |> List.filter_map (function
                       | Ok x -> Some x
                       | _ -> None)
                else [ state ]
              in
              let state' = State.set_store state old_store in
              let state'' = update_store state' x v_ret in
              make_confcont ~state:state'' ~callstack:cs'
                ~invariant_frames:iframes ~prev_idx:prev'
                ~loop_ids:start_loop_ids ~next_idx:j ~branch_count:b_counter ()
          | _ -> raise (Failure "Malformed callstack")
        in
        L.verbose (fun m -> m "Returning.");
        result

      let eval_return_error eval_state =
        let {
          store;
          cs;
          loop_ids;
          branch_path;
          make_confcont;
          state;
          iframes;
          b_counter;
          prev_cmd_report_id;
          annot;
          _;
        } =
          eval_state
        in
        let v_ret = Store.get store Names.return_variable in
        match (v_ret, cs) with
        | None, _ ->
            raise (Failure "Return variable not in store (error return) ")
        | Some v_ret, { store = None; loop_ids = start_loop_ids; _ } :: _ ->
            check_loop_ids loop_ids start_loop_ids;
            Fmt.pr "e @?";
            [
              CConf.ConfFinish
                {
                  flag = Error;
                  ret_val = v_ret;
                  final_state = state;
                  branch_path : branch_path;
                  prev_cmd_report_id;
                  loc = Annot.get_origin_loc annot;
                };
            ]
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
              (* Framing on should never fail *)
              if Exec_mode.is_verification_exec !Config.current_exec_mode then
                State.frame_on state iframes to_frame_on
                |> List.filter_map (function
                     | Ok x -> Some x
                     | _ -> None)
              else [ state ]
            in
            let state' = State.set_store state old_store in
            let state'' = update_store state' x v_ret in
            make_confcont ~state:state'' ~callstack:cs'
              ~invariant_frames:iframes ~prev_idx:prev' ~loop_ids:start_loop_ids
              ~next_idx:j ~branch_count:b_counter ()
        | _ -> raise (Failure "Malformed callstack")

      let f (cmd : int Cmd.t) eval_state =
        let {
          eval_expr;
          make_confcont;
          state;
          cs;
          iframes;
          loop_ids;
          i;
          b_counter;
          branch_path;
          _;
        } =
          eval_state
        in
        match cmd with
        (* Skip *)
        | Skip ->
            [
              make_confcont ~state ~callstack:cs ~invariant_frames:iframes
                ~prev_idx:i ~loop_ids ~next_idx:(i + 1) ~branch_count:b_counter
                ();
            ]
        (* Assignment *)
        | Assignment (x, e) ->
            DL.log ~v:true (fun m ->
                m
                  ~json:[ ("target", `String x); ("expr", Expr.to_yojson e) ]
                  "Assignment");
            let v = eval_expr e in
            let state' = update_store state x v in
            [
              make_confcont ~state:state' ~callstack:cs
                ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                ~next_idx:(i + 1) ~branch_count:b_counter ();
            ]
        | LAction (x, a, es) -> eval_laction ~branch_path x a es eval_state
        | Logic lcmd -> eval_logic lcmd eval_state
        (* Unconditional goto *)
        | Goto j ->
            [
              make_confcont ~state ~callstack:cs ~invariant_frames:iframes
                ~prev_idx:i ~loop_ids ~next_idx:j ~branch_count:b_counter ();
            ]
        | GuardedGoto (e, j, k) -> eval_guarded_goto e j k eval_state
        | PhiAssignment lxarr -> eval_phi_assignment lxarr eval_state
        | Call (x, e, args, j, subst) -> eval_call x e args j subst eval_state
        | ECall (x, pid, args, j) -> eval_ecall x pid args j eval_state
        | Apply (x, pid_args, j) -> eval_apply x pid_args j eval_state
        (* Arguments *)
        | Arguments x ->
            DL.log ~v:true (fun m -> m "Arguments");
            let args = Call_stack.get_cur_args cs in
            let args = Val.from_list args in
            let state' = update_store state x args in
            [
              make_confcont ~state:state' ~callstack:cs
                ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                ~next_idx:(i + 1) ~branch_count:b_counter ();
            ]
        (* Normal-mode return *)
        | ReturnNormal -> eval_return_normal eval_state
        (* Error-mode return *)
        | ReturnError -> eval_return_error eval_state
        (* Explicit failure *)
        | Fail (fail_code, fail_params) ->
            let fail_params = List.map (State.eval_expr state) fail_params in
            let err = Exec_err.EFailReached { fail_code; fail_params } in
            raise (Interpreter_error ([ err ], state))
    end

    let do_eval = Do_eval.f

    let rec eval_cmd
        (prog : annot MP.prog)
        (state : State.t)
        (cs : Call_stack.t)
        (iframes : invariant_frames)
        (prev : int)
        (prev_loop_ids : string list)
        (i : int)
        (b_counter : int)
        (last_known_loc : Location.t option ref)
        (report_id_ref : L.Report_id.t option ref)
        (branch_path : branch_path)
        (branch_case : branch_case option) : CConf.t list =
      let _, (annot, _) = get_cmd prog cs i in

      (* The full list of loop ids is the concatenation
         of the loop ids of the current procedure plus
         the loop ids that have come from the call stack *)
      let loop_ids = Annot.get_loop_info annot @ Call_stack.get_loop_ids cs in

      let loop_action : loop_action =
        if Exec_mode.is_verification_exec !Config.current_exec_mode then
          understand_loop_action loop_ids prev_loop_ids
        else Nothing
      in
      let eval_in_state state =
        eval_cmd_after_frame_handling prog state cs iframes prev prev_loop_ids i
          b_counter last_known_loc report_id_ref branch_path branch_case
      in
      match loop_action with
      | Nothing -> eval_in_state state
      | FrameOff id ->
          L.verbose (fun fmt -> fmt "INFO: Expecting to frame off %s" id);
          eval_in_state state
      | Malformed -> L.fail "Malformed loop identifiers"
      | FrameOn ids ->
          L.verbose (fun fmt ->
              fmt "INFO: Going to frame on %a" pp_str_list ids);
          (* Framing on should never fail *)
          let states =
            State.frame_on state iframes ids
            |> List.filter_map (function
                 | Ok x -> Some x
                 | _ -> None)
          in
          let n = List.length states in
          if n == 0 then
            L.normal (fun fmt ->
                fmt "WARNING: FRAMING ON RESULTED IN 0 STATES !")
          else if n > 1 then
            L.verbose (fun fmt ->
                fmt
                  "WARNING: FRAMING ON AFTER EXITING LOOP BRANCHED INTO %i \
                   STATES"
                  n);
          List.concat_map eval_in_state states

    and eval_cmd_after_frame_handling
        (prog : annot MP.prog)
        (state : State.t)
        (cs : Call_stack.t)
        (iframes : invariant_frames)
        (prev : int)
        (prev_loop_ids : string list)
        (i : int)
        (b_counter : int)
        (last_known_loc : Location.t option ref)
        (report_id_ref : L.Report_id.t option ref)
        (branch_path : branch_path)
        (branch_case : branch_case option) : CConf.t list =
      let store = State.get_store state in
      let eval_expr = make_eval_expr state in
      let proc_name, annot_cmd = get_cmd prog cs i in
      let annot, cmd = annot_cmd in
      let () =
        let is_internal =
          let pid = (List.hd cs).pid in
          let proc = Hashtbl.find prog.prog.procs pid in
          proc.proc_internal
        in
        match Annot.get_origin_loc annot with
        | Some loc when not is_internal -> last_known_loc := Some loc
        | _ -> ()
      in
      let loop_ids = Annot.get_loop_info annot @ Call_stack.get_loop_ids cs in
      let loop_action : loop_action =
        if Exec_mode.is_verification_exec !Config.current_exec_mode then
          understand_loop_action loop_ids prev_loop_ids
        else Nothing
      in
      (* if !Config.stats then Statistics.exec_cmds := !Statistics.exec_cmds + 1; *)
      MP.update_coverage prog proc_name i;

      log_configuration annot_cmd state cs i b_counter branch_case proc_name
      |> Option.iter (fun report_id ->
             report_id_ref := Some report_id;
             L.Parent.set report_id);

      let branch_path = List_utils.cons_opt branch_case branch_path in
      let loc = !last_known_loc in
      let prev_cmd_report_id = !report_id_ref in
      let make_confcont =
        CConf.make_cont ?loc ?prev_cmd_report_id ~branch_path
      in

      (* DL.log (fun m ->
          m
            ~json:[ ("path", branch_path_to_yojson branch_path) ]
            "G_interpreter: stepping with path"); *)
      let eval_state =
        {
          prog;
          store;
          annot;
          i;
          cs;
          iframes;
          loop_ids;
          b_counter;
          state;
          prev;
          prev_loop_ids;
          make_confcont;
          eval_expr;
          loop_action;
          branch_path;
          last_known_loc = !last_known_loc;
          prev_cmd_report_id;
        }
      in

      do_eval cmd eval_state
  end

  let evaluate_cmd = Evaluate_cmd.eval_cmd

  let simplify state =
    snd (State.simplify ~save:true ~kill_new_lvars:true state)

  let protected_evaluate_cmd
      (prog : annot MP.prog)
      (state : State.t)
      (cs : Call_stack.t)
      (iframes : invariant_frames)
      (prev : int)
      (prev_loop_ids : string list)
      (i : int)
      (b_counter : int)
      (last_known_loc : Location.t option)
      (report_id_ref : L.Report_id.t option ref)
      (branch_path : branch_path)
      (branch_case : branch_case option) : CConf.t list =
    let states =
      match get_cmd prog cs i with
      | _, (_, LAction _) -> simplify state
      | _ -> [ state ]
    in
    List.concat_map
      (fun state ->
        let last_known_loc = ref last_known_loc in
        try
          evaluate_cmd prog state cs iframes prev prev_loop_ids i b_counter
            last_known_loc report_id_ref branch_path branch_case
        with
        | Interpreter_error (errors, error_state) ->
            [
              ConfErr
                {
                  callstack = cs;
                  proc_idx = i;
                  error_state;
                  errors;
                  branch_path = List_utils.cons_opt branch_case branch_path;
                  prev_cmd_report_id = !report_id_ref;
                  loc = !last_known_loc;
                };
            ]
        | State.Internal_State_Error (errs, error_state) ->
            (* Return: current procedure name, current command index, the state, and the associated errors *)
            [
              ConfErr
                {
                  callstack = cs;
                  proc_idx = i;
                  error_state;
                  errors = List.map (fun x -> Exec_err.EState x) errs;
                  branch_path = List_utils.cons_opt branch_case branch_path;
                  prev_cmd_report_id = !report_id_ref;
                  loc = !last_known_loc;
                };
            ])
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
  module Evaluate_cmd_step = struct
    open CConf

    type results = (L.Report_id.t option * branch_path * result_t) list
    type 'a f = CConf.t list -> conf_selector option -> results -> 'a cont_func

    type 'a eval_step =
      (result_t -> 'a) ->
      bool ->
      annot MP.prog ->
      'a list ->
      (CConf.t * string) list ->
      'a f

    type 'a eval_step_state = {
      ret_fun : result_t -> 'a;
      retry : bool;
      prog : annot MP.prog;
      hold_results : 'a list;
      on_hold : (CConf.t * string) list;
      branch_path : branch_path option;
      results : results;
      conf : CConf.t option;
      rest_confs : CConf.t list;
      parent_id_ref : L.Report_id.t option ref;
      f : 'a f;
      eval_step : 'a eval_step;
      selector : conf_selector option;
    }

    let log_confcont parent_id_ref is_first = function
      | ConfCont
          {
            state;
            callstack;
            next_idx = proc_body_index;
            branch_case;
            prev_cmd_report_id;
            _;
          } ->
          let cmd_step : CmdResult.t =
            {
              callstack;
              proc_body_index;
              state = Some state;
              errors = [];
              branch_case;
            }
          in
          (* if is_first then (
             prev_cmd_report_id
             |> Option.iter (fun prev_report_id ->
                    L.Parent.release !parent_id_ref;
                    L.Parent.set prev_report_id;
                    parent_id_ref := Some prev_report_id);
             DL.log (fun m ->
                 m
                   ~json:[ ("conf", CmdResult.to_yojson cmd_step) ]
                   "Debugger.evaluate_cmd_step: New ConfCont")); *)
          ignore (parent_id_ref, is_first, prev_cmd_report_id);
          CmdResult.log_result cmd_step |> ignore;
          Some cmd_step
      | _ -> None

    let continue_or_pause
        ?(new_confs = false)
        rest_confs
        cont_func
        eval_step_state =
      let { parent_id_ref; branch_path; _ } = eval_step_state in
      match rest_confs with
      | ConfCont { branch_path; _ } :: _ ->
          rest_confs
          |> List.iteri (fun i conf ->
                 log_confcont parent_id_ref (i = 0) conf |> ignore);
          let new_branch_cases =
            if new_confs then
              rest_confs |> List.filter_map CConf.get_branch_case
            else []
          in
          Continue
            {
              report_id = !parent_id_ref;
              branch_path;
              new_branch_cases;
              cont_func;
            }
      | ConfErr
          {
            callstack;
            proc_idx = proc_body_index;
            error_state = state;
            errors;
            branch_path;
            _;
          }
        :: _ ->
          CmdResult.log_result
            {
              callstack;
              proc_body_index;
              state = Some state;
              errors;
              branch_case = None;
            }
          |> ignore;
          Continue
            {
              report_id = !parent_id_ref;
              branch_path;
              new_branch_cases = [];
              cont_func;
            }
      | _ ->
          if !Config.debug then
            let branch_path = Option.value branch_path ~default:[] in
            Continue
              {
                report_id = !parent_id_ref;
                branch_path;
                new_branch_cases = [];
                cont_func;
              }
          else cont_func ()

    let select_conf selector confs =
      match selector with
      | None ->
          DL.log (fun m ->
              m
                "select_conf - HORROR: selector shouldn't be None when \
                 debugging!");
          List_utils.hd_tl confs
      | Some selector ->
          let f =
            match selector with
            | Path path -> fun conf -> CConf.get_branch_path conf = path
            | IdCase (id, case) -> (
                fun conf ->
                  let case' = CConf.get_branch_case conf in
                  match CConf.get_prev_cmd_id conf with
                  | Some id' when id = id' && case = case' -> true
                  | _ -> false)
          in
          List_utils.pop_where f confs

    let debug_log conf rest_confs =
      DL.log ~v:true (fun m ->
          let conf_json =
            match conf with
            | None -> `Null
            | Some conf -> CConf.to_yojson conf
          in
          m
            ~json:
              [
                ("conf", conf_json);
                ("rest_confs", `List (List.map CConf.to_yojson rest_confs));
              ]
            "G_interpreter.evaluate_cmd_step: Evaluating conf (%a)"
            (pp_option CConf.pp_short) conf)

    let find_result selector results =
      let* pred =
        match selector with
        | None ->
            Error
              "find_result - HORROR: selector shouldn't be None when debugging!"
        | Some (IdCase (id, _)) ->
            Ok
              (fun (id', _, result) ->
                match id' with
                | Some id' when id' = id -> Some result
                | _ -> None)
        | Some (Path path) ->
            Ok
              (fun (_, path', result) ->
                if path = path' then Some result else None)
      in
      match List.find_map pred results with
      | Some result -> Ok result
      | None ->
          let () =
            DL.log (fun m ->
                let json =
                  results
                  |> List.map (fun (a, b, _) -> (a, b))
                  |> list_to_yojson [%to_yojson: L.Report_id.t option * path]
                in
                m ~json:[ ("results", json) ] "All results")
          in
          Fmt.error "No result for selector (%a)!"
            (pp_option pp_conf_selector)
            selector

    let end_of_branch results eval_step_state =
      let { conf; rest_confs; ret_fun; f; selector; _ } = eval_step_state in
      match find_result selector results with
      | Error msg ->
          DL.failwith
            (fun () ->
              let result_jsons =
                results
                |> List.map (fun (id, path, result) ->
                       `Assoc
                         [
                           ("id", opt_to_yojson L.Report_id.to_yojson id);
                           ("path", branch_path_to_yojson path);
                           ("result", result_t_to_yojson result);
                         ])
              in
              let conf_json =
                match conf with
                | None -> `Null
                | Some conf -> CConf.to_yojson conf
              in
              [
                ("selector", opt_to_yojson conf_selector_to_yojson selector);
                ("results", `List result_jsons);
                ("conf", conf_json);
                ("rest_confs", `List (List.map CConf.to_yojson rest_confs));
              ])
            msg
      | Ok result ->
          EndOfBranch
            (ret_fun result, fun ?selector () -> f rest_confs selector results)

    module Handle_conf = struct
      let none eval_step_state =
        let {
          results;
          rest_confs;
          hold_results;
          retry;
          prog;
          on_hold;
          eval_step;
          ret_fun;
          _;
        } =
          eval_step_state
        in
        match rest_confs with
        | _ :: _ -> end_of_branch results eval_step_state
        | [] when !Config.debug -> end_of_branch results eval_step_state
        | [] ->
            let results =
              List.map (fun (_, _, result) -> ret_fun result) results
            in
            let results = hold_results @ results in
            if not retry then Finished results
            else
              (L.(verbose (fun m -> m "Relaunching suspended confs"));
               let hold_confs =
                 List.filter_map
                   (fun (conf, pid) ->
                     if Hashtbl.mem prog.specs pid then Some conf else None)
                   on_hold
               in
               continue_or_pause hold_confs (fun ?selector () ->
                   eval_step ret_fun false prog results [] hold_confs selector
                     []))
                eval_step_state

      let cont (cconf : CConf.cont) eval_step_state =
        let { prog; parent_id_ref; f; rest_confs; results; _ } =
          eval_step_state
        in
        let {
          state;
          callstack = cs;
          invariant_frames = iframes;
          prev_idx = prev;
          loop_ids = prev_loop_ids;
          next_idx = i;
          branch_count = b_counter;
          prev_cmd_report_id;
          branch_path;
          branch_case;
          loc;
          _;
        } =
          cconf
        in
        L.set_previous prev_cmd_report_id;
        let next_confs =
          protected_evaluate_cmd prog state cs iframes prev prev_loop_ids i
            b_counter loc parent_id_ref branch_path branch_case
        in
        continue_or_pause ~new_confs:true next_confs
          (fun ?selector () -> f (next_confs @ rest_confs) selector results)
          eval_step_state

      let max_branch (cconf : CConf.cont) eval_step_state =
        let { f; rest_confs; results; parent_id_ref; prog; _ } =
          eval_step_state
        in
        let {
          state;
          callstack = cs;
          next_idx = i;
          branch_count = b_counter;
          prev_cmd_report_id;
          branch_case;
          _;
        } =
          cconf
        in
        let proc_name, annot_cmd = get_cmd prog cs i in
        if !Config.current_exec_mode <> Exec_mode.BiAbduction then
          L.normal (fun m -> m "WARNING: MAX BRANCHING STOP: %d.\n" b_counter);
        L.set_previous prev_cmd_report_id;
        L.(
          verbose (fun m ->
              m "Stopping Symbolic Execution due to MAX BRANCHING with %d."
                b_counter));
        log_configuration annot_cmd state cs i b_counter branch_case proc_name
        |> Option.iter (fun report_id ->
               parent_id_ref := Some report_id;
               L.Parent.set report_id);
        continue_or_pause []
          (fun ?selector () -> f rest_confs selector results)
          eval_step_state

      let err (cconf : CConf.err) eval_step_state =
        let { results; rest_confs; f; _ } = eval_step_state in
        let { callstack; proc_idx; error_state; errors; branch_path; loc; _ } =
          cconf
        in
        let proc = Call_stack.get_cur_proc_id callstack in
        let result =
          Exec_res.RFail { proc; proc_idx; error_state; errors; loc }
        in
        let results =
          (cconf.prev_cmd_report_id, branch_path, result) :: results
        in
        if !Config.debug then end_of_branch results eval_step_state
        else
          continue_or_pause rest_confs
            (fun ?selector () -> f rest_confs selector results)
            eval_step_state

      let finish (cconf : CConf.finish) eval_step_state =
        let { results; rest_confs; f; _ } = eval_step_state in
        let { flag; ret_val; final_state; branch_path; loc; _ } = cconf in
        let result =
          Exec_res.RSucc
            { flag; ret_val; final_state; last_report = L.Parent.get (); loc }
        in
        let results =
          (cconf.prev_cmd_report_id, branch_path, result) :: results
        in
        if !Config.debug then end_of_branch results eval_step_state
        else
          continue_or_pause rest_confs
            (fun ?selector () -> f rest_confs selector results)
            eval_step_state

      let susp (cconf : CConf.susp) eval_step_state =
        let {
          eval_step;
          ret_fun;
          retry;
          prog;
          hold_results;
          on_hold;
          results;
          rest_confs;
          _;
        } =
          eval_step_state
        in
        let {
          spec_id = fid;
          state;
          callstack;
          invariant_frames;
          prev_idx;
          loop_ids;
          next_idx;
          branch_count;
          branch_path;
          loc;
          prev_cmd_report_id;
          _;
        } =
          cconf
        in
        let conf =
          CConf.make_cont ~state ~callstack ~invariant_frames ~prev_idx ?loc
            ?prev_cmd_report_id ~loop_ids ~next_idx ~branch_count ~branch_path
            ()
        in
        L.(
          verbose (fun m ->
              m "Suspending a computation that was trying to call %s" fid));
        continue_or_pause []
          (fun ?selector () ->
            eval_step ret_fun retry prog hold_results ((conf, fid) :: on_hold)
              rest_confs selector results)
          eval_step_state
    end

    let rec eval_step
        (ret_fun : result_t -> 'a)
        (retry : bool)
        (prog : annot MP.prog)
        (hold_results : 'a list)
        (on_hold : (CConf.t * string) list)
        (confs : CConf.t list)
        (selector : conf_selector option)
        (results : results) : 'a cont_func =
      let f = eval_step ret_fun retry prog hold_results on_hold in
      let parent_id_ref = ref None in

      Fun.protect
        ~finally:(fun () -> L.Parent.release !parent_id_ref)
        (fun () ->
          let conf, rest_confs = select_conf selector confs in
          let branch_path = Option.map get_branch_path conf in
          let eval_step_state =
            {
              ret_fun;
              retry;
              prog;
              hold_results;
              on_hold;
              branch_path;
              results;
              conf;
              rest_confs;
              parent_id_ref;
              f;
              eval_step;
              selector;
            }
          in
          debug_log conf rest_confs;

          match conf with
          | None -> Handle_conf.none eval_step_state
          | Some (ConfCont ({ branch_count; _ } as c))
            when branch_count < !Config.max_branching ->
              Handle_conf.cont c eval_step_state
          | Some (ConfCont c) -> Handle_conf.max_branch c eval_step_state
          | Some (ConfErr c) -> Handle_conf.err c eval_step_state
          | Some (ConfFinish c) -> Handle_conf.finish c eval_step_state
          | Some (ConfSusp c) when retry -> Handle_conf.susp c eval_step_state
          | Some _ ->
              continue_or_pause rest_confs
                (fun ?selector () -> f rest_confs selector results)
                eval_step_state)
  end

  let evaluate_cmd_step = Evaluate_cmd_step.eval_step

  (**
  Evaluates commands iteratively

  @param init_func The initial continuation function which evaluates the first
                   step of the program
*)
  let rec evaluate_cmd_iter (init_func : 'a cont_func) : 'a list =
    match init_func with
    | Finished results -> results
    | Continue { cont_func; _ } -> evaluate_cmd_iter (cont_func ())
    | EndOfBranch _ ->
        failwith "HORROR: EndOfBranch encountered in continuous eval!"

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
      (prog : annot MP.prog)
      (name : string)
      (params : string list)
      (state : State.t) : 'a cont_func =
    let () = Call_graph.add_proc call_graph name in
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
          | None ->
              raise (Failure "Symbolic State does NOT contain formal parameter"))
        params
    in
    let cs : Call_stack.t =
      Call_stack.push Call_stack.empty ~pid:name ~arguments ~loop_ids:[]
        ~ret_var:"out" ~call_index:(-1) ~continue_index:(-1) ~error_index:(-1)
        ()
    in
    let proc_body_index = 0 in
    let conf : CConf.t =
      CConf.make_cont ~state ~callstack:cs ~invariant_frames:[] ~prev_idx:(-1)
        ~loop_ids:[] ~next_idx:proc_body_index ~branch_count:0 ~branch_path:[]
        ()
    in
    let report_id =
      CmdResult.log_init
        {
          callstack = cs;
          proc_body_index;
          state = Some state;
          errors = [];
          branch_case = None;
        }
    in
    Continue
      {
        report_id;
        branch_path = [];
        new_branch_cases = [];
        cont_func =
          (fun ?selector () ->
            evaluate_cmd_step ret_fun true prog [] [] [ conf ] selector []);
      }

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
      (prog : annot MP.prog)
      (name : string)
      (params : string list)
      (state : State.t) : 'a list =
    let init_func = init_evaluate_proc ret_fun prog name params state in
    evaluate_cmd_iter init_func

  (* Checks for memory leaks.
     This check might not raise an issue even though there
     is a leak.
  *)
  let check_leaks result =
    match result with
    | Exec_res.RSucc { final_state; loc; _ }
      when State.sure_is_nonempty final_state ->
        Exec_res.RFail
          {
            proc = "Memory Leak Check post-execution";
            proc_idx = -1;
            error_state = final_state;
            errors = [ Exec_err.ELeak ];
            loc;
          }
    | _ -> result
end
