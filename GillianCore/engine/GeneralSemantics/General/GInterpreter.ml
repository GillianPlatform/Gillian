open Literal
module L = Logging
module DL = Debugger_log

type 'state_vt branch_case' =
  | GuardedGoto of bool
  | LCmd of int
  | SpecExec of Flag.t
  | LAction of 'state_vt list
  | LActionFail of int
[@@deriving yojson]

module type S = sig
  module CallStack : CallStack.S

  type vt
  type st
  type store_t
  type state_t
  type state_err_t [@@deriving show]
  type state_vt [@@deriving show]
  type heap_t

  module Val : Val.S with type t = vt
  module Store : Store.S with type t = store_t and type vt = vt

  type invariant_frames = (string * state_t) list
  type err_t = (vt, state_err_t) ExecErr.t [@@deriving show, yojson]
  type branch_case = state_vt branch_case' [@@deriving yojson]
  type branch_path = branch_case list [@@deriving yojson]

  type cconf_t =
    | ConfErr of {
        callstack : CallStack.t;
        proc_idx : int;
        error_state : state_t;
        errors : err_t list;
        branch_path : branch_path;
      }
    | ConfCont of {
        state : state_t;
        callstack : CallStack.t;
        invariant_frames : invariant_frames;
        prev_idx : int;
        next_idx : int;
        loop_ids : string list;
        branch_count : int;
        prev_cmd_report_id : L.ReportId.t option;
        branch_case : branch_case option;
        branch_path : branch_path;
        new_branches : (state_t * int * branch_case) list;
      }
    | ConfFinish of {
        flag : Flag.t;
        ret_val : state_vt;
        final_state : state_t;
        branch_path : branch_path;
      }  (** Equal to Conf cont + the id of the required spec *)
    | ConfSusp of {
        spec_id : string;
        state : state_t;
        callstack : CallStack.t;
        invariant_frames : invariant_frames;
        prev_idx : int;
        next_idx : int;
        loop_ids : string list;
        branch_count : int;
        branch_path : branch_path;
      }

  type conf_t = BConfErr of err_t list | BConfCont of state_t
  type result_t = (state_t, state_vt, err_t) ExecRes.t

  type 'a cont_func_f = ?path:branch_path -> unit -> 'a cont_func

  and 'a cont_func =
    | Finished of 'a list
    | Continue of
        (Logging.ReportId.t option
        * branch_path
        * branch_case list option
        * 'a cont_func_f)
    | EndOfBranch of 'a * 'a cont_func_f

  module Logging : sig
    module ConfigReport : sig
      type t = {
        proc_line : int;
        time : float;
        cmd : string;
        callstack : CallStack.t;
        annot : Annot.t;
        branching : int;
        state : state_t;
        branch_case : branch_case option;
      }
      [@@deriving yojson]
    end

    module CmdResult : sig
      type t = {
        callstack : CallStack.t;
        proc_body_index : int;
        state : state_t option;
        errors : err_t list;
        branch_case : branch_case option;
      }
      [@@deriving yojson]
    end

    val pp_err : Format.formatter -> (vt, state_err_t) ExecErr.t -> unit
    val pp_result : Format.formatter -> result_t list -> unit
  end

  val call_graph : CallGraph.t
  val reset : unit -> unit
  val evaluate_lcmds : UP.prog -> LCmd.t list -> state_t -> state_t list

  val init_evaluate_proc :
    (result_t -> 'a) ->
    UP.prog ->
    string ->
    string list ->
    state_t ->
    'a cont_func

  val evaluate_proc :
    (result_t -> 'a) -> UP.prog -> string -> string list -> state_t -> 'a list

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
  type state_err_t = State.err_t [@@deriving yojson]

  let pp_state_err_t = State.pp_err
  let show_state_err_t = Fmt.to_to_string pp_state_err_t

  type state_vt = State.vt [@@deriving yojson, show]
  type heap_t = State.heap_t
  type invariant_frames = (string * State.t) list [@@deriving yojson]
  type err_t = (Val.t, state_err_t) ExecErr.t [@@deriving show, yojson]
  type branch_case = state_vt branch_case' [@@deriving yojson]
  type branch_path = branch_case list [@@deriving yojson]

  (** Type of configurations: state, call stack, previous index, previous loop ids, current index, branching *)
  type cconf_t =
    | ConfErr of {
        callstack : CallStack.t;
        proc_idx : int;
        error_state : state_t;
        errors : err_t list;
        branch_path : branch_path;
      }
    | ConfCont of {
        state : State.t;
        callstack : CallStack.t;
        invariant_frames : invariant_frames;
        prev_idx : int;
        next_idx : int;
        loop_ids : string list;
        branch_count : int;
        prev_cmd_report_id : L.ReportId.t option;
        branch_case : branch_case option;
        branch_path : branch_path;
        new_branches : (state_t * int * branch_case) list;
      }
    | ConfFinish of {
        flag : Flag.t;
        ret_val : State.vt;
        final_state : State.t;
        branch_path : branch_path;
      }  (** Equal to Conf cont + the id of the required spec *)
    | ConfSusp of {
        spec_id : string;
        state : state_t;
        callstack : CallStack.t;
        invariant_frames : invariant_frames;
        prev_idx : int;
        next_idx : int;
        loop_ids : string list;
        branch_count : int;
        branch_path : branch_path;
      }
  [@@deriving yojson]

  let make_confcont
      ~state
      ~callstack
      ~invariant_frames
      ~prev_idx
      ~next_idx
      ~loop_ids
      ~branch_count
      ~branch_path
      ?prev_cmd_report_id
      ?branch_case
      ?(new_branches = [])
      () =
    (* We only want to track branches for the base function. *)
    let branch_case, new_branches =
      if List.length callstack > 1 then (None, [])
      else (branch_case, new_branches)
    in
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
        prev_cmd_report_id;
        branch_case;
        new_branches;
      }

  let cconf_path = function
    | ConfErr { branch_path; _ } -> branch_path
    | ConfFinish { branch_path; _ } -> branch_path
    | ConfSusp { branch_path; _ } -> branch_path
    | ConfCont { branch_path; branch_case; _ } ->
        List_utils.cons_opt branch_case branch_path

  type conf_t = BConfErr of err_t list | BConfCont of State.t
  type result_t = (State.t, State.vt, err_t) ExecRes.t [@@deriving yojson]

  type 'a cont_func_f = ?path:branch_path -> unit -> 'a cont_func

  and 'a cont_func =
    | Finished of 'a list
    | Continue of
        (Logging.ReportId.t option
        * branch_path
        * branch_case list option
        * 'a cont_func_f)
    | EndOfBranch of 'a * 'a cont_func_f

  module Logging = struct
    let pp_str_list = Fmt.(brackets (list ~sep:comma string))

    module ConfigReport = struct
      type t = {
        proc_line : int;
        time : float;
        cmd : string;
        callstack : CallStack.t;
        annot : Annot.t;
        branching : int;
        state : state_t;
        branch_case : branch_case option;
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
           CMD: %s@\n\
           PROCS: %a@\n\
           LOOPS: %a ++ %a@\n\
           BRANCHING: %d@\n\
           @\n\
           %a@\n\
           ------------------------------------------------------@]\n"
          (CallStack.get_cur_proc_id cs)
          i time cmd pp_str_list
          (CallStack.get_cur_procs cs)
          pp_str_list
          (Annot.get_loop_info annot)
          pp_str_list
          (CallStack.get_loop_ids cs)
          branching state_printer state

      let to_loggable state_printer =
        L.Loggable.make (pp state_printer) of_yojson to_yojson

      let log state_printer report =
        L.normal_specific
          (to_loggable state_printer report)
          L.LoggingConstants.ContentType.cmd
    end

    module CmdResult = struct
      type t = {
        callstack : CallStack.t;
        proc_body_index : int;
        state : state_t option;
        errors : err_t list;
        branch_case : branch_case option;
      }
      [@@deriving yojson]

      let pp fmt cmd_step =
        (* TODO: Cmd step should contain all things in a configuration
                 print the same contents as log_configuration *)
        CallStack.pp fmt cmd_step.callstack

      let to_loggable = L.Loggable.make pp of_yojson to_yojson
      let log type_ report = L.normal_specific (to_loggable report) type_

      open L.LoggingConstants.ContentType

      let log_result = log cmd_result
      let log_init = log proc_init
      let log_step = log cmd_step
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
        L.normal_specific (to_loggable report)
          L.LoggingConstants.ContentType.annotated_action
    end

    let log_configuration
        (cmd : Annot.t * int Cmd.t)
        (state : State.t)
        (cs : CallStack.t)
        (i : int)
        (b_counter : int)
        (branch_case : branch_case option) : L.ReportId.t option =
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
        (ConfigReport.make ~proc_line:i ~time:(Sys.time ())
           ~cmd:(Fmt.to_to_string Cmd.pp_indexed cmd)
           ~callstack:cs ~annot ~branching:b_counter ~state ?branch_case ())

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

    let pp_err = ExecErr.pp Val.pp State.pp_err
    let pp_single_result ft res = ExecRes.pp State.pp Val.pp pp_err ft res

    (** Configuration pretty-printer *)
    let pp_result (ft : Format.formatter) (reslt : result_t list) : unit =
      let open Fmt in
      let pp_one ft (i, res) =
        pf ft "RESULT: %d.@\n%a" i pp_single_result res
      in
      (iter_bindings List.iteri pp_one) ft reslt
  end

  open Logging

  let max_branching = 100

  exception Interpreter_error of err_t list * State.t

  (** Internal error, carrying a string description *)
  exception Internal_error of string

  (** Syntax error, carrying a string description *)
  exception Syntax_error of string

  let call_graph = CallGraph.make ~init_capacity:128 ()
  let reset () = CallGraph.reset call_graph

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
      | None -> raise (Failure ("Procedure " ^ pid ^ " does not exist."))
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
      raise (Interpreter_error (List.map (fun x -> ExecErr.ESt x) errs, s))

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
            | _ ->
                raise
                  (Failure
                     (Printf.sprintf
                        "ERROR: AssumeType: Cannot assume type %s for variable \
                         %s."
                        (Type.str t) x)))
        | _ ->
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
        List.concat
          (List.map
             (fun (f'', state) ->
               match State.assume_a state [ f'' ] with
               | Some state' -> [ state' ]
               | _ -> [])
             fos)
    | SpecVar xs -> [ State.add_spec_vars state (Var.Set.of_list xs) ]
    | Assert f -> (
        let store_subst = Store.to_ssubst (State.get_store state) in
        let f' = SVal.SESubst.substitute_formula store_subst ~partial:true f in
        match State.assert_a state [ f' ] with
        | true -> [ state ]
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
        | None ->
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
        | Some (foe, nfoe) ->
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
        | None ->
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
    | [] -> [ state ]
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
      (b_counter : int)
      (report_id_ref : L.ReportId.t option ref)
      (branch_path : branch_path)
      (branch_case : branch_case option) : cconf_t list =
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
        i b_counter report_id_ref branch_path branch_case
    in
    match loop_action with
    | Nothing -> eval_in_state state
    | FrameOff id ->
        L.verbose (fun fmt -> fmt "INFO: Expecting to frame off %s" id);
        eval_in_state state
    | Malformed -> L.fail "Malformed loop identifiers"
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
      (b_counter : int)
      (report_id_ref : L.ReportId.t option ref)
      (branch_path : branch_path)
      (branch_case : branch_case option) : cconf_t list =
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

    log_configuration annot_cmd state cs i b_counter branch_case
    |> Option.iter (fun report_id ->
           report_id_ref := Some report_id;
           L.set_parent report_id);

    let branch_path = List_utils.cons_opt branch_case branch_path in
    let make_confcont =
      make_confcont ?prev_cmd_report_id:!report_id_ref ~branch_path
    in
    DL.log (fun m ->
        m
          ~json:[ ("path", branch_path_to_yojson branch_path) ]
          "GInterpreter: stepping with path");

    let evaluate_procedure_call x pid v_args j subst =
      let pid =
        match Val.to_literal pid with
        | Some (String pid) -> pid
        | Some _ ->
            let err = [ ExecErr.EProc pid ] in
            raise (Interpreter_error (err, state))
        | None ->
            raise
              (Internal_error
                 "Procedure Call Error - unlifting procedure ID failed")
      in

      let proc = Prog.get_proc prog.prog pid in
      let spec = Hashtbl.find_opt prog.specs pid in
      let params =
        match (proc, spec) with
        | Some proc, _ -> Proc.get_params proc
        | None, Some spec -> Spec.get_params spec.spec
        | _ ->
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

      let process_ret is_first ret_state fl b_counter others : cconf_t =
        let new_cs =
          match is_first with
          | true -> CallStack.copy cs
          | false -> cs
        in

        let new_j =
          match (fl, j) with
          | Flag.Normal, _ -> i + 1
          | Flag.Error, Some j -> j
          | Flag.Error, None ->
              let msg =
                Printf.sprintf
                  "SYNTAX ERROR: No error label provided when calling \
                   procedure %s"
                  pid
              in
              L.normal (fun fmt -> fmt "%s" msg);
              raise (Syntax_error msg)
        in

        let branch_case = SpecExec fl in
        let branch_case, new_branches =
          match (is_first, others) with
          | _, Some (_ :: _ as others) ->
              let new_branches =
                Some
                  (List_utils.get_list_somes
                  @@ List.map
                       (fun conf ->
                         match conf with
                         | ConfCont { state; next_idx; _ } ->
                             Some (state, next_idx, branch_case)
                         | _ -> None)
                       others)
              in
              (Some branch_case, new_branches)
          | false, _ -> (Some branch_case, None)
          | _ -> (None, None)
        in

        make_confcont ~state:ret_state ~callstack:new_cs
          ~invariant_frames:iframes ~prev_idx:i ~loop_ids ~next_idx:new_j
          ~branch_count:b_counter ?branch_case ?new_branches ()
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
        [
          make_confcont ~state:state' ~callstack:cs' ~invariant_frames:iframes
            ~prev_idx:(-1) ~loop_ids ~next_idx:0 ~branch_count:b_counter ();
        ]
      in

      let spec_exec_proc () =
        match spec with
        | Some spec -> (
            match !symb_exec_next with
            | true ->
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
                    let others =
                      List.map
                        (fun (ret_state, fl) ->
                          process_ret false ret_state fl b_counter None)
                        rest_rets
                    in
                    process_ret true ret_state fl b_counter (Some others)
                    :: others
                (* Run spec returned no results *)
                | _ -> (
                    match spec.spec.spec_incomplete with
                    | true ->
                        L.normal (fun fmt ->
                            fmt "Proceeding with symbolic execution.");
                        symb_exec_proc ()
                    | false ->
                        L.fail
                          (Format.asprintf
                             "ERROR: Unable to use specification of function %s"
                             spec.spec.spec_name))))
        | None ->
            if Hashtbl.mem prog.prog.bi_specs pid then
              [
                ConfSusp
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
                  };
              ]
            else symb_exec_proc ()
      in

      match ExecMode.biabduction_exec !Config.current_exec_mode with
      | true -> (
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
    | Skip ->
        [
          make_confcont ~state ~callstack:cs ~invariant_frames:iframes
            ~prev_idx:i ~loop_ids ~next_idx:(i + 1) ~branch_count:b_counter ();
        ]
    (* Assignment *)
    | Assignment (x, e) ->
        DL.log (fun m ->
            m
              ~json:[ ("target", `String x); ("expr", Expr.to_yojson e) ]
              "Assignment");
        let v = eval_expr e in
        let state' = update_store state x v in
        [
          make_confcont ~state:state' ~callstack:cs ~invariant_frames:iframes
            ~prev_idx:i ~loop_ids ~next_idx:(i + 1) ~branch_count:b_counter ();
        ]
    (* Action *)
    | LAction (x, a, es) -> (
        DL.log (fun m ->
            m
              ~json:
                [
                  ("x", `String x);
                  ("a", `String a);
                  ("es", `List (List.map Expr.to_yojson es));
                ]
              "LAction");
        AnnotatedAction.log { annot; action_name = a } |> ignore;
        let v_es = List.map eval_expr es in
        match State.execute_action a state v_es with
        | ASucc [] -> failwith "HORROR: Successful action resulted in no states"
        | ASucc ((state', vs) :: rest_rets) -> (
            DL.log (fun m ->
                m
                  ~json:
                    [
                      ("state'", state_t_to_yojson state');
                      ("vs", `List (List.map state_vt_to_yojson vs));
                    ]
                  "ASucc");
            let e' = Expr.EList (List.map Val.to_expr vs) in
            let v' = eval_expr e' in
            let state'' = update_store state' x v' in
            let rest_confs, new_branches =
              List.split
              @@ List.map
                   (fun (r_state, r_vs) ->
                     let r_e = Expr.EList (List.map Val.to_expr r_vs) in
                     let r_v = eval_expr r_e in
                     let r_state' = update_store r_state x r_v in
                     let branch_case = LAction r_vs in
                     ( make_confcont ~state:r_state'
                         ~callstack:(CallStack.copy cs)
                         ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                         ~next_idx:(i + 1) ~branch_count:b_counter ~branch_case
                         (),
                       (r_state', i + 1, branch_case) ))
                   rest_rets
            in
            let ret_len = 1 + List.length rest_rets in
            let b_counter = b_counter + if ret_len > 1 then 1 else 0 in
            let branch_case = LAction vs in
            match
              (ret_len >= 3 && !Config.parallel, ret_len = 2 && !Config.parallel)
              (* XXX: && !Config.act_threads < !Config.max_threads ) *)
            with
            | true, _ -> (
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
                      make_confcont ~state:state'' ~callstack:cs
                        ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                        ~next_idx:(i + 1) ~branch_count:b_counter ~branch_case
                        ~new_branches ();
                    ])
            | false, true -> (
                (* Can split into two threads *)
                let b_counter = b_counter + 1 in
                (* print_endline (Printf.sprintf "Action returned 2: %d" (!Config.act_threads + 1)); *)
                let pid = Unix.fork () in
                match pid with
                | 0 ->
                    [
                      make_confcont ~state:state'' ~callstack:cs
                        ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                        ~next_idx:(i + 1) ~branch_count:b_counter ~branch_case
                        ~new_branches ();
                    ]
                | _ -> rest_confs)
            | _ ->
                make_confcont ~state:state'' ~callstack:cs
                  ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                  ~next_idx:(i + 1) ~branch_count:b_counter ()
                :: rest_confs)
        | AFail errs ->
            DL.log (fun m ->
                m
                  ~json:
                    [ ("errs", `List (List.map state_err_t_to_yojson errs)) ]
                  "AFail");
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
                  List.mapi
                    (fun ix state ->
                      let branch_case =
                        if List.length recovery_states > 1 then
                          Some (LActionFail ix)
                        else None
                      in
                      let new_branches =
                        match (ix, recovery_states) with
                        | 0, _ :: rest ->
                            Some
                              (List.mapi
                                 (fun ix state ->
                                   (state, i, LActionFail (ix + 1)))
                                 rest)
                        | _ -> None
                      in
                      make_confcont ~state ~callstack:cs
                        ~invariant_frames:iframes ~prev_idx:prev
                        ~loop_ids:prev_loop_ids ~next_idx:i
                        ~branch_count:b_counter ?branch_case ?new_branches ())
                    recovery_states
              | _ ->
                  L.normal ~title:"failure" ~severity:Error (fun m ->
                      m "Action call failed with:@.%a"
                        (Fmt.Dump.list State.pp_err)
                        errs);
                  raise (State.Internal_State_Error (errs, state)))
            else Fmt.failwith "Local Action Failed: %a" Cmd.pp_indexed cmd)
    (* Logic command *)
    | Logic lcmd -> (
        DL.log (fun m -> m "LCmd");
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
                make_confcont ~state ~callstack:cs ~invariant_frames:iframes
                  ~prev_idx:i ~loop_ids ~next_idx:(i + 1)
                  ~branch_count:b_counter ())
              frames_and_states
        | _ ->
            let resulting_states : State.t list =
              evaluate_lcmd prog lcmd state
            in
            let b_counter =
              if List.length resulting_states > 1 then b_counter + 1
              else b_counter
            in
            List.mapi
              (fun ix state ->
                let branch_case =
                  if List.length resulting_states > 1 then Some (LCmd ix)
                  else None
                in
                let new_branches =
                  match (ix, resulting_states) with
                  | 0, _ :: rest ->
                      Some
                        (List.mapi
                           (fun ix state -> (state, i, LCmd (ix + 1)))
                           rest)
                  | _ -> None
                in
                make_confcont ~state ~callstack:cs ~invariant_frames:iframes
                  ~prev_idx:i ~loop_ids ~next_idx:(i + 1)
                  ~branch_count:b_counter ?branch_case ?new_branches ())
              resulting_states)
    (* Unconditional goto *)
    | Goto j ->
        [
          make_confcont ~state ~callstack:cs ~invariant_frames:iframes
            ~prev_idx:i ~loop_ids ~next_idx:j ~branch_count:b_counter ();
        ]
    (* Conditional goto *)
    | GuardedGoto (e, j, k) -> (
        let vt = eval_expr e in
        let lvt = Val.to_literal vt in
        let vf =
          match lvt with
          | Some (Bool true) -> vfalse
          | Some (Bool false) -> vtrue
          | _ -> eval_expr (UnOp (UNot, e))
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
        let result =
          sp
          |> List.mapi (fun j ((state, next), case) ->
                 make_confcont ~state
                   ~callstack:(if j = 0 then cs else CallStack.copy cs)
                   ~invariant_frames:iframes ~prev_idx:i ~loop_ids
                   ~next_idx:next ~branch_count:b_counter
                   ~branch_case:(GuardedGoto case)
                   ~new_branches:
                     (if j = 0 then
                      List.map
                        (fun ((state, next), case) ->
                          (state, next, GuardedGoto case))
                        (List.tl sp)
                     else [])
                   ())
        in
        match
          List.length result = 2 && !Config.parallel
          (* XXX: && !Config.act_threads < !Config.max_threads *)
        with
        | true -> (
            (* print_endline (Printf.sprintf "Conditional goto: %d" (!Config.act_threads + 1)); *)
            let pid = Unix.fork () in
            match pid with
            | 0 -> [ List.hd result ]
            | _ -> List.tl result)
        | false -> result)
    | PhiAssignment lxarr ->
        DL.log (fun m -> m "PhiAssignment");
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
    | Call (x, e, args, j, subst) ->
        DL.log (fun m -> m "Call");
        let pid = eval_expr e in
        let v_args = List.map eval_expr args in
        let result = evaluate_procedure_call x pid v_args j subst in
        result
    (* External function call *)
    | ECall (x, pid, args, j) ->
        DL.log (fun m -> m "ECall");
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
    | Apply (x, pid_args, j) -> (
        DL.log (fun m -> m "Apply");
        let v_pid_args = eval_expr pid_args in
        let v_pid_args_list = Val.to_list v_pid_args in
        match v_pid_args_list with
        | Some v_pid_args_list ->
            let pid = List.hd v_pid_args_list in
            let v_args = List.tl v_pid_args_list in
            evaluate_procedure_call x pid v_args j None
        | None ->
            raise
              (Failure
                 (Fmt.str "Apply not called with a list: @[<h>%a@]" Val.pp
                    v_pid_args)))
    (* Arguments *)
    | Arguments x ->
        DL.log (fun m -> m "Arguments");
        let args = CallStack.get_cur_args cs in
        let args = Val.from_list args in
        let state' = update_store state x args in
        [
          make_confcont ~state:state' ~callstack:cs ~invariant_frames:iframes
            ~prev_idx:i ~loop_ids ~next_idx:(i + 1) ~branch_count:b_counter ();
        ]
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
              [
                ConfFinish
                  {
                    flag = Normal;
                    ret_val = v_ret;
                    final_state = state;
                    branch_path;
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
              let ( let+ ) x f = List.map f x in
              let+ state =
                if ExecMode.verification_exec !Config.current_exec_mode then
                  State.frame_on state iframes to_frame_on
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
    (* Error-mode return *)
    | ReturnError -> (
        let v_ret = Store.get store Names.return_variable in
        match (v_ret, cs) with
        | None, _ ->
            raise (Failure "Return variable not in store (error return) ")
        | Some v_ret, { store = None; loop_ids = start_loop_ids; _ } :: _ ->
            check_loop_ids loop_ids start_loop_ids;
            Fmt.pr "e @?";
            [
              ConfFinish
                {
                  flag = Error;
                  ret_val = v_ret;
                  final_state = state;
                  branch_path : branch_path;
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
              if ExecMode.verification_exec !Config.current_exec_mode then
                State.frame_on state iframes to_frame_on
              else [ state ]
            in
            let state' = State.set_store state old_store in
            let state'' = update_store state' x v_ret in
            make_confcont ~state:state'' ~callstack:cs'
              ~invariant_frames:iframes ~prev_idx:prev' ~loop_ids:start_loop_ids
              ~next_idx:j ~branch_count:b_counter ()
        | _ -> raise (Failure "Malformed callstack"))
    (* Explicit failure *)
    | Fail (fail_code, fail_params) ->
        let fail_params = List.map (State.eval_expr state) fail_params in
        let err = ExecErr.EFailReached { fail_code; fail_params } in
        raise (Interpreter_error ([ err ], state))

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
      (b_counter : int)
      (report_id_ref : L.ReportId.t option ref)
      (branch_path : branch_path)
      (branch_case : branch_case option) : cconf_t list =
    let states =
      match get_cmd prog cs i with
      | _, (_, LAction _) -> simplify state
      | _ -> [ state ]
    in
    List.concat_map
      (fun state ->
        try
          evaluate_cmd prog state cs iframes prev prev_loop_ids i b_counter
            report_id_ref branch_path branch_case
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
                  errors = List.map (fun x -> ExecErr.ESt x) errs;
                  branch_path = List_utils.cons_opt branch_case branch_path;
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
  let rec evaluate_cmd_step
      (ret_fun : result_t -> 'a)
      (retry : bool)
      (prog : UP.prog)
      (hold_results : 'a list)
      (on_hold : (cconf_t * string) list)
      (confs : cconf_t list)
      (branch_path : branch_path option)
      (results : (branch_path * result_t) list) : 'a cont_func =
    let f = evaluate_cmd_step ret_fun retry prog hold_results on_hold in
    let parent_id_ref = ref None in

    let log_confcont is_first = function
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
          if is_first then (
            prev_cmd_report_id
            |> Option.iter (fun prev_report_id ->
                   L.release_parent !parent_id_ref;
                   L.set_parent prev_report_id;
                   parent_id_ref := Some prev_report_id);
            DL.log (fun m ->
                m
                  ~json:[ ("conf", CmdResult.to_yojson cmd_step) ]
                  "Debugger.evaluate_cmd_step: New ConfCont"));
          CmdResult.log_result cmd_step |> ignore;
          Some cmd_step
      | _ -> None
    in

    let continue_or_pause rest_confs cont_func =
      match rest_confs with
      | ConfCont { branch_case; new_branches; branch_path; _ } :: _ ->
          rest_confs
          |> List.iteri (fun i conf -> log_confcont (i = 0) conf |> ignore);
          let new_branch_cases =
            branch_case
            |> Option.map (fun branch_case ->
                   branch_case
                   :: (new_branches |> List.map (fun (_, _, case) -> case)))
          in
          Continue (!parent_id_ref, branch_path, new_branch_cases, cont_func)
      | ConfErr
          {
            callstack;
            proc_idx = proc_body_index;
            error_state = state;
            errors;
            branch_path;
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
          Continue (!parent_id_ref, branch_path, None, cont_func)
      | _ ->
          if !Config.debug then
            let branch_path = Option.value branch_path ~default:[] in
            Continue (!parent_id_ref, branch_path, None, cont_func)
          else cont_func ()
    in

    Fun.protect
      ~finally:(fun () -> L.release_parent !parent_id_ref)
      (fun () ->
        let conf, rest_confs =
          match branch_path with
          | None ->
              DL.log (fun m ->
                  m "HORROR: branch_path shouldn't be None when debugging!");
              List_utils.hd_tl confs
          | Some branch_path ->
              confs
              |> List_utils.pop_where (fun conf ->
                     cconf_path conf = branch_path)
        in

        DL.log (fun m ->
            let conf_json =
              match conf with
              | None -> `Null
              | Some conf -> cconf_t_to_yojson conf
            in
            m
              ~json:
                [
                  ("conf", conf_json);
                  ("rest_confs", `List (List.map cconf_t_to_yojson rest_confs));
                ]
              "GInterpreter.evaluate_cmd_step: Evaluating conf");

        let end_of_branch ?branch_case results =
          match branch_path with
          | None ->
              failwith "HORROR: branch_path shouldn't be None when debugging!"
          | Some branch_path -> (
              match results |> List.assoc_opt branch_path with
              | None ->
                  DL.failwith
                    (fun () ->
                      let result_jsons =
                        results
                        |> List.map (fun (path, result) ->
                               `Assoc
                                 [
                                   ("path", branch_path_to_yojson path);
                                   ("result", result_t_to_yojson result);
                                 ])
                      in
                      let conf_json =
                        match conf with
                        | None -> `Null
                        | Some conf -> cconf_t_to_yojson conf
                      in
                      [
                        ("branch_path", branch_path_to_yojson branch_path);
                        ( "branch_case",
                          opt_to_yojson branch_case_to_yojson branch_case );
                        ("results", `List result_jsons);
                        ("conf", conf_json);
                        ( "rest_confs",
                          `List (List.map cconf_t_to_yojson rest_confs) );
                      ])
                    "No result for branch path!"
              | Some result ->
                  EndOfBranch
                    (ret_fun result, fun ?path () -> f rest_confs path results))
        in

        match (conf, rest_confs) with
        | None, _ :: _ -> end_of_branch results
        | None, [] when !Config.debug -> end_of_branch results
        | None, [] ->
            let results =
              List.map (fun (_, result) -> ret_fun result) results
            in
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
              continue_or_pause hold_confs (fun ?path () ->
                  evaluate_cmd_step ret_fun false prog results [] hold_confs
                    path []))
        | ( Some
              (ConfCont
                {
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
                  _;
                }),
            _ )
          when b_counter < max_branching ->
            L.set_previous prev_cmd_report_id;
            let next_confs =
              protected_evaluate_cmd prog state cs iframes prev prev_loop_ids i
                b_counter parent_id_ref branch_path branch_case
            in
            continue_or_pause next_confs (fun ?path () ->
                f (next_confs @ rest_confs) path results)
        | ( Some
              (ConfCont
                {
                  state;
                  callstack = cs;
                  next_idx = i;
                  branch_count = b_counter;
                  prev_cmd_report_id;
                  branch_case;
                  _;
                }),
            _ ) ->
            let _, annot_cmd = get_cmd prog cs i in
            Printf.printf "WARNING: MAX BRANCHING STOP: %d.\n" b_counter;
            L.set_previous prev_cmd_report_id;
            L.(
              verbose (fun m ->
                  m
                    "Stopping Symbolic Execution due to MAX BRANCHING with %d. \
                     STOPPING CONF:\n"
                    b_counter));
            log_configuration annot_cmd state cs i b_counter branch_case
            |> Option.iter (fun report_id ->
                   parent_id_ref := Some report_id;
                   L.set_parent report_id);
            continue_or_pause [] (fun ?path () -> f rest_confs path results)
        | ( Some
              (ConfErr
                { callstack; proc_idx; error_state; errors; branch_path }),
            _ ) ->
            let proc = CallStack.get_cur_proc_id callstack in
            let result =
              ExecRes.RFail { proc; proc_idx; error_state; errors }
            in
            let results = (branch_path, result) :: results in
            if !Config.debug then end_of_branch results
            else
              continue_or_pause rest_confs (fun ?path () ->
                  f rest_confs path results)
        | Some (ConfFinish { flag; ret_val; final_state; branch_path }), _ ->
            let result =
              ExecRes.RSucc
                { flag; ret_val; final_state; last_report = L.get_parent () }
            in
            let results = (branch_path, result) :: results in
            if !Config.debug then end_of_branch results
            else
              continue_or_pause rest_confs (fun ?path () ->
                  f rest_confs path results)
        | ( Some
              (ConfSusp
                {
                  spec_id = fid;
                  state;
                  callstack;
                  invariant_frames;
                  prev_idx;
                  loop_ids;
                  next_idx;
                  branch_count;
                  branch_path;
                }),
            _ )
          when retry ->
            let conf =
              make_confcont ~state ~callstack ~invariant_frames ~prev_idx
                ~loop_ids ~next_idx ~branch_count ~branch_path ()
            in
            L.(
              verbose (fun m ->
                  m "Suspending a computation that was trying to call %s" fid));
            continue_or_pause [] (fun ?path () ->
                evaluate_cmd_step ret_fun retry prog hold_results
                  ((conf, fid) :: on_hold) rest_confs path results)
        | Some _, _ ->
            continue_or_pause rest_confs (fun ?path () ->
                f rest_confs path results))

  (**
  Evaluates commands iteratively

  @param init_func The initial continuation function which evaluates the first
                   step of the program
*)
  let rec evaluate_cmd_iter (init_func : 'a cont_func) : 'a list =
    match init_func with
    | Finished results -> results
    | Continue (_, _, _, cont_func) -> evaluate_cmd_iter (cont_func ())
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
          | None ->
              raise (Failure "Symbolic State does NOT contain formal parameter"))
        params
    in
    let cs : CallStack.t =
      CallStack.push CallStack.empty ~pid:name ~arguments ~loop_ids:[]
        ~ret_var:"out" ~call_index:(-1) ~continue_index:(-1) ~error_index:(-1)
        ()
    in
    let proc_body_index = 0 in
    let conf : cconf_t =
      make_confcont ~state ~callstack:cs ~invariant_frames:[] ~prev_idx:(-1)
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
      ( report_id,
        [],
        None,
        fun ?path () ->
          evaluate_cmd_step ret_fun true prog [] [] [ conf ] path [] )

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
    let initial_state = State.init ~preds:prog.preds () in
    let initial_conf =
      make_confcont ~state:initial_state ~callstack:initial_cs
        ~invariant_frames:[] ~prev_idx:(-1) ~loop_ids:[]
        ~next_idx:initial_proc_body_index ~branch_count:0 ~branch_path:[] ()
    in
    let report_id =
      CmdResult.log_step
        {
          callstack = initial_cs;
          proc_body_index = initial_proc_body_index;
          state = Some initial_state;
          errors = [];
          branch_case = None;
        }
    in
    let init_func =
      Continue
        ( report_id,
          [],
          None,
          fun ?path () ->
            evaluate_cmd_step ret_fun true prog [] [] [ initial_conf ] path []
        )
    in
    evaluate_cmd_iter init_func
end
