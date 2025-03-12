(** @canonical Gillian.General.G_interpreter.S *)
module type S = sig
  module Call_stack : Call_stack.S

  (** Value type *)
  type vt

  (** Subst type *)
  type st

  type store_t
  type state_t
  type state_err_t [@@deriving show]
  type state_vt [@@deriving yojson, show]
  type heap_t

  (** Data necessary to initialize the state; language-dependent *)
  type init_data

  (** Command annotation; language-dependent *)
  type annot

  module Val : Val.S with type t = vt
  module Store : Store.S with type t = store_t and type vt = vt

  type invariant_frames = (string * state_t) list
  type err_t = (vt, state_err_t) Exec_err.t [@@deriving show, yojson]

  (** Type of configurations: state, call stack, previous index, previous loop ids, current index, branching *)
  module CConf : sig
    type err = {
      callstack : Call_stack.t;
      proc_idx : int;
      error_state : state_t;
      errors : err_t list;
      branch_path : Branch_case.path;
      prev_cmd_report_id : Logging.Report_id.t option;
      loc : Location.t option;
    }

    type cont = {
      state : state_t;
      callstack : Call_stack.t;
      invariant_frames : invariant_frames;
      prev_idx : int;
      next_idx : int;
      loop_ids : string list;
      branch_count : int;
      prev_cmd_report_id : Logging.Report_id.t option;
      branch_case : Branch_case.t option;
      branch_path : Branch_case.path;
      loc : Location.t option;
    }

    (** Equal to conf_cont + the id of the required spec *)
    type finish = {
      flag : Flag.t;
      ret_val : state_vt;
      final_state : state_t;
      branch_path : Branch_case.path;
      prev_cmd_report_id : Logging.Report_id.t option;
      loc : Location.t option;
    }

    type susp = {
      spec_id : string;
      state : state_t;
      callstack : Call_stack.t;
      invariant_frames : invariant_frames;
      prev_idx : int;
      next_idx : int;
      loop_ids : string list;
      branch_count : int;
      branch_path : Branch_case.path;
      prev_cmd_report_id : Logging.Report_id.t option;
      loc : Location.t option;
    }

    type t =
      | ConfErr of err
      | ConfCont of cont
      | ConfFinish of finish
      | ConfSusp of susp
  end

  type conf_t = BConfErr of err_t list | BConfCont of state_t

  (** The result of execution
      
    In the symbolic case, this is the result of {i one branch} of execution *)
  type result_t = (state_t, state_vt, err_t) Exec_res.t

  type conf_selector =
    | Path of Branch_case.path
    | IdCase of Logging.Report_id.t * Branch_case.t option

  (** To support the step-by-step behaviour of the debugger, execution is split into thunks; each invocation executes one GIL command.
    By supplying a branch path, or an ID and branch case, a particular branch of execution can be selected. *)
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

  (** Types and functions for logging to the database *)
  module Logging : sig
    module ConfigReport : sig
      type t = {
        proc_line : int;
        time : float;
        cmd : int Cmd.t;
        callstack : Call_stack.t;
        annot : annot;
        branching : int;
        state : state_t;
        branch_case : Branch_case.t option;
        proc_name : string;
      }
      [@@deriving yojson]

      val pp :
        (Format.formatter -> state_t -> unit) -> Format.formatter -> t -> unit
    end

    module CmdResult : sig
      type t = {
        callstack : Call_stack.t;
        proc_body_index : int;
        state : state_t option;
        errors : err_t list;
        branch_case : Branch_case.t option;
      }
      [@@deriving yojson]
    end

    val pp_err : Format.formatter -> (vt, state_err_t) Exec_err.t -> unit
    val pp_result : Format.formatter -> result_t list -> unit
  end

  val call_graph : Call_graph.t
  val reset_call_graph : unit -> unit

  (** Evaluates a list of logical commands, in the context of a given state *)
  val evaluate_lcmds :
    annot MP.prog ->
    LCmd.t list ->
    ?annot:annot option ->
    state_t ->
    (state_t, state_err_t) Res_list.t

  (** Begins execution of a proc, given parameters and initial state *)
  val init_evaluate_proc :
    (result_t -> 'a) ->
    annot MP.prog ->
    string ->
    string list ->
    state_t ->
    'a cont_func

  (** As with {! init_evaluate_proc}, but immediately executes the proc to completion *)
  val evaluate_proc :
    (result_t -> 'a) ->
    annot MP.prog ->
    string ->
    string list ->
    state_t ->
    'a list

  (* Checks for memory leaks (might not detect all leaks).
     If the result is a success but there is a leak, it is transformed to an error.
  *)
  val check_leaks : result_t -> result_t
end

(** @canonical Gillian.General.G_interpreter *)
module type Intf = sig
  (** @canonical Gillian.General.G_interpreter.S *)
  module type S = S

  module Make
      (Val : Val.S)
      (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
      (Store : Store.S with type vt = Val.t)
      (State : State.S
                 with type vt = Val.t
                  and type st = ESubst.t
                  and type store_t = Store.t)
      (PC : ParserAndCompiler.S)
      (External : External.T(PC.Annot).S) :
    S
      with type vt = Val.t
       and type st = ESubst.t
       and type store_t = Store.t
       and type state_t = State.t
       and type state_err_t = State.err_t
       and type state_vt = State.vt
       and type heap_t = State.heap_t
       and type init_data = State.init_data
       and type annot = PC.Annot.t
end
