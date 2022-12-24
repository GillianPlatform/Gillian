type branch_case = BranchCase.t

module type S = sig
  module CallStack : CallStack.S

  type vt
  type st
  type store_t
  type state_t
  type state_err_t [@@deriving show]
  type state_vt [@@deriving yojson, show]
  type heap_t

  (** Data necessary to initialize the state, language-dependent *)
  type init_data

  type annot

  module Val : Val.S with type t = vt
  module Store : Store.S with type t = store_t and type vt = vt

  type invariant_frames = (string * state_t) list
  type err_t = (vt, state_err_t) ExecErr.t [@@deriving show, yojson]
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
        prev_cmd_report_id : Logging.Report_id.t option;
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
        (Logging.Report_id.t option
        * branch_path
        * branch_case list option
        * 'a cont_func_f)
    | EndOfBranch of 'a * 'a cont_func_f

  module Logging : sig
    module ConfigReport : sig
      type t = {
        proc_line : int;
        time : float;
        cmd : int Cmd.t;
        callstack : CallStack.t;
        annot : annot;
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

  val evaluate_lcmds :
    annot UP.prog ->
    LCmd.t list ->
    state_t ->
    (state_t list, state_err_t list) result

  val init_evaluate_proc :
    (result_t -> 'a) ->
    annot UP.prog ->
    string ->
    string list ->
    state_t ->
    'a cont_func

  val evaluate_proc :
    (result_t -> 'a) ->
    annot UP.prog ->
    string ->
    string list ->
    state_t ->
    'a list
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
