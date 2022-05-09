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
  type state_err_t
  type state_vt
  type heap_t

  module Val : Val.S with type t = vt
  module Store : Store.S with type t = store_t and type vt = vt

  type invariant_frames = (string * state_t) list
  type err_t = (vt, state_err_t) ExecErr.t [@@deriving yojson]

  type branch_case = state_vt branch_case'

  type cconf_t =
    | ConfErr of {
        callstack : CallStack.t;
        proc_idx : int;
        error_state : state_t;
        errors : err_t list;
      }
    | ConfCont of {
        state : state_t;
        callstack : CallStack.t;
        invariant_frames : invariant_frames;
        prev_idx : int;
        next_idx : int;
        loop_ids : string list;
        branch_count : int;
        prev_report_id : Logging.ReportId.t option;
        branch_case : branch_case option;
        new_branches : (state_t * int * branch_case) list;
      }
    | ConfFinish of { flag : Flag.t; ret_val : state_vt; final_state : state_t }
        (** Equal to Conf cont + the id of the required spec *)
    | ConfSusp of {
        spec_id : string;
        state : state_t;
        callstack : CallStack.t;
        invariant_frames : invariant_frames;
        prev_idx : int;
        next_idx : int;
        loop_ids : string list;
        branch_count : int;
      }

  type conf_t = BConfErr of err_t list | BConfCont of state_t
  type result_t = (state_t, state_vt, err_t) ExecRes.t

  type 'a cont_func =
    | Finished of 'a list
    | Continue of (Logging.ReportId.t option * (unit -> 'a cont_func))

  module Logging : sig
    module CmdStep : sig
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
    (External : External.S) :
  S
    with type vt = Val.t
     and type st = ESubst.t
     and type store_t = Store.t
     and type state_t = State.t
     and type state_err_t = State.err_t
     and type state_vt = State.vt
     and type heap_t = State.heap_t
