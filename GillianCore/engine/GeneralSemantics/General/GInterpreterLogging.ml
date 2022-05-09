module Types = struct
  type 'state_vt branch_case' =
    | GuardedGoto of bool
    | LCmd of int
    | SpecExec of Flag.t
    | LAction of 'state_vt list
    | LActionFail of int
  [@@deriving yojson]
end

open Types

module type S = sig
  module CallStack : CallStack.S

  type vt
  type st
  type store_t
  type state_t
  type state_err_t
  type state_vt
  type heap_t
  type cs

  module Val : Val.S with type t = vt
  module Store : Store.S with type t = store_t and type vt = vt

  type invariant_frames = (string * state_t) list
  type err_t

  type branch_case = state_vt branch_case'
  type 'a fmt = Format.formatter -> 'a -> unit

  module CmdStep : sig
    type t = {
      callstack : cs;
      proc_body_index : int;
      state : state_t option;
      errors : err_t list;
      branch_case : branch_case option;
    }
    [@@deriving yojson]
  end

  module ConfigReport : sig
    type t = {
      proc_line : int;
      time : float;
      cmd : string;
      callstack : cs;
      annot : Annot.t;
      branching : int;
      state : state_t;
      branch_case : branch_case option;
    } [@@deriving yojson, make]

    val log : t fmt -> t -> Logging.ReportId.t option
  end
end

module Make
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (CallStack : CallStack.S) =
struct
  (* *************** *
   * Auxiliary Types *
   * *************** *)

  module CallStack = CallStack
  module Val = Val
  module State = State
  module Store = Store

  module L = Logging

  type vt = Val.t
  type st = ESubst.t
  type store_t = Store.t
  type state_t = State.t [@@deriving yojson]
  type state_err_t = State.err_t
  type state_vt = State.vt [@@deriving yojson]
  type heap_t = State.heap_t
  type cs = CallStack.t [@@deriving yojson]
  type invariant_frames = (string * State.t) list
  type err_t = (Val.t, State.err_t) ExecErr.t [@@deriving yojson]

  type branch_case = state_vt branch_case' [@@deriving yojson]
  type 'a fmt = Format.formatter -> 'a -> unit

  module CmdStep = struct
    type t = {
      callstack : cs;
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

    let log type_ cmd_step = L.normal_specific
      (L.Loggable.make pp of_yojson to_yojson cmd_step)
      type_

    let log_step = log L.LoggingConstants.ContentType.cmd_step

    let log_result = log L.LoggingConstants.ContentType.cmd_result
  end

  module ConfigReport = struct
    type t = {
      proc_line : int;
      time : float;
      cmd : string;
      callstack : cs;
      annot : Annot.t;
      branching : int;
      state : state_t;
      branch_case : branch_case option;
    } [@@deriving yojson, make]

    let log pp report = L.normal_specific
      (L.Loggable.make pp of_yojson to_yojson report)
      L.LoggingConstants.ContentType.cmd
  end
end