module DebuggerTypes = DebuggerTypes
module DebuggerUtils = DebuggerUtils
module Gil_to_tl_lifter = Gil_to_tl_lifter
open DebuggerTypes

module type S = sig
  type tl_ast
  type debug_state

  module PackagedBranchCase : sig
    type t [@@deriving yojson]
  end

  module UnifyMap : sig
    type t [@@deriving yojson]
  end

  module ExecMap : sig
    type 'a t [@@deriving yojson]
  end

  module Inspect : sig
    type debug_state_view [@@deriving yojson]

    val get_debug_state : debug_state -> debug_state_view

    val get_unification :
      Logging.ReportId.t -> debug_state -> Logging.ReportId.t * UnifyMap.t
  end

  val launch : string -> string option -> (debug_state, string) result
  val jump_to_id : Logging.ReportId.t -> debug_state -> (unit, string) result
  val jump_to_start : debug_state -> unit
  val step_in : ?reverse:bool -> debug_state -> stop_reason
  val step : ?reverse:bool -> debug_state -> stop_reason

  val step_specific :
    PackagedBranchCase.t option ->
    Logging.ReportId.t ->
    debug_state ->
    (stop_reason, string) result

  val step_out : debug_state -> stop_reason
  val run : ?reverse:bool -> ?launch:bool -> debug_state -> stop_reason
  val terminate : debug_state -> unit
  val get_frames : debug_state -> frame list
  val get_scopes : debug_state -> scope list
  val get_variables : int -> debug_state -> variable list
  val get_exception_info : debug_state -> exception_info
  val set_breakpoints : string option -> int list -> debug_state -> unit
end

module Make
    (PC : ParserAndCompiler.S)
    (Verification : Verifier.S)
    (Lifter : Gil_to_tl_lifter.S
                with type memory = Verification.SAInterpreter.heap_t
                 and type memory_error = Verification.SPState.m_err_t
                 and type tl_ast = PC.tl_ast) : S
