module MemoryErrorLifter = MemoryErrorLifter
module StoreAndSMemoryLifter = StoreAndSMemoryLifter
module DebuggerTypes = DebuggerTypes
module DebuggerUtils = DebuggerUtils
open DebuggerTypes

module type S = sig
  type tl_ast

  type debugger_state

  val launch : string -> string option -> (debugger_state, string) result

  val step_in : ?reverse:bool -> debugger_state -> stop_reason

  val step : debugger_state -> stop_reason

  val step_out : debugger_state -> stop_reason

  val run : ?reverse:bool -> ?launch:bool -> debugger_state -> stop_reason

  val terminate : debugger_state -> unit

  val get_frames : debugger_state -> frame list

  val get_scopes : debugger_state -> scope list

  val get_variables : int -> debugger_state -> variable list

  val get_exception_info : debugger_state -> exception_info

  val set_breakpoints : string option -> int list -> debugger_state -> unit
end

module Make
    (PC : ParserAndCompiler.S)
    (Verification : Verifier.S)
    (StoreAndSMemoryLifter : StoreAndSMemoryLifter.S
                               with type smemory =
                                     Verification.SAInterpreter.heap_t)
    (MemoryErrorLifter : MemoryErrorLifter.S
                           with type merr = Verification.SPState.m_err_t
                            and type tl_ast = PC.tl_ast) : S
