module Displayable = Displayable
module DisplayFilterMap = DisplayFilterMap
module MemoryErrorLifter = MemoryErrorLifter

module type S = sig
  type stop_reason =
    | Step
    | ReachedStart
    | ReachedEnd
    | Breakpoint
    | ExecutionError

  type frame = {
    index : int;
    name : string;
    source_path : string;
    start_line : int;
    start_column : int;
    end_line : int;
    end_column : int;
  }

  type scope = { name : string; id : int }

  type variable = {
    name : string;
    value : string;
    type_ : string option;
    var_ref : int;
  }

  type exception_info = { id : string; description : string option }

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
    (TLDisplayFilterMap : DisplayFilterMap.S)
    (Displayable : Displayable.S with type t = Verification.SAInterpreter.heap_t)
    (MemoryErrorLifter : MemoryErrorLifter.S
                           with type merr = Verification.SPState.m_err_t) : S
