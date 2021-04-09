module type S = sig
  type stop_reason = Step | ReachedEnd

  type debugger_state

  val launch : string -> (debugger_state, string) result

  val step : debugger_state -> stop_reason

  val run : debugger_state -> stop_reason

  val terminate : debugger_state -> unit
end

module Make (PC : ParserAndCompiler.S) (Verification : Verifier.S) : S
