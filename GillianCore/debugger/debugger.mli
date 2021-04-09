module type S = sig
  type stop_reason = Step | ReachedEnd

  type frame = {
    index : int;
    name : string;
    source_path : string;
    line_num : int;
    col_num : int;
  }

  type scope = { name : string; id : int }

  type variable = { name : string; value : string; type_ : string option }

  type debugger_state

  val launch : string -> (debugger_state, string) result

  val step : debugger_state -> stop_reason

  val run : debugger_state -> stop_reason

  val terminate : debugger_state -> unit

  val get_frames : debugger_state -> frame list

  val get_scopes : debugger_state -> scope list

  val get_variables : int -> debugger_state -> variable list
end

module Make (PC : ParserAndCompiler.S) (Verification : Verifier.S) : S
