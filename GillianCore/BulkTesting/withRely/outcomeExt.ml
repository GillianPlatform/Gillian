module Make (Outcome : Outcome.S) = struct
  type resExts = {
    toFinishInErrorMode : unit -> unit;
    toFinishInErrorModeWith :
      constraint_name:string ->
      (Outcome.Val.t -> Outcome.State.t -> bool) ->
      unit;
    toFinishInNormalMode : unit -> unit;
    toFinishInNormalModeWith :
      constraint_name:string ->
      (Outcome.Val.t -> Outcome.State.t -> bool) ->
      unit;
  }

  type outcomeExts = {
    toFailAtParsing : unit -> unit;
    toFailAtParsingWith :
      constraint_name:string -> (Outcome.ParserAndCompiler.err -> bool) -> unit;
    toFailAtExec : unit -> unit;
    allBranches : resExts;
    atLeastOneBranch : resExts;
    exactlyOneBranch : resExts;
  }

  type ext = { outcome : Outcome.t -> outcomeExts }
end
