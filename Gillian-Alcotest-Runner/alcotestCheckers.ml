module Make (Outcome : Bulk.Outcome.S) = struct
  type matcher = {
    fail_at_parsing : Outcome.t -> unit;
    fail_at_parsing_with :
      constraint_name:string ->
      (Utils.Gillian_result.Error.compilation_error -> bool) ->
      Outcome.t ->
      unit;
    fail_at_exec : Outcome.t -> unit;
    finish_in_error_mode : BranchReasoning.branches -> Outcome.t -> unit;
    finish_in_error_mode_with :
      BranchReasoning.branches ->
      constraint_name:string ->
      (Outcome.Val.t -> Outcome.State.t -> bool) ->
      Outcome.t ->
      unit;
    finish_in_normal_mode : BranchReasoning.branches -> Outcome.t -> unit;
    finish_in_normal_mode_with :
      BranchReasoning.branches ->
      constraint_name:string ->
      (Outcome.Val.t -> Outcome.State.t -> bool) ->
      Outcome.t ->
      unit;
  }
end
