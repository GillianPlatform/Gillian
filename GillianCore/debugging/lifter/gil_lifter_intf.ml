module type S = sig
  include Lifter.S

  (** A version of [init] that allows manually supplying exec_data instead of triggering
      the Step effect - this is used when Gil is not the primary lifter. *)
  val init_manual :
    string ->
    string list ->
    t
    * (cmd_report Lifter.executed_cmd_data option ->
      unit ->
      Logging.Report_id.t * stop_reason)

  val handle_cmd :
    Logging.Report_id.t ->
    Branch_case.t option ->
    cmd_report Lifter.executed_cmd_data ->
    t ->
    unit

  val path_of_id : Logging.Report_id.t -> t -> Branch_case.path
  val should_skip_cmd : cmd_report Lifter.executed_cmd_data -> t -> bool
  val cases_at_id : Logging.Report_id.t -> t -> Branch_case.t list
end

module type Make = functor
  (SMemory : SMemory.S)
  (PC : ParserAndCompiler.S)
  (Verifier : Verifier.S with type annot = PC.Annot.t)
  ->
  S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = Verifier.SAInterpreter.Logging.ConfigReport.t
     and type annot = PC.Annot.t
     and type init_data = PC.init_data
     and type pc_err = PC.err

module type Intf = sig
  module type S = S
  module type Make = Make

  module Make : Make
end
