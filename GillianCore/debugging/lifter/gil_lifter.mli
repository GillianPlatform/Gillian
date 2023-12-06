(** A basic "GIL-to-GIL" lifter implementation. *)

module type S = sig
  include Lifter.S

  val should_skip_cmd : cmd_report Lifter.executed_cmd_data -> t -> bool
end

module Make
    (PC : ParserAndCompiler.S)
    (V : Verifier.S with type annot = PC.Annot.t)
    (SMemory : SMemory.S) :
  S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
     and type annot = PC.Annot.t
