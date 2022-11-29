module type Gil_lifterWithState = sig
  module Lifter : Lifter.S

  val get_state : unit -> Lifter.t
end

module Make
    (SMemory : SMemory.S)
    (PC : ParserAndCompiler.S)
    (TLLifter : functor
      (Gil : Gil_lifterWithState)
      (V : Verifier.S with type annot = PC.Annot.t)
      ->
      Lifter.S
        with type memory = SMemory.t
         and type tl_ast = PC.tl_ast
         and type memory_error = SMemory.err_t
         and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
         and type annot = PC.Annot.t)
    (V : Verifier.S with type annot = PC.Annot.t) :
  Lifter.S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
     and type annot = PC.Annot.t
