module type GilLifterWithState = sig
  module Lifter : Lifter.S

  val get_state : unit -> Lifter.t
end

module Make
    (SMemory : SMemory.S)
    (PC : ParserAndCompiler.S)
    (TLLifter : functor
      (Gil : GilLifterWithState)
      (V : Verifier.S)
      ->
      Lifter.S
        with type memory = SMemory.t
         and type tl_ast = PC.tl_ast
         and type memory_error = SMemory.err_t
         and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t)
    (V : Verifier.S) :
  Lifter.S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
