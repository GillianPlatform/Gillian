open PwSemantics
open Gillian.Debugger

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Engine.Verifier.S with type annot = PwParserAndCompiler.Annot.t) :
  Lifter.S
    with type memory_error = PwSemantics.WislSHeap.err
     and type tl_ast = PwParserAndCompiler.tl_ast
     and type memory = WislSMemory.t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
     and type annot = PwParserAndCompiler.Annot.t
