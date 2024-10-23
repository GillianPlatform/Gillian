open WSemantics
open Gillian.Debugger

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Engine.Verifier.S with type annot = WParserAndCompiler.Annot.t) :
  Lifter.S
    with type memory_error = WSemantics.WislSHeap.err
     and type tl_ast = WParserAndCompiler.tl_ast
     and type memory = WislSMemory.t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
     and type annot = WParserAndCompiler.Annot.t
     and type init_data = WParserAndCompiler.init_data
     and type pc_err = WParserAndCompiler.err
