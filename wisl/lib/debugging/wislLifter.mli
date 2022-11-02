open WSemantics
open Gillian.Debugger

module Make
    (Gil : Gillian.Debugger.Lifter.GilFallbackLifter.GilLifterWithState)
    (V : Engine.Verifier.S) :
  Lifter.S
    with type memory_error = WSemantics.WislSHeap.err
     and type tl_ast = WParserAndCompiler.tl_ast
     and type memory = WislSMemory.t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
