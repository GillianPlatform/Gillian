open Gillian
open Js2jsil_lib.JS2GIL_ParserAndCompiler

module Make
    (V : Abstraction.Verifier.S with type annot = Gil_syntax.Annot.Basic.t) :
  Debugger.Lifter.S
    with type memory = Semantics.Symbolic.t
     and type memory_error = Semantics.Symbolic.err_t
     and type tl_ast = tl_ast
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
     and type annot = Gil_syntax.Annot.Basic.t
