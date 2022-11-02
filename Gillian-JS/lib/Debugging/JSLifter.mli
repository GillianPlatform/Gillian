module Make (V : Gillian.Abstraction.Verifier.S) :
  Gillian.Debugger.Lifter.S
    with type memory = Semantics.Symbolic.t
     and type memory_error = Semantics.Symbolic.err_t
     and type tl_ast = Js2jsil_lib.JS2GIL_ParserAndCompiler.tl_ast
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
