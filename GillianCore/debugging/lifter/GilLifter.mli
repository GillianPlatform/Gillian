module Make (V : Verifier.S) (SMemory : SMemory.S) (PC : ParserAndCompiler.S) :
  Lifter.S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
