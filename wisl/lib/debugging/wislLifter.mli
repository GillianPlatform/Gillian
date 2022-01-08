open WSemantics

include
  Gillian.Debugger.Gil_to_tl_lifter.S
    with type memory_error = WSemantics.WislSHeap.err
     and type tl_ast = WParserAndCompiler.tl_ast
     and type memory = WislSMemory.t
