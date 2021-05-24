module type S = sig
  type merr

  type tl_ast

  val error_to_exception_info :
    merr ->
    int Cmd.t option ->
    Annot.t option ->
    tl_ast option ->
    DebuggerTypes.exception_info
end

module Dummy (SMemory : SMemory.S) (PC : ParserAndCompiler.S) :
  S with type merr = SMemory.err_t and type tl_ast = PC.tl_ast
