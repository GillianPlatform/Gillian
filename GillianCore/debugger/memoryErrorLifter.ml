module type S = sig
  type merr

  type tl_ast

  (* TODO: This should probably have one argument "Debugging info" *)
  val error_to_exception_info :
    merr ->
    int Cmd.t option ->
    Annot.t option ->
    tl_ast option ->
    DebuggerTypes.exception_info
end

module Dummy (SMemory : SMemory.S) (PC : ParserAndCompiler.S) = struct
  type merr = SMemory.err_t

  type tl_ast = PC.tl_ast

  let error_to_exception_info merr _ _ _ : DebuggerTypes.exception_info =
    { id = Fmt.to_to_string SMemory.pp_err merr; description = None }
end
