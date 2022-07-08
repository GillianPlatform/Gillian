type ('err, 'ast) memory_error_info = {
  error : 'err;  (** The memory error that needs to be lifted *)
  command : (int Cmd.t * Annot.t) option;  (** The command where it happened *)
  tl_ast : 'ast option;
      (** If the program was compiled from the target language, we keep the tl ast around *)
}

module type S = sig
  type memory_error
  type tl_ast
  type memory

  val source_map_ability : bool

  (** Take the origin [tl_ast], an origin [node_id] and returns
      a string representing the evaluation step for the exec map.
      Should never be called if [source_map_ability] is false *)
  val get_origin_node_str : tl_ast -> int option -> string

  val memory_error_to_exception_info :
    (memory_error, tl_ast) memory_error_info -> DebuggerTypes.exception_info

  val add_variables :
    store:(string * Expr.t) list ->
    memory:memory ->
    is_gil_file:bool ->
    get_new_scope_id:(unit -> int) ->
    DebuggerTypes.variables ->
    DebuggerTypes.scope list
end

module Default (SMemory : SMemory.S) (PC : ParserAndCompiler.S) :
  S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
