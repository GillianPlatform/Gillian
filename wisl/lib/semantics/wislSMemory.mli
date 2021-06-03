include Gillian.Symbolic.Memory_S with type err_t = WislSHeap.err

val add_debugger_variables :
  (string * Gillian.Gil_syntax.Expr.t) list ->
  t ->
  is_gil_file:bool ->
  get_new_scope_id:(unit -> int) ->
  Debugger.DebuggerTypes.variables ->
  Debugger.DebuggerTypes.scope list
