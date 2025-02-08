open Gillian.Debugger.Utils

include
  Gillian.Symbolic.Legacy_s_memory.S
    with type err_t = WislSHeap.err
     and type init_data = unit

val add_debugger_variables :
  store:(Gillian.Gil_syntax.Var.t * Gillian.Gil_syntax.Expr.t) list ->
  memory:t ->
  is_gil_file:bool ->
  get_new_scope_id:(unit -> int) ->
  Variable.ts ->
  Variable.scope list
