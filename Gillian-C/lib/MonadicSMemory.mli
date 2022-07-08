include Gillian.Monadic.MonadicSMemory.S

module Lift : sig
  open Debugger.DebuggerTypes

  val add_variables :
    store:(string * vt) list ->
    memory:t ->
    is_gil_file:'a ->
    get_new_scope_id:(unit -> int) ->
    (int, variable list) Hashtbl.t ->
    scope list
end
