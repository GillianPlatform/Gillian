include Gillian.Monadic.MonadicSMemory.S with type init_data = unit

module Lift : sig
  open Gillian.Debugger.Utils

  val add_variables :
    store:(string * vt) list ->
    memory:t ->
    is_gil_file:'a ->
    get_new_scope_id:(unit -> int) ->
    (int, Variable.t list) Hashtbl.t ->
    Variable.scope list
end
