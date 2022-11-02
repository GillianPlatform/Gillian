include Gillian.Monadic.MonadicSMemory.S with type init_data = Global_env.t

module Lift : sig
  open Gillian.Debugger.Utils

  val add_variables :
    store:(string * vt) list ->
    memory:t ->
    is_gil_file:'a ->
    get_new_scope_id:(unit -> int) ->
    (int, variable list) Hashtbl.t ->
    scope list
end
