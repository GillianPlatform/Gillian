include Gillian.Monadic.MonadicSMemory.S with type init_data = unit

module Lift : sig
  open Gillian.Debugger.Utils

  val add_heap_variables :
    t -> Variable.ts -> (unit -> int) -> Variable.scope list
end
