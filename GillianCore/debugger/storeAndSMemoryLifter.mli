open DebuggerTypes

(** Type of the store *)
type store = (string * Expr.t) list

module type S = sig
  (** Type of the symbolic memory *)
  type smemory

  (** Lifts the store and symbolic memory into a list of top level scopes and
      the corresponding variables *)
  val add_variables :
    store ->
    smemory ->
    is_gil_file:bool ->
    get_new_scope_id:(unit -> int) ->
    variables ->
    scope list
end

(** Default lifter for the store and symbolic memory *)
module Default (SMemory : SMemory.S) : S with type smemory = SMemory.t
