open DebuggerTypes

(** Type of the store *)
type store = (string * Expr.t) list

module type S = sig
  (** Type of the symbolic memory *)
  type smemory

  (* TODO: get_new_scope_id should be hidden away in a "VariableStore" module
           which deals with adding things to the variables type. *)

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
module Default (SMemory : SMemory.S) : S with type smemory = SMemory.t = struct
  type smemory = SMemory.t

  (** Only display the store, by converting the store values to strings *)
  let add_variables (store : store) _ ~is_gil_file ~get_new_scope_id variables :
      scope list =
    let () = ignore is_gil_file in
    let store_id = get_new_scope_id () in
    let scopes : scope list = [ { id = store_id; name = "Store" } ] in
    let store_vars =
      store
      |> List.map (fun (var, value) : variable ->
             let value = Fmt.to_to_string Expr.pp value in
             { name = var; value; type_ = None; var_ref = 0 })
      |> List.sort (fun v w -> Stdlib.compare v.name w.name)
    in
    let () = Hashtbl.replace variables store_id store_vars in
    scopes
end
