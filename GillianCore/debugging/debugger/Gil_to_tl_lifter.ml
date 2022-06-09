type ('err, 'ast) memory_error_info = {
  error : 'err;  (** The memory error that needs to be lifted *)
  command : (int Cmd.t * Annot.t) option;  (** The command where it happened *)
  tl_ast : 'ast option;
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
     and type memory_error = SMemory.err_t = struct
  type memory = SMemory.t
  type tl_ast = PC.tl_ast
  type memory_error = SMemory.err_t

  let source_map_ability = false
  let get_origin_node_str _ _ = failwith "Not implemented in Default lifter"

  let memory_error_to_exception_info { error; _ } : DebuggerTypes.exception_info
      =
    { id = Fmt.to_to_string SMemory.pp_err error; description = None }

  let add_variables ~store ~memory ~is_gil_file ~get_new_scope_id variables :
      DebuggerTypes.scope list =
    let open DebuggerTypes in
    let () = ignore is_gil_file in
    let store_id = get_new_scope_id () in
    let memory_id = get_new_scope_id () in
    let scopes : scope list =
      [ { id = store_id; name = "Store" }; { id = memory_id; name = "Memory" } ]
    in
    let store_vars =
      store
      |> List.map (fun (var, value) : variable ->
             let value = Fmt.to_to_string (Fmt.hbox Expr.pp) value in
             create_leaf_variable var value ())
      |> List.sort (fun (v : DebuggerTypes.variable) w ->
             Stdlib.compare v.name w.name)
    in
    let memory_vars =
      [
        create_leaf_variable ""
          (Fmt.to_to_string (Fmt.hbox SMemory.pp) memory)
          ();
      ]
    in
    let () = Hashtbl.replace variables store_id store_vars in
    let () = Hashtbl.replace variables memory_id memory_vars in
    scopes
end
