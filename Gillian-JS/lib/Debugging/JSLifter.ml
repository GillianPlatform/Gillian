open Gil_syntax
open Semantics
open Debugger.DebuggerTypes

include
  Debugger.Gil_to_tl_lifter.Default
    (Semantics.Symbolic)
    (Js2jsil_lib.JS2GIL_ParserAndCompiler)

let to_str pp = Fmt.to_to_string (Fmt.hbox pp)

(* TODO: All of this node stuff should be refactored to hide the scope id *)
let create_node_var name nodes get_new_scope_id (variables : variables) =
  let id = get_new_scope_id () in
  let () = Hashtbl.replace variables id nodes in
  (id, create_node_variable name id ())

let get_store_vars store =
  store
  |> List.map (fun (var, value) : variable ->
         let value = Fmt.to_to_string (Fmt.hbox Expr.pp) value in
         create_leaf_variable var value ())
  |> List.sort (fun v w -> Stdlib.compare v.name w.name)

let add_properties_vars properties get_new_scope_id (variables : variables) =
  let add_lst_vars name to_string lst get_new_scope_id (variables : variables) =
    let list_nodes =
      List.mapi
        (fun index element ->
          create_leaf_variable (string_of_int index) (to_string element) ())
        lst
    in
    let _, node = create_node_var name list_nodes get_new_scope_id variables in
    node
  in
  let property_nodes =
    properties |> Expr.Map.to_seq
    |> Seq.map (fun (name, value) ->
           let name = to_str Expr.pp name in
           match value with
           | Expr.EList lst ->
               add_lst_vars name (to_str Expr.pp) lst get_new_scope_id variables
           | Expr.Lit (Literal.LList lst) ->
               add_lst_vars name (to_str Literal.pp) lst get_new_scope_id
                 variables
           | _ -> create_leaf_variable name (to_str Expr.pp value) ())
    |> List.of_seq
  in
  let _, node =
    create_node_var "properties" property_nodes get_new_scope_id variables
  in
  node

let add_memory_vars smemory get_new_scope_id (variables : variables) =
  let sorted_locs_with_vals = Symbolic.sorted_locs_with_vals smemory in
  let value_nodes (loc, ((properties, domain), metadata)) : variable =
    let () = ignore properties in
    let properties =
      add_properties_vars properties get_new_scope_id variables
    in
    let domain =
      create_leaf_variable "domain" (to_str (Fmt.option Expr.pp) domain) ()
    in
    let metadata =
      create_leaf_variable "metadata"
        (to_str (Fmt.option ~none:(Fmt.any "unknown") Expr.pp) metadata)
        ()
    in
    let loc_id = get_new_scope_id () in
    let () =
      Hashtbl.replace variables loc_id [ properties; domain; metadata ]
    in
    create_node_variable loc loc_id ()
  in
  List.map value_nodes sorted_locs_with_vals

let rec add_loc_vars
    (loc : string)
    smemory
    get_new_scope_id
    (variables : variables)
    (loc_to_scope_id : (string, int) Hashtbl.t) : unit =
  let rec add_lit_vars name lit : variable =
    match lit with
    | Literal.Loc loc ->
        let () =
          add_loc_vars loc smemory get_new_scope_id variables loc_to_scope_id
        in
        let id = Hashtbl.find loc_to_scope_id loc in
        create_node_variable name id ()
    | LList lst ->
        let nodes =
          List.mapi
            (fun index element ->
              let name = string_of_int index in
              add_lit_vars name element)
            lst
        in
        let _, node = create_node_var name nodes get_new_scope_id variables in
        node
    | _ -> create_leaf_variable name (to_str Literal.pp lit) ()
  in
  let add_expr_vars name expr =
    match expr with
    | Expr.ALoc loc ->
        let () =
          add_loc_vars loc smemory get_new_scope_id variables loc_to_scope_id
        in
        let id = Hashtbl.find loc_to_scope_id loc in
        create_node_variable name id ()
    (* TODO: The below causes a stack overflow error in large pieces of
       code, so they is probably a more efficient way to write this algorithm
       or something else is just incorrect*)
    (* | Expr.EList lst ->
           let nodes =
             List.mapi
               (fun index element ->
                 let name = string_of_int index in
                 add_expr_vars name element)
               lst
           in
           let _, node = create_node_var name nodes get_new_scope_id variables in
           node
       | Expr.Lit lit   -> add_lit_vars name lit *)
    | _ -> create_leaf_variable name (to_str Expr.pp expr) ()
  in
  let add_properties_vars properties =
    let property_nodes =
      properties |> Expr.Map.to_seq
      |> Seq.map (fun (name, value) ->
             let name = to_str Expr.pp name in
             let field_names =
               [
                 "descriptor_type";
                 "value";
                 "enumerable";
                 "writable";
                 "configurable";
               ]
             in
             match value with
             | Expr.EList lst ->
                 let nodes =
                   if List.length lst == 5 then
                     (* TODO: Not sure if this is always the case *)
                     List.map2
                       (fun name element -> add_expr_vars name element)
                       field_names lst
                   else
                     List.mapi
                       (fun index element ->
                         let name = string_of_int index in
                         add_expr_vars name element)
                       lst
                 in
                 let _, node =
                   create_node_var name nodes get_new_scope_id variables
                 in
                 node
             | Expr.Lit (Literal.LList lst) ->
                 let nodes =
                   if List.length lst == 5 then
                     List.map2
                       (fun name element -> add_lit_vars name element)
                       field_names lst
                   else
                     List.mapi
                       (fun index element ->
                         let name = string_of_int index in
                         add_lit_vars name element)
                       lst
                 in

                 let _, node =
                   create_node_var name nodes get_new_scope_id variables
                 in
                 node
             | _ -> add_expr_vars name value)
      |> List.of_seq
    in
    let _, node =
      create_node_var "properties" property_nodes get_new_scope_id variables
    in
    node
  in
  match Hashtbl.find_opt loc_to_scope_id loc with
  | None ->
      let loc_id = get_new_scope_id () in
      let vars =
        match SHeap.get smemory loc with
        | None -> []
        | Some ((properties, domain), metadata_opt) ->
            let metadata_node metadata_opt : variable =
              let name = "metadata" in
              match metadata_opt with
              | None -> create_leaf_variable name "unknown" ()
              | Some metadata -> (
                  match metadata with
                  | Expr.ALoc child_loc ->
                      let () =
                        add_loc_vars child_loc smemory get_new_scope_id
                          variables loc_to_scope_id
                      in
                      let id = Hashtbl.find loc_to_scope_id child_loc in
                      create_node_variable name id ()
                  | _ -> create_leaf_variable name (to_str Expr.pp metadata) ())
            in
            let properties = add_properties_vars properties in
            (* let properties =
                 add_properties_vars properties get_new_scope_id variables
               in *)
            let domain =
              create_leaf_variable "domain"
                (to_str (Fmt.option Expr.pp) domain)
                ()
            in
            [ properties; domain; metadata_node metadata_opt ]
      in

      let () = Hashtbl.replace variables loc_id vars in
      Hashtbl.replace loc_to_scope_id loc loc_id
  | Some _ -> ()

let add_variables
    ~store
    ~memory
    ~is_gil_file
    ~get_new_scope_id
    (variables : variables) =
  if is_gil_file then
    let store_id = get_new_scope_id () in
    let memory_id = get_new_scope_id () in
    let scopes : scope list =
      [ { id = store_id; name = "Store" }; { id = memory_id; name = "Memory" } ]
    in
    let store_vars = get_store_vars store in
    let memory_vars = add_memory_vars memory get_new_scope_id variables in
    let vars = [ store_vars; memory_vars ] in
    let () =
      List.iter2
        (fun (scope : scope) vars -> Hashtbl.replace variables scope.id vars)
        scopes vars
    in
    scopes
  else
    let sorted_locs_with_vals = Symbolic.sorted_locs_with_vals memory in
    let loc_to_scope_id = Hashtbl.create 0 in
    let () =
      List.iter
        (fun (loc, _) ->
          add_loc_vars loc memory get_new_scope_id variables loc_to_scope_id)
        sorted_locs_with_vals
    in
    (* TODO: probably better if we got a hash map of the store *)
    let local_scope =
      List.filter_map
        (fun (var, value) ->
          if var = Js2jsil_lib.JS2JSIL_Helpers.var_sc_first then
            Some (var, value)
          else None)
        store
    in
    let local_scope_loc =
      if List.length local_scope == 0 then ""
      else
        match List.hd local_scope with
        | _, Expr.EList [ _; ALoc loc ] -> loc
        | _ -> ""
    in
    let local_id : int =
      match Hashtbl.find_opt loc_to_scope_id local_scope_loc with
      | None ->
          let local_id = get_new_scope_id () in
          let () = Hashtbl.replace variables local_id [] in
          local_id
      | Some local_id -> local_id
    in
    let global_id : int =
      match Hashtbl.find_opt loc_to_scope_id "$lg" with
      | None ->
          let global_id = get_new_scope_id () in
          let () = Hashtbl.replace variables global_id [] in
          global_id
      | Some global_id -> global_id
    in
    let scopes : scope list =
      [
        { id = local_id; name = "Local Scope" };
        { id = global_id; name = "Global Scope" };
      ]
    in
    scopes
