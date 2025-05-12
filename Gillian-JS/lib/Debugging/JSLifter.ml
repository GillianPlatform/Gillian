open Gil_syntax
open Semantics
open Debugger.Utils
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler

module Make
    (Verification : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t) =
struct
  module Gil_lifter =
    Gillian.Debugger.Lifter.Gil_lifter.Make (Semantics.Symbolic) (PC)
      (Verification)

  include Gil_lifter

  let to_str pp = Fmt.to_to_string (Fmt.hbox pp)

  (* TODO: All of this node stuff should be refactored to hide the scope id *)
  let create_node_var name nodes get_new_scope_id (variables : Variable.ts) =
    let id = get_new_scope_id () in
    let () = Hashtbl.replace variables id nodes in
    (id, Variable.create_node name id ())

  let get_store_vars store =
    store
    |> List.map (fun (var, value) : Variable.t ->
           let value = Fmt.to_to_string (Fmt.hbox Expr.pp) value in
           Variable.create_leaf var value ())
    |> List.sort (fun (v : Variable.t) (w : Variable.t) ->
           Stdlib.compare v.name w.name)

  let add_properties_vars properties get_new_scope_id (variables : Variable.ts)
      =
    let add_lst_vars
        name
        to_string
        lst
        get_new_scope_id
        (variables : Variable.ts) =
      let list_nodes =
        List.mapi
          (fun index element ->
            Variable.create_leaf (string_of_int index) (to_string element) ())
          lst
      in
      let _, node =
        create_node_var name list_nodes get_new_scope_id variables
      in
      node
    in
    let property_nodes =
      properties |> Expr.Map.to_seq
      |> Seq.map (fun (name, value) ->
             let name = to_str Expr.pp name in
             match value with
             | Expr.EList lst ->
                 add_lst_vars name (to_str Expr.pp) lst get_new_scope_id
                   variables
             | Expr.Lit (Literal.LList lst) ->
                 add_lst_vars name (to_str Literal.pp) lst get_new_scope_id
                   variables
             | _ -> Variable.create_leaf name (to_str Expr.pp value) ())
      |> List.of_seq
    in
    let _, node =
      create_node_var "properties" property_nodes get_new_scope_id variables
    in
    node

  let add_memory_vars smemory get_new_scope_id (variables : Variable.ts) =
    let sorted_locs_with_vals = Legacy_symbolic.sorted_locs_with_vals smemory in
    let value_nodes (loc, ((properties, domain), metadata)) : Variable.t =
      let () = ignore properties in
      let properties =
        add_properties_vars properties get_new_scope_id variables
      in
      let domain =
        Variable.create_leaf "domain" (to_str (Fmt.option Expr.pp) domain) ()
      in
      let metadata =
        Variable.create_leaf "metadata"
          (to_str (Fmt.option ~none:(Fmt.any "unknown") Expr.pp) metadata)
          ()
      in
      let loc_id = get_new_scope_id () in
      let () =
        Hashtbl.replace variables loc_id [ properties; domain; metadata ]
      in
      Variable.create_node loc loc_id ()
    in
    List.map value_nodes sorted_locs_with_vals

  let rec add_loc_vars
      (loc : string)
      smemory
      get_new_scope_id
      (variables : Variable.ts)
      (loc_to_scope_id : (string, int) Hashtbl.t) : unit =
    let rec add_lit_vars name lit : Variable.t =
      match lit with
      | Literal.Loc loc ->
          let () =
            add_loc_vars loc smemory get_new_scope_id variables loc_to_scope_id
          in
          let id = Hashtbl.find loc_to_scope_id loc in
          Variable.create_node name id ()
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
      | _ -> Variable.create_leaf name (to_str Literal.pp lit) ()
    in
    let add_expr_vars name expr =
      match expr with
      | Expr.ALoc loc ->
          let () =
            add_loc_vars loc smemory get_new_scope_id variables loc_to_scope_id
          in
          let id = Hashtbl.find loc_to_scope_id loc in
          Variable.create_node name id ()
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
      | _ -> Variable.create_leaf name (to_str Expr.pp expr) ()
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
              let metadata_node metadata_opt : Variable.t =
                let name = "metadata" in
                match metadata_opt with
                | None -> Variable.create_leaf name "unknown" ()
                | Some metadata -> (
                    match metadata with
                    | Expr.ALoc child_loc ->
                        let () =
                          add_loc_vars child_loc smemory get_new_scope_id
                            variables loc_to_scope_id
                        in
                        let id = Hashtbl.find loc_to_scope_id child_loc in
                        Variable.create_node name id ()
                    | _ ->
                        Variable.create_leaf name (to_str Expr.pp metadata) ())
              in
              let properties = add_properties_vars properties in
              (* let properties =
                   add_properties_vars properties get_new_scope_id variables
                 in *)
              let domain =
                Variable.create_leaf "domain"
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
      (variables : Variable.ts) =
    if is_gil_file then
      let store_id = get_new_scope_id () in
      let memory_id = get_new_scope_id () in
      let scopes : Variable.scope list =
        [
          { id = store_id; name = "Store" }; { id = memory_id; name = "Memory" };
        ]
      in
      let store_vars = get_store_vars store in
      let memory_vars = add_memory_vars memory get_new_scope_id variables in
      let vars = [ store_vars; memory_vars ] in
      let () =
        List.iter2
          (fun (scope : Variable.scope) vars ->
            Hashtbl.replace variables scope.id vars)
          scopes vars
      in
      scopes
    else
      let sorted_locs_with_vals =
        Legacy_symbolic.sorted_locs_with_vals memory
      in
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
      let scopes : Variable.scope list =
        [
          { id = local_id; name = "Local Scope" };
          { id = global_id; name = "Global Scope" };
        ]
      in
      scopes

  let get_variables _ ~store ~memory ~pfs ~types ~preds _ =
    let open Gil_lifter in
    let open Variable in
    let variables = Hashtbl.create 0 in
    (* New scope ids must be higher than last top level scope id to prevent
        duplicate scope ids *)
    let scope_id = ref (List.length top_level_scopes) in
    let get_new_scope_id () =
      let () = scope_id := !scope_id + 1 in
      !scope_id
    in
    let lifted_scopes =
      let lifted_scopes =
        add_variables ~store ~memory ~is_gil_file:false ~get_new_scope_id
          variables
      in
      let pure_formulae_vars = get_pure_formulae_vars pfs in
      let type_env_vars = get_type_env_vars types in
      let pred_vars = get_pred_vars preds in
      let vars_list = [ pure_formulae_vars; type_env_vars; pred_vars ] in
      let () =
        List.iter2
          (fun (scope : scope) vars -> Hashtbl.replace variables scope.id vars)
          top_level_scopes vars_list
      in
      lifted_scopes
    in
    (lifted_scopes, variables)
end
