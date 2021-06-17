open Gil_syntax
open Semantics
open Debugger.DebuggerTypes

type smemory = Symbolic.t

(* TODO: This should implement StoreSMemoryLifter *)

(* open Gillian.Debugger.Displayable
open Gil_syntax
open Semantics

type t = Semantics.Symbolic.t

let to_str pp = Fmt.to_to_string (Fmt.hbox pp)

let rec location_to_debugger_tree
    (name : string) (loc : string) (smemory : t) locs_to_debugger_tree :
    debugger_tree =
  let rec lit_to_debugger_tree name lit =
    match lit with
    | Literal.Loc loc ->
        location_to_debugger_tree name loc smemory locs_to_debugger_tree
    | LList lst       ->
        let nodes =
          List.mapi
            (fun index element ->
              let name = string_of_int index in
              lit_to_debugger_tree name element)
            lst
        in
        Node (name, nodes)
    | _               -> Leaf (name, to_str Literal.pp lit)
  in
  let rec expr_to_debugger_tree name expr =
    match expr with
    | Expr.ALoc loc  ->
        location_to_debugger_tree name loc smemory locs_to_debugger_tree
    | Expr.EList lst ->
        let nodes =
          List.mapi
            (fun index element ->
              expr_to_debugger_tree (string_of_int index) element)
            lst
        in
        Node (name, nodes)
    | Expr.Lit lit   -> lit_to_debugger_tree name lit
    | _              -> Leaf (name, to_str Expr.full_pp expr)
  in
  let properties_to_debugger_tree properties =
    let property_nodes =
      properties |> Expr.YojsonableMap.to_seq
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
                   List.map2
                     (fun field_name element ->
                       expr_to_debugger_tree field_name element)
                     field_names lst
                 in
                 Node (name, nodes)
             | Expr.Lit (Literal.LList lst) ->
                 let nodes =
                   List.map2
                     (fun field_name lit -> lit_to_debugger_tree field_name lit)
                     field_names lst
                 in
                 Node (name, nodes)
             | _ -> expr_to_debugger_tree name value)
      |> List.of_seq
    in
    Node ("properties", property_nodes)
  in
  match SHeap.get smemory loc with
  | None -> Leaf (name, loc)
  | Some ((properties, domain), metadata_opt) ->
      (* Remove and add back location to prevent cycles in the debugger tree.
         The better way to do this would be to have Debugger.Displayable convert
         into a Hashtbl (i.e. similar to scopes table in debugger.ml which handles
         where nodes in the debugger tree have new ids in the scopes table and
         can be lazily loaded by VSCode). *)
      let () = SHeap.remove smemory loc in
      let metadata_node metadata_opt =
        let name = "metadata" in
        match metadata_opt with
        | None          -> Leaf (name, "unknown")
        | Some metadata -> (
            match metadata with
            | Expr.ALoc child_loc ->
                location_to_debugger_tree name child_loc smemory
                  locs_to_debugger_tree
            | _                   -> Leaf
                                       (name, Fmt.to_to_string Expr.pp metadata)
            )
      in
      let node =
        Node
          ( name ^ ": " ^ loc,
            [
              properties_to_debugger_tree properties;
              Leaf ("domain", to_str (Fmt.option Expr.pp) domain);
              metadata_node metadata_opt;
            ] )
      in
      let () = SHeap.set smemory loc properties domain metadata_opt in
      let () = Hashtbl.add locs_to_debugger_tree loc node in
      node

let properties_to_debugger_tree properties =
  let property_nodes =
    properties |> Expr.YojsonableMap.to_seq
    |> Seq.map (fun (name, value) ->
           let name = to_str Expr.pp name in
           match value with
           | Expr.EList lst -> list_to_debugger_tree name (to_str Expr.pp) lst
           | Expr.Lit (Literal.LList lst) ->
               list_to_debugger_tree name (to_str Literal.pp) lst
           | _ -> Leaf (name, to_str Expr.pp value))
    |> List.of_seq
  in
  Node ("properties", property_nodes)

let to_debugger_tree (smemory : t) : debugger_tree list =
  let locs_to_debugger_tree = Hashtbl.create 0 in
  let global_scope =
    location_to_debugger_tree "Global scope" "$lg" smemory locs_to_debugger_tree
  in
  let local_scope =
    location_to_debugger_tree "Local scope" "_$l_17" smemory
      locs_to_debugger_tree
  in
  let sorted_locs_with_vals = Symbolic.sorted_locs_with_vals smemory in
  let to_str pp = Fmt.to_to_string (Fmt.hbox pp) in
  let value_nodes ((properties, domain), metadata) : debugger_tree list =
    [
      properties_to_debugger_tree properties;
      Leaf ("domain", to_str (Fmt.option Expr.pp) domain);
      Leaf
        ( "metadata",
          to_str (Fmt.option ~none:(Fmt.any "unknown") Expr.pp) metadata );
    ]
  in
  List.filter_map
    (fun (loc, value) ->
      if Hashtbl.mem locs_to_debugger_tree loc then None
      else Some (Node (loc, value_nodes value)))
    sorted_locs_with_vals
  @ [ global_scope; local_scope ] *)

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
  (* let field_names =
       [
         "descriptor_type";
         "value";
         "enumerable";
         "writable";
         "configurable";
       ]
     in *)
  (* let add_lst_vars name to_string lst get_new_scope_id (variables : variables) =
       let list_nodes = List.map2 (fun field_name element -> create_leaf_variable field_name (to_string element) ()) field_names lst in
       let id = get_new_scope_id () in
       let () = Hashtbl.replace variables id list_nodes in
       create_node_variable name id ()
     in *)
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
    properties |> Expr.YojsonableMap.to_seq
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

(* let rec add_loc_vars
  (name : string) (loc : string) (smemory : smemory) (loc_to_scope : (string, (int * variable list)) Hashtbl.t) get_new_scope_id variables :
(int * variable) =
let rec add_lit_vars name lit : variable =
  match lit with
  | Literal.Loc loc ->
    (match Hashtbl.find_opt loc_to_scope loc with
    | None ->
    let _, node = add_loc_vars name loc smemory loc_to_scope get_new_scope_id variables in
    node
    | Some ->)
  | LList lst       ->
      let nodes =
        List.mapi
          (fun index element ->
            let name = string_of_int index in
            add_lit_vars name element)
          lst
      in
      let _, node = create_node_var name nodes get_new_scope_id variables in
      node
  | _               -> Leaf (name, to_str Literal.pp lit)
in
let rec expr_to_debugger_tree name expr =
  match expr with
  | Expr.ALoc loc  ->
      location_to_debugger_tree name loc smemory locs_to_debugger_tree
  | Expr.EList lst ->
      let nodes =
        List.mapi
          (fun index element ->
            expr_to_debugger_tree (string_of_int index) element)
          lst
      in
      Node (name, nodes)
  | Expr.Lit lit   -> lit_to_debugger_tree name lit
  | _              -> Leaf (name, to_str Expr.full_pp expr)
in
let properties_to_debugger_tree properties =
  let property_nodes =
    properties |> Expr.YojsonableMap.to_seq
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
                 List.map2
                   (fun field_name element ->
                     expr_to_debugger_tree field_name element)
                   field_names lst
               in
               Node (name, nodes)
           | Expr.Lit (Literal.LList lst) ->
               let nodes =
                 List.map2
                   (fun field_name lit -> lit_to_debugger_tree field_name lit)
                   field_names lst
               in
               Node (name, nodes)
           | _ -> expr_to_debugger_tree name value)
    |> List.of_seq
  in
  Node ("properties", property_nodes)
in
match Hashtbl.find_opt loc_to_scope loc with
| None -> (
  let loc_id = let
  match SHeap.get smemory loc with
  | None ->

  | Some ->
)
| Some scope -> scope

match SHeap.get smemory loc with
| None -> Leaf (name, loc)
| Some ((properties, domain), metadata_opt) ->
    let metadata_node metadata_opt =
      let name = "metadata" in
      match metadata_opt with
      | None          -> Leaf (name, "unknown")
      | Some metadata -> (
          match metadata with
          | Expr.ALoc child_loc ->
              location_to_debugger_tree name child_loc smemory
                locs_to_debugger_tree
          | _                   -> Leaf
                                     (name, Fmt.to_to_string Expr.pp metadata)
          )
    in
    let node =
      Node
        ( name ^ ": " ^ loc,
          [
            properties_to_debugger_tree properties;
            Leaf ("domain", to_str (Fmt.option Expr.pp) domain);
            metadata_node metadata_opt;
          ] )
    in
    let () = SHeap.set smemory loc properties domain metadata_opt in
    let () = *)

let rec add_loc_vars
    (loc : string)
    (smemory : smemory)
    get_new_scope_id
    (variables : variables)
    (loc_to_scope_id : (string, int) Hashtbl.t) : unit =
  (* let rec add_lit_vars name lit : variable =
       match lit with
       | Literal.Loc loc ->
           let () =
             add_loc_vars loc smemory get_new_scope_id variables loc_to_scope_id
           in
           let id = Hashtbl.find loc_to_scope_id loc in
           create_node_variable name id ()
       | LList lst       ->
           let nodes =
             List.mapi
               (fun index element ->
                 let name = string_of_int index in
                 add_lit_vars name element)
               lst
           in
           let _, node = create_node_var name nodes get_new_scope_id variables in
           node
       | _               -> create_leaf_variable name (to_str Literal.pp lit) ()
     in
     let rec add_expr_vars name expr =
       match expr with
       | Expr.ALoc loc  ->
           let () =
             add_loc_vars loc smemory get_new_scope_id variables loc_to_scope_id
           in
           let id = Hashtbl.find loc_to_scope_id loc in
           create_node_variable name id ()
       | Expr.EList lst ->
           let nodes =
             List.mapi
               (fun index element ->
                 let name = string_of_int index in
                 add_expr_vars name element)
               lst
           in
           let _, node = create_node_var name nodes get_new_scope_id variables in
           node
       | Expr.Lit lit   -> add_lit_vars name lit
       | _              -> create_leaf_variable name (to_str Expr.full_pp expr) ()
     in
     let add_properties_vars properties =
       let property_nodes =
         properties |> Expr.YojsonableMap.to_seq
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
                      List.map2
                        (fun name element -> add_expr_vars name element)
                        field_names lst
                    in
                    let _, node =
                      create_node_var name nodes get_new_scope_id variables
                    in
                    node
                | Expr.Lit (Literal.LList lst) ->
                    let nodes =
                      List.map2
                        (fun name element -> add_lit_vars name element)
                        field_names lst
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
     in *)
  match Hashtbl.find_opt loc_to_scope_id loc with
  | None   ->
      let loc_id = get_new_scope_id () in
      let vars =
        match SHeap.get smemory loc with
        | None -> []
        | Some ((properties, domain), metadata_opt) ->
            let metadata_node metadata_opt : variable =
              let name = "metadata" in
              match metadata_opt with
              | None          -> create_leaf_variable name "unknown" ()
              | Some metadata -> (
                  match metadata with
                  | Expr.ALoc child_loc ->
                      let () =
                        add_loc_vars child_loc smemory get_new_scope_id
                          variables loc_to_scope_id
                      in
                      let id = Hashtbl.find loc_to_scope_id child_loc in
                      create_node_variable name id ()
                  | _                   -> create_leaf_variable name
                                             (to_str Expr.pp metadata) ())
            in
            (* let properties = add_properties_vars properties in *)
            let properties =
              add_properties_vars properties get_new_scope_id variables
            in
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
    store smemory ~is_gil_file ~get_new_scope_id (variables : variables) =
  if is_gil_file then
    let store_id = get_new_scope_id () in
    let memory_id = get_new_scope_id () in
    let scopes : scope list =
      [ { id = store_id; name = "Store" }; { id = memory_id; name = "Memory" } ]
    in
    let store_vars = get_store_vars store in
    let memory_vars = add_memory_vars smemory get_new_scope_id variables in
    let vars = [ store_vars; memory_vars ] in
    let () =
      List.iter2
        (fun (scope : scope) vars -> Hashtbl.replace variables scope.id vars)
        scopes vars
    in
    scopes
  else
    let sorted_locs_with_vals = Symbolic.sorted_locs_with_vals smemory in
    let loc_to_scope_id = Hashtbl.create 0 in
    let () =
      List.iter
        (fun (loc, _) ->
          add_loc_vars loc smemory get_new_scope_id variables loc_to_scope_id)
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
      | None          ->
          let local_id = get_new_scope_id () in
          let () = Hashtbl.replace variables local_id [] in
          local_id
      | Some local_id -> local_id
    in
    let global_id : int =
      match Hashtbl.find_opt loc_to_scope_id "$lg" with
      | None           ->
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
