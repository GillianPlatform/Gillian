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
