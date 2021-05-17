open Gillian.Debugger.Displayable
open Gil_syntax
open Semantics

type t = Semantics.Symbolic.t

let to_debugger_tree (smemory : t) : debugger_tree list =
  let sorted_locs_with_vals = Symbolic.sorted_locs_with_vals smemory in
  let to_str pp = Fmt.to_to_string (Fmt.hbox pp) in
  let property_nodes fv_pairs : debugger_tree list =
    fv_pairs |> Expr.YojsonableMap.to_seq
    |> Seq.map (fun (name, value) ->
           let name = to_str Expr.pp name in
           match value with
           | Expr.EList lst -> list_to_debugger_tree name (to_str Expr.pp) lst
           | Expr.Lit (Literal.LList lst) ->
               list_to_debugger_tree name (to_str Literal.pp) lst
           | _ -> Leaf (name, to_str Expr.pp value))
    |> List.of_seq
  in
  let value_nodes ((fv_pairs, domain), metadata) : debugger_tree list =
    [
      Node ("properties", property_nodes fv_pairs);
      Leaf ("domain", to_str (Fmt.option Expr.pp) domain);
      Leaf
        ( "metadata",
          to_str (Fmt.option ~none:(Fmt.any "unknown") Expr.pp) metadata );
    ]
  in
  List.map
    (fun (loc, value) -> Node (loc, value_nodes value))
    sorted_locs_with_vals
