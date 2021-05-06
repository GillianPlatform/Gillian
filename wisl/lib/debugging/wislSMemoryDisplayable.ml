open WSemantics
open Gillian.Symbolic
open Gillian.Debugger.Displayable
open Gillian.Gil_syntax

type t = WislSMemory.t

let to_debugger_tree (heap : t) =
  let bindings = WislSMemory.bindings heap in
  let vstr = WPrettyUtils.to_str Values.pp in
  let compare_offsets (v, _) (w, _) =
    try
      let open Expr.Infix in
      let difference = v - w in
      (match difference with
        | Expr.Lit (Int i) -> if i < 0 then -1 else if i > 0 then 1 else 0
        | _ -> 0)
    with _ ->
      (* Do not sort the offsets if an exception has occurred *)
      0
  in
  let loc_nodes l : debugger_tree list =
    List.sort compare_offsets l |>
    List.map (fun (offset, value) -> Leaf (vstr offset, vstr value))
  in
  let all_loc_nodes =
    List.map (fun (loc, assocs) -> Node (loc, loc_nodes assocs)) bindings
  in
  all_loc_nodes
