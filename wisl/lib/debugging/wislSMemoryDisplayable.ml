open WSemantics
open Gillian.Symbolic
open Gillian.Debugger.Displayable

type t = WislSMemory.t

let to_debugger_tree (heap : t) =
  let bindings = WislSMemory.bindings heap in
  let vstr = WPrettyUtils.to_str Values.pp in
  let loc_nodes l : debugger_tree list =
    List.map (fun (offset, value) -> Leaf (vstr offset, vstr value)) l
  in
  let all_loc_nodes =
    List.map (fun (loc, assocs) -> Node (loc, loc_nodes assocs)) bindings
  in
  all_loc_nodes
