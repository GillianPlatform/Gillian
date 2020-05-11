open Containers

let cur_source_files = SourceFiles.make ()

let reset () = SourceFiles.reset cur_source_files

type t = {
  changed_procs : string list;
  new_procs : string list;
  deleted_procs : string list;
  dependents : string list;
}

let pp fmt changes =
  let pp_procs sec fmt = function
    | []    -> Fmt.pf fmt "%s:@\n<none>@\n" sec
    | procs ->
        let newline = Fmt.any "@\n" in
        Fmt.pf fmt "%s:@\n%a@\n" sec (Fmt.list ~sep:newline Fmt.string) procs
  in
  Fmt.pf fmt "%a%a%a%a" (pp_procs "Changed procs") changes.changed_procs
    (pp_procs "New procs") changes.new_procs (pp_procs "Deleted procs")
    changes.deleted_procs
    (pp_procs "Transitive dependents")
    changes.dependents

let to_key_set (table : (string, 'b) Hashtbl.t) : SS.t =
  Hashtbl.fold (fun key _ keys -> SS.add key keys) table SS.empty

let to_list (set : SS.t) : string list =
  SS.fold (fun elem acc -> elem :: acc) set []

let get_changed_files prev_files new_files =
  let rec get_changed paths changed =
    match paths with
    | []           -> changed
    | path :: rest ->
        (* Check if file contents have changed *)
        let prev_hash = SourceFiles.get_contents_hash prev_files path in
        let new_hash = SourceFiles.get_contents_hash new_files path in
        let contents_changed = not (String.equal prev_hash new_hash) in
        let dependents = SourceFiles.get_dependents new_files path in
        let changed =
          if List.length dependents = 0 && contents_changed then path :: changed
          else if contents_changed then dependents @ changed
          else changed
        in
        get_changed rest changed
  in
  let prev_paths = to_key_set prev_files in
  let new_paths = to_key_set new_files in
  let created = to_list (SS.diff new_paths prev_paths) in
  let existing = to_list (SS.inter prev_paths new_paths) in
  let changed = get_changed existing [] in
  (changed, created)

let string_opt_equal string str_opt =
  match str_opt with
  | Some str -> String.equal string str
  | None     -> false

let get_procs_with_path (prog : ('a, 'b) Prog.t) path =
  Hashtbl.fold
    (fun pname (proc : ('a, 'b) Proc.t) acc ->
      if string_opt_equal path proc.proc_source_path then pname :: acc else acc)
    prog.procs []

let get_preds_with_path (prog : ('a, 'b) Prog.t) path =
  Hashtbl.fold
    (fun pname (pred : Pred.t) acc ->
      if string_opt_equal path pred.pred_source_path then pname :: acc else acc)
    prog.preds []

let map_concat f list = List.concat (List.map f list)

let get_changed_procs prog changed_files new_files prev_call_graph =
  let changed_files_procs =
    map_concat (get_procs_with_path prog) changed_files
  in
  let new_files_procs = map_concat (get_procs_with_path prog) new_files in
  (* Distinguish between new procedures and those that existed before *)
  let changed_procs, new_procs =
    List.partition (CallGraph.contains_proc prev_call_graph) changed_files_procs
  in
  let other_changed_procs, other_new_procs =
    List.partition (CallGraph.contains_proc prev_call_graph) new_files_procs
  in
  let changed_procs = changed_procs @ other_changed_procs in
  let new_procs = new_procs @ other_new_procs in
  let all_prev_procs = SS.of_list (CallGraph.get_proc_names prev_call_graph) in
  let all_current_procs = SS.of_list (Prog.get_noninternal_proc_names prog) in
  let deleted_procs = to_list (SS.diff all_prev_procs all_current_procs) in
  (changed_procs, new_procs, deleted_procs)

let get_changed_preds prog changed_files new_files prev_call_graph =
  let changed_files_preds =
    map_concat (get_preds_with_path prog) changed_files
  in
  let new_files_preds = map_concat (get_preds_with_path prog) new_files in
  let changed_preds =
    List.filter (CallGraph.contains_pred prev_call_graph) changed_files_preds
  in
  let other_changed_preds =
    List.filter (CallGraph.contains_pred prev_call_graph) new_files_preds
  in
  changed_preds @ other_changed_preds

let get_proc_callers reverse_graph proc_name =
  let proc_id = CallGraph.id_of_proc_name proc_name in
  let caller_ids = CallGraph.get_children reverse_graph proc_id in
  List.map (CallGraph.get_name reverse_graph) caller_ids

let get_pred_dependents reverse_graph pred_name =
  let rec get_dependents visited to_visit dependents =
    match to_visit with
    | []         -> dependents
    | id :: rest ->
        if not (SS.mem id visited) then
          let children = CallGraph.get_children reverse_graph id in
          let pred_ids =
            List.filter (CallGraph.is_pred reverse_graph) children
          in
          let proc_ids =
            List.filter (CallGraph.is_proc reverse_graph) children
          in
          let new_visited = SS.add id visited in
          let new_to_visit = to_visit @ pred_ids in
          let new_dependents = dependents @ proc_ids in
          get_dependents new_visited new_to_visit new_dependents
        else get_dependents visited rest dependents
  in
  let pred_id = CallGraph.id_of_pred_name pred_name in
  let dependent_proc_ids = get_dependents SS.empty [ pred_id ] [] in
  List.map (CallGraph.get_name reverse_graph) dependent_proc_ids

let get_changes prog prev_source_files prev_call_graph =
  let changed_files, new_files =
    get_changed_files prev_source_files cur_source_files
  in
  let changed_procs, new_procs, deleted_procs =
    get_changed_procs prog changed_files new_files prev_call_graph
  in
  let reverse_graph = CallGraph.to_reverse_graph prev_call_graph in
  let changed_procs_set = SS.of_list changed_procs in
  (* Determine callers of changed procedures that themselves did not change *)
  let callers = map_concat (get_proc_callers reverse_graph) changed_procs in
  let callers =
    List.filter (fun pname -> not (SS.mem pname changed_procs_set)) callers
  in
  let callers_set = SS.of_list callers in
  (* Determine transitive dependents of changed predicates *)
  let changed_preds =
    get_changed_preds prog changed_files new_files prev_call_graph
  in
  let dependent_procs =
    map_concat (get_pred_dependents reverse_graph) changed_preds
  in
  let dependent_procs =
    List.filter
      (fun name ->
        not (SS.mem name changed_procs_set || SS.mem name callers_set))
      dependent_procs
  in
  let dependents = callers @ dependent_procs in
  { changed_procs; new_procs; deleted_procs; dependents }
