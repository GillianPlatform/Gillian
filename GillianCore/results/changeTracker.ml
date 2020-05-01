open Containers

type changed_files = {
  changed : string list;
  created : string list;
  deleted : string list;
}

let pp_changed_files fmt (files : changed_files) =
  let pp_paths sec fmt = function
    | []    -> Fmt.pf fmt "%s:@\n<none>@\n" sec
    | paths ->
        let newline = Fmt.any "@\n" in
        Fmt.pf fmt "%s:@\n%a@\n" sec (Fmt.list ~sep:newline Fmt.string) paths
  in
  Fmt.pf fmt "%a%a%a" (pp_paths "Changed") files.changed (pp_paths "Created")
    files.created (pp_paths "Deleted") files.deleted

let to_list (set : SS.t) : string list =
  SS.fold (fun elem acc -> elem :: acc) set []

let get_changed_files prev_paths new_paths =
  let rec get_changed paths changed =
    match paths with
    | []           -> changed
    | path :: rest ->
        (* Check if file contents have changed *)
        let prev_hash = Hashtbl.find prev_paths path in
        let new_hash = Hashtbl.find new_paths path in
        let changed =
          if String.equal prev_hash new_hash then changed else path :: changed
        in
        get_changed rest changed
  in
  let prev_paths = to_key_set prev_paths in
  let new_paths = to_key_set new_paths in
  let created = to_list (SS.diff new_paths prev_paths) in
  let deleted = to_list (SS.diff prev_paths new_paths) in
  let existing = to_list (SS.inter prev_paths new_paths) in
  let changed = get_changed existing [] in
  { changed; created; deleted }

let get_procs_with_path prog path =
  let string_opt_equal string str_opt =
    match str_opt with
    | Some str -> String.equal string str
    | None     -> false
  in
  Hashtbl.fold
    (fun pname (proc : (Annot.t, int) Proc.t) acc ->
      if string_opt_equal path proc.proc_source_path then pname :: acc else acc)
    prog.Prog.procs []

let map_concat f list = List.concat (List.map f list)

let get_proc_callers reverse_graph proc_name =
  let proc_id = CallGraph.id_of_proc_name proc_name in
  let caller_ids = CallGraph.get_children reverse_graph proc_id in
  List.map (CallGraph.get_proc_name reverse_graph) caller_ids

let is_in_graph call_graph proc_name =
  CallGraph.contains call_graph (CallGraph.id_of_proc_name proc_name)

let get_procs_to_verify prog prev_source_paths prev_call_graph =
  (* TODO (Alexis): Procedures from deleted files? *)
  let ({ changed; created; _ } as changed_files) =
    get_changed_files prev_source_paths ResultsDir.cur_source_paths
  in
  (* let () = pp_changed_files Format.std_formatter changed_files in *)
  let changed_files_procs = map_concat (get_procs_with_path prog) changed in
  let new_files_procs = map_concat (get_procs_with_path prog) created in
  (* Distinguish between new procedures and those that existed before *)
  let changed_procs, new_procs =
    List.partition (is_in_graph prev_call_graph) changed_files_procs
  in
  let other_changed_procs, other_new_procs =
    List.partition (is_in_graph prev_call_graph) new_files_procs
  in
  let changed_procs = changed_procs @ other_changed_procs in
  let new_procs = new_procs @ other_new_procs in
  (* Determine callers of changed procedures that themselves did not change *)
  let reverse_graph = CallGraph.to_reverse_graph prev_call_graph in
  let callers = map_concat (get_proc_callers reverse_graph) changed_procs in
  let changed_procs_set = SS.of_list changed_procs in
  let callers =
    List.filter (fun pname -> not (SS.mem pname changed_procs_set)) callers
  in
  print_endline "Changed procedures:";
  List.iter print_endline changed_procs;
  print_endline "Callers of changed procedures:";
  List.iter print_endline callers;
  print_endline "New procedures:";
  List.iter print_endline new_procs;
  SS.of_list (changed_procs @ callers @ new_procs)
