open Containers

type t = {
  changed_procs : string list;
  new_procs : string list;
  deleted_procs : string list;
  dependent_procs : string list;
  changed_lemmas : string list;
  new_lemmas : string list;
  deleted_lemmas : string list;
  dependent_lemmas : string list;
}

let pp fmt changes =
  let pp_elem_list sec fmt = function
    | []    -> Fmt.pf fmt "%s:@\n<none>@\n" sec
    | procs ->
        let newline = Fmt.any "@\n" in
        Fmt.pf fmt "%s:@\n%a@\n" sec (Fmt.list ~sep:newline Fmt.string) procs
  in
  let pp_elems sec fmt elems =
    pp_elem_list sec fmt (List.sort String.compare elems)
  in
  Fmt.pf fmt "%a%a%a%a" (pp_elems "Changed procs") changes.changed_procs
    (pp_elems "New procs") changes.new_procs (pp_elems "Deleted procs")
    changes.deleted_procs
    (pp_elems "Transitive dependents")
    changes.dependent_procs

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
  Option.fold ~none:false ~some:(fun str -> String.equal string str) str_opt

let get_procs_with_path (prog : ('a, 'b) Prog.t) path =
  Hashtbl.fold
    (fun name (proc : ('a, 'b) Proc.t) acc ->
      if string_opt_equal path proc.proc_source_path then name :: acc else acc)
    prog.procs []

let get_preds_with_path (prog : ('a, 'b) Prog.t) path =
  Hashtbl.fold
    (fun name (pred : Pred.t) acc ->
      if string_opt_equal path pred.pred_source_path then name :: acc else acc)
    prog.preds []

let get_lemmas_with_path (prog : ('a, 'b) Prog.t) path =
  Hashtbl.fold
    (fun name (lemma : Lemma.t) acc ->
      if string_opt_equal path lemma.lemma_source_path then name :: acc else acc)
    prog.lemmas []

let map_concat f list = List.concat (List.map f list)

let get_changed_elements
    ~extract_elems
    ~cg_contains
    ~cg_get_all
    ~cur_elems
    changed_files
    new_files
    prev_call_graph =
  let changed_files_elems = map_concat extract_elems changed_files in
  let new_files_elems = map_concat extract_elems new_files in
  let changed_elems, new_elems =
    List.partition (cg_contains prev_call_graph) changed_files_elems
  in
  let other_changed_elems, other_new_elems =
    List.partition (cg_contains prev_call_graph) new_files_elems
  in
  let changed_elems = changed_elems @ other_changed_elems in
  let new_elems = new_elems @ other_new_elems in
  let all_prev_elems = SS.of_list (cg_get_all prev_call_graph) in
  let all_current_elems = SS.of_list cur_elems in
  let deleted_elems = to_list (SS.diff all_prev_elems all_current_elems) in
  (changed_elems, new_elems, deleted_elems)

let get_proc_changes prog =
  get_changed_elements ~extract_elems:(get_procs_with_path prog)
    ~cg_contains:CallGraph.contains_proc ~cg_get_all:CallGraph.get_proc_names
    ~cur_elems:(Prog.get_noninternal_proc_names prog)

let get_pred_changes prog =
  get_changed_elements ~extract_elems:(get_preds_with_path prog)
    ~cg_contains:CallGraph.contains_pred ~cg_get_all:CallGraph.get_pred_names
    ~cur_elems:(Prog.get_noninternal_pred_names prog)

let get_lemma_changes prog =
  get_changed_elements
    ~extract_elems:(get_lemmas_with_path prog)
    ~cg_contains:CallGraph.contains_lemma ~cg_get_all:CallGraph.get_lemma_names
    ~cur_elems:(Prog.get_noninternal_lemma_names prog)

let map_to_names call_graph = List.map (CallGraph.get_name call_graph)

let get_proc_callers reverse_graph proc_name ~filter_id =
  let proc_id = CallGraph.id_of_proc_name proc_name in
  let caller_ids =
    List.filter filter_id (CallGraph.get_children reverse_graph proc_id)
  in
  map_to_names reverse_graph caller_ids

let get_dependent_procs_and_lemmas reverse_graph start_ids ~filter_id =
  (* Perform a breadth-first traversal of the reverse graph *)
  let open CallGraph in
  let rec get_dependents visited to_visit dep_procs dep_lemmas =
    match to_visit with
    | []         -> (dep_procs, dep_lemmas)
    | id :: rest ->
        if not (IdSet.mem id visited) then
          let children =
            List.filter filter_id (get_children reverse_graph id)
          in
          let pred_ids = List.filter (is_pred reverse_graph) children in
          let proc_ids = List.filter (is_proc reverse_graph) children in
          let lemma_ids = List.filter (is_lemma reverse_graph) children in
          let new_visited = IdSet.add id visited in
          let new_to_visit = to_visit @ pred_ids @ lemma_ids in
          let proc_names = map_to_names reverse_graph proc_ids in
          let lemma_names = map_to_names reverse_graph lemma_ids in
          let new_dep_procs = SS.union dep_procs (SS.of_list proc_names) in
          let new_dep_lemmas = SS.union dep_lemmas (SS.of_list lemma_names) in
          get_dependents new_visited new_to_visit new_dep_procs new_dep_lemmas
        else get_dependents visited rest dep_procs dep_lemmas
  in
  get_dependents IdSet.empty start_ids SS.empty SS.empty

let get_changes prog prev_source_files prev_call_graph cur_source_files =
  let changed_files, new_files =
    get_changed_files prev_source_files cur_source_files
  in
  let changed_procs, new_procs, deleted_procs =
    get_proc_changes prog changed_files new_files prev_call_graph
  in
  let changed_preds, _, _ =
    get_pred_changes prog changed_files new_files prev_call_graph
  in
  let changed_lemmas, new_lemmas, deleted_lemmas =
    get_lemma_changes prog changed_files new_files prev_call_graph
  in
  let reverse_graph = CallGraph.to_reverse_graph prev_call_graph in

  (* Determine callers of changed procedures that did not themselves change *)
  let changed_proc_ids = List.map CallGraph.id_of_proc_name changed_procs in
  let changed_pred_ids = List.map CallGraph.id_of_pred_name changed_preds in
  let changed_lemma_ids = List.map CallGraph.id_of_lemma_name changed_lemmas in
  let changed_ids = changed_proc_ids @ changed_pred_ids @ changed_lemma_ids in
  let changed_ids_set = CallGraph.IdSet.of_list changed_ids in
  let filter_id id = not (CallGraph.IdSet.mem id changed_ids_set) in
  let callers =
    map_concat (get_proc_callers reverse_graph ~filter_id) changed_procs
  in
  let dependent_procs = SS.of_list callers in

  (* Determine transitive dependents of changed predicates and lemmas *)
  let start_ids = changed_pred_ids @ changed_lemma_ids in
  let trans_dep_procs, trans_dep_lemmas =
    get_dependent_procs_and_lemmas reverse_graph start_ids ~filter_id
  in
  let dependent_procs = SS.union dependent_procs trans_dep_procs in
  let dependent_lemmas = trans_dep_lemmas in
  {
    changed_procs;
    new_procs;
    deleted_procs;
    dependent_procs = to_list dependent_procs;
    changed_lemmas;
    new_lemmas;
    deleted_lemmas;
    dependent_lemmas = to_list dependent_lemmas;
  }
