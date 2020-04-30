module Json = Yojson.Basic
module Json_utils = Yojson.Basic.Util
module SS = Containers.SS

let results_dir = ".gillian"

let prev_results_exist =
  Sys.file_exists results_dir && Sys.is_directory results_dir

let delete_results_dir () = Io_utils.rm_rf results_dir

let create_results_dir () = Io_utils.safe_mkdir results_dir

module Filenames = struct
  let sources = "sources.json"

  let call_graph = "call_graph.json"
end

module SourcePaths = struct
  type t = (string, string) Hashtbl.t

  let filename = "sources.json"

  let make () : t = Hashtbl.create Config.small_tbl_size

  let add_source_path paths path ?(contents_path = path) () =
    let contents_hash = Digest.to_hex (Digest.file contents_path) in
    Hashtbl.add paths path contents_hash

  let from_json json =
    let paths = make () in
    let () =
      List.iter
        (fun source_obj ->
          let source = Json_utils.to_assoc source_obj in
          let path = Json_utils.to_string (List.assoc "path" source) in
          let hash = Json_utils.to_string (List.assoc "contents" source) in
          Hashtbl.add paths path hash)
        (Json_utils.to_list json)
    in
    paths

  let to_json paths =
    `List
      (Hashtbl.fold
         (fun path hash acc ->
           `Assoc [ ("path", `String path); ("contents", `String hash) ] :: acc)
         paths [])
end

let cur_source_paths = SourcePaths.make ()

type results = { sources : SourcePaths.t; call_graph : CallGraph.t }

let read_prev_results () =
  let read_json filename =
    let json_path = Filename.concat results_dir filename in
    Json.from_file json_path
  in
  {
    sources = SourcePaths.from_json (read_json Filenames.sources);
    call_graph = CallGraph.from_json (read_json Filenames.call_graph);
  }

let write_results { sources; call_graph } =
  let write_json json filename =
    let json_path = Filename.concat results_dir filename in
    Json.to_file ~std:true json_path json
  in
  delete_results_dir ();
  create_results_dir ();
  write_json (SourcePaths.to_json sources) Filenames.sources;
  write_json (CallGraph.to_json call_graph) Filenames.call_graph

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

let to_key_set (table : (string, 'b) Hashtbl.t) : SS.t =
  Hashtbl.fold (fun key _ keys -> SS.add key keys) table SS.empty

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

let get_procs_to_verify (prog : (Annot.t, int) Prog.t) =
  if not prev_results_exist then to_key_set prog.procs
  else
    let { sources; call_graph } = read_prev_results () in
    let ({ changed; created; deleted } as changed_files) =
      get_changed_files sources cur_source_paths
    in
    let () = pp_changed_files Format.std_formatter changed_files in
    let changed_files_procs = map_concat (get_procs_with_path prog) changed in
    let new_files_procs = map_concat (get_procs_with_path prog) created in
    let deleted_files_procs = map_concat (get_procs_with_path prog) deleted in
    (* Distinguish between new procedures and those that existed before *)
    let changed_procs, new_procs =
      List.partition (is_in_graph call_graph) changed_files_procs
    in
    let moved_procs, other_new_procs =
      List.partition (is_in_graph call_graph) new_files_procs
    in
    let changed_procs = changed_procs @ moved_procs in
    let new_procs = new_procs @ other_new_procs in
    (* Determine callers of changed procedures that themselves did not change *)
    let reverse_graph = CallGraph.to_reverse_graph call_graph in
    let callers = map_concat (get_proc_callers reverse_graph) changed_procs in
    let filter = SS.of_list (changed_procs @ deleted_files_procs) in
    let callers =
      List.filter (fun pname -> not (SS.mem pname filter)) callers
    in
    print_endline "Changed procedures:";
    List.iter print_endline changed_procs;
    print_endline "Callers of changed procedures:";
    List.iter print_endline callers;
    print_endline "New procedures:";
    List.iter print_endline new_procs;
    SS.of_list (changed_procs @ callers @ new_procs)
