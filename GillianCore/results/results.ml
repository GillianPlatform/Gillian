module Json = Yojson.Basic
module Json_utils = Yojson.Basic.Util
module SS = Containers.SS

let results_dir = ".gillian"

let prev_results_exist =
  Sys.file_exists results_dir && Sys.is_directory results_dir

let delete_results_dir () = Io_utils.rm_rf results_dir

let create_results_dir () = Io_utils.safe_mkdir results_dir

let sources_file = "sources.json"

let prev_source_paths = Hashtbl.create Config.small_tbl_size

let new_source_paths = Hashtbl.create Config.small_tbl_size

let read_prev_results () =
  let json_path = Filename.concat results_dir sources_file in
  let sources = Json_utils.to_list (Json.from_file json_path) in
  List.iter
    (fun source ->
      let source = Json_utils.to_assoc source in
      let path = Json_utils.to_string (List.assoc "path" source) in
      let hash = Json_utils.to_string (List.assoc "contents" source) in
      Hashtbl.add prev_source_paths path hash)
    sources

let init_source_paths paths =
  List.iter
    (fun (key, path) ->
      let contents_hash = Digest.to_hex (Digest.file path) in
      Hashtbl.add new_source_paths key contents_hash)
    paths

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

let get_changed_files () =
  let rec get_changed paths changed =
    match paths with
    | []           -> changed
    | path :: rest ->
        (* Check if file contents have changed *)
        let prev_hash = Hashtbl.find prev_source_paths path in
        let new_hash = Hashtbl.find new_source_paths path in
        let changed =
          if String.equal prev_hash new_hash then changed else path :: changed
        in
        get_changed rest changed
  in
  let prev_paths = to_key_set prev_source_paths in
  let new_paths = to_key_set new_source_paths in
  let created = to_list (SS.diff new_paths prev_paths) in
  let deleted = to_list (SS.diff prev_paths new_paths) in
  let existing = to_list (SS.inter prev_paths new_paths) in
  let changed = get_changed existing [] in
  { changed; created; deleted }

let write_results () =
  let json =
    `List
      (Hashtbl.fold
         (fun path hash acc ->
           `Assoc [ ("path", `String path); ("contents", `String hash) ] :: acc)
         new_source_paths [])
  in
  let () = create_results_dir () in
  let json_path = Filename.concat results_dir sources_file in
  Json.to_file ~std:true json_path json

let call_graph_file = "call_graph.json"

let read_call_graph () =
  let json_path = Filename.concat results_dir call_graph_file in
  CallGraph.from_json (Json.from_file json_path)

let write_call_graph () =
  let json = CallGraph.to_json GInterpreter.call_graph in
  let () = create_results_dir () in
  let json_path = Filename.concat results_dir call_graph_file in
  Json.to_file ~std:true json_path json
