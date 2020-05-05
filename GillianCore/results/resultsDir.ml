open Containers

let results_dir = Config.results_dir

let prev_results_exist =
  Sys.file_exists results_dir && Sys.is_directory results_dir

let delete_results_dir () = Io_utils.rm_rf results_dir

let create_results_dir () = Io_utils.safe_mkdir results_dir

module Filenames = struct
  let sources = "sources.json"

  let call_graph = "call_graph.json"

  let verif_results = "verif_results.json"
end

module SourcePaths = struct
  type t = (string, string) Hashtbl.t

  let filename = "sources.json"

  let make () : t = Hashtbl.create Config.small_tbl_size

  let add_source_path paths path =
    let contents_hash = Digest.to_hex (Digest.file path) in
    Hashtbl.add paths path contents_hash

  let of_yojson_exn json =
    let open Yojson.Safe.Util in
    let paths = make () in
    let () =
      List.iter
        (fun source_obj ->
          let source = to_assoc source_obj in
          let path = to_string (List.assoc "path" source) in
          let hash = to_string (List.assoc "contents" source) in
          Hashtbl.add paths path hash)
        (to_list json)
    in
    paths

  let to_yojson paths =
    `List
      (Hashtbl.fold
         (fun path hash acc ->
           `Assoc [ ("path", `String path); ("contents", `String hash) ] :: acc)
         paths [])
end

let cur_source_paths = SourcePaths.make ()

type t = {
  sources : SourcePaths.t;
  call_graph : CallGraph.t;
  results : VerificationResults.t;
}

let read_results_dir () =
  let read_json filename =
    let json_path = Filename.concat results_dir filename in
    Yojson.Safe.from_file json_path
  in
  {
    sources = SourcePaths.of_yojson_exn (read_json Filenames.sources);
    call_graph = CallGraph.of_yojson_exn (read_json Filenames.call_graph);
    results =
      VerificationResults.of_yojson_exn (read_json Filenames.verif_results);
  }

let write_results_dir { sources; call_graph; results } =
  let write_json json filename =
    let json_path = Filename.concat results_dir filename in
    let channel = open_out json_path in
    Yojson.Safe.pretty_to_channel ~std:true channel json;
    close_out channel
  in
  delete_results_dir ();
  create_results_dir ();
  write_json (SourcePaths.to_yojson sources) Filenames.sources;
  write_json (CallGraph.to_yojson call_graph) Filenames.call_graph;
  write_json (VerificationResults.to_yojson results) Filenames.verif_results
