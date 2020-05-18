open Containers

module Filenames = struct
  let sources = "sources.json"

  let call_graph = "call_graph.json"

  let verif_results = "verif_results.json"

  let diff = "diff.txt"

  let exec_mode = "exec_mode.txt"
end

let results_dir = Config.results_dir

let prev_results_exist () =
  let results_dir = results_dir () in
  if not (Sys.file_exists results_dir && Sys.is_directory results_dir) then
    false
  else
    (* Do not use previous results if the analysis mode has changed *)
    let read_str filename =
      let file_path = Filename.concat results_dir filename in
      Io_utils.load_file file_path
    in
    let prev_exec_mode = ExecMode.of_string (read_str Filenames.exec_mode) in
    prev_exec_mode = !Config.current_exec_mode

let delete_results_dir () = Io_utils.rm_rf (results_dir ())

let create_results_dir () = Io_utils.safe_mkdir (results_dir ())

type t = {
  sources : SourceFiles.t;
  call_graph : CallGraph.t;
  results : VerificationResults.t;
  diff : string; (* Used for testing and debugging *)
}

let read_results_dir () =
  let read_json filename =
    let json_path = Filename.concat (results_dir ()) filename in
    Yojson.Safe.from_file json_path
  in
  {
    sources = SourceFiles.t_of_yojson (read_json Filenames.sources);
    call_graph = CallGraph.t_of_yojson (read_json Filenames.call_graph);
    results =
      VerificationResults.t_of_yojson (read_json Filenames.verif_results);
    diff = "";
  }

let write_results_dir { sources; call_graph; results; diff } =
  let write_json json filename =
    let json_path = Filename.concat (results_dir ()) filename in
    let channel = open_out json_path in
    Yojson.Safe.pretty_to_channel ~std:true channel json;
    close_out channel
  in
  let write_str str filename =
    let out_path = Filename.concat (results_dir ()) filename in
    let channel = open_out out_path in
    Printf.fprintf channel "%s" str;
    close_out channel
  in
  delete_results_dir ();
  create_results_dir ();
  write_json (SourceFiles.yojson_of_t sources) Filenames.sources;
  write_json (CallGraph.yojson_of_t call_graph) Filenames.call_graph;
  write_json (VerificationResults.yojson_of_t results) Filenames.verif_results;
  write_str (ExecMode.to_string !Config.current_exec_mode) Filenames.exec_mode;
  write_str diff Filenames.diff
