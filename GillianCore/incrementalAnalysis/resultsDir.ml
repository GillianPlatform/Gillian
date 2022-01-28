module Filenames = struct
  (* Common *)

  let exec_mode = "exec_mode.txt"

  (* Used in verification, bi-abduction and individual symbolic testing *)

  let sources = "sources.json"
  let call_graph = "call_graph.json"
  let verif_results = "verif_results.json"
  let biabduction_results = "specs.json"

  (* Used in bulk symbolic testing *)

  let sources_dir = "sources"
  let call_graphs_dir = "call-graphs"
  let tests_ran = "tests_ran.txt"

  (* Used in tests and for debugging *)

  let diff = "diff.txt"
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

let read_json filename =
  let json_path = Filename.concat (results_dir ()) filename in
  Yojson.Safe.from_file json_path

let read_results_dir () =
  let sources = SourceFiles.of_yojson (read_json Filenames.sources) in
  let call_graph = CallGraph.of_yojson (read_json Filenames.call_graph) in
  (Result.get_ok sources, Result.get_ok call_graph)

let read_verif_results () =
  let sources, call_graph = read_results_dir () in
  let results_json = read_json Filenames.verif_results in
  ( sources,
    call_graph,
    Result.get_ok (VerificationResults.of_yojson results_json) )

let read_biabduction_results () =
  let sources, call_graph = read_results_dir () in
  let results_json = read_json Filenames.biabduction_results in
  ( sources,
    call_graph,
    Result.get_ok (BiAbductionResults.of_yojson results_json) )

let read_symbolic_results = read_results_dir

let read_bulk_symbolic_results () =
  let read_table dirname of_json =
    let dir_path = Filename.concat (results_dir ()) dirname in
    let json_paths = Io_utils.get_files dir_path in
    let table = Hashtbl.create Config.small_tbl_size in
    let () =
      List.iter
        (fun path ->
          let test_name = Filename.basename (Filename.chop_extension path) in
          let json = Yojson.Safe.from_file path in
          Hashtbl.add table test_name (of_json json))
        json_paths
    in
    table
  in
  let source_files =
    read_table Filenames.sources_dir (fun x ->
        Result.get_ok (SourceFiles.of_yojson x))
  in
  let call_graphs =
    read_table Filenames.call_graphs_dir (fun x ->
        Result.get_ok (CallGraph.of_yojson x))
  in
  (source_files, call_graphs)

let write_json json ?dirname filename =
  let dir_path =
    Option.fold ~none:(results_dir ())
      ~some:(fun dirname ->
        let dir_path = Filename.concat (results_dir ()) dirname in
        Io_utils.safe_mkdir dir_path;
        dir_path)
      dirname
  in
  let json_path = Filename.concat dir_path filename in
  let channel = open_out json_path in
  Yojson.Safe.pretty_to_channel ~std:true channel json;
  close_out channel

let write_str str filename =
  let out_path = Filename.concat (results_dir ()) filename in
  let channel = open_out out_path in
  Printf.fprintf channel "%s" str;
  close_out channel

let write_results_dir sources call_graph ~diff =
  delete_results_dir ();
  create_results_dir ();
  write_json (SourceFiles.to_yojson sources) Filenames.sources;
  write_json (CallGraph.to_yojson call_graph) Filenames.call_graph;
  write_str (ExecMode.to_string !Config.current_exec_mode) Filenames.exec_mode;
  write_str diff Filenames.diff

let write_verif_results sources call_graph ~diff results =
  let results_json = VerificationResults.to_yojson results in
  write_results_dir sources call_graph ~diff;
  write_json results_json Filenames.verif_results

let write_biabduction_results sources call_graph ~diff results =
  let results_json = BiAbductionResults.to_yojson results in
  write_results_dir sources call_graph ~diff;
  write_json results_json Filenames.biabduction_results

let write_symbolic_results = write_results_dir

let write_bulk_symbolic_results ~tests_ran sources_table call_graph_table =
  let write_str_list str_list filename =
    let out_path = Filename.concat (results_dir ()) filename in
    let channel = open_out out_path in
    let fmt = Format.formatter_of_out_channel channel in
    let newline = Fmt.any "@\n" in
    Fmt.pf fmt "%a@?" (Fmt.list ~sep:newline Fmt.string) str_list;
    close_out channel
  in
  let write_table table dirname to_json =
    Hashtbl.iter
      (fun test_name results ->
        write_json (to_json results) ~dirname (test_name ^ ".json"))
      table
  in
  delete_results_dir ();
  create_results_dir ();
  write_str_list tests_ran Filenames.tests_ran;
  write_table sources_table Filenames.sources_dir SourceFiles.to_yojson;
  write_table call_graph_table Filenames.call_graphs_dir CallGraph.to_yojson;
  write_str (ExecMode.to_string !Config.current_exec_mode) Filenames.exec_mode
