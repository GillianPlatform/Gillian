module Json = Yojson.Basic

let results_dir = ".gillian"

let sources_file = "sources.json"

let prev_results_exist () =
  Sys.file_exists results_dir && Sys.is_directory results_dir

let new_source_paths = Hashtbl.create Config.small_tbl_size

let init_source_paths paths =
  List.iter
    (fun (key, path) ->
      let contents_hash = Digest.to_hex (Digest.file path) in
      Hashtbl.add new_source_paths key contents_hash)
    paths

let clear_prev_results () = Io_utils.rm_rf results_dir

let create_results_dir () = Io_utils.safe_mkdir results_dir

let read_results () =
  let in_path = Filename.concat results_dir sources_file in
  let json = Json.from_file in_path in
  Json.pretty_print Format.std_formatter json

let write_results () =
  let json =
    `List
      (Hashtbl.fold
         (fun path hash acc ->
           `Assoc [ ("path", `String path); ("contents", `String hash) ] :: acc)
         new_source_paths [])
  in
  let () = create_results_dir () in
  let out_path = Filename.concat results_dir sources_file in
  Json.to_file ~std:true out_path json
