let harness_path () =
  let resolve_import_path fname =
    let list_paths = "." :: Js_config.import_paths in
    let rec find fn l =
      match l with
      | []     -> failwith ("Cannot resolve \"" ^ fname ^ "\"")
      | p :: r -> (
          try
            let complete_path = Filename.concat p fname in
            let _ = Unix.stat complete_path in
            complete_path
          with Unix.Unix_error (Unix.ENOENT, "stat", _) -> find fn r)
    in
    find fname list_paths
  in
  resolve_import_path "harness.js"

(* Load file from string *)
let load_file f : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let harness =
  (* Only load harness on first call *)
  let loaded_harness = ref None in
  fun () ->
    match !loaded_harness with
    | Some s -> s
    | None   ->
        let harness = load_file (harness_path ()) ^ "\n\n" in
        loaded_harness := Some harness;
        harness

(* Load a JavaScript file *)
let load_js_file path =
  let use_strict =
    if !Js_config.use_strict then "\"use strict\";\n\n" else ""
  in
  let harness = if !Js_config.js2jsil_harnessing then harness () else "" in
  Printf.sprintf "%s%s%s" use_strict harness (load_file path)
