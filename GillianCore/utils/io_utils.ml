(** Input/output helper functions *)

(** Create a folder safely *)
let safe_mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o777

(** Recursively delete a folder and its contents *)
let rec rm_r path =
  if Sys.is_directory path then (
    Sys.readdir path
    |> Array.iter (fun fname -> rm_r (Filename.concat path fname));
    Unix.rmdir path)
  else Sys.remove path

(** Same as {!rm_r}, but doesn't fail if the path doesn't exist *)
let rm_rf path = if Sys.file_exists path then rm_r path

(** Write string to file *)
let save_file path data =
  let oc = open_out path in
  output_string oc data;
  close_out oc

(** Same as {!save_file}, but uses a formatter *)
let save_file_pp
    (path : string)
    (pretty_printer : Format.formatter -> 'a -> unit)
    (data : 'a) =
  let oc = open_out path in
  let foc = Format.formatter_of_out_channel oc in
  pretty_printer foc data;
  Format.pp_print_flush foc ();
  close_out oc

(** Read a file *)
let load_file f : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(** Read multiple files *)
let get_files path =
  let open Unix in
  let rec walk acc_files paths_left =
    match paths_left with
    | [] -> acc_files
    | p :: rest -> (
        match (stat p).st_kind with
        | S_REG -> (* p is a file *) walk (p :: acc_files) rest
        | S_DIR ->
            (* p is a directory *)
            let content =
              List.map (Filename.concat p) (Array.to_list (Sys.readdir p))
            in
            (* Content is the list of paths contained in the directory *)
            walk acc_files (rest @ content)
        | _ ->
            (* p is something else that we'll ignore *)
            walk acc_files rest)
  in
  walk [] [ path ]
