(** Create a folder safely *)
let safe_mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o777

(** Delete a folder and all its contents *)
let rec rm_r path =
  if Sys.is_directory path then (
    Sys.readdir path
    |> Array.iter (fun fname -> rm_r (Filename.concat path fname));
    Unix.rmdir path )
  else Sys.remove path

let rm_rf path = if Sys.file_exists path then rm_r path

(** Save string to file *)
let save_file path data =
  let oc = open_out path in
  output_string oc data;
  close_out oc

let save_file_pp
    (path : string)
    (pretty_printer : Format.formatter -> 'a -> unit)
    (data : 'a) =
  let oc = open_out path in
  let foc = Format.formatter_of_out_channel oc in
  pretty_printer foc data;
  Format.pp_print_flush foc ();
  close_out oc

(** Load a file given its path *)
let load_file f : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(** List files within a directory recursively *)
let get_files path =
  let open Unix in
  let rec walk acc_files paths_left =
    match paths_left with
    | []        -> acc_files
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
        | _     ->
            (* p is something else that we'll ignore *)
            walk acc_files rest )
  in
  walk [] [ path ]
