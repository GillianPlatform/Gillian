(** Extraction of files embedded in the binary *)

let rec mkdir_p path =
  if not (Sys.file_exists path) then (
    mkdir_p (Filename.dirname path);
    try Sys.mkdir path 0o755 with Sys_error _ when Sys.file_exists path -> ())

let write_files root files =
  List.iter
    (fun (path, contents) ->
      let path = Filename.concat root path in
      mkdir_p (Filename.dirname path);
      Out_channel.with_open_bin path (fun oc ->
          Out_channel.output_string oc contents))
    files

let cache_root () =
  match (Sys.getenv_opt "XDG_CACHE_HOME", Sys.getenv_opt "HOME") with
  | Some dir, _ when dir <> "" -> Some dir
  | _, Some home when home <> "" -> Some (Filename.concat home ".cache")
  | _ -> None

(* A predictable name in a shared temp directory could be planted by another
   user, so the fallback is a fresh directory for each run. *)
let fresh_dir ~name files =
  let dir = Filename.temp_dir "gillian-" ("-" ^ name) in
  at_exit (fun () ->
      try Io_utils.rm_rf dir with Sys_error _ | Unix.Unix_error _ -> ());
  write_files dir files;
  dir

(** [dir ~name files] returns a directory that contains [files] (relative path,
    contents). The directory is created on first use, in the user's cache
    directory, under a name that depends on the contents. *)
let dir ~name files =
  let digest =
    files
    |> List.map (fun (path, contents) ->
           Digest.string path ^ Digest.string contents)
    |> String.concat "" |> Digest.string |> Digest.to_hex
  in
  match cache_root () with
  | None -> fresh_dir ~name files
  | Some root -> (
      let final =
        Filename.concat (Filename.concat root "gillian") (name ^ "-" ^ digest)
      in
      if Sys.file_exists final then final
      else
        let tmp = Printf.sprintf "%s.tmp-%d" final (Unix.getpid ()) in
        try
          mkdir_p tmp;
          write_files tmp files;
          (* If the rename fails because [final] exists, another process won
             the race *)
          (try Sys.rename tmp final
           with Sys_error _ when Sys.file_exists final -> Io_utils.rm_rf tmp);
          final
        with Sys_error _ ->
          (try Io_utils.rm_rf tmp with Sys_error _ | Unix.Unix_error _ -> ());
          fresh_dir ~name files)
