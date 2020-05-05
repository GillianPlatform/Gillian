type t = (string, string) Hashtbl.t

let make () : t = Hashtbl.create Config.small_tbl_size

let reset : t -> unit = Hashtbl.reset

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
