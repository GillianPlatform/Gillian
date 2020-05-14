module SourceFile : sig
  type t = private {
    path : string;
    contents : string;
    mutable dependents : string list;
  }
  [@@deriving yojson { exn = true }]

  val make : path:string -> dependents:string list -> t

  val add_dependent : t -> string -> unit
end = struct
  type t = {
    path : string;
    contents : string;
    mutable dependents : string list;
  }
  [@@deriving yojson { exn = true }]

  let make ~path ~dependents =
    let contents = Digest.to_hex (Digest.file path) in
    { path; contents; dependents }

  let add_dependent file dep = file.dependents <- dep :: file.dependents
end

type t = (string, SourceFile.t) Hashtbl.t

let make () : t = Hashtbl.create Config.small_tbl_size

let reset : t -> unit = Hashtbl.reset

let get_or_make_file files path =
  match Hashtbl.find_opt files path with
  | Some file -> file
  | None      ->
      let file = SourceFile.make ~path ~dependents:[] in
      Hashtbl.add files path file;
      file

let add_source_file files path = ignore (get_or_make_file files path)

let add_dependency files path dependent =
  SourceFile.add_dependent (get_or_make_file files path) dependent

let get_file files path : SourceFile.t =
  match Hashtbl.find_opt files path with
  | Some file -> file
  | None      ->
      failwith (Printf.sprintf "could not find file entry with path '%s'" path)

let get_contents_hash files path = (get_file files path).contents

let get_dependents files path = (get_file files path).dependents

let to_yojson files =
  `List
    (Hashtbl.fold
       (fun path file acc -> SourceFile.to_yojson file :: acc)
       files [])

let of_yojson_exn json =
  let files = make () in
  let () =
    List.iter
      (fun json ->
        let file = SourceFile.of_yojson_exn json in
        Hashtbl.add files file.path file)
      (Yojson.Safe.Util.to_list json)
  in
  files
