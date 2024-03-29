module SourceFile : sig
  type t = private {
    path : string;
    contents : string;
    mutable dependents : string list;
  }
  [@@deriving yojson]

  val make : path:string -> dependents:string list -> t
  val add_dependent : t -> string -> unit
end = struct
  type t = {
    path : string;
    contents : string;
    mutable dependents : string list;
  }
  [@@deriving yojson]

  let hash_cache = Hashtbl.create Config.small_tbl_size

  let make ~path ~dependents =
    let contents =
      if Hashtbl.mem hash_cache path then Hashtbl.find hash_cache path
      else
        let hash = Digest.to_hex (Digest.file path) in
        Hashtbl.add hash_cache path hash;
        hash
    in
    { path; contents; dependents }

  let add_dependent file dep = file.dependents <- dep :: file.dependents
end

type ('a, 'b) hashtbl = ('a, 'b) Hashtbl.t
type t = (string, SourceFile.t) hashtbl

let to_yojson t =
  Hashtbl.to_seq t |> List.of_seq |> [%to_yojson: (string * SourceFile.t) list]

let of_yojson yj =
  let ( >| ) o f = Result.map f o in
  yj |> [%of_yojson: (string * SourceFile.t) list] >| List.to_seq
  >| Hashtbl.of_seq

let make () : t = Hashtbl.create Config.small_tbl_size
let reset = Hashtbl.reset

let get_or_make_file files path =
  match Hashtbl.find_opt files path with
  | Some file -> file
  | None ->
      let file = SourceFile.make ~path ~dependents:[] in
      Hashtbl.add files path file;
      file

let add_source_file files ~path = ignore (get_or_make_file files path)

let add_dependency files ~path ~dependent_path =
  SourceFile.add_dependent (get_or_make_file files path) dependent_path

let get_file files path : SourceFile.t =
  match Hashtbl.find_opt files path with
  | Some file -> file
  | None ->
      failwith (Printf.sprintf "could not find file entry with path '%s'" path)

let get_contents_hash files ~path = (get_file files path).contents
let get_dependents files ~path = (get_file files path).dependents

let to_key_set (table : (string, 'b) Hashtbl.t) : SS.t =
  Hashtbl.fold (fun key _ keys -> SS.add key keys) table SS.empty

let get_paths_set = to_key_set
