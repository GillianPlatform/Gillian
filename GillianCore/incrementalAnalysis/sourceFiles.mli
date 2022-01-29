type t [@@deriving yojson]

val make : unit -> t
val reset : t -> unit
val add_source_file : t -> path:string -> unit
val add_dependency : t -> path:string -> dependent_path:string -> unit
val get_contents_hash : t -> path:string -> string
val get_dependents : t -> path:string -> string list
val get_paths_set : t -> Containers.SS.t
