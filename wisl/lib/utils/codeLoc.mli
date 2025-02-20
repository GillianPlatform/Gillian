type t

val curr : Lexing.lexbuf -> t
val fname : t -> string
val get_start : t -> Lexing.position
val get_end : t -> Lexing.position
val str : t -> string
val from_str : string -> t
val json_of_pos : Lexing.position -> Yojson.Safe.t
val json : t -> Yojson.Safe.t
val json_with_uri : t -> Yojson.Safe.t
val merge : t -> t -> t
val dummy : t
val to_location : t -> Gillian.Utils.Location.t
