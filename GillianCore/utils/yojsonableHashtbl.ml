(** Extension of Hashtbl with functions to serialize to and deserialize
    from yojson  *)

include Hashtbl

let yojson_of_t = yojson_of_hashtbl

let t_of_yojson = hashtbl_of_yojson
