(** Extension of SS with functions to serialize to and deserialize from
    yojson. A SS is a represented as a list of key-value pairs, where a
    key-value pair is list of two elements. *)

include Containers.SS

let yojson_of_t (set : t) : Yojson.Safe.t =
  `List (set |> to_seq |> List.of_seq |> List.map (fun e -> `String e))

let t_of_yojson (yojson : Yojson.Safe.t) : t =
  let set = empty in
  let str_list : string list =
    list_of_yojson
      (fun elem_yojson ->
        match elem_yojson with
        | `String e -> e
        | _         -> failwith
                         "Cannot parse yojson into SS: element must of a string")
      yojson
  in
  List.fold_left (fun set e -> add e set) set str_list
