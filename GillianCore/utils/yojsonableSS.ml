(** Extension of SS with functions to serialize to and deserialize from
    yojson. A SS is a represented as a list of key-value pairs, where a
    key-value pair is list of two elements. *)

include Containers.SS

let to_yojson (set : t) : Yojson.Safe.t =
  `List (set |> to_seq |> List.of_seq |> List.map (fun e -> `String e))

let of_yojson (yojson : Yojson.Safe.t) : (t, string) result =
  let set = empty in
  match [%of_yojson: string list] yojson with
  | Ok str_list -> Ok (List.fold_left (fun set e -> add e set) set str_list)
  | Error err   -> Error err
