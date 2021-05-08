(** Extension of Map with functions to serialize to and deserialize from
    yojson. A Map is a represented as a list of key-value pairs, where a
    key-value pair is list of two elements. *)

module type YojsonableOrd = sig
  type key

  include Map.OrderedType with type t = key

  val key_of_yojson : Yojson.Safe.t -> (key, string) result

  val key_to_yojson : key -> Yojson.Safe.t
end

module type S = sig
  include Map.S

  val of_yojson :
    (Yojson.Safe.t -> ('a, string) result) ->
    Yojson.Safe.t ->
    ('a t, string) result

  val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
end

module Make (YojsonableOrd : YojsonableOrd) = struct
  include Map.Make (YojsonableOrd)

  let of_yojson
      (val_of_yojson : Yojson.Safe.t -> ('a, string) result)
      (yojson : Yojson.Safe.t) : ('a t, string) result =
    match yojson with
    | `List lst ->
        let kv_of_yojson kv_yojson =
          match kv_yojson with
          | `List [ k_yojson; v_yojson ] ->
              Result.bind (YojsonableOrd.key_of_yojson k_yojson) (fun k ->
                  val_of_yojson v_yojson |> Result.map (fun v -> (k, v)))
          | _ -> Error "map_of_yojson: tuple list needed"
        in

        List.fold_left
          (fun map kv_yojson : ('a t, string) result ->
            Result.bind map (fun map ->
                kv_of_yojson kv_yojson |> Result.map (fun (k, v) -> add k v map)))
          (Ok empty) lst
    | _         -> Error "map_of_yojson: list needed"

  let to_yojson (val_to_yojson : 'a -> Yojson.Safe.t) (map : 'a t) :
      Yojson.Safe.t =
    let kv_to_yojson kv =
      let k, v = kv in
      `List [ YojsonableOrd.key_to_yojson k; val_to_yojson v ]
    in
    `List (map |> to_seq |> Seq.map kv_to_yojson |> List.of_seq)
end
