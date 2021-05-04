(** Extension of Map with functions to serialize to and deserialize from
    yojson. A Map is a represented as a list of key-value pairs, where a
    key-value pair is list of two elements. *)

module type YojsonableOrd = sig
  type key

  include Map.OrderedType with type t = key

  val key_of_yojson : Yojson.Safe.t -> key

  val yojson_of_key : key -> Yojson.Safe.t
end

module type S = sig
  include Map.S

  val t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t

  val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
end

module Make (YojsonableOrd : YojsonableOrd) = struct
  include Map.Make (YojsonableOrd)

  let t_of_yojson (val_of_yojson : Yojson.Safe.t -> 'a) (yojson : Yojson.Safe.t)
      : 'a t =
    match yojson with
    | `List lst ->
        let map = ref empty in
        let act = function
          | `List [ k_yojson; v_yojson ] ->
              map :=
                add
                  (YojsonableOrd.key_of_yojson k_yojson)
                  (val_of_yojson v_yojson) !map
          | _ -> failwith "map_of_yojson: tuple list needed"
        in
        List.iter act lst;
        !map
    | _         -> failwith "map_of_yojson: list needed"

  let yojson_of_t (yojson_of_val : 'a -> Yojson.Safe.t) (map : 'a t) :
      Yojson.Safe.t =
    let yojson_of_kv kv =
      let k, v = kv in
      `List [ YojsonableOrd.yojson_of_key k; yojson_of_val v ]
    in
    `List (map |> to_seq |> Seq.map yojson_of_kv |> List.of_seq)
end
