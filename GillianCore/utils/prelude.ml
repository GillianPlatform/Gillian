module Hashtbl = struct
  (** Extension of Hashtbl with functions to serialize to and deserialize
    from yojson. A Hashtbl is a represented as a list of key-value pairs,
    where a key-value pair is list of two elements.*)

  include Hashtbl

  let of_yojson
      (key_of_yojson : Yojson.Safe.t -> ('a, string) result)
      (val_of_yojson : Yojson.Safe.t -> ('b, string) result)
      (yojson : Yojson.Safe.t) : (('a, 'b) t, string) result =
    match yojson with
    | `List lst ->
        let kv_of_yojson kv_yojson =
          match kv_yojson with
          | `List [ k_yojson; v_yojson ] ->
              Result.bind (key_of_yojson k_yojson) (fun k ->
                  val_of_yojson v_yojson |> Result.map (fun v -> (k, v)))
          | _ -> Error "hashtbl_of_yojson: tuple list needed"
        in
        let hashtbl = create 0 in
        List.fold_left
          (fun hashtbl kv_yojson ->
            Result.bind hashtbl (fun hashtbl ->
                kv_of_yojson kv_yojson
                |> Result.map (fun (k, v) ->
                       let () = add hashtbl k v in
                       hashtbl)))
          (Ok hashtbl) lst
    | _ -> Error "hashtbl_of_yojson: list needed"

  let to_yojson
      (key_to_yojson : 'a -> Yojson.Safe.t)
      (val_to_yojson : 'b -> Yojson.Safe.t)
      (hashtbl : ('a, 'b) t) : Yojson.Safe.t =
    let kv_to_yojson kv =
      let k, v = kv in
      `List [ key_to_yojson k; val_to_yojson v ]
    in
    `List (hashtbl |> to_seq |> Seq.map kv_to_yojson |> List.of_seq)
end

module Map = struct
  (** Extension of Map with functions to serialize to and deserialize from
yojson. A Map is a represented as a list of key-value pairs, where a
key-value pair is list of two elements. *)

  module type OrderedType = sig
    type t [@@deriving yojson]

    include Map.OrderedType with type t := t
  end

  module type S = sig
    include Map.S

    val of_yojson :
      (Yojson.Safe.t -> ('a, string) result) ->
      Yojson.Safe.t ->
      ('a t, string) result

    val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
  end

  module Make (Ord : OrderedType) = struct
    include Map.Make (Ord)

    let of_yojson
        (val_of_yojson : Yojson.Safe.t -> ('a, string) result)
        (yojson : Yojson.Safe.t) : ('a t, string) result =
      match yojson with
      | `List lst ->
          let kv_of_yojson kv_yojson =
            match kv_yojson with
            | `List [ k_yojson; v_yojson ] ->
                Result.bind (Ord.of_yojson k_yojson) (fun k ->
                    val_of_yojson v_yojson |> Result.map (fun v -> (k, v)))
            | _ -> Error "map_of_yojson: tuple list needed"
          in

          List.fold_left
            (fun map kv_yojson : ('a t, string) result ->
              Result.bind map (fun map ->
                  kv_of_yojson kv_yojson
                  |> Result.map (fun (k, v) -> add k v map)))
            (Ok empty) lst
      | _ -> Error "map_of_yojson: list needed"

    let to_yojson (val_to_yojson : 'a -> Yojson.Safe.t) (map : 'a t) :
        Yojson.Safe.t =
      let kv_to_yojson kv =
        let k, v = kv in
        `List [ Ord.to_yojson k; val_to_yojson v ]
      in
      `List (map |> to_seq |> Seq.map kv_to_yojson |> List.of_seq)
  end
end

module Hashset = struct
  type 'a t = ('a, unit) Hashtbl.t

  let empty ?(size = 1) () : 'a t = Hashtbl.create size
  let mem (h : 'a t) (x : 'a) = Hashtbl.mem h x
  let add (h : 'a t) (x : 'a) = Hashtbl.add h x ()
  let remove (h : 'a t) (x : 'a) = Hashtbl.remove h x
  let length (h : 'a t) = Hashtbl.length h
end

module SS = Containers.SS
module SI = Containers.SI
module SN = Containers.SN
module Syntaxes = Syntaxes
