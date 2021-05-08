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

module Make (YojsonableOrd : YojsonableOrd) :
  S with type key = YojsonableOrd.key
