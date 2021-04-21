module type Loggable = sig
  (* Type to be logged *)
  type t [@@deriving yojson]

  (* Pretty prints the value *)
  val pp : Format.formatter -> t -> unit
end

type 'a loggable = (module Loggable with type t = 'a)

val pp : 'a loggable -> Format.formatter -> 'a -> unit

val of_yojson : 'a loggable -> Yojson.Safe.t -> ('a, string) result

val to_yojson : 'a loggable -> 'a -> Yojson.Safe.t

val loggable :
  (Format.formatter -> 'a -> unit) ->
  (Yojson.Safe.t -> ('a, string) result) ->
  ('a -> Yojson.Safe.t) ->
  'a loggable
