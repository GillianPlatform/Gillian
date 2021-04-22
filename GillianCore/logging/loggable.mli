module type t = sig
  (* Type to be logged *)
  type t [@@deriving yojson]

  (* Pretty prints the value *)
  val pp : Format.formatter -> t -> unit
end

type 'a t = (module t with type t = 'a)

type loggable = L : ('a t * 'a) -> loggable

val pp : loggable -> Format.formatter -> unit

val loggable_to_yojson : loggable -> Yojson.Safe.t

val make :
  (Format.formatter -> 'a -> unit) ->
  (Yojson.Safe.t -> ('a, string) result) ->
  ('a -> Yojson.Safe.t) ->
  'a ->
  loggable

val make_string : string -> loggable
