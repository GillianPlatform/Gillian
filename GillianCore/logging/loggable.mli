module type t = sig
  (* Type to be logged *)
  type t [@@deriving yojson]

  (* Pretty prints the value *)
  val pp : Format.formatter -> t -> unit
end

type 'a t = (module t with type t = 'a)

type loggable = L : ('a t * 'a) -> loggable

(* val pp : 'a t -> Format.formatter -> 'a -> unit

val of_yojson : 'a t -> Yojson.Safe.t -> ('a, string) result

val to_yojson : 'a t -> 'a -> Yojson.Safe.t *)

(* val pp : loggable -> unit *)

(* val of_yojson : loggable -> Yojson.Safe.t -> ('a, string) result *)

val to_yojson : loggable -> Yojson.Safe.t

val make :
  (Format.formatter -> 'a -> unit) ->
  (Yojson.Safe.t -> ('a, string) result) ->
  ('a -> Yojson.Safe.t) ->
  'a ->
  loggable
