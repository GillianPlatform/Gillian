(** Module specifying functions required for a type to be loggable *)
module type t = sig
  (** Type to be logged *)
  type t [@@deriving yojson]

  (** Pretty printer for the type *)
  val pp : Format.formatter -> t -> unit
end

(** Type for a module which specifies functions required for a type to be loggable *)
type 'a t = (module t with type t = 'a)

(** Type storing the functions required to log the specified type and the
    actual content to be logged *)
type loggable = L : ('a t * 'a) -> loggable

(** Pretty prints the contents of a loggable *)
val pp : loggable -> Format.formatter -> unit

(** Converts a loggable to Yojson *)
val loggable_to_yojson : loggable -> Yojson.Safe.t

(** Returns a loggable, given the required functions and content *)
val make :
  (Format.formatter -> 'a -> unit) ->
  (Yojson.Safe.t -> ('a, string) result) ->
  ('a -> Yojson.Safe.t) ->
  'a ->
  loggable

(** Returns a loggable given a string to be logged *)
val make_string : string -> loggable
