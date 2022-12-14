module type S = sig
  (** Type to be logged *)
  type t [@@deriving yojson]

  (** Pretty printer for the type *)
  val pp : Format.formatter -> t -> unit
end

module Types = struct
  (* Type for a module which specifies functions required for a type to be loggable *)
  type 'a unpacked = (module S with type t = 'a)

  (** Type storing the functions required to log the specified type and the
      actual content to be logged *)
  type t = L : ('a unpacked * 'a) -> t
end

include Types

module type Intf = sig
  (** @inline *)
  include module type of struct
    (** @inline *)
    include Types
  end

  (** Pretty prints the contents of a loggable *)
  val pp : t -> Format.formatter -> unit

  (** Converts a loggable to Yojson *)
  val loggable_to_yojson : t -> Yojson.Safe.t

  (** Returns a loggable, given the required functions and content *)
  val make :
    (Format.formatter -> 'a -> unit) ->
    (Yojson.Safe.t -> ('a, string) result) ->
    ('a -> Yojson.Safe.t) ->
    'a ->
    t

  (** Returns a loggable given a string to be logged *)
  val make_string : string -> t
end
