module type S = sig
  (** Type to be logged *)
  type t [@@deriving yojson]

  (** Pretty printer for the type *)
  val pp : Format.formatter -> t -> unit

  (** {i HTML} pretty printer for the type *)
  val pp_html : Format.formatter -> t -> unit
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
  type t

  val pp : t -> Format.formatter -> unit
  val pp_html : t -> Format.formatter -> unit
  val loggable_to_yojson : t -> Yojson.Safe.t

  val make :
    (Format.formatter -> 'a -> unit) ->
    ?pp_html:(Format.formatter -> 'a -> unit) ->
    (Yojson.Safe.t -> ('a, string) result) ->
    ('a -> Yojson.Safe.t) ->
    'a ->
    t

  (** Returns a simple loggable of a string *)
  val make_string : string -> t
end
