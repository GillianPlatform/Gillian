(** Module specifying functions required for a type to be loggable *)
module type t = sig
  (** Type to be logged *)
  type t [@@deriving yojson]

  (** Pretty printer for the type *)
  val pp : Format.formatter -> t -> unit
end

(* Type for a module which specifies functions required for a type to be loggable *)
type 'a t = (module t with type t = 'a)

(** Type storing the functions required to log the specified type and the
    actual content to be logged *)
type loggable = L : ('a t * 'a) -> loggable

(** Calls the pretty print function of a loggable on its content *)
let pp (loggable : loggable) (formatter : Format.formatter) =
  match loggable with
  | L (t, content) ->
      let (module T) = t in
      T.pp formatter content

(** Converts a loggable to Yojson *)
let loggable_to_yojson = function
  | L (t, content) ->
      let (module T) = t in
      T.yojson_of_t content

(** Returns a loggable, given the required functions and content *)
let make
    (type a)
    (pp : Format.formatter -> a -> unit)
    (t_of_yojson : Yojson.Safe.t -> a)
    (yojson_of_t : a -> Yojson.Safe.t)
    (content : a) : loggable =
  let module M = struct
    type t = a

    let pp = pp

    let t_of_yojson = t_of_yojson

    let yojson_of_t = yojson_of_t
  end in
  L ((module M), content)

(** Returns a loggable given a string to be logged *)
let make_string (s : string) : loggable =
  let pp = Fmt.string in
  let t_of_yojson yojson =
    match yojson with
    | `String s -> s
    | _         -> failwith "Cannot parse yojson to a string"
  in
  let yojson_of_t s = `String s in
  make pp t_of_yojson yojson_of_t s
