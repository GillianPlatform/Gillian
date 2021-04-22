module type t = sig
  (* Type to be logged *)
  type t [@@deriving yojson]

  (* Pretty prints the value *)
  val pp : Format.formatter -> t -> unit
end

type 'a t = (module t with type t = 'a)

type loggable = L : ('a t * 'a) -> loggable

let pp (loggable : loggable) (formatter : Format.formatter) =
  match loggable with
  | L (t, content) ->
      let (module T) = t in
      T.pp formatter content

let loggable_to_yojson = function
  | L (t, content) ->
      let (module T) = t in
      T.to_yojson content

let make
    (type a)
    (pp : Format.formatter -> a -> unit)
    (of_yojson : Yojson.Safe.t -> (a, string) result)
    (to_yojson : a -> Yojson.Safe.t)
    (content : a) : loggable =
  let module M = struct
    type t = a

    let pp = pp

    let of_yojson = of_yojson

    let to_yojson = to_yojson
  end in
  L ((module M), content)

let make_string (s : string) : loggable =
  let pp = Fmt.string in
  let of_yojson yojson =
    match yojson with
    | `String s -> Ok s
    | _         -> Error "Cannot parse yojson to a string"
  in
  let to_yojson s = `String s in
  make pp of_yojson to_yojson s
