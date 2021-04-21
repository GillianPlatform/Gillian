module type Loggable = sig
  (* Type to be logged *)
  type t [@@deriving yojson]

  (* Pretty prints the value *)
  val pp : Format.formatter -> t -> unit
end

type 'a loggable = (module Loggable with type t = 'a)

let pp (type a) (t : a loggable) =
  let (module T) = t in
  T.pp

let of_yojson (type a) (t : a loggable) =
  let (module T) = t in
  T.of_yojson

let to_yojson (type a) (t : a loggable) =
  let (module T) = t in
  T.to_yojson

let loggable
    (type a)
    (pp : Format.formatter -> a -> unit)
    (of_yojson : Yojson.Safe.t -> (a, string) result)
    (to_yojson : a -> Yojson.Safe.t) : a loggable =
  let module M = struct
    type t = a

    let pp = pp

    let of_yojson = of_yojson

    let to_yojson = to_yojson
  end in
  (module M)
