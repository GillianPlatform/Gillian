type 'a t = 'a Option.t Delayed.t

let some ?learned x = Delayed.return ?learned (Some x)
let none ?learned () = Delayed.return ?learned None
let of_option = Delayed.return

let to_dr ~(none : 'e) (o : 'a t) : ('a, 'e) result Delayed.t =
  Delayed.map o (fun opt -> Option.to_result ~none opt)

let map (x : 'a t) (f : 'a -> 'b) : 'b t =
  Delayed.map x (fun z -> Option.map f z)

let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
  Delayed.bind x (function
    | Some x -> f x
    | None -> Delayed.return None)

let map_bind (x : 'a t) (f : 'a -> 'b Option.t) : 'b t =
  Delayed.map x (fun z -> Option.bind z f)

let value ~(default : 'a) (x : 'a t) : 'a Delayed.t =
  let open Delayed.Syntax in
  let+ opt = x in
  Option.value ~default opt

module Syntax = struct
  let ( let** ) = bind
  let ( let++ ) = map
  let ( let+* ) = map_bind
end
