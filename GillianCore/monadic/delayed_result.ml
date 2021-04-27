type ('a, 'b) t = ('a, 'b) Result.t Delayed.t

let ok ?learned x = Delayed.return ?learned (Result.ok x)

let error ?learned x = Delayed.return ?learned (Result.error x)

let of_result = Delayed.return

let map (x : ('a, 'e) t) (f : 'a -> 'b) : ('b, 'e) t =
  Delayed.map x (fun z -> Result.map f z)

let map_error (x : ('a, 'e) t) (f : 'e -> 'ee) : ('a, 'ee) t =
  Delayed.map x (fun z -> Result.map_error f z)

let bind (x : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
  Delayed.bind x (function
    | Ok x    -> f x
    | Error z -> Delayed.return (Error z))

let map_bind (x : ('a, 'e) t) (f : 'a -> ('b, 'e) Result.t) : ('b, 'e) t =
  Delayed.map x (fun z -> Result.bind z f)

let of_do = Delayed_option.to_dr

let of_option ~(none : 'e) (o : 'a option) =
  of_result (Option.to_result ~none o)

module Syntax = struct
  let ( let** ) = bind

  let ( let++ ) = map

  let ( let+* ) = map_bind
end
