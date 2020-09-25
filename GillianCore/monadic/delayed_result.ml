type ('a, 'b) t = ('a, 'b) Result.t Delayed.t

let ok x = Delayed.return (Result.ok x)

let error x = Delayed.return (Result.error x)

let of_result = Delayed.return

let map (x : ('a, 'e) t) (f : 'a -> 'b) : ('b, 'e) t =
  Delayed.map x (fun z -> Result.map f z)

let bind (x : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
  Delayed.bind x (function
    | Ok x    -> f x
    | Error z -> Delayed.return (Error z))

let map_bind (x : ('a, 'e) t) (f : 'a -> ('b, 'e) Result.t) : ('b, 'e) t =
  Delayed.map x (fun z -> Result.bind z f)

module Syntax = struct
  let ( let** ) = bind

  let ( let++ ) = map

  let ( let+* ) = map_bind
end
