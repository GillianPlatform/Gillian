type ('ok, 'err) t = ('ok, 'err) result list

let return x = [ Ok x ]
let error_with x = [ Error x ]

let bind l f =
  List.concat_map
    (function
      | Ok x -> f x
      | Error x -> [ Error x ])
    l

let filter_errors = List.filter Result.is_ok
let map f l = List.map (fun x -> Result.map f x) l
let map_error f l = List.map (fun x -> Result.map_error f x) l
let vanish = []

module Syntax = struct
  let ( let** ) = bind
  let ( let++ ) x f = map f x
end
