type ('ok, 'err) t = ('ok, 'err) result list

let pp ~ok ~error : ('ok, 'err) t Fmt.t =
  Fmt.Dump.list @@ Fmt.Dump.result ~ok ~error

let vanish = []
let return x = [ Ok x ]
let error_with x = [ Error x ]
let just_errors errs = List.map (fun x -> Error x) errs
let just_oks succs = List.map (fun x -> Ok x) succs

let iter_errors f l =
  List.iter
    (function
      | Ok _ -> ()
      | Error x -> f x)
    l

let bind l f =
  List.concat_map
    (function
      | Ok x -> f x
      | Error x -> [ Error x ])
    l

let bind_errors l f =
  List.concat_map
    (function
      | Ok x -> [ Ok x ]
      | Error x -> f x)
    l

let filter_errors t = List.filter Result.is_ok t
let map f l = List.map (fun x -> Result.map f x) l
let map_error f l = List.map (fun x -> Result.map_error f x) l

let split t =
  List.partition_map
    (function
      | Ok x -> Left x
      | Error x -> Right x)
    t

let all =
  let rec loop vs = function
    | [] -> return (List.rev vs)
    | t :: ts -> bind t (fun v -> loop (v :: vs) ts)
  in
  fun ts -> loop [] ts

let of_list_res (lr : ('a, 'b) List_res.t) =
  match lr with
  | Ok x -> just_oks x
  | Error y -> just_errors y

module Syntax = struct
  let ( let** ) = bind
  let ( let++ ) x f = map f x
  let ( let-- ) = bind_errors
end
