(** This module implements what could be called the "verification" monad.
   Each step may branch, but if any branch fails, then the entirety of the
   process fails. *)
type ('a, 'b) t = ('a list, 'b list) result

let pp ~ok ~err =
  Fmt.Dump.result ~ok:(Fmt.Dump.list ok) ~error:(Fmt.Dump.list err)

let return (v : 'a) : ('a, 'b) t = Ok [ v ]
let vanish = Ok []

let flat (l : ('a, 'b) t list) : ('a, 'b) t =
  List.fold_left
    (fun acc x ->
      match (acc, x) with
      | Ok l, Ok l' -> Ok (l @ l')
      | Error l, Error l' -> Error (l @ l')
      | Ok _, (Error _ as err) | (Error _ as err), Ok _ -> err)
    (Ok []) l

let bind (x : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
  match x with
  | Error errs -> Error errs
  | Ok l ->
      (* Could be using rev_append to make it more efficient,
         but I think the lists are usually tiny here *)
      List.fold_left
        (fun acc x ->
          match (acc, f x) with
          | Ok l, Ok l' -> Ok (l @ l')
          | Error l, Error l' -> Error (l @ l')
          | Ok _, (Error _ as err) -> err
          | (Error _ as err), Ok _ -> err)
        (Ok []) l

let map (f : 'a -> 'b) (x : ('a, 'e) t) : ('b, 'e) t =
  match x with
  | Error errs -> Error errs
  | Ok l -> Ok (List.map f l)

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
end
