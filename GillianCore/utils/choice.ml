type ('c, 'r) t =
  | Vanish
  | Single of 'r
  | Choice of ('c * (unit -> ('c, 'r) t)) list

type ('c, 'r) cont = unit -> ('c, 'r) t

let onceify cont =
  let called = ref false in
  fun () ->
    if !called then failwith "Choice continuation called twice!"
    else
      let () = called := true in
      cont ()

let onceify_choices choices =
  choices |> List.map (fun (case, cont) -> (case, onceify cont))

let make ?(collapse_single = true) ?(prevent_dupe = true) choices =
  let choices = if prevent_dupe then onceify_choices choices else choices in
  match choices with
  | [] -> Vanish
  | [ (_, cont) ] when collapse_single -> cont ()
  | _ -> Choice choices

module Indexed = struct
  let make_const vs =
    let choices = vs |> List.mapi @@ fun i v -> (i, fun () -> Single v) in
    Choice choices

  let make (conts : (int, 'a) cont list) =
    let choices = List.mapi (fun i cont -> (i, cont)) conts in
    Choice choices
end

let return x = Single x
let vanish = Vanish

let rec map f = function
  | Vanish -> Vanish
  | Single x -> Single (f x)
  | Choice cs ->
      let cs' =
        cs
        |> List.map @@ fun (case, cont) ->
           let cont' () = map f (cont ()) in
           (case, cont')
      in
      Choice cs'

let rec bind f = function
  | Vanish -> Vanish
  | Single x -> f x
  | Choice cs ->
      let cs' =
        cs
        |> List.map @@ fun (case, cont) ->
           let cont' () = bind f (cont ()) in
           (case, cont')
      in
      Choice cs'

let rec fold f acc = function
  | Vanish -> acc
  | Single x -> f acc x
  | Choice cs ->
      List.fold_left
        (fun acc (_, cont) ->
          let c = cont () in
          fold f acc c)
        acc cs

let fold' f acc c =
  let rec aux acc = function
    | [] -> acc
    | [] :: contss -> aux acc contss
    | (cont :: conts) :: contss -> (
        match cont () with
        | Vanish -> aux acc (conts :: contss)
        | Single x -> aux (f acc x) (conts :: contss)
        | Choice cs -> aux acc (List.map snd cs :: conts :: contss))
  in
  aux acc [ [ (fun () -> c) ] ]

let to_list c = fold (fun acc x -> x :: acc) [] c |> List.rev

module Syntax = struct
  let ( let&* ) c f = bind f c
  let ( let&+ ) c f = map f c

  let ( let&** ) r f =
    match r with
    | Ok x -> f x
    | Error e -> Single e
end
