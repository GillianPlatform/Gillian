type 'a cell = Nil | Cons of { mutable contents : 'a; mutable next : 'a cell }
[@@deriving yojson]

type 'a t = {
  mutable length : int;
  mutable first : 'a cell;
  mutable last : 'a cell;
}
[@@deriving yojson]

let make () = { length = 0; first = Nil; last = Nil }

let clear l =
  l.length <- 0;
  l.first <- Nil;
  l.last <- Nil

let prepend x l =
  let cell = Cons { contents = x; next = l.first } in
  l.length <- l.length + 1;
  l.first <- cell

let append x l =
  let cell = Cons { contents = x; next = Nil } in
  match l.last with
  | Nil ->
      l.length <- 1;
      l.first <- cell;
      l.last <- cell
  | Cons last ->
      l.length <- l.length + 1;
      last.next <- cell;
      l.last <- cell

let add = append
let length t = t.length

let to_list t =
  let rec aux = function
    | Nil -> []
    | Cons { contents; next } -> contents :: aux next
  in
  aux t.first

let to_seq t =
  let rec aux t () =
    match t with
    | Nil -> Seq.Nil
    | Cons { contents; next } -> Seq.Cons (contents, aux next)
  in
  aux t.first

let of_list l =
  let el = make () in
  let rec aux = function
    | [] -> el
    | a :: r ->
        append a el;
        aux r
  in
  aux l

let mem ?(equal = ( = )) a l =
  let rec aux = function
    | Nil -> false
    | Cons cell when equal cell.contents a -> true
    | Cons cell -> aux cell.next
  in
  aux l.first

let copy =
  let rec copy q_res prev cell =
    match cell with
    | Nil ->
        q_res.last <- prev;
        q_res
    | Cons { contents; next } ->
        let res = Cons { contents; next = Nil } in
        (match prev with
        | Nil -> q_res.first <- res
        | Cons p -> p.next <- res);
        copy q_res res next
  in
  fun q -> copy { length = q.length; first = Nil; last = Nil } Nil q.first

let concat q2 q1 =
  if q1.length > 0 then
    match q2.last with
    | Nil ->
        q2.length <- q1.length;
        q2.first <- q1.first;
        q2.last <- q1.last;
        clear q1
    | Cons last ->
        q2.length <- q2.length + q1.length;
        last.next <- q1.first;
        q2.last <- q1.last;
        clear q1

let map_inplace f t =
  let rec aux = function
    | Nil -> ()
    | Cons cell ->
        cell.contents <- f cell.contents;
        aux cell.next
  in
  aux t.first

let fold_left f init t =
  let rec aux f acc = function
    | Nil -> acc
    | Cons { contents; next } -> aux f (f acc contents) next
  in
  aux f init t.first

let iter f t =
  let rec iter f = function
    | Nil -> ()
    | Cons { contents; next } ->
        f contents;
        iter f next
  in
  iter f t.first

let for_all2 f la lb =
  let rec aux = function
    | Cons { contents = a; _ }, Cons { contents = b; _ } when not (f a b) ->
        false
    | Cons { next = a; _ }, Cons { next = b; _ } -> aux (a, b)
    | Nil, Nil -> true
    | _ -> false
  in
  if not (Int.equal la.length lb.length) then false else aux (la.first, lb.first)

let remove_duplicates ?(equal = ( = )) l =
  let rec remove_in_rest x = function
    | Nil -> ()
    | Cons cell as whole -> (
        match cell.next with
        | Nil -> ()
        | Cons { contents; next } ->
            if equal contents x then
              let () = cell.next <- next in
              let () = l.length <- l.length - 1 in
              let () = if next = Nil then l.last <- whole in
              remove_in_rest x whole
            else remove_in_rest x cell.next)
  in
  let rec outer = function
    | Nil | Cons { next = Nil; _ } -> ()
    | Cons { contents; next } as cell ->
        remove_in_rest contents cell;
        outer next
  in
  outer l.first

(** Filters-maps the list in place according to fmap.
    For each element, if it is not filtered, the cond on the element is also checked.
    If the condition is true, then the filtering stops and the function returns true.
    If the condition is false, then the element is replaced.
    If the condition is never met, the function returns false

    For example
    [let f = (fun x ->
      if x < 5 then `Filter else if x = 10 then `Stop else `Replace (x + 1))]

    [filter_map_stop_cond f \[ 1; 2; 6;7; 4 \]] will modify the list into [\[ 7; 8 \]] and return false
    [filter_map_stop_cond f \[ 1; 2; 6; 7; 10; 8; 4 \]] will modify the list into [\[ 7; 8; 10; 8; 4 \]] and return true
*)
let filter_map_stop f l =
  let rec aux ~last_kept ~set_tail = function
    | Nil ->
        set_tail Nil;
        l.last <- last_kept;
        false
    | Cons cell as whole -> (
        match f cell.contents with
        | `Filter ->
            l.length <- l.length - 1;
            aux ~set_tail ~last_kept cell.next
        | `Replace y ->
            set_tail whole;
            cell.contents <- y;
            aux ~set_tail:(fun t -> cell.next <- t) ~last_kept:whole cell.next
        | `Stop ->
            set_tail whole;
            true)
  in
  aux ~last_kept:Nil ~set_tail:(fun t -> l.first <- t) l.first

let filter_stop_cond ~keep ~cond l =
  let f x = if keep x then if cond x then `Stop else `Replace x else `Filter in
  filter_map_stop f l

let filter keep l = ignore (filter_stop_cond ~keep ~cond:(fun _ -> false) l)

let filter_map f l =
  let fmap l =
    match f l with
    | None -> `Filter
    | Some x -> `Replace x
  in
  ignore (filter_map_stop fmap l)

let exists f l =
  let rec aux = function
    | Nil -> false
    | Cons { contents; _ } when f contents -> true
    | Cons { next; _ } -> aux next
  in
  aux l.first

let nth n l =
  if n >= l.length || n < 0 then None
  else
    let rec aux curr = function
      | Cons { contents; _ } when Int.equal curr 0 -> Some contents
      | Cons { next; _ } -> aux (curr - 1) next
      | Nil -> failwith "impossible"
    in
    aux n l.first

let pp ~sep pp_el = Fmt.iter ~sep iter pp_el

let of_yojson v_of_yojson = function
  | `List l ->
      let open Syntaxes.Result in
      List.fold_left
        (fun acc el ->
          let* acc = acc in
          let* el = v_of_yojson el in
          let () = append el acc in
          Ok acc)
        (Ok (make ()))
        l
  | _ -> Error "Invalid yojson Extlist"

let to_yojson v_to_yojson l =
  `List (to_seq l |> Seq.map v_to_yojson |> List.of_seq)
