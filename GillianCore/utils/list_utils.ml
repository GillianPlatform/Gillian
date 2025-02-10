(** Helper functions for {!List}s *)

(** Cross product of two lists, l1 and l2, combining its elements with function f

  If l1 is of size n and l2 of size m, the cross product is of size [n * m] *)
let cross_product (l1 : 'a list) (l2 : 'b list) (f : 'a -> 'b -> 'c) : 'c list =
  List.fold_left (fun result e1 -> result @ List.map (f e1) l2) [] l1

(** Uses [Stdlib.compare] to produce a list with duplicate elements removed

  The resulting list will also be sorted. *)
let remove_duplicates l = List.sort_uniq Stdlib.compare l

(** Gives the intersection of two lists
    
  This uses the polymorphic [=] *)
let intersect (lst1 : 'a list) (lst2 : 'a list) : 'a list =
  let lst =
    cross_product lst1 lst2 (fun a b -> if a = b then Some a else None)
  in
  List.map Option.get (List.filter (fun x -> x <> None) lst)

(** Gives the cross-product of a 2D list
  
  A 2D list of size [x * y] will give a product of size [x ^ y] *)
let rec list_product l =
  let rec aux ~acc l1 l2 =
    match (l1, l2) with
    | [], _ | _, [] -> acc
    | h1 :: t1, h2 :: t2 ->
        let acc = (h1 :: h2) :: acc in
        let acc = aux ~acc t1 l2 in
        aux ~acc [ h1 ] t2
    (* now we can do the actual computation *)
  in
  match l with
  | [] -> []
  | [ l1 ] -> List.map (fun x -> [ x ]) l1
  | l1 :: tl ->
      let tail_product = list_product tl in
      aux ~acc:[] l1 tail_product

(** The same as {!List.combine}, but allows the first list to be longer than the second *)
let right_combine (lst1 : 'a list) (lst2 : 'b list) : ('a * 'b) list =
  let rec loop lst1 lst2 comb_lst =
    match (lst1, lst2) with
    | _, [] -> List.rev comb_lst
    | a :: r_lst1, b :: r_lst2 -> loop r_lst1 r_lst2 ((a, b) :: comb_lst)
    | _, _ -> raise (Failure "Unsupported list right-combine.")
  in
  loop lst1 lst2 []

(** Filters [None]s from a list while unwrapping the [Some]s *)
let get_list_somes (lst : 'a option list) : 'a list = List.filter_map Fun.id lst

(** Splits a list at a specified index *)
let split_at (lst : 'a list) (len : int) : 'a list * 'a list =
  let rec f (i : int) (l_lst : 'a list) (r_list : 'a list) : 'a list * 'a list =
    if i >= len then (List.rev l_lst, r_list)
    else
      match r_list with
      | [] -> raise (Failure "split_at: list too short")
      | hd :: r_list' -> f (i + 1) (hd :: l_lst) r_list'
  in
  f 0 [] lst

(** Similar to {!List.filter_map}, but returns [None] if, for any element [x], [f x = None] *)
let rec flaky_map f =
  let ( let* ) = Option.bind in
  function
  | [] -> Some []
  | a :: r ->
      let* a = f a in
      let* r = flaky_map f r in
      Some (a :: r)

(** Returns a sublist using an offset and length

  Returns [None] if the list is too short *)
let list_sub l ofs len =
  let rec aux l i acc =
    if i >= ofs + len then Some (List.rev acc)
    else
      match l with
      | [] -> None
      | a :: r when i >= ofs -> aux r (i + 1) (a :: acc)
      | _ :: r -> aux r (i + 1) acc
  in
  aux l 0 []

(** Makes a list of a repeated element *)
let make n el =
  let rec aux acc i = if i <= 0 then acc else aux (el :: acc) (i - 1) in
  aux [] n

(** Gives the index of an element in a list
    
  Uses polymorphic [=], and returns [None] if not found *)
let index_of element list =
  let rec aux cursor l =
    match l with
    | [] -> None
    | k :: _ when k = element -> Some cursor
    | _ :: r -> aux (cursor + 1) r
  in
  aux 0 list

(** Returns [Left v] if [v] is the nth element of [lst] or [Right sz] if
    the list is too short and is of size [sz]. Fails if provided a negative [n]. *)
let nth_or_size list n =
  if n < 0 then Fmt.failwith "Invalid nth : %d" n;
  let rec aux sz list n =
    match list with
    | [] -> Either.Right sz
    | k :: r -> if Int.equal sz n then Either.Left k else aux (sz + 1) r n
  in
  aux 0 list n

(** Tests if a list starts with another
  
  Uses polymorphic [=] *)
let rec starts_with a b =
  match (a, b) with
  | _, [] -> true
  | a' :: a, b' :: b when a' = b' -> starts_with a b
  | _, _ -> false

(** Tests if a list ends with another
  
  Uses polymorphic [=] *)
let ends_with a b = starts_with (List.rev a) (List.rev b)

(** Returns the list without the first element that matches the predicate,
  and the removed element if it exists *)
let pop_where f =
  let rec aux pre = function
    | [] -> (None, List.rev pre)
    | x :: xs when f x -> (Some x, List.rev_append pre xs)
    | x :: xs -> aux (x :: pre) xs
  in
  aux []

let pop_map f =
  let rec aux pre = function
    | [] -> None
    | x :: xs -> (
        match f x with
        | Some res -> Some (res, List.rev_append pre xs)
        | None -> aux (x :: pre) xs)
  in
  aux []

(** Splits a list into head and tail
    
  Returns [(None, [])] if the list is empty *)
let hd_tl = function
  | x :: xs -> (Some x, xs)
  | [] -> (None, [])

(** [Option]-returning version of {!List.hd}*)
let hd_opt = function
  | x :: _ -> Some x
  | [] -> None

(** [Option]-returning version of {!List.tl}*)
let rec tl_opt = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: rest -> tl_opt rest

(** Prepends an element if it is [Some] *)
let cons_opt x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

(** Similar to {!flaky_map}, but with [Result]s, giving the first error if one exists *)
let map_results f l =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs -> (
        match f x with
        | Ok x -> aux (x :: acc) xs
        | Error e -> Error e)
  in
  aux [] l

let iter_results f l =
  let rec aux = function
    | [] -> Ok ()
    | x :: xs -> (
        match f x with
        | Ok () -> aux xs
        | Error e -> Error e)
  in
  aux l

let for_alli f l =
  let rec aux i = function
    | [] -> true
    | x :: xs -> if f i x then aux (i + 1) xs else false
  in
  aux 0 l

let at_least_two f l =
  let rec aux ~found_one = function
    | [] -> false
    | x :: r ->
        if f x then if found_one then true else aux ~found_one:true r
        else aux ~found_one r
  in
  aux ~found_one:false l

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs

let[@tail_mod_cons] rec filter_mapi i f = function
  | [] -> []
  | x :: r -> (
      match f i x with
      | None -> filter_mapi (i + 1) f r
      | Some x -> x :: filter_mapi (i + 1) f r)

let filter_mapi f l = filter_mapi 0 f l

let get_and_remove_nth n l =
  let found = ref None in
  let[@tail_mod_cons] rec aux i = function
    | [] -> []
    | x :: r ->
        if i = n then (
          found := Some x;
          r)
        else x :: aux (i + 1) r
  in
  (!found, aux 0 l)

let map_head f = function
  | [] -> []
  | x :: xs -> f x :: xs

let[@tail_mod_cons] rec map_last f l =
  match l with
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: l' -> x :: map_last f l'

let[@tail_mod_cons] rec assoc_replace k v = function
  | [] -> [ (k, v) ]
  | (k', _) :: r when k = k' -> (k, v) :: r
  | x :: r -> x :: assoc_replace k v r

let[@tail_mod_cons] rec drop n = function
  | [] -> []
  | l when n = 0 -> l
  | _ :: r -> drop (n - 1) r
