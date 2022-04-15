(** List utilities that are used in Gillian *)

(** Cross product of two lists, l1 and l2, combining its elements with function f
$   if l1 is of size n and l2 of size m, the cross product is of size [n * m]*)
let cross_product (l1 : 'a list) (l2 : 'b list) (f : 'a -> 'b -> 'c) : 'c list =
  List.fold_left (fun result e1 -> result @ List.map (f e1) l2) [] l1

let remove_duplicates l = List.sort_uniq Stdlib.compare l

let list_inter (lst1 : 'a list) (lst2 : 'a list) : 'a list =
  let lst =
    cross_product lst1 lst2 (fun a b -> if a = b then Some a else None)
  in
  List.map Option.get (List.filter (fun x -> x <> None) lst)

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

let right_combine (lst1 : 'a list) (lst2 : 'b list) : ('a * 'b) list =
  let rec loop lst1 lst2 comb_lst =
    match (lst1, lst2) with
    | _, [] -> List.rev comb_lst
    | a :: r_lst1, b :: r_lst2 -> loop r_lst1 r_lst2 ((a, b) :: comb_lst)
    | _, _ -> raise (Failure "Unsupported list right-combine.")
  in
  loop lst1 lst2 []

let get_list_somes (lst : 'a option list) : 'a list =
  let rec aux = function
    | [] -> []
    | Some x :: r -> x :: aux r
    | None :: r -> aux r
  in
  aux lst

let divide_list_by_index (lst : 'a list) (len : int) : 'a list * 'a list =
  let rec f (i : int) (l_lst : 'a list) (r_list : 'a list) : 'a list * 'a list =
    if i >= len then (List.rev l_lst, r_list)
    else
      match r_list with
      | [] -> raise (Failure "DEATH. divide_by_index")
      | hd :: r_list' -> f (i + 1) (hd :: l_lst) r_list'
  in
  f 0 [] lst

let rec flaky_map (f : 'a -> 'b option) (xs : 'a list) : 'b list option =
  match xs with
  | [] -> Some []
  | x :: xs' -> (
      match f x with
      | None -> None
      | Some y -> (
          match flaky_map f xs' with
          | None -> None
          | Some ys' -> Some (y :: ys')))

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

let make n el =
  let rec aux acc i = if i <= 0 then acc else aux (el :: acc) (i - 1) in
  aux [] n

let index_of element list =
  let rec aux cursor l =
    match l with
    | [] -> None
    | k :: _ when k = element -> Some cursor
    | _ :: r -> aux (cursor + 1) r
  in
  aux 0 list

(** [nth_or_size lst n] returns [Left v] if [v] is the nth element of [lst] or [Right sz] if
    the list is too short and is of size [sz]. Fails if provided a negative [n]. *)
let nth_or_size list n =
  if n < 0 then Fmt.failwith "Invalid nth : %d" n;
  let rec aux sz list n =
    match list with
    | [] -> Either.Right sz
    | k :: r -> if Int.equal sz n then Either.Left k else aux (sz + 1) r n
  in
  aux 0 list n
