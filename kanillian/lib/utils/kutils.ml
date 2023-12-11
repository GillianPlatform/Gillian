include Utils

module J = struct
  open Yojson.Safe.Util

  exception Parse_error of Yojson.Safe.t * string

  let ( $ ) x y = member y x
  let parse_error j msg = raise (Parse_error (j, msg))

  let catch_type_error (f : 'a -> 'b) : 'a -> 'b =
   fun x -> try f x with Type_error (s, j) -> parse_error j s

  let to_string = catch_type_error to_string
  let to_bool = catch_type_error to_bool
  let to_list = catch_type_error to_list
  let to_assoc = catch_type_error to_assoc
end

let z_of_hex s =
  let ret = ref Z.zero in
  String.iter
    (fun c ->
      let to_add =
        match c with
        | '0' .. '9' -> Char.code c - Char.code '0'
        | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
        | _ -> failwith "invalid hexadecimal value"
      in
      ret := Z.add (Z.shift_left !ret 4) (Z.of_int to_add))
    s;
  !ret

let batch_list ~batch_size l =
  if batch_size <= 0 then
    raise (Invalid_argument "batch_list: negative argument");
  let rec split_at ~acc i l =
    if i <= 0 then (List.rev acc, l)
    else
      match l with
      | [] -> (List.rev acc, [])
      | x :: l -> split_at (i - 1) ~acc:(x :: acc) l
  in
  let split_at i l = split_at ~acc:[] i l in
  let rec aux of_length acc l =
    match l with
    | [] -> List.rev acc
    | _ :: _ ->
        let sublist, l = split_at batch_size l in
        aux of_length (sublist :: acc) l
  in
  aux batch_size [] l
