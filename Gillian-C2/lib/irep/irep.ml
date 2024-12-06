type t = {
  id : Id.t;
  sub : t list;
  named_sub : (Id.t * t) list;
  unique_id : int;
}

let make =
  let counter = ref 0 in
  fun ?(sub = []) ?(named_sub = []) id ->
    let unique_id = !counter in
    incr counter;
    { id; sub; named_sub; unique_id }

let nil = make Nil
let lookup name irep = List.assoc name irep.named_sub
let lookup_opt name irep = List.assoc_opt name irep.named_sub

module Infix = struct
  let ( $ ) irep name = lookup name irep
  let ( $$ ) irep name = lookup (Id.of_string name) irep
  let ( $? ) irep name = lookup_opt name irep
  let ( $$? ) irep name = lookup_opt (Id.of_string name) irep
end

let as_just_string irep = Id.to_string irep.id
let as_just_int irep = Id.to_int irep.id

let as_just_bitpattern ~width ~signed irep =
  Id.to_bitpattern ~width ~signed irep.id

let is_nil irep =
  match irep.id with
  | Id.Nil -> true
  | _ -> false

let rec of_yojson json =
  let open Kutils.J in
  let id = Id.of_yojson (json $ "id") in
  let sub =
    match json $ "sub" with
    | `Null -> []
    | `List l -> List.map of_yojson l
    | _ -> Kutils.J.parse_error json "sub is not a list"
  in
  let named_sub =
    match json $ "namedSub" with
    | `Null -> []
    | `Assoc l -> List.map (fun (n, j) -> (Id.of_string n, of_yojson j)) l
    | _ -> Kutils.J.parse_error json "namedSub is not an object!"
  in
  make ~sub ~named_sub id

let rec to_yojson irep =
  `Assoc
    [
      ("id", `String (Id.to_string irep.id));
      ("sub", `List (List.map to_yojson irep.sub));
      ( "namedSub",
        `Assoc
          (List.map
             (fun (x, i) -> (Id.to_string x, to_yojson i))
             irep.named_sub) );
    ]
