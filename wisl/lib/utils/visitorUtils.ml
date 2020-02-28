let ( |>> ) ast_el (getter, target) =
  match ast_el with
  | `None -> getter target
  | some  -> some

let list_visitor_builder getter id =
  let rec aux lst =
    match lst with
    | []      -> `None
    | el :: r -> getter id el |>> (aux, r)
  in
  aux

let list_visitor_builder2 getter id =
  let rec aux lst =
    match lst with
    | []           -> `None
    | (_, el) :: r -> getter id el |>> (aux, r)
  in
  aux
