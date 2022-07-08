let to_result = function
  | Some (Ok x) -> Ok (Some x)
  | Some (Error e) -> Error e
  | None -> Ok None

let somes_and_eq ox y =
  match ox with
  | Some x -> x = y
  | None -> false

let or_else f = function
  | Some x -> x
  | None -> f ()
