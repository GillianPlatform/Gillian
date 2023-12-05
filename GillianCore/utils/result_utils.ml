let of_option ~none x =
  match x with
  | None -> Error none
  | Some x -> Ok x

let or_else f = function
  | Ok x -> x
  | Error e -> f e
