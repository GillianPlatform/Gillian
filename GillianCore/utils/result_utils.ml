let of_option ~none x =
  match x with
  | None -> Error none
  | Some x -> Ok x
