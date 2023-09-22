let of_option ~none x =
  match x with
  | None -> Error none
  | Some x -> Ok x

let all =
  let rec loop vs = function
    | [] -> Ok (List.rev vs)
    | t :: ts -> Result.bind t (fun v -> loop (v :: vs) ts)
  in
  fun ts -> loop [] ts
