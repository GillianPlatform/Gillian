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

let bind_error x f =
  match x with
  | Ok x -> Ok x
  | Error err -> f err

let fold_bind f =
  let open Syntaxes.Result in
  List.fold_left (fun acc x ->
      let* acc = acc in
      f acc x)

let map_bind f x =
  let open Syntaxes.Result in
  let+ rev_res =
    fold_bind
      (fun acc x ->
        let+ x = f x in
        x :: acc)
      (Ok []) x
  in
  List.rev rev_res
