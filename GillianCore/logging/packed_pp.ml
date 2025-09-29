(** Module which wraps a message format to allow it to be a loggable type *)

type t = PP : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> t

let make x = PP x

let pp fmt (PP msgf) =
  try Format.fprintf fmt |> msgf
  with Sys_error s as exc ->
    let bt = Printexc.get_backtrace () in
    let () = Effect.perform (Utils.Sys_error_during_logging (s, bt)) in
    raise exc

let to_string (PP msgf) =
  try
    let str = ref "" in
    (fun fmt -> Format.kasprintf (fun s -> str := s) fmt) |> msgf;
    !str
  with Sys_error s as exc ->
    let bt = Printexc.get_backtrace () in
    let () = Effect.perform (Utils.Sys_error_during_logging (s, bt)) in
    raise exc

let of_string s = PP (fun m -> m "%s" s)
let to_yojson t = `String (to_string t)
let of_yojson yojson = Result.map of_string ([%of_yojson: string] yojson)
