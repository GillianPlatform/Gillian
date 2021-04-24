(**
    Module which wraps a message format to allow it to be a loggable type
*)

type t = PP : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> t

let make x = PP x

let pp fmt (PP msgf) = Format.fprintf fmt |> msgf

let to_string (PP msgf) =
  let str = ref "" in
  (fun fmt -> Format.kasprintf (fun s -> str := s) fmt) |> msgf;
  !str

let of_string s = PP (fun m -> m "%s" s)

let to_yojson t = `String (to_string t)

let of_yojson yojson =
  Result.map of_string
    (match yojson with
    | `String s -> Ok s
    | _         -> Error "Cannot parse yojson to PackedPP: should be a string")
