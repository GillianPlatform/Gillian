type uuidm = Uuidm.t

let uuidm_to_yojson uuidm = `String (Uuidm.to_string uuidm)

let uuidm_of_yojson yojson =
  Option.to_result ~none:"uuidm should e a string"
    (match yojson with
    | `String s -> Uuidm.of_string s
    | _         -> None)

type severity = Info | Log | Success | Error | Warning
[@@deriving enum, yojson]

type t = {
  id : uuidm;
  title : string;
  elapsed_time : float;
  previous : uuidm option;
  parent : uuidm option;
  content : Loggable.loggable;
  severity : severity;
  type_ : string;
}
