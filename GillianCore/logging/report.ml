type 'a content =
  | Debug of ((('a, Format.formatter, unit) format -> 'a) -> unit)

type severity = Info | Log | Success | Error | Warning

type 'a t = {
  id : Uuidm.t;
  title : string;
  elapsed_time : float;
  previous : Uuidm.t option;
  parent : Uuidm.t option;
  content : 'a content;
  severity : severity;
}
