type phase = ParsingAndCompiling | Parsing | Preprocessing | Verification

let string_of_phase = function
  | ParsingAndCompiling -> "ParsingAndCompiling"
  | Parsing             -> "Parsing"
  | Preprocessing       -> "Preprocessing"
  | Verification        -> "Verification"

type 'a content =
  | Debug of ((('a, Format.formatter, unit) format -> 'a) -> unit)
  | Phase of phase

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
