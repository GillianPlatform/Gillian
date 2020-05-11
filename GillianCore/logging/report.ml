type phase = ParsingAndCompiling | Parsing | Preprocessing | Verification

let string_of_phase = function
  | ParsingAndCompiling -> "ParsingAndCompiling"
  | Parsing             -> "Parsing"
  | Preprocessing       -> "Preprocessing"
  | Verification        -> "Verification"

type ('a, 'b) content =
  | Debug      of ((('a, Format.formatter, unit) format -> 'a) -> unit)
  | Phase      of phase
  | TargetLang of 'b

type severity = Info | Log | Success | Error | Warning

type ('a, 'b) t = {
  id : Uuidm.t;
  title : string;
  elapsed_time : float;
  previous : Uuidm.t option;
  parent : Uuidm.t option;
  content : ('a, 'b) content;
  severity : severity;
}
