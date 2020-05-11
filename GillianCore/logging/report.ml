type phase = ParsingAndCompiling | Parsing | Preprocessing | Verification

let string_of_phase = function
  | ParsingAndCompiling -> "ParsingAndCompiling"
  | Parsing             -> "Parsing"
  | Preprocessing       -> "Preprocessing"
  | Verification        -> "Verification"

type target_lang_agnostic = TargetLangAgnostic

type target_lang_specific = TargetLangSpecific

type ('a, 'b, 'c) content =
  | Debug      :
      ((('b, Format.formatter, unit) format -> 'b) -> unit)
      -> (target_lang_agnostic, 'b, _) content
  | Phase      : phase -> (target_lang_agnostic, _, _) content
  | TargetLang : 'c -> (target_lang_specific, _, 'c) content

type severity = Info | Log | Success | Error | Warning

type ('a, 'b, 'c) t = {
  id : Uuidm.t;
  title : string;
  elapsed_time : float;
  previous : Uuidm.t option;
  parent : Uuidm.t option;
  content : ('a, 'b, 'c) content;
  severity : severity;
}
