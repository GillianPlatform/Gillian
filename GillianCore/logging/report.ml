module PackedPP : sig
  type t

  val make : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> t

  val pf : Format.formatter -> t -> unit

  val str : t -> string
end = struct
  type t = PP : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> t

  let make x = PP x

  let pf fmt (PP msgf) = msgf @@ Format.fprintf fmt

  let str (PP msgf) =
    let str = ref "" in
    let () = msgf @@ fun fmt -> Format.kasprintf (fun s -> str := s) fmt in
    !str
end

type phase = ParsingAndCompiling | Parsing | Preprocessing | Verification

let string_of_phase = function
  | ParsingAndCompiling -> "ParsingAndCompiling"
  | Parsing             -> "Parsing"
  | Preprocessing       -> "Preprocessing"
  | Verification        -> "Verification"

type agnostic = Agnostic

type specific = Specific

type ('kind, 'tl) content =
  | Debug      : PackedPP.t -> (agnostic, _) content
  | Phase      : phase -> (agnostic, _) content
  | TargetLang : 'tl -> (specific, 'tl) content

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
