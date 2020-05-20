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

type agnostic_content = Debug of PackedPP.t | Phase of phase

type 'a content = Agnostic of agnostic_content | Specific of 'a

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
