type uuidm = Uuidm.t

let yojson_of_uuidm uuidm = Uuidm.to_string uuidm |> yojson_of_string

let uuidm_of_yojson yojson =
  string_of_yojson yojson |> Uuidm.of_string |> Option.get

module PackedPP : sig
  type t [@@deriving yojson]

  val make : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> t

  val pf : Format.formatter -> t -> unit

  val to_string : t -> string

  val of_string : string -> t
end = struct
  type t = PP : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> t

  let make x = PP x

  let pf fmt (PP msgf) = Format.fprintf fmt |> msgf

  let to_string (PP msgf) =
    let str = ref "" in
    (fun fmt -> Format.kasprintf (fun s -> str := s) fmt) |> msgf;
    !str

  let of_string s = PP (fun m -> m "%s" s)

  let yojson_of_t t = to_string t |> yojson_of_string

  let t_of_yojson yojson = string_of_yojson yojson |> of_string
end

type phase = ParsingAndCompiling | Parsing | Preprocessing | Verification
[@@deriving yojson]

let string_of_phase = function
  | ParsingAndCompiling -> "ParsingAndCompiling"
  | Parsing             -> "Parsing"
  | Preprocessing       -> "Preprocessing"
  | Verification        -> "Verification"

type agnostic_content = Debug of PackedPP.t | Phase of phase
[@@deriving yojson]

type 'a content = Agnostic of agnostic_content | Specific of 'a
[@@deriving yojson]

type severity = Info | Log | Success | Error | Warning [@@deriving yojson]

type 'a t = {
  id : uuidm;
  title : string;
  elapsed_time : float;
  previous : uuidm option;
  parent : uuidm option;
  content : 'a content;
  severity : severity;
}
[@@deriving yojson]
