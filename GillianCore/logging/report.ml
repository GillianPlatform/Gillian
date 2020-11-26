type uuidm = Uuidm.t

let uuidm_to_yojson uuidm = `String (Uuidm.to_string uuidm)

let uuidm_of_yojson yojson =
  Option.to_result ~none:"uuidm should e a string"
    (match yojson with
    | `String s -> Uuidm.of_string s
    | _         -> None)

type id = int * uuidm [@@deriving yojson { exn = true }]

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

  let to_yojson t = to_string t |> fun s -> `String s

  let of_yojson yojson =
    Result.map of_string
      (match yojson with
      | `String s -> Ok s
      | _         -> Error "should be a string")
end

type agnostic_content = Debug of PackedPP.t | Phase [@@deriving yojson]

type 'a content = Agnostic of agnostic_content | Specific of 'a
[@@deriving yojson]

type severity = Info | Log | Success | Error | Warning [@@deriving yojson]

type 'a t = {
  id : id;
  title : string;
  elapsed_time : float;
  previous : id option;
  parent : id option;
  content : 'a content;
  severity : severity;
}
[@@deriving yojson]
