type position = Flow_parser.Loc.position = { line : int; column : int }
[@@deriving yojson]

type file_key = Flow_parser.File_key.t

let file_key_to_yojson k =
  let open Flow_parser.File_key in
  match k with
  | LibFile s -> `List [ `String "LibFile"; `String s ]
  | SourceFile s -> `List [ `String "SourceFile"; `String s ]
  | JsonFile s -> `List [ `String "JsonFile"; `String s ]
  | ResourceFile s -> `List [ `String "ResourceFile"; `String s ]
  | Builtins -> `List [ `String "Builtins" ]

let file_key_of_yojson json =
  let open Flow_parser.File_key in
  match json with
  | `List [ `String "LibFile"; `String s ] -> Ok (LibFile s)
  | `List [ `String "SourceFile"; `String s ] -> Ok (SourceFile s)
  | `List [ `String "JsonFile"; `String s ] -> Ok (JsonFile s)
  | `List [ `String "ResourceFile"; `String s ] -> Ok (ResourceFile s)
  | `List [ `String "Builtins" ] -> Ok Builtins
  | _ -> Error "file_key_of_yojson"

type t = Flow_parser.Loc.t = {
  source : file_key option;
  start : position;
  _end : position;
}
[@@deriving yojson]

let none = Flow_parser.Loc.none
let file_key_to_string = Flow_parser.File_key.to_string
let pp : Format.formatter -> t -> unit = Flow_parser.Loc.pp
