type position = Flow_parser.Loc.position = { line : int; column : int }

type file_key = Flow_parser.File_key.t

type t = Flow_parser.Loc.t = {
  source : file_key option;
  start : position;
  _end : position;
}

let none = Flow_parser.Loc.none

let file_key_to_string = Flow_parser.File_key.to_string

let pp : Format.formatter -> t -> unit = Flow_parser.Loc.pp
