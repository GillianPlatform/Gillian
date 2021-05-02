type position = Flow_parser.Loc.position = { line : int; column : int }

type file_key = Flow_parser.File_key.t

type t = Flow_parser.Loc.t = {
  source : Flow_parser.File_key.t option;
  start : position;
  _end : position;
}

val none : t

val file_key_to_string : file_key -> string

val pp : Format.formatter -> t -> unit
