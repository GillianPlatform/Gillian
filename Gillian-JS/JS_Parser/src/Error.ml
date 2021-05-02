type t =
  | Overlapping_Syntax
  | Unhandled_Statement  of Loc.t
  | Unhandled_Expression of Loc.t
  | NotEcmaScript5       of string * Loc.t
  | UnusedAnnotations    of string list * Loc.t
  | FlowParser           of string * string
  | LoaderError          of string * int * string * string

let str = function
  | Overlapping_Syntax ->
      Printf.sprintf
        "Something went wrong with the parser, some syntax is overlapping"
  | Unhandled_Statement loc ->
      Format.asprintf
        "The statement at location %a is not handled. Maybe because it is not \
         part of ES5"
        Loc.pp loc
  | Unhandled_Expression loc ->
      Format.asprintf
        "The expression at location %a is not handled. Maybe because it is not \
         part of ES5"
        Loc.pp loc
  | NotEcmaScript5 (s, loc) -> Format.asprintf "%s at location %a" s Loc.pp loc
  | UnusedAnnotations (sl, loc) ->
      Format.asprintf
        "At location %a, the following annotations could not be placed in the \
         AST:\n\
         %s"
        Loc.pp loc (String.concat "\n" sl)
  | FlowParser (msg, error_type) ->
      Printf.sprintf "Flow_parser failed: %s: %s" msg error_type
  | LoaderError (path, line, func, msg) ->
      Printf.sprintf "%s: line %d, function '%s':\n%s" path line func msg

exception ParserError of t
