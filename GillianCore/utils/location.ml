(** Representation of a location in a source file *)

type position = { pos_line : int; pos_column : int } [@@deriving yojson, eq]

type t = { loc_start : position; loc_end : position; loc_source : string }
[@@deriving yojson, eq]

let none =
  let pos_none = { pos_line = 0; pos_column = 0 } in
  { loc_start = pos_none; loc_end = pos_none; loc_source = "(none)" }

let pp fmt loc =
  Fmt.pf fmt "%i:%i-%i:%i" loc.loc_start.pos_line loc.loc_start.pos_column
    loc.loc_end.pos_line loc.loc_end.pos_column

let pp_log_opt fmt loc =
  match loc with
  | Some { loc_start; loc_source; _ } ->
      Fmt.pf fmt "%s:%d:%d" loc_source loc_start.pos_line loc_start.pos_column
  | None -> Fmt.pf fmt "unknown loc"

let pp_full fmt = function
  | None -> Fmt.nop fmt ()
  | Some { loc_source; loc_start; loc_end } ->
      Fmt.pf fmt " [%s %d:%d-%s%d]" loc_source loc_start.pos_line
        (loc_start.pos_column + 1)
        (if loc_start.pos_line <> loc_end.pos_line then
           Fmt.str "%d:" loc_end.pos_line
         else "")
        (loc_end.pos_column + 1)

let min_position a b =
  if a.pos_line < b.pos_line then a
  else if a.pos_line > b.pos_line then b
  else if a.pos_column < b.pos_column then a
  else b

let max_position a b =
  if a.pos_line > b.pos_line then a
  else if a.pos_line < b.pos_line then b
  else if a.pos_column > b.pos_column then a
  else b

let merge ?(check_source = true) a b =
  let () =
    if check_source && a.loc_source <> b.loc_source then
      Fmt.failwith "Cannot merge locations from different sources: %s and %s"
        a.loc_source b.loc_source
  in
  let loc_start = min_position a.loc_start b.loc_start in
  let loc_end = max_position a.loc_end b.loc_end in
  { loc_start; loc_end; loc_source = a.loc_source }

type 'a located = 'a * t option
