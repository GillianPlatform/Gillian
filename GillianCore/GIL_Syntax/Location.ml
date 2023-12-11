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
