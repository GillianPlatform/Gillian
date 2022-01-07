(* VSCode column numbers start from 1 while GIL location columns start from 0 *)
let location_to_display_location (loc : Location.t) : Location.t =
  let loc_source = loc.loc_source in
  let loc_start : Location.position =
    {
      pos_line = loc.loc_start.pos_line;
      pos_column = loc.loc_start.pos_column + 1;
    }
  in
  let loc_end : Location.position =
    { pos_line = loc.loc_end.pos_line; pos_column = loc.loc_end.pos_column + 1 }
  in
  { loc_start; loc_end; loc_source }
