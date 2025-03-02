open Lexing

type t = { loc_start : Lexing.position; loc_end : Lexing.position }

let curr lexbuf =
  let loc_start = lexeme_start_p lexbuf in
  let loc_end = lexeme_end_p lexbuf in
  { loc_start; loc_end }

let fname loc = loc.loc_start.pos_fname
let merge lstart lend = { loc_start = lstart.loc_start; loc_end = lend.loc_end }
let get_start l = l.loc_start
let get_end l = l.loc_end
let col pos = pos.pos_cnum - pos.pos_bol + 1

let[@warning "-8"] from_str str =
  let from_raw_data fname a b c d =
    let loc_start =
      { pos_fname = fname; pos_lnum = a; pos_bol = 0; pos_cnum = b - 1 }
    in
    let loc_end =
      { pos_fname = fname; pos_lnum = c; pos_bol = 0; pos_cnum = d - 1 }
    in
    { loc_start; loc_end }
  in
  let get_numbers s =
    let r = Str.regexp "[0-9]+" in
    let rec aux np k l =
      if np = 0 then List.rev l
      else
        let i = Str.search_forward r s k in
        let ms = Str.matched_string s in
        let lnth = String.length ms in
        aux (np - 1) (i + lnth) (int_of_string ms :: l)
    in
    aux 4 0 []
  in
  let [ fname; rest ] = Str.split (Str.regexp "--") str in
  let [ a; b; c; d ] = get_numbers rest in
  from_raw_data fname a b c d

let str { loc_start; loc_end } =
  let start_col = col loc_start in
  let end_col = col loc_end in
  Printf.sprintf "%s--%i:%i-%i:%i" loc_start.pos_fname loc_start.pos_lnum
    start_col loc_end.pos_lnum end_col

let json_of_pos pos =
  let character = col pos in
  let line = pos.pos_lnum in
  `Assoc [ ("line", `Int (line - 1)); ("character", `Int (character - 1)) ]

let json { loc_start; loc_end } =
  `Assoc [ ("start", json_of_pos loc_start); ("end", json_of_pos loc_end) ]

let json_with_uri loc =
  `Assoc [ ("uri", `String !WConfig.current_uri); ("range", json loc) ]

let dummy = { loc_start = dummy_pos; loc_end = dummy_pos }

let to_location (loc : t) : Gillian.Utils.Location.t =
  let to_position loc : Gillian.Utils.Location.position =
    { pos_line = loc.pos_lnum; pos_column = loc.pos_cnum - loc.pos_bol }
  in
  {
    loc_start = to_position loc.loc_start;
    loc_end = to_position loc.loc_end;
    loc_source = loc.loc_start.pos_fname;
  }
