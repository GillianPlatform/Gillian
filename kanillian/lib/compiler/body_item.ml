open Gil_syntax

type t = K_annot.t * string option * string Cmd.t [@@deriving eq]

let pp ft t =
  let _, lab, cmd = t in
  Option.iter (fun l -> Fmt.pf ft "%s: " l) lab;
  Cmd.pp_labeled ft cmd

let compile_location (loc : Goto_lib.Location.t) =
  match loc.source with
  | None -> Location.none
  | Some source ->
      let pos_line = Option.value ~default:0 loc.line in
      let pos_column = Option.value ~default:0 loc.col in
      let loc_start = Location.{ pos_line; pos_column } in
      let loc_end = Location.{ pos_line; pos_column = pos_column + 2 } in
      Location.{ loc_source = source; loc_start; loc_end }

let get_or_set_fresh_lab ~ctx list =
  match list with
  | [] -> Error.code_error "get_or_set_fresh_lab for empty body"
  | (a, None, b) :: r ->
      let lab = Ctx.fresh_lab ctx in
      (lab, (a, Some lab, b) :: r)
  | (_, Some lab, _) :: _ -> (lab, list)

let make ?loop ?label ?loc ?tl_ref ?is_end_of_stmt cmd : t =
  let annot =
    K_annot.make ?origin_loc:loc ?loop_info:loop ?tl_ref ?is_end_of_stmt ()
  in
  (annot, label, cmd)

let make_hloc ?loop ?label ?loc ?tl_ref ?is_end_of_stmt cmd : t =
  let origin_loc = Option.map compile_location loc in
  make ?loop ?label ?loc:origin_loc ?tl_ref ?is_end_of_stmt cmd
