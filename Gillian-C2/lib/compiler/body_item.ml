open Gil_syntax
open Utils

type t = C2_annot.t * string option * string Cmd.t [@@deriving eq]

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

let make ?loop ?label ?loc ?display ?branch_kind ?cmd_kind ?nest_kind cmd : t =
  let annot =
    C2_annot.make ?origin_loc:loc ?loop_info:loop ?display ?branch_kind
      ?cmd_kind ?nest_kind ()
  in
  (annot, label, cmd)

let make_hloc ?loop ?label ?loc ?display ?cmd_kind cmd : t =
  let origin_loc = Option.map compile_location loc in
  make ?loop ?label ?loc:origin_loc ?display ?cmd_kind cmd

let map_annot f (annot, label, cmd) = (f annot, label, cmd)
let with_cmd_kind cmd_kind = map_annot (fun a -> C2_annot.{ a with cmd_kind })

let with_branch_kind branch_kind =
  map_annot (fun a -> C2_annot.{ a with branch_kind })

let set_end ?(is_end = true) (annot, label, cmd) =
  (C2_annot.set_end ~is_end annot, label, cmd)
