module KAnnot = Annot
open Gil_syntax

type t = KAnnot.t * string option * string Cmd.t [@@deriving eq]

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

let make ?loop ?label ?loc cmd : t =
  let annot = KAnnot.make ?origin_loc:loc ?loop_info:loop () in
  (annot, label, cmd)

let make_hloc ?loop ?label ?loc cmd : t =
  let origin_loc = Option.map compile_location loc in
  make ?loop ?label ?loc:origin_loc cmd
