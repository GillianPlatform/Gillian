open WSemantics
open Gil_syntax

type merr = WislSHeap.err

let error_to_string merr cmd =
  let loc_pp fmt (loc : Location.t) =
    Fmt.pf fmt "%i:%i-%i:%i" loc.loc_start.pos_line
      (loc.loc_start.pos_column + 1)
      loc.loc_end.pos_line
      (loc.loc_end.pos_column + 1)
  in
  match merr with
  | WislSHeap.DoubleFree prev_annot -> (
      let var =
        match cmd with
        | Some cmd -> (
            match cmd with
            | Cmd.LAction (_, "dispose", args) -> (
                match args with
                | [ Expr.BinOp (PVar var, _, _) ] -> var
                | _ -> "")
            | _ -> "")
        | None     -> ""
      in
      let msg_prefix = Fmt.str "%a: %s already freed" WislSMemory.pp_err merr var in
      match prev_annot with
      | None       -> Fmt.str "%s in specification" msg_prefix
      | Some annot -> (
          let origin_loc = Annot.get_origin_loc annot in
          match origin_loc with
          | None            ->
              Fmt.str "%s at unknown location" msg_prefix
          | Some origin_loc ->
              Fmt.str "%s at %a" msg_prefix loc_pp origin_loc
          ))
  | _                          -> Fmt.to_to_string WislSMemory.pp_err merr
