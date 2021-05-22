open WSemantics
open Gil_syntax

type merr = WislSHeap.err

let get_cell_var_from_cmd cmd =
  let open WislLActions in
  match cmd with
  | Some cmd -> (
      match cmd with
      | Cmd.LAction (_, name, args) when name = str_ac GetCell -> (
          match args with
          | [ _; Expr.BinOp (PVar var, _, _) ] -> var
          | _ -> "")
      | _ -> "")
  | None     -> ""

let free_error_to_string msg_prefix prev_annot cmd =
  let loc_pp fmt (loc : Location.t) =
    Fmt.pf fmt "%i:%i-%i:%i" loc.loc_start.pos_line
      (loc.loc_start.pos_column + 1)
      loc.loc_end.pos_line
      (loc.loc_end.pos_column + 1)
  in
  (* TODO: Display difference variable names when debugging in GIL and WISL *)
  (* TODO: Get correct variables when intermediate GIL variables are used (e.g. x + 1) *)
  let var =
    let open WislLActions in
    match cmd with
    | Some cmd -> (
        match cmd with
        | Cmd.LAction (_, name, args) when name = str_ac Dispose -> (
            match args with
            | [ Expr.BinOp (PVar var, _, _) ] -> var
            | _ -> "")
        (* TODO: Catch all the cases that use after free can happen to get the
                 variable names *)
        | Cmd.LAction (_, name, args) when name = str_ac GetCell -> (
            match args with
            | [ _; Expr.BinOp (PVar var, _, _) ] -> var
            | _ -> "")
        | _ -> "")
    | None     -> ""
  in
  let msg_prefix = msg_prefix var in
  match prev_annot with
  | None       -> Fmt.str "%s in specification" msg_prefix
  | Some annot -> (
      let origin_loc = Annot.get_origin_loc annot in
      match origin_loc with
      | None            -> Fmt.str "%s at unknown location" msg_prefix
      | Some origin_loc -> Fmt.str "%s at %a" msg_prefix loc_pp origin_loc)

let error_to_exception_info merr cmd : Debugger.DebuggerTypes.exception_info =
  let id = Fmt.to_to_string WislSMemory.pp_err merr in
  let description =
    match merr with
    | WislSHeap.DoubleFree prev_annot ->
        let msg_prefix var = Fmt.str "%s already freed" var in
        Some (free_error_to_string msg_prefix prev_annot cmd)
    | UseAfterFree prev_annot ->
        let msg_prefix var = Fmt.str "%s freed" var in
        Some (free_error_to_string msg_prefix prev_annot cmd)
    | OutOfBounds (bound, _, _) ->
        let var = get_cell_var_from_cmd cmd in
        Some
          (Fmt.str "%s is not in bounds %a" var
             (Fmt.option ~none:(Fmt.any "none") Fmt.int)
             bound)
    | _ -> None
  in
  { id; description }
