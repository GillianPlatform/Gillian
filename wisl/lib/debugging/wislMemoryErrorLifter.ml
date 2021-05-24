open WSemantics
open WSyntax
open Gil_syntax
open Debugger

type merr = WislSHeap.err

type tl_ast = WParserAndCompiler.tl_ast

let get_cell_var_from_cmd cmd annot wisl_ast =
  match wisl_ast with
  | Some ast -> (
      match annot with
      | Some annot -> (
          let origin_id = Annot.get_origin_id annot in
          let wprog = WProg.get_by_id ast origin_id in
          match wprog with
          | `WStmt wstmt -> (
              match wstmt.snode with
              | WStmt.Lookup (_, e) -> WExpr.str e
              | _                   -> "")
          | _            -> "")
      | None       -> "")
  | None     -> (
      let open WislLActions in
      match cmd with
      | Some cmd -> (
          match cmd with
          | Cmd.LAction (_, name, args) when name = str_ac GetCell -> (
              match args with
              | [ _; Expr.BinOp (PVar var, _, _) ] -> var
              | _ -> "")
          | _ -> "")
      | None     -> "")

let free_error_to_string msg_prefix prev_annot gil_cmd cur_annot wisl_ast =
  let var =
    match wisl_ast with
    | Some ast -> (
        match cur_annot with
        | Some annot -> (
            let origin_id = Annot.get_origin_id annot in
            let wprog = WProg.get_by_id ast origin_id in
            match wprog with
            | `WStmt wstmt -> (
                match wstmt.snode with
                | WStmt.Dispose e -> WExpr.str e
                (* TODO: Catch all the cases that use after free can happen to get the
                     variable names *)
                | WStmt.Lookup (_, e) -> WExpr.str e
                | _ -> "")
            | _            -> "")
        | None       -> "")
    | None     -> (
        let open WislLActions in
        match gil_cmd with
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
        | None     -> "")
  in
  let msg_prefix = msg_prefix var in
  match prev_annot with
  | None       -> Fmt.str "%s in specification" msg_prefix
  | Some annot -> (
      let origin_loc = Annot.get_origin_loc annot in
      match origin_loc with
      | None            -> Fmt.str "%s at unknown location" msg_prefix
      | Some origin_loc ->
          let origin_loc =
            DebuggerUtils.location_to_display_location origin_loc
          in
          Fmt.str "%s at %a" msg_prefix Location.pp origin_loc)

let get_previously_freed_annot loc =
  let annot = Logging.LogQueryer.get_previously_freed_annot loc in
  match annot with
  | None       -> None
  | Some annot ->
      annot |> Yojson.Safe.from_string |> Annot.of_yojson |> Result.to_option

let error_to_exception_info merr gil_cmd cur_annot wisl_ast :
    Debugger.DebuggerTypes.exception_info =
  let id = Fmt.to_to_string WislSMemory.pp_err merr in
  let description =
    match merr with
    | WislSHeap.DoubleFree loc  ->
        let prev_annot = get_previously_freed_annot loc in
        let msg_prefix var = Fmt.str "%s already freed" var in
        Some
          (free_error_to_string msg_prefix prev_annot gil_cmd cur_annot wisl_ast)
    | UseAfterFree loc          ->
        let prev_annot = get_previously_freed_annot loc in
        let msg_prefix var = Fmt.str "%s freed" var in
        Some
          (free_error_to_string msg_prefix prev_annot gil_cmd cur_annot wisl_ast)
    | OutOfBounds (bound, _, _) ->
        let var = get_cell_var_from_cmd gil_cmd cur_annot wisl_ast in
        Some
          (Fmt.str "%s is not in bounds %a" var
             (Fmt.option ~none:(Fmt.any "none") Fmt.int)
             bound)
    | _                         -> None
  in
  { id; description }
