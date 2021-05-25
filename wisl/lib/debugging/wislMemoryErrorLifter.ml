open WSemantics
open WSyntax
open Gil_syntax
open Debugger

type merr = WislSHeap.err

type tl_ast = WParserAndCompiler.tl_ast

let get_wisl_stmt annot wisl_ast =
  Option.bind wisl_ast (fun wisl_ast ->
      Option.bind annot (fun annot ->
          let origin_id = Annot.get_origin_id annot in
          let wprog = WProg.get_by_id wisl_ast origin_id in
          match wprog with
          | `WStmt wstmt -> Some wstmt.snode
          | _            -> None))

let get_cell_var_from_cmd cmd annot wisl_ast =
  match wisl_ast with
  | Some ast -> (
      match get_wisl_stmt annot (Some ast) with
      | Some stmt -> (
          match stmt with
          | WStmt.Lookup (_, e) -> WExpr.str e
          | _                   -> "")
      | None      -> "")
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
        match get_wisl_stmt cur_annot (Some ast) with
        | Some stmt -> (
            match stmt with
            (* TODO: Catch all the cases that use after free can happen to get the
                       variable names *)
            | WStmt.Dispose e | WStmt.Lookup (_, e) -> WExpr.str e
            | _ -> "")
        | None      -> "")
    | None     -> (
        let open WislLActions in
        match gil_cmd with
        | Some cmd -> (
            match cmd with
            (* TODO: Catch all the cases that use after free can happen to get the
                     variable names *)
            | Cmd.LAction (_, name, args)
              when name = str_ac Dispose || name = str_ac GetCell -> (
                match args with
                | [ Expr.BinOp (PVar var, _, _) ] -> var
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

let get_missing_resource_var wstmt =
  match wstmt with
  | Some stmt -> (
      match stmt with
      | WStmt.Lookup (_, e) | Update (e, _) -> Some (WExpr.str e)
      | _ -> None)
  | None      -> None

let get_missing_resource_msg core_pred cur_annot wisl_ast =
  match core_pred with
  | WislLActions.Cell -> (
      let wstmt = get_wisl_stmt cur_annot wisl_ast in
      let var = get_missing_resource_var wstmt in
      match var with
      | Some var -> Fmt.str "Try adding %s -> #new_var to the specification" var
      (* TODO: Display the locations if no fix was found *)
      | None -> "We could not find a fix")
  | _                 -> WislLActions.str_ga core_pred

let error_to_exception_info merr gil_cmd cur_annot wisl_ast :
    Debugger.DebuggerTypes.exception_info =
  let id = Fmt.to_to_string WislSMemory.pp_err merr in
  let description =
    match merr with
    | WislSHeap.MissingResource core_pred ->
        Some (get_missing_resource_msg core_pred cur_annot wisl_ast)
    | DoubleFree loc ->
        let prev_annot = get_previously_freed_annot loc in
        let msg_prefix var = Fmt.str "%s already freed" var in
        Some
          (free_error_to_string msg_prefix prev_annot gil_cmd cur_annot wisl_ast)
    | UseAfterFree loc ->
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
    | _ -> None
  in
  { id; description }
