open WSemantics
open WSyntax
open Gil_syntax
open Debugger

type memory_error = WislSMemory.err_t
type tl_ast = WParserAndCompiler.tl_ast
type memory = WislSMemory.t

let get_wisl_stmt gil_cmd wisl_ast =
  let open Syntaxes.Option in
  let* wisl_ast in
  let* annot =
    match gil_cmd with
    | Some (_, annot) -> Some annot
    | _ -> None
  in
  let origin_id = Annot.get_origin_id annot in
  let wprog = WProg.get_by_id wisl_ast origin_id in
  match wprog with
  | `WStmt wstmt -> Some wstmt.snode
  | _ -> None

let get_cell_var_from_cmd gil_cmd wisl_ast =
  let open Syntaxes.Option in
  match wisl_ast with
  | Some ast -> (
      let* stmt = get_wisl_stmt gil_cmd (Some ast) in
      match stmt with
      | WStmt.Lookup (_, e) | WStmt.Update (e, _) -> Some (WExpr.str e)
      | _ -> None)
  | None -> (
      let open WislLActions in
      match gil_cmd with
      | Some (Cmd.LAction (_, name, [ _; Expr.BinOp (PVar var, _, _) ]), _)
        when name = str_ac GetCell -> Some var
      | _ -> None)

let free_error_to_string msg_prefix prev_annot gil_cmd wisl_ast =
  let open Syntaxes.Option in
  let var =
    match wisl_ast with
    | Some ast -> (
        let* stmt = get_wisl_stmt gil_cmd (Some ast) in
        match stmt with
        (* TODO: Catch all the cases that use after free can happen to get the
                    variable names *)
        | WStmt.Dispose e | WStmt.Lookup (_, e) | WStmt.Update (e, _) ->
            Some (WExpr.str e)
        | _ -> None)
    | None -> (
        let open WislLActions in
        let* cmd, _ = gil_cmd in
        match cmd with
        | Cmd.LAction (_, name, [ Expr.BinOp (PVar var, _, _) ])
          when name = str_ac Dispose -> Some var
        | Cmd.LAction (_, name, [ _; Expr.BinOp (PVar var, _, _) ])
          when name = str_ac GetCell -> Some var
        | _ -> None)
  in
  let var = Option.value ~default:"" var in
  let msg_prefix = msg_prefix var in
  match prev_annot with
  | None -> Fmt.str "%s in specification" msg_prefix
  | Some annot -> (
      let origin_loc = Annot.get_origin_loc annot in
      match origin_loc with
      | None -> Fmt.str "%s at unknown location" msg_prefix
      | Some origin_loc ->
          let origin_loc =
            DebuggerUtils.location_to_display_location origin_loc
          in
          Fmt.str "%s at %a" msg_prefix Location.pp origin_loc)

let get_previously_freed_annot loc =
  let annot = Logging.LogQueryer.get_previously_freed_annot loc in
  match annot with
  | None -> None
  | Some annot ->
      annot |> Yojson.Safe.from_string |> Annot.of_yojson |> Result.to_option

let get_missing_resource_var wstmt =
  match wstmt with
  | Some stmt -> (
      match stmt with
      | WStmt.Lookup (_, e) | Update (e, _) -> Some (WExpr.str e)
      | _ -> None)
  | None -> None

let get_missing_resource_msg missing_resource_error_info gil_cmd wisl_ast =
  let core_pred, loc, offset = missing_resource_error_info in
  let default_err_msg =
    let prefix =
      Fmt.str "Missing %s at location='%s'" (WislLActions.str_ga core_pred) loc
    in
    match offset with
    | None -> prefix
    | Some offset -> Fmt.str "%s, offset='%a'" prefix Expr.pp offset
  in
  match core_pred with
  | WislLActions.Cell -> (
      let wstmt = get_wisl_stmt gil_cmd wisl_ast in
      let var = get_missing_resource_var wstmt in
      match var with
      | Some var -> Fmt.str "Try adding %s -> #new_var to the specification" var
      | None -> default_err_msg)
  | _ -> default_err_msg

let memory_error_to_exception_info info : Debugger.DebuggerTypes.exception_info
    =
  let open Gil_to_tl_lifter in
  let id = Fmt.to_to_string WislSMemory.pp_err info.error in
  let description =
    match info.error with
    | WislSHeap.MissingResource missing_resource_error_info ->
        Some
          (get_missing_resource_msg missing_resource_error_info info.command
             info.tl_ast)
    | DoubleFree loc ->
        let prev_annot = get_previously_freed_annot loc in
        let msg_prefix var = Fmt.str "%s already freed" var in
        Some
          (free_error_to_string msg_prefix prev_annot info.command info.tl_ast)
    | UseAfterFree loc ->
        let prev_annot = get_previously_freed_annot loc in
        let msg_prefix var = Fmt.str "%s freed" var in
        Some
          (free_error_to_string msg_prefix prev_annot info.command info.tl_ast)
    | OutOfBounds (bound, _, _) ->
        let var = get_cell_var_from_cmd info.command info.tl_ast in
        Some
          (Fmt.str "%a is not in bounds %a" (Fmt.option Fmt.string) var
             (Fmt.option ~none:(Fmt.any "none") Fmt.int)
             bound)
    | _ -> None
  in
  { id; description }

let add_variables = WislSMemory.add_debugger_variables
let source_map_ability = true

let get_origin_node_str wisl_ast origin_id =
  let node = WProg.get_by_id wisl_ast origin_id in
  match node with
  | `Return we -> Fmt.str "return %a" WExpr.pp we
  | `WExpr we -> Fmt.str "Evaluating: %a" WExpr.pp we
  | `WLCmd lcmd -> Fmt.str "%a" WLCmd.pp lcmd
  | `WStmt stmt -> Fmt.str "%a" WStmt.pp_head stmt
  | `WLExpr le -> Fmt.str "LEXpr: %a" WLExpr.pp le
  | `WFun f -> Fmt.str "WFun: %s" f.name
  | `None -> "No info!"
  | _ -> "Unknown Kind of Node"
