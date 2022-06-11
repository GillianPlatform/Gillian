open VisitorUtils

type tt =
  | Skip
  | VarAssign of string * WExpr.t
  | New of string * int
  | Dispose of WExpr.t
  | Lookup of string * WExpr.t (* x := [e] *)
  | Update of WExpr.t * WExpr.t (* [e] := [e] *)
  | FunCall of string * string * WExpr.t list * (string * string list) option (* The last bit is only for internal use *)
  | While of WExpr.t * t list
  | If of WExpr.t * t list * t list
  | Logic of WLCmd.t

and t = { sid : int; sloc : CodeLoc.t; snode : tt }

let get lcmd = lcmd.snode
let get_id lcmd = lcmd.sid
let get_loc lcmd = lcmd.sloc

let make (bare_stmt : tt) (loc : CodeLoc.t) =
  let sid = Generators.gen_id () in
  { sloc = loc; sid; snode = bare_stmt }

let rec pp_list fmt =
  (WPrettyUtils.pp_list ~sep:(format_of_string ";@\n") pp) fmt

and pp fmt stmt =
  match get stmt with
  | Skip -> Format.fprintf fmt "@[%s@]" "skip"
  | VarAssign (v, e) -> Format.fprintf fmt "@[%s := %a@]" v WExpr.pp e
  | New (v, r) -> Format.fprintf fmt "@[%s := new(%i)@]" v r
  | Dispose e -> Format.fprintf fmt "@[free@ %a@]" WExpr.pp e
  | Lookup (v, e) -> Format.fprintf fmt "@[%s := [%a]@]" v WExpr.pp e
  | Update (e1, e2) ->
      Format.fprintf fmt "@[[%a] := %a@]" WExpr.pp e1 WExpr.pp e2
  | FunCall (v, f, el, _) ->
      Format.fprintf fmt "@[%s := %s(%a)@]" v f
        (WPrettyUtils.pp_list WExpr.pp)
        el
  | While (e, s) ->
      Format.fprintf fmt "@[@[<v 2>while(%a) {@\n%a@]@\n}@]" WExpr.pp e pp_list
        s
  | If (e, s1, s2) ->
      Format.fprintf fmt
        "@[@[<v 2>if(%a) {@\n%a@]@\n@[<v 2>} else {@\n%a@]@\n}@]" WExpr.pp e
        pp_list s1 pp_list s2
  | Logic lcmd -> Format.fprintf fmt "@[[[ %a ]]@]" WLCmd.pp lcmd

and pp_head fmt stmt =
  match get stmt with
  | If (e, _, _) -> Format.fprintf fmt "if (%a)" WExpr.pp e
  | _ -> pp fmt stmt

let is_while s =
  match get s with
  | While _ -> true
  | _ -> false

let is_fold s =
  match get s with
  | Logic lcmd when WLCmd.is_fold lcmd -> true
  | _ -> false

let is_unfold s =
  match get s with
  | Logic lcmd when WLCmd.is_unfold lcmd -> true
  | _ -> false

let functions_called_by_list sl =
  let rec aux already = function
    | [] -> already
    | { snode = FunCall (_, fname, _, _); _ } :: r -> aux (fname :: already) r
    | { snode = While (_, slp); _ } :: r -> aux (aux already slp @ already) r
    | { snode = If (_, slp1, slp2); _ } :: r ->
        aux (aux already slp1 @ aux already slp2 @ already) r
    | _l :: r -> aux already r
  in
  aux [] sl

let rec get_by_id id stmt =
  let expr_getter = WExpr.get_by_id id in
  let expr_list_visitor = list_visitor_builder WExpr.get_by_id id in
  let list_visitor = list_visitor_builder get_by_id id in
  let lcmd_getter = WLCmd.get_by_id id in
  let aux s =
    match get s with
    | Dispose e | Lookup (_, e) | VarAssign (_, e) -> expr_getter e
    | Update (e1, e2) -> expr_getter e1 |>> (expr_getter, e2)
    | FunCall (_, _, el, _) -> expr_list_visitor el
    | While (e, sl) -> expr_getter e |>> (list_visitor, sl)
    | If (e, sl1, sl2) ->
        expr_getter e |>> (list_visitor, sl1) |>> (list_visitor, sl2)
    | Logic lcmd -> lcmd_getter lcmd
    | New _ | Skip -> `None
  in
  let self_or_none = if get_id stmt = id then `WStmt stmt else `None in
  self_or_none |>> (aux, stmt)

(** This function checks that the statement list has at least one concrete statement
    and that every loop is preceded by an invariant
    It returns true and an empty string if it is the case, false with a message otherwise *)

(* let check_consistency sl loc =
   let message_need_concrete = "This statement bloc contains only logic commands, it needs concrete statements." in
   let message_need_invariant =  "This while loop needs to be preceeded by an invariant." in
   let message_empty_statement = "This bloc cannot be empty, it needs at least one concrete statement." in
   let lcmd_is_inv lcmd =
     match (WLCmd.get lcmd) with
     | WLCmd.Invariant _ -> true
     | _                 -> false
   in
   let rec aux concrete_seen has_invariant start slp =
     match slp with
     | [] when start                   -> (false, message_empty_statement, loc)
     | [] when concrete_seen           -> (true, "", loc)
     | [] (* when not concrete_seen *) -> (false, message_need_concrete, loc)
     | s::rest                         ->
       begin
         match (get s) with
         | Logic lcmd                         -> aux concrete_seen (has_invariant || lcmd_is_inv lcmd) false rest
         | While _ when not has_invariant ->
           (* TODO: make that work *)
           let while_loc = get_loc s in
           (* let cl = WCodeLens.make_add_invariant while_loc in
           let () = LSPP.new_codelens cl in  *)
           let warning = WError.build_warning_invariant while_loc in
           let () = LSPP.new_diagnostic warning in
           (* Uncomment to create error *)
           (* (false, message_need_invariant, while_loc) *)
           aux true false false rest
         | _any_other_concrete_case           -> aux true false false rest
       end
   in
   let (is_consistent, message, eloc) = aux false false true sl in
   if (not is_consistent) then (
     let () = WError.consistency_errors := true in
     let werror = WError.build_consistency_error message eloc "" in
     let () = LSPP.new_diagnostic werror in
     LSPP.output_and_exit (fun _ _ -> ()) () ();
   ) else () *)
