(* TODO: Before merging, this has to be entirely documented. Step by step, how does the compilation work.
   More exactly, the compilation of logical expr and expr, as well a stmts should be detailed. *)

open WislConstants.Prefix
open WislConstants.InternalProcs
open WislConstants.InternalPreds
open Gillian.Gil_syntax
module SS = Gillian.Utils.Containers.SS

(* Some utility functions *)

let rec list_split_3 l =
  match l with
  | [] -> ([], [], [])
  | (a, b, c) :: r ->
      let la, lb, lc = list_split_3 r in
      (a :: la, b :: lb, c :: lc)

(* Compiler functions *)
let compile_type t =
  WType.(
    match t with
    | WList -> Some Type.ListType
    | WNull -> Some Type.NullType
    | WBool -> Some Type.BooleanType
    | WString -> Some Type.StringType
    | WPtr -> Some Type.ObjectType
    | WInt -> Some Type.IntType
    | WSet -> Some Type.SetType
    | WAny -> None)

let compile_binop b =
  WBinOp.(
    match b with
    | EQUAL -> BinOp.Equal
    | LESSTHAN -> BinOp.ILessThan
    | LESSEQUAL -> BinOp.ILessThanEqual
    | PLUS -> BinOp.IPlus
    | MINUS -> BinOp.IMinus
    | TIMES -> BinOp.ITimes
    | DIV -> BinOp.IDiv
    | MOD -> BinOp.IMod
    | AND -> BinOp.And
    | OR -> BinOp.Or
    | LSTNTH -> BinOp.LstNth
    (* operators that do not exist in gil are compiled separately *)
    | _ ->
        failwith
          (Format.asprintf "compile_binop should not be used to compile %a" pp b))

let compile_unop u =
  WUnOp.(
    match u with
    | NOT -> UnOp.Not
    | LEN -> UnOp.LstLen
    | HEAD -> UnOp.Car
    | TAIL -> UnOp.Cdr
    | REV -> UnOp.LstRev)

let rec compile_val v =
  let open WVal in
  match v with
  | Bool b -> Literal.Bool b
  | Null -> Literal.Null
  | Int n -> Literal.Int (Z.of_int n)
  | Str s -> Literal.String s
  | VList l -> Literal.LList (List.map compile_val l)

let rec compile_expr ?(fname = "main") ?(is_loop_prefix = false) expr :
    (WAnnot.t * string option * string Cmd.t) list * Expr.t =
  let gen_str = Generators.gen_str fname in
  let compile_expr = compile_expr ~fname ~is_loop_prefix in
  let expr_of_string s = Expr.Lit (Literal.String s) in
  let expr_fname_of_binop b =
    WBinOp.(
      match b with
      | PLUS -> expr_of_string internal_add
      | MINUS -> expr_of_string internal_minus
      | LESSEQUAL -> expr_of_string internal_leq
      | LESSTHAN -> expr_of_string internal_lt
      | GREATEREQUAL -> expr_of_string internal_geq
      | GREATERTHAN -> expr_of_string internal_gt
      | _ ->
          failwith
            (Format.asprintf
               "Binop %a does not correspond to an internal function" WBinOp.pp
               b))
  in
  let is_internal_func =
    WBinOp.(
      function
      | PLUS | MINUS | LESSEQUAL | LESSTHAN | GREATEREQUAL | GREATERTHAN -> true
      | _ -> false)
  in
  let stmt_kind = if is_loop_prefix then Some WAnnot.LoopPrefix else None in
  let open WExpr in
  match get expr with
  | Val v -> ([], Expr.Lit (compile_val v))
  | Var "ret" ->
      failwith
        (Format.asprintf
           "ret (at location %s) is the special name used for the return\n\
           \                            value in the logic. It cannot be used \
            as a variable name"
           (CodeLoc.str (get_loc expr)))
  | Var x -> ([], Expr.PVar x)
  | BinOp (e1, WBinOp.LSTCONS, e2) ->
      let cmdl1, comp_expr1 = compile_expr e1 in
      let cmdl2, comp_expr2 = compile_expr e2 in
      let expr = Expr.NOp (LstCat, [ EList [ comp_expr1 ]; comp_expr2 ]) in
      (cmdl1 @ cmdl2, expr)
  | BinOp (e1, WBinOp.LSTCAT, e2) ->
      let cmdl1, comp_expr1 = compile_expr e1 in
      let cmdl2, comp_expr2 = compile_expr e2 in
      let expr = Expr.NOp (LstCat, [ comp_expr1; comp_expr2 ]) in
      (cmdl1 @ cmdl2, expr)
  | BinOp (e1, b, e2) when is_internal_func b ->
      (* Operator corresponds to pointer arithmetics *)
      let call_var = gen_str gvar in
      let internal_func = expr_fname_of_binop b in
      let cmdl1, comp_expr1 = compile_expr e1 in
      let cmdl2, comp_expr2 = compile_expr e2 in
      let call_i_plus =
        Cmd.Call
          (call_var, internal_func, [ comp_expr1; comp_expr2 ], None, None)
      in
      ( cmdl1 @ cmdl2
        @ [
            ( WAnnot.make ~origin_id:(get_id expr)
                ~origin_loc:(CodeLoc.to_location (get_loc expr))
                ?stmt_kind (),
              None,
              call_i_plus );
          ],
        Expr.PVar call_var )
  | BinOp (e1, b, e2) ->
      (* Operator cannot do pointer arithmetics *)
      let cmdl1, comp_e1 = compile_expr e1 in
      let cmdl2, comp_e2 = compile_expr e2 in
      (cmdl1 @ cmdl2, Expr.BinOp (comp_e1, compile_binop b, comp_e2))
  | UnOp (u, e) ->
      let cmdl, comp_expr = compile_expr e in
      (cmdl, Expr.UnOp (compile_unop u, comp_expr))
  | List el ->
      let cmds, comp_es = List.split (List.map compile_expr el) in
      let cmds = List.concat cmds in
      (cmds, Expr.EList comp_es)

(* compile_lexpr : WLExpr.t -> (string list * Asrt.t list * Expr.t)
    compiles a WLExpr into an output expression and a list of Global Assertions.
    the string list contains the name of the variables that are generated. They are existentials. *)
let rec compile_lexpr ?(fname = "main") (lexpr : WLExpr.t) :
    string list * Asrt.t * Expr.t =
  let gen_str = Generators.gen_str fname in
  let compile_lexpr = compile_lexpr ~fname in
  let expr_pname_of_binop b =
    WBinOp.(
      match b with
      | PLUS -> internal_pred_add
      | MINUS -> internal_pred_minus
      | LESSEQUAL -> internal_pred_leq
      | LESSTHAN -> internal_pred_lt
      | GREATEREQUAL -> internal_pred_geq
      | GREATERTHAN -> internal_pred_gt
      | _ ->
          failwith
            (Format.asprintf
               "Binop %a does not correspond to an internal function" WBinOp.pp
               b))
  in
  let is_internal_pred =
    WBinOp.(
      function
      | PLUS | MINUS | LESSEQUAL | LESSTHAN | GREATEREQUAL | GREATERTHAN -> true
      | _ -> false)
  in
  WLExpr.(
    match get lexpr with
    | LVal v -> ([], [], Expr.Lit (compile_val v))
    | PVar x -> ([], [], Expr.PVar x)
    | LVar x -> ([], [], Expr.LVar x)
    | LBinOp (e1, b, e2) when is_internal_pred b ->
        (* Operator corresponds to pointer arithmetics *)
        let lout = gen_str sgvar in
        let internal_pred = expr_pname_of_binop b in
        let gvars1, asrtl1, comp_expr1 = compile_lexpr e1 in
        let gvars2, asrtl2, comp_expr2 = compile_lexpr e2 in
        let pred_i_plus =
          Asrt.Pred (internal_pred, [ comp_expr1; comp_expr2; Expr.LVar lout ])
        in
        ( gvars1 @ gvars2 @ [ lout ],
          asrtl1 @ asrtl2 @ [ pred_i_plus ],
          Expr.LVar lout )
    | LBinOp (e1, WBinOp.LSTCONS, e2) ->
        let gvars1, asrt1, comp_e1 = compile_lexpr e1 in
        let gvars2, asrt2, comp_e2 = compile_lexpr e2 in
        let expr = Expr.NOp (LstCat, [ EList [ comp_e1 ]; comp_e2 ]) in
        (gvars1 @ gvars2, asrt1 @ asrt2, expr)
    | LBinOp (e1, WBinOp.LSTCAT, e2) ->
        let gvars1, asrt1, comp_e1 = compile_lexpr e1 in
        let gvars2, asrt2, comp_e2 = compile_lexpr e2 in
        let expr = Expr.NOp (LstCat, [ comp_e1; comp_e2 ]) in
        (gvars1 @ gvars2, asrt1 @ asrt2, expr)
    | LBinOp (e1, b, e2) ->
        (* Operator cannot do pointer arithmetics *)
        let gvars1, asrt1, comp_e1 = compile_lexpr e1 in
        let gvars2, asrt2, comp_e2 = compile_lexpr e2 in
        ( gvars1 @ gvars2,
          asrt1 @ asrt2,
          Expr.BinOp (comp_e1, compile_binop b, comp_e2) )
    | LUnOp (u, e) ->
        let gvars, asrt, comp_expr = compile_lexpr e in
        (gvars, asrt, Expr.UnOp (compile_unop u, comp_expr))
    | LLSub (e1, e2, e3) ->
        let gvars1, asrt1, comp_e1 = compile_lexpr e1 in
        let gvars2, asrt2, comp_e2 = compile_lexpr e2 in
        let gvars3, asrt3, comp_e3 = compile_lexpr e3 in
        ( gvars1 @ gvars2 @ gvars3,
          asrt1 @ asrt2 @ asrt3,
          Expr.LstSub (comp_e1, comp_e2, comp_e3) )
    | LEList l ->
        let gvars, asrtsl, comp_exprs =
          list_split_3 (List.map compile_lexpr l)
        in
        (List.concat gvars, List.concat asrtsl, Expr.EList comp_exprs)
    | LESet l ->
        let gvars, asrtsl, comp_exprs =
          list_split_3 (List.map compile_lexpr l)
        in
        (List.concat gvars, List.concat asrtsl, Expr.ESet comp_exprs))

(* compile_lassert returns the compiled assertion + the list of generated existentials *)
let rec compile_lassert ?(fname = "main") asser : string list * Asrt.t =
  let compile_lassert = compile_lassert ~fname in
  let gen_str = Generators.gen_str fname in
  let compile_lexpr = compile_lexpr ~fname in
  let gil_add e k =
    (* builds GIL expression that is e + k *)
    let k_e = Expr.int k in
    let open Expr.Infix in
    e + k_e
  in
  (* compiles le1 -> lle, returns the assertion AND the list of existential variables generated *)
  let rec compile_pointsto
      ?(start = true)
      ~(block : bool)
      ?(ptr_opt = None)
      ?(curr = 0)
      (le1 : WLExpr.t)
      (lle : WLExpr.t list) : string list * Asrt.t =
    let compile_pointsto = compile_pointsto ~start:false in
    let exs1, la1, (loc, offset), expr_offset =
      match ptr_opt with
      | Some (l, bo) ->
          let expr_offset =
            match bo with
            | Some o when not block -> Expr.LVar o
            | None when block -> Expr.zero_i
            | _ ->
                failwith
                  "The algorithm to compile pointsto seems to be wrong, cannot \
                   create an offset variable for a block assertion"
          in
          ([], [], (l, bo), expr_offset)
      | None ->
          let exs1, la1, e1 = compile_lexpr le1 in
          let loc = gen_str sgvar in
          let offset, expr_offset =
            if not block then
              let offset = gen_str sgvar in
              (Some offset, Expr.LVar offset)
            else (None, Expr.zero_i)
          in
          ( Option.fold ~some:(fun x -> [ x ]) ~none:[] offset @ (loc :: exs1),
            Asrt.Types
              ([ (Expr.LVar loc, Type.ObjectType) ]
              @
              match expr_offset with
              | Lit (Int _) -> []
              | _ -> [ (expr_offset, Type.IntType) ])
            :: Asrt.Pure
                 (BinOp (e1, Equal, Expr.EList [ Expr.LVar loc; expr_offset ]))
            :: la1,
            (loc, offset),
            expr_offset )
    in
    let eloc, eoffs = (Expr.LVar loc, gil_add expr_offset curr) in
    let cell = WislLActions.(str_ga Cell) in
    let bound =
      if start && block then [ Constr.bound ~loc:eloc ~bound:(List.length lle) ]
      else []
    in
    match lle with
    | [] ->
        failwith
          (Format.asprintf
             "In LPointsTo assertions, a location should always point to at \
              least one value\n\
              It is not the case in : %a" WLAssert.pp asser)
    | [ le ] ->
        let exs2, la2, e2 = compile_lexpr le in
        ( exs1 @ exs2,
          Asrt.CorePred (cell, [ eloc; eoffs ], [ e2 ]) :: (bound @ la1 @ la2)
        )
    | le :: r ->
        let exs2, la2, e2 = compile_lexpr le in
        let exs3, la3 =
          compile_pointsto ~block
            ~ptr_opt:(Some (loc, offset))
            le1 r ~curr:(curr + 1)
        in
        ( exs1 @ exs2 @ exs3,
          Asrt.CorePred (cell, [ eloc; eoffs ], [ e2 ])
          :: (bound @ la1 @ la2 @ la3) )
  in
  WLAssert.(
    match get asser with
    | LEmp -> ([], [])
    | LStar (la1, la2) ->
        let exs1, cla1 = compile_lassert la1 in
        let exs2, cla2 = compile_lassert la2 in
        (exs1 @ exs2, cla1 @ cla2)
    | LPointsTo (le1, lle) -> compile_pointsto ~block:false le1 lle
    | LBlockPointsTo (le1, lle) -> compile_pointsto ~block:true le1 lle
    | LPred (pr, lel) ->
        let exsl, all, el = list_split_3 (List.map compile_lexpr lel) in
        let exs = List.concat exsl in
        let al = List.concat all in
        (exs, Asrt.Pred (pr, el) :: al)
    | LWand { lhs = lname, largs; rhs = rname, rargs } ->
        let exs1, al1, el1 = list_split_3 (List.map compile_lexpr largs) in
        let exs2, al2, el2 = list_split_3 (List.map compile_lexpr rargs) in
        let exs = List.concat (exs1 @ exs2) in
        let al = List.concat (al1 @ al2) in
        (exs, Asrt.Wand { lhs = (lname, el1); rhs = (rname, el2) } :: al)
    | LPure lf ->
        let _, al, e = compile_lexpr lf in
        let e =
          match e with
          | LVar _ -> Expr.BinOp (e, Equal, Expr.true_)
          | _ -> e
        in
        ([], Asrt.Pure e :: al))

let rec compile_lcmd ?(fname = "main") lcmd =
  let compile_lassert = compile_lassert ~fname in
  let compile_lcmd = compile_lcmd ~fname in
  let compile_lexpr = compile_lexpr ~fname in
  let build_assert existentials lasrts =
    match lasrts with
    | [] -> None
    | _ ->
        let cmd = LCmd.SL (SLCmd.SepAssert (lasrts, existentials)) in
        (* assert (assertions) {existentials: gvars} *)
        Some cmd
  in
  let open WLCmd in
  match get lcmd with
  | Fold (pname, lel) ->
      let gvars, lasrts, params = list_split_3 (List.map compile_lexpr lel) in
      let existentials = List.concat gvars in
      let to_assert = List.concat lasrts in
      ( build_assert existentials to_assert,
        LCmd.SL (SLCmd.Fold (pname, params, None)) )
  | Unfold (pname, lel) ->
      let gvars, lasrts, params = list_split_3 (List.map compile_lexpr lel) in
      let existentials = List.concat gvars in
      let to_assert = List.concat lasrts in
      ( build_assert existentials to_assert,
        LCmd.SL (SLCmd.Unfold (pname, params, None, false)) )
  | Package { lhs = lname, largs; rhs = rname, rargs } ->
      let lgvars, lasrts, lparams =
        list_split_3 (List.map compile_lexpr largs)
      in
      let rgvars, rasrts, rparams =
        list_split_3 (List.map compile_lexpr rargs)
      in
      let existentials = List.concat (lgvars @ rgvars) in
      let to_assert = List.concat (lasrts @ rasrts) in
      ( build_assert existentials to_assert,
        LCmd.SL
          (SLCmd.Package { lhs = (lname, lparams); rhs = (rname, rparams) }) )
  | ApplyLem (ln, lel, bindings) ->
      let gvars, lasrts, params = list_split_3 (List.map compile_lexpr lel) in
      let existentials = List.concat gvars in
      let to_assert = List.concat lasrts in
      ( build_assert existentials to_assert,
        LCmd.SL (SLCmd.ApplyLem (ln, params, bindings)) )
  | LogicIf (guard, lc1, lc2) ->
      let compile_and_agregate c =
        let assert_opt, clcmd = compile_lcmd c in
        match assert_opt with
        | None -> [ clcmd ]
        | Some asser -> [ asser; clcmd ]
      in
      let existentials, to_assert, comp_guard = compile_lexpr guard in
      let comp_lc1 = List.(concat (map compile_and_agregate lc1)) in
      let comp_lc2 = List.(concat (map compile_and_agregate lc2)) in
      ( build_assert existentials to_assert,
        LCmd.If (comp_guard, comp_lc1, comp_lc2) )
  | Assert (la, lb) ->
      let exs, comp_la = compile_lassert la in
      (None, LCmd.SL (SLCmd.SepAssert (comp_la, exs @ lb)))
  | Invariant _ -> failwith "Invariant is not before a loop."

let compile_inv_and_while ~fname ~while_stmt ~invariant =
  (* FIXME: Variables that are in the invariant but not existential might be wrong. *)
  let loopretvar = "loopretvar__" in
  let gen_str = Generators.gen_str fname in
  let loop_fname = gen_str (fname ^ "_loop") in
  let while_loc = WStmt.get_loc while_stmt in
  let invariant_loc = WLCmd.get_loc invariant in
  let inv_asrt, inv_exs, inv_variant =
    match WLCmd.get invariant with
    | Invariant (la, lb, lv) -> (la, lb, lv)
    | _ -> failwith "That can't happen, it's not an invariant"
  in
  let guard, wcmds =
    match WStmt.get while_stmt with
    | While (e, c) -> (e, c)
    | _ -> failwith "That can't happen, not a while command"
  in
  let lvar_exs, var_exs = List.partition Utils.Names.is_lvar_name inv_exs in
  let vars, lvars = WLAssert.get_vars_and_lvars inv_asrt in
  let vars, lvars = (SS.elements vars, SS.elements lvars) in
  let old_lv x = "#pvar_" ^ x in
  let new_lv x = "#new_pvar_" ^ x in
  let pre =
    let var_subst = List.map (fun x -> (x, WLExpr.LVar (old_lv x))) vars in
    let subst = Hashtbl.of_seq (List.to_seq var_subst) in
    let pre_without_bind = WLAssert.substitution subst inv_asrt in
    List.fold_left
      (fun acc (x, lx) ->
        let px = WLExpr.PVar x in
        let px = WLExpr.make px invariant_loc in
        let lx = WLExpr.make lx invariant_loc in
        let f = WLExpr.make (LBinOp (px, EQUAL, lx)) invariant_loc in
        let new_a = WLAssert.make (LPure f) invariant_loc in
        match WLAssert.get acc with
        | LEmp -> new_a
        | _ -> WLAssert.make (LStar (new_a, acc)) invariant_loc)
      pre_without_bind var_subst
  in
  let post_subst =
    let var_subst =
      List.map
        (fun x ->
          let new_name = if List.mem x var_exs then new_lv x else old_lv x in
          (x, WLExpr.LVar new_name))
        vars
    in
    let lvar_subst =
      List.filter_map
        (fun x ->
          if List.mem x lvar_exs then Some (x, WLExpr.LVar (x ^ "__new"))
          else None)
        lvars
    in
    Hashtbl.of_seq (List.to_seq (var_subst @ lvar_subst))
  in
  let loop_funct =
    let guard_loc = WExpr.get_loc guard in
    let post_guard =
      WLAssert.make
        (LPure
           (WLExpr.not
              (WLExpr.as_bool_fml ~codeloc:guard_loc (WLExpr.from_expr guard))))
        guard_loc
    in
    let post_ret =
      let make_lexpr tt = WLExpr.make tt while_loc in
      let make_var_lexpr x = make_lexpr (PVar x) in
      let make_pvar_lexpr x =
        let new_name = if List.mem x var_exs then new_lv x else old_lv x in
        make_lexpr (LVar new_name)
      in
      let ret_list = make_lexpr (LEList (List.map make_pvar_lexpr vars)) in
      WLAssert.make
        (LPure
           (WLExpr.make
              (LBinOp (make_var_lexpr "ret", EQUAL, ret_list))
              while_loc))
        while_loc
    in
    let post_add =
      WLAssert.make (LStar (post_ret, post_guard)) (WLAssert.get_loc inv_asrt)
    in
    let post_without_subst =
      WLAssert.make (LStar (inv_asrt, post_add)) (WLAssert.get_loc inv_asrt)
    in
    let post = WLAssert.substitution post_subst post_without_subst in
    let spec =
      WSpec.
        {
          pre;
          post;
          (* FIGURE OUT VARIANT *)
          variant = inv_variant;
          spid = Generators.gen_id ();
          fname = loop_fname;
          fparams = vars;
          sploc = while_loc;
          existentials = None;
        }
    in
    let pvars = List.map (fun x -> WExpr.make (Var x) while_loc) vars in
    let rec_call =
      WStmt.make (FunCall (loopretvar, loop_fname, pvars, None)) while_loc
    in
    let allvars = WExpr.make (WExpr.List pvars) while_loc in
    let ret_not_rec = WStmt.make (VarAssign (loopretvar, allvars)) while_loc in
    let body =
      [
        WStmt.make (If (guard, wcmds @ [ rec_call ], [ ret_not_rec ])) while_loc;
      ]
    in
    WFun.
      {
        name = loop_fname;
        params = vars;
        body;
        spec = Some spec;
        return_expr = WExpr.make (Var loopretvar) while_loc;
        floc = while_loc;
        fid = Generators.gen_id ();
        is_loop_body = true;
      }
  in
  let retv = gen_str gvar in
  let call_cmd =
    Cmd.Call
      ( retv,
        Lit (String loop_fname),
        List.map (fun x -> Expr.PVar x) vars,
        None,
        None )
  in
  let reassign_vars =
    List.mapi
      (fun i vn ->
        Cmd.Assignment
          (vn, BinOp (PVar retv, BinOp.LstNth, Lit (Int (Z.of_int i)))))
      vars
  in
  let annot_while =
    WAnnot.make ~origin_id:(WStmt.get_id while_stmt)
      ~origin_loc:(CodeLoc.to_location while_loc)
      ~stmt_kind:(Normal false) ()
  in
  let rec map_reassign_vars acc = function
    | cmd :: rest ->
        let annot_while =
          match rest with
          | [] -> { annot_while with stmt_kind = Normal true }
          | _ -> annot_while
        in
        map_reassign_vars ((annot_while, None, cmd) :: acc) rest
    | [] -> List.rev acc
  in
  let annot_call_while =
    { annot_while with nest_kind = Some (LoopBody loop_fname) }
  in
  let lab_cmds =
    (annot_call_while, None, call_cmd) :: map_reassign_vars [] reassign_vars
  in
  (lab_cmds, loop_funct)

let rec compile_stmt_list ?(fname = "main") ?(is_loop_prefix = false) stmtl =
  (* create generator that works in the context of this function *)
  let compile_expr = compile_expr ~fname in
  let compile_lcmd = compile_lcmd ~fname in
  let compile_list = compile_stmt_list ~fname in
  let gen_str = Generators.gen_str fname in
  let gil_expr_of_str s = Expr.Lit (Literal.String s) in
  let get_or_create_lab cmdl pre =
    match cmdl with
    | (_, Some lab, _) :: _r ->
        (cmdl, lab) (* there is already a label on the first command *)
    | (a, None, c) :: r ->
        let lab = gen_str pre in
        ((a, Some lab, c) :: r, lab)
        (* There is no label on the first command *)
    | _ -> failwith "Cannot call get_or_create_lab with en empty list"
  in
  let nth = Expr.list_nth in
  let setcell = WislLActions.str_ac WislLActions.SetCell in
  let dispose = WislLActions.str_ac WislLActions.Dispose in
  let getcell = WislLActions.str_ac WislLActions.GetCell in
  let alloc = WislLActions.str_ac WislLActions.Alloc in
  let open WStmt in
  match stmtl with
  | [] -> ([], [])
  | { snode = Logic invariant; _ } :: while_stmt :: rest
    when WLCmd.is_inv invariant && WStmt.is_while while_stmt
         && !Gillian.Utils.Config.current_exec_mode = Verification ->
      let cmds, fct = compile_inv_and_while ~fname ~while_stmt ~invariant in
      let comp_rest, new_functions = compile_list rest in
      (cmds @ comp_rest, fct :: new_functions)
  | { snode = While _; _ } :: _
    when !Gillian.Utils.Config.current_exec_mode = Verification ->
      failwith "While loop without invariant in Verification mode!"
  | { snode = While (e, sl); sid = sid_while; sloc } :: rest ->
      let looplab = gen_str loop_lab in
      let cmdle, guard = compile_expr e in
      let comp_body, new_functions = compile_list sl in
      let comp_body, bodlab = get_or_create_lab comp_body lbody_lab in
      let endlab = gen_str end_lab in
      let annot =
        WAnnot.make ~origin_id:sid_while ~origin_loc:(CodeLoc.to_location sloc)
          ()
      in
      let annot_branch = { annot with branch_kind = Some WhileLoopKind } in
      let annot_hidden = WAnnot.{ annot with stmt_kind = Hidden } in
      let headlabopt = Some looplab in
      let headcmd = Cmd.Skip in
      let headcmd_lab = (annot_hidden, headlabopt, headcmd) in
      let loopcmd = Cmd.GuardedGoto (guard, bodlab, endlab) in
      let loopcmd_lab = (annot_branch, None, loopcmd) in
      let backcmd = Cmd.Goto looplab in
      let backcmd_lab = (annot_hidden, None, backcmd) in
      let endcmd = Cmd.Skip in
      let endcmd_lab = (annot_hidden, Some endlab, endcmd) in
      let comp_rest, new_functions_2 = compile_list rest in
      ( [ headcmd_lab ] @ cmdle @ [ loopcmd_lab ] @ comp_body
        @ [ backcmd_lab; endcmd_lab ]
        @ comp_rest,
        new_functions @ new_functions_2 )
  (* Skip *)
  | { snode = Skip; sid; sloc } :: rest ->
      let cmd = Cmd.Skip in
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let comp_rest, new_functions = compile_list rest in
      ((annot, None, cmd) :: comp_rest, new_functions)
  (* Variable assignment *)
  | { snode = VarAssign (v, e); sid; sloc } :: rest ->
      let cmdle, comp_e = compile_expr e in
      let cmd = Cmd.Assignment (v, comp_e) in
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let comp_rest, new_functions = compile_list rest in
      (cmdle @ [ (annot, None, cmd) ] @ comp_rest, new_functions)
  (* Fresh s-var *)
  | { snode = Fresh v; sid; sloc } :: rest ->
      let cmd = Cmd.Logic (LCmd.FreshSVar v) in
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let comp_rest, new_functions = compile_list rest in
      ((annot, None, cmd) :: comp_rest, new_functions)
  (* Object Deletion *)
  | { snode = Dispose e; sid; sloc } :: rest ->
      let cmdle, comp_e = compile_expr e in
      let annot, annot_final =
        WAnnot.make_multi ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc)
          ()
      in
      let faillab, ctnlab = (gen_str fail_lab, gen_str ctn_lab) in
      let testcmd =
        Cmd.GuardedGoto
          ( Expr.BinOp (nth comp_e 1, BinOp.Equal, Expr.Lit (Literal.Int Z.zero)),
            ctnlab,
            faillab )
      in
      let g_var = gen_str gvar in
      let failcmd = Cmd.Fail ("InvalidBlockPointer", [ comp_e ]) in
      let cmd = Cmd.LAction (g_var, dispose, [ nth comp_e 0 ]) in
      let comp_rest, new_functions = compile_list rest in
      ( cmdle
        @ [
            (annot, None, testcmd);
            (annot_final, Some faillab, failcmd);
            (annot_final, Some ctnlab, cmd);
          ]
        @ comp_rest,
        new_functions )
  (* Delete e =>
          ce := Ce(e); // (bunch of commands and then assign the result to e)
          v_get := [getcell](ce[0], ce[1]);
          u := [remcell](v_get[0], v_get[1]);
  *)
  (* Property Lookup *)
  | { snode = Lookup (x, e); sid; sloc } :: rest ->
      let cmdle, comp_e = compile_expr e in
      let annot, annot_final =
        WAnnot.make_multi ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc)
          ()
      in
      let v_get = gen_str gvar in
      let faillab, ctnlab = (gen_str fail_lab, gen_str ctn_lab) in
      let checkptrcmd =
        Cmd.GuardedGoto
          ( Expr.BinOp
              ( Expr.UnOp (UnOp.TypeOf, comp_e),
                BinOp.Equal,
                Expr.Lit (Literal.Type ListType) ),
            ctnlab,
            faillab )
      in
      let failcmd = Cmd.Fail ("InvalidPointer", []) in
      let lookupcmd =
        Cmd.LAction (v_get, getcell, [ nth comp_e 0; nth comp_e 1 ])
      in
      let getvalcmd = Cmd.Assignment (x, nth (Expr.PVar v_get) 2) in
      let cmds =
        [
          (annot, None, checkptrcmd);
          (annot_final, Some faillab, failcmd);
          (annot, Some ctnlab, lookupcmd);
          (annot_final, None, getvalcmd);
        ]
      in
      let comp_rest, new_functions = compile_list rest in
      (cmdle @ cmds @ comp_rest, new_functions)
  (*
          x := [e] =>
          ce := Ce(e); // (bunch of commands and then assign the result to ce)
          v_get := [getcell](ce[0], ce[1]);
          x := v_get[2];
      *)
  (* Property Update *)
  | { snode = Update (e1, e2); sid; sloc } :: rest ->
      let get_annot, set_annot =
        WAnnot.make_multi ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc)
          ()
      in
      let cmdle1, comp_e1 = compile_expr e1 in
      let cmdle2, comp_e2 = compile_expr e2 in
      let v_get = gen_str gvar in
      let getcmd =
        Cmd.LAction (v_get, getcell, [ nth comp_e1 0; nth comp_e1 1 ])
      in
      let e_v_get = Expr.PVar v_get in
      let v_set = gen_str gvar in
      let setcmd =
        Cmd.LAction (v_set, setcell, [ nth e_v_get 0; nth e_v_get 1; comp_e2 ])
      in
      let comp_rest, new_functions = compile_list rest in
      ( cmdle1 @ cmdle2
        @ ((get_annot, None, getcmd) :: (set_annot, None, setcmd) :: comp_rest),
        new_functions )
  (* [e1] := e2 =>
          ce1 := Ce(e1);
          ce2 := Ce(e2);
          l1 := ce1[0];
          o1 := ce1[1];
          v_get := [getcell](l1, l2);
          l2 := v_get[0];
          o2 := v_get[1];
          u := [setcell](l2, o2, ce2);
  *)
  (* Object Creation *)
  | { snode = New (x, k); sid; sloc } :: rest ->
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let newcmd =
        Cmd.LAction (x, alloc, [ Expr.Lit (Literal.Int (Z.of_int k)) ])
      in
      let comp_rest, new_functions = compile_list rest in
      ((annot, None, newcmd) :: comp_rest, new_functions)
  (* x := new(k) =>
          x := [alloc](k); // this is already a pointer
  *)
  (* Function call *)
  | { snode = FunCall (x, fn, el, to_bind); sid; sloc } :: rest ->
      let expr_fn = gil_expr_of_str fn in
      let cmdles, params = List.split (List.map compile_expr el) in
      let bindings =
        match to_bind with
        | Some (spec_name, lvars) ->
            Some (spec_name, List.map (fun x -> (x, Expr.LVar x)) lvars)
        | None -> None
      in
      let cmd = Cmd.Call (x, expr_fn, params, None, bindings) in
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc)
          ~nest_kind:(FunCall fn) ()
      in
      let comp_rest, new_functions = compile_list rest in
      (List.concat cmdles @ [ (annot, None, cmd) ] @ comp_rest, new_functions)
  (* If-Else bloc *)
  | { snode = If (e, sl1, sl2); sid; sloc } :: rest ->
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let annot_hidden = { annot with stmt_kind = Hidden } in
      let annot_prefix =
        let stmt_kind =
          WAnnot.(if is_loop_prefix then LoopPrefix else Normal true)
        in
        let branch_kind = Some WBranchCase.IfElseKind in
        { annot with stmt_kind; branch_kind }
      in
      let cmdle, guard = compile_expr e in
      let comp_sl1, new_functions1 = compile_list sl1 in
      let comp_sl2, new_functions2 = compile_list sl2 in
      let endlab = gen_str endif_lab in
      let comp_sl1, thenlab = get_or_create_lab comp_sl1 then_lab in
      let comp_sl2, elselab = get_or_create_lab comp_sl2 else_lab in
      let ifelsecmd = Cmd.GuardedGoto (guard, thenlab, elselab) in
      let ifelsecmd_lab = (annot_prefix, None, ifelsecmd) in
      let gotoendcmd = Cmd.Goto endlab in
      let gotoendcmd_lab = (annot_hidden, None, gotoendcmd) in
      let endcmd = Cmd.Skip in
      let endcmd_lab = (annot_hidden, Some endlab, endcmd) in
      let comp_rest, new_functions3 = compile_list rest in
      ( cmdle
        @ (ifelsecmd_lab :: comp_sl1)
        @ (gotoendcmd_lab :: comp_sl2)
        @ [ endcmd_lab ] @ comp_rest,
        new_functions1 @ new_functions2 @ new_functions3 )
  (* Logic commands *)
  | { snode = Logic lcmd; sid; sloc } :: rest ->
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let to_assert_opt, clcmd = compile_lcmd lcmd in
      let lcmds =
        match to_assert_opt with
        | None -> [ clcmd ]
        | Some to_assert -> [ to_assert; clcmd ]
      in
      let cmds_with_annot =
        List.map (fun lcmdp -> (annot, None, Cmd.Logic lcmdp)) lcmds
      in
      let comp_rest, new_functions = compile_list rest in
      (cmds_with_annot @ comp_rest, new_functions)
  | { snode = Assert e; sid; sloc } :: rest ->
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let cmdle, comp_e = compile_expr e in
      let cmd = Cmd.Logic (Assert (BinOp (comp_e, Equal, Expr.true_))) in
      let comp_rest, new_functions = compile_list rest in
      (cmdle @ [ (annot, None, cmd) ] @ comp_rest, new_functions)
  | { snode = Assume e; sid; sloc } :: rest ->
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let cmdle, comp_e = compile_expr e in
      let cmd = Cmd.Logic (Assume (BinOp (comp_e, Equal, Expr.true_))) in
      let comp_rest, new_functions = compile_list rest in
      (cmdle @ [ (annot, None, cmd) ] @ comp_rest, new_functions)
  | { snode = AssumeType (e, t); sid; sloc } :: rest ->
      let typ = WType.to_gil t in
      let annot =
        WAnnot.make ~origin_id:sid ~origin_loc:(CodeLoc.to_location sloc) ()
      in
      let cmdle, comp_e = compile_expr e in
      let cmd = Cmd.Logic (AssumeType (comp_e, typ)) in
      let comp_rest, new_functions = compile_list rest in
      (cmdle @ [ (annot, None, cmd) ] @ comp_rest, new_functions)

let compile_spec
    ?(fname = "main")
    WSpec.{ pre; post; variant; fparams; existentials; _ } =
  let comp_pre =
    let _, comp_pre = compile_lassert ~fname pre in
    let loc = WLAssert.get_loc pre |> CodeLoc.to_location in
    (comp_pre, Some loc)
  in
  let comp_post =
    let _, comp_post = compile_lassert ~fname post in
    let loc = WLAssert.get_loc post |> CodeLoc.to_location in
    (comp_post, Some loc)
  in
  let comp_variant =
    Option.map
      (fun variant ->
        (* FIXME: what happens with the global assertions? *)
        let _, _, comp_variant = compile_lexpr variant in
        comp_variant)
      variant
  in
  let label_opt =
    match existentials with
    | None -> None
    | Some (n, ss) -> Some (n, ss)
  in
  let single_spec =
    match label_opt with
    | None -> Spec.s_init comp_pre [ comp_post ] comp_variant Flag.Normal true
    | Some ss_label ->
        Spec.s_init ~ss_label comp_pre [ comp_post ] comp_variant Flag.Normal
          true
  in
  Spec.init fname fparams [ single_spec ] false false true

let compile_pred filepath pred =
  let WPred.{ pred_definitions; pred_params; pred_name; pred_ins; pred_loc; _ }
      =
    pred
  in
  let types = WType.infer_types_pred pred_params pred_definitions in
  let getWISLTypes str = (str, WType.of_variable str types) in
  let paramsWISLType = List.map (fun (x, _) -> getWISLTypes x) pred_params in
  let getGILTypes (str, t) =
    (str, Option.fold ~some:compile_type ~none:None t)
  in
  let pred_params = List.map getGILTypes paramsWISLType in
  let build_def pred_def =
    let _, casrt = compile_lassert pred_def in
    (None, casrt)
  in
  let pred_loc = Some (CodeLoc.to_location pred_loc) in
  Pred.
    {
      pred_name;
      pred_source_path = Some filepath;
      pred_loc;
      pred_internal = false;
      pred_num_params = List.length pred_params;
      pred_params;
      pred_ins;
      pred_definitions = List.map build_def pred_definitions;
      pred_normalised = false;
      (* FIXME: ADD SUPPORT FOR FACTS, GUARD, ABSTRACT, PURE *)
      pred_facts = [];
      pred_guard = None;
      pred_abstract = false;
      pred_pure = false;
      pred_nounfold = pred.pred_nounfold;
    }

let rec compile_function
    filepath
    WFun.{ name; params; body; spec; return_expr; is_loop_body; _ } =
  let lbodylist, new_functions =
    compile_stmt_list ~fname:name ~is_loop_prefix:is_loop_body body
  in
  let other_procs =
    List.concat (List.map (compile_function filepath) new_functions)
  in
  let cmdle, comp_ret_expr = compile_expr ~fname:name return_expr in
  let ret_annot, final_ret_annot =
    WAnnot.make_multi
      ~origin_loc:(CodeLoc.to_location (WExpr.get_loc return_expr))
      ~origin_id:(WExpr.get_id return_expr) ~is_return:true ()
  in
  let retassigncmds =
    cmdle
    @ [
        ( ret_annot,
          None,
          Cmd.Assignment (Gillian.Utils.Names.return_variable, comp_ret_expr) );
      ]
  in
  let retcmd = (final_ret_annot, None, Cmd.ReturnNormal) in
  let lbody_withret = lbodylist @ retassigncmds @ [ retcmd ] in
  let gil_body = Array.of_list lbody_withret in
  let gil_spec = Option.map (compile_spec ~fname:name) spec in
  Proc.
    {
      proc_name = name;
      proc_source_path = Some filepath;
      proc_internal = false;
      proc_body = gil_body;
      proc_spec = gil_spec;
      proc_params = params;
      proc_aliases = [];
      proc_calls = [];
      (* TODO *)
    }
  :: other_procs

let preprocess_lemma
    WLemma.
      {
        lemma_name;
        lemma_params;
        lemma_proof;
        lemma_variant;
        lemma_hypothesis;
        lemma_conclusion;
        lemma_id;
        lemma_loc;
      } =
  let lvar_params = List.map (fun x -> "#" ^ x) lemma_params in
  let param_subst = Hashtbl.create 1 in
  let lvar_params_eqs : WLAssert.t list =
    List.map2
      (fun x lvarx ->
        let ex = WLExpr.make (PVar x) lemma_loc in
        let elx = WLExpr.make (LVar lvarx) lemma_loc in
        Hashtbl.replace param_subst x (WLExpr.LVar lvarx);
        let formula = WLExpr.make (LBinOp (ex, EQUAL, elx)) lemma_loc in
        WLAssert.make (LPure formula) lemma_loc)
      lemma_params lvar_params
  in
  let lvar_params_eq : WLAssert.t =
    let lvar_params_eq =
      List.fold_left
        (fun ac next -> WLAssert.LStar (WLAssert.make ac lemma_loc, next))
        WLAssert.LEmp lvar_params_eqs
    in
    WLAssert.make lvar_params_eq lemma_loc
  in
  let new_lemma_hypothesis =
    WLAssert.substitution param_subst lemma_hypothesis
  in
  let new_lemma_hypothesis =
    WLAssert.make
      (WLAssert.LStar (lvar_params_eq, new_lemma_hypothesis))
      lemma_loc
  in
  let new_lemma_conclusion =
    WLAssert.substitution param_subst lemma_conclusion
  in
  let new_lemma_proof =
    Option.map (List.map (WLCmd.substitution param_subst)) lemma_proof
  in
  WLemma.
    {
      lemma_name;
      lemma_params;
      lemma_proof = new_lemma_proof;
      lemma_variant;
      lemma_hypothesis = new_lemma_hypothesis;
      lemma_conclusion = new_lemma_conclusion;
      lemma_id;
      lemma_loc;
    }

let compile_lemma
    filepath
    WLemma.
      {
        lemma_name;
        lemma_params;
        lemma_proof;
        lemma_variant;
        lemma_hypothesis;
        lemma_conclusion;
        _;
      } =
  let compile_lcmd = compile_lcmd ~fname:lemma_name in
  let compile_lexpr = compile_lexpr ~fname:lemma_name in
  let compile_lassert = compile_lassert ~fname:lemma_name in
  let compile_and_agregate_lcmd lcmd =
    let a_opt, clcmd = compile_lcmd lcmd in
    match a_opt with
    | None -> [ clcmd ]
    | Some a -> [ a; clcmd ]
  in
  let lemma_proof =
    Option.map
      (fun l -> List.(concat (map compile_and_agregate_lcmd l)))
      lemma_proof
  in
  (* FIXME: compilation can get wrong here if we compile stuff with pointer arith in the variant *)
  (* FIXME: not sure where the global assertions should go *)
  let lemma_variant =
    Option.map
      (fun x ->
        let _, _, comp_lexpr = compile_lexpr x in
        comp_lexpr)
      lemma_variant
  in
  let lemma_hyp =
    let _, lemma_hyp = compile_lassert lemma_hypothesis in
    let loc = WLAssert.get_loc lemma_hypothesis |> CodeLoc.to_location in
    (lemma_hyp, Some loc)
  in
  let post =
    let _, post = compile_lassert lemma_conclusion in
    let loc = WLAssert.get_loc lemma_conclusion |> CodeLoc.to_location in
    (post, Some loc)
  in
  let lemma_existentials = [] in
  (* TODO: What about existentials for lemma in WISL ? *)
  Lemma.
    {
      lemma_name;
      lemma_source_path = Some filepath;
      lemma_internal = false;
      lemma_params;
      lemma_proof;
      lemma_variant;
      lemma_specs =
        [
          {
            lemma_hyp;
            lemma_concs = [ post ];
            lemma_spec_variant = lemma_variant;
          };
        ];
      lemma_existentials;
    }

let compile ~filepath WProg.{ context; predicates; lemmas } =
  (* stuff useful to build hashtables *)
  let make_hashtbl get_name deflist =
    let hashtbl = Hashtbl.create (List.length deflist) in
    let () =
      List.iter (fun def -> Hashtbl.add hashtbl (get_name def) def) deflist
    in
    hashtbl
  in
  let get_proc_name proc = proc.Proc.proc_name in
  let get_pred_name pred = pred.Pred.pred_name in
  let get_lemma_name lemma = lemma.Lemma.lemma_name in
  (* compile everything *)
  let comp_context = List.map (compile_function filepath) context in
  let comp_preds = List.map (compile_pred filepath) predicates in
  let comp_lemmas =
    List.map
      (fun lemma -> compile_lemma filepath (preprocess_lemma lemma))
      lemmas
  in
  (* build the hashtables *)
  let gil_procs = make_hashtbl get_proc_name (List.concat comp_context) in
  let gil_preds = make_hashtbl get_pred_name comp_preds in
  let gil_lemmas = make_hashtbl get_lemma_name comp_lemmas in
  let proc_names = Hashtbl.fold (fun s _ l -> s :: l) gil_procs [] in
  let bi_specs = Hashtbl.create 1 in
  if Gillian.Utils.(Exec_mode.is_biabduction_exec !Config.current_exec_mode)
  then
    Hashtbl.iter
      (fun name proc ->
        let pre =
          List.map
            (fun var -> Asrt.Pure (BinOp (PVar var, Equal, LVar ("#" ^ var))))
            proc.Proc.proc_params
        in

        let bispec =
          BiSpec.
            {
              bispec_name = name;
              bispec_params = proc.Proc.proc_params;
              bispec_pres = [ (pre, None) ];
              bispec_normalised = false;
            }
        in
        Hashtbl.replace bi_specs name bispec)
      gil_procs;
  Prog.make
    ~imports:(List.map (fun imp -> (imp, false)) WislConstants.internal_imports)
    ~lemmas:gil_lemmas ~preds:gil_preds ~procs:gil_procs ~proc_names ~bi_specs
    ~only_specs:(Hashtbl.create 1) ~macros:(Hashtbl.create 1)
    ~predecessors:(Hashtbl.create 1) ()
