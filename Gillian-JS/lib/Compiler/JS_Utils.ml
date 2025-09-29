open JS_Parser.Syntax

(********************************************)
(********************************************)
(***       JS AST transformers            ***)
(********************************************)
(********************************************)

let flat_map f l = List.flatten (List.map f l)

let rec js_fold f_ac f_state state expr =
  let new_state = f_state expr state in
  let f = js_fold f_ac f_state new_state in
  let f_ac = f_ac expr new_state state in
  let fo e =
    match e with
    | Some e -> f e
    | None -> []
  in

  let analyse_cases cases =
    flat_map
      (fun (e_case, s_case) ->
        let f_e_case =
          match e_case with
          | Case e -> f e
          | DefaultCase -> []
        in
        let f_s_case = f s_case in
        f_e_case @ f_s_case)
      cases
  in

  let e_stx = expr.exp_stx in
  match e_stx with
  (* expressions *)
  | Num _ | String _ | Null | Bool _ | Var _ | This -> f_ac []
  | Delete e | Unary_op (_, e) | Access (e, _) -> f_ac (f e)
  | Comma (e1, e2)
  | BinOp (e1, _, e2)
  | Assign (e1, e2)
  | AssignOp (e1, _, e2)
  | CAccess (e1, e2) -> f_ac (f e1 @ f e2)
  | ConditionalOp (e1, e2, e3) -> f_ac (f e1 @ f e2 @ f e3)
  | Call (e, es) | New (e, es) -> f_ac (flat_map f (e :: es))
  | FunctionExp (_, _, _, s) -> f_ac (f s)
  | Obj pes -> f_ac (flat_map (fun (_, _, e) -> f e) pes)
  | Array es -> f_ac (flat_map fo es)
  (* statement *)
  | Label (_, s) -> f_ac (f s)
  | If (e, s1, s2) -> f_ac (f e @ f s1 @ fo s2)
  | While (e, s) -> f_ac (f e @ f s)
  | DoWhile (s, e) -> f_ac (f s @ f e)
  | Skip | Break _ | Continue _ | Debugger -> f_ac []
  | Throw e -> f_ac (f e)
  | Return e -> f_ac (fo e)
  | Script (_, ss) | Block ss -> f_ac (flat_map f ss)
  | VarDec ves ->
      f_ac
        (flat_map
           (fun ve ->
             match ve with
             | _, None -> []
             | _, Some e -> f e)
           ves)
  | For (e1, e2, e3, s) -> f_ac (fo e1 @ fo e2 @ fo e3 @ f s)
  | ForIn (e1, e2, s) -> f_ac (f e1 @ f e2 @ f s)
  | Try (s1, Some (_, s2), s3) -> f_ac (f s1 @ f s2 @ fo s3)
  | Try (s1, None, s2) -> f_ac (f s1 @ fo s2)
  | Switch (e, cases) -> f_ac (f e @ analyse_cases cases)
  | Function (_, _, _, s) -> f_ac (f s)
  | With (e, s) -> f_ac (f e @ f s)
  (* Non-supported constructs *)
  | RegExp _ -> raise (Failure "JS Construct Not Supported")

let rec js_map f_m expr =
  let f = js_map f_m in
  let fo = Option.map f in
  let f_switch (sc, e2) =
    ( (match sc with
      | Case e1 -> Case (f e1)
      | DefaultCase -> DefaultCase),
      f e2 )
  in

  let e_stx = expr.exp_stx in
  let new_e_stx =
    match e_stx with
    (* expressions *)
    | Num _ | String _ | Null | Bool _ | Var _ | This -> e_stx
    | Delete e -> Delete (f e)
    | Unary_op (op, e) -> Unary_op (op, f e)
    | Access (e, x) -> Access (f e, x)
    | Comma (e1, e2) -> Comma (f e1, f e2)
    | BinOp (e1, op, e2) -> BinOp (f e1, op, f e2)
    | Assign (e1, e2) -> Assign (f e1, f e2)
    | AssignOp (e1, op, e2) -> AssignOp (f e1, op, f e2)
    | CAccess (e1, e2) -> CAccess (f e1, f e2)
    | ConditionalOp (e1, e2, e3) -> ConditionalOp (f e1, f e2, f e3)
    | Call (e, es) -> Call (f e, List.map f es)
    | New (e, es) -> New (f e, List.map f es)
    | FunctionExp (x, n, vs, e) -> FunctionExp (x, n, vs, f e)
    | Obj lppe -> Obj (List.map (fun (pp, pt, e) -> (pp, pt, f e)) lppe)
    | Array leo -> Array (List.map fo leo)
    (* statement *)
    | Label (lab, s) -> Label (lab, f s)
    | If (e, s1, s2) -> If (f e, f s1, fo s2)
    | While (e, s) -> While (f e, f s)
    | DoWhile (s, e) -> DoWhile (f e, f s)
    | Skip | Break _ | Continue _ | Debugger -> e_stx
    | Throw e -> Throw (f e)
    | Return eo -> Return (fo eo)
    | Script (b, le) -> Script (b, List.map f le)
    | Block le -> Block (List.map f le)
    | VarDec lveo -> VarDec (List.map (fun (v, eo) -> (v, fo eo)) lveo)
    | For (eo1, eo2, eo3, e) -> For (fo eo1, fo eo2, fo eo3, f e)
    | ForIn (e1, e2, e3) -> ForIn (f e1, f e2, f e3)
    | Try (e, seo1, eo2) ->
        Try (f e, Option.map (fun (s, e) -> (s, f e)) seo1, fo eo2)
    | Switch (e, les) -> Switch (f e, List.map f_switch les)
    | Function (b, os, lv, s) -> Function (b, os, lv, f s)
    | With (e, s) -> With (f e, f s)
    (* Non-supported constructs *)
    | RegExp _ -> raise (Failure "JS Construct Not Supported")
  in

  let new_e = { expr with exp_stx = new_e_stx } in
  f_m new_e

let rec js_map_with_state f_transform f_state init_state state exp =
  let f = js_map_with_state f_transform f_state init_state in
  let fo state e =
    match e with
    | None -> (None, state)
    | Some e ->
        let e', state' = f state e in
        (Some e', state')
  in

  let state0 = f_state state exp in

  let new_exp_stx, next_state =
    match exp.exp_stx with
    (* Literals *)
    | Null | Bool _ | String _ | Num _ -> (exp.exp_stx, state0)
    (***************)
    (* Expressions *)
    (***************)
    | This | Var _ -> (exp.exp_stx, state0)
    | Obj xs ->
        let xs', state' =
          List.fold_left
            (fun (xs, state) (x, p, e) ->
              let e', state' = f state e in
              ((x, p, e') :: xs, state'))
            ([], state0) xs
        in
        (Obj (List.rev xs'), state')
    | Access (e, v) ->
        let e', state' = f state0 e in
        (Access (e', v), state')
    | CAccess (e1, e2) ->
        let e1', state1 = f state0 e1 in
        let e2', state2 = f state1 e2 in
        (CAccess (e1', e2'), state2)
    | New (e1, e2s) ->
        let e1', state1 = f state0 e1 in
        let e2s', state2 =
          List.fold_left
            (fun (es, state) e ->
              let e', state' = f state e in
              (e' :: es, state'))
            ([], state1) e2s
        in
        let e2s'' = List.rev e2s' in
        (New (e1', e2s''), state2)
    | Call (e1, e2s) ->
        let e1', state1 = f state0 e1 in
        let e2s', state2 =
          List.fold_left
            (fun (es, state) e ->
              let e', state' = f state e in
              (e' :: es, state'))
            ([], state1) e2s
        in
        let e2s'' = List.rev e2s' in
        (Call (e1', e2s''), state2)
    | FunctionExp (b, f_name, args, fb) ->
        (* Printf.printf "I got a ****FUNCTION*** BABY!!!!\n"; *)
        let fb', state' = f state0 fb in
        (FunctionExp (b, f_name, args, fb'), state')
    | Function (b, f_name, args, fb) ->
        let fb', state' = f state0 fb in
        (Function (b, f_name, args, fb'), state')
    | Unary_op (op, e) ->
        let e', state' = f state0 e in
        (Unary_op (op, e'), state')
    | Delete e ->
        let e', state' = f state0 e in
        (Delete e', state')
    | BinOp (e1, op, e2) ->
        let e1', state1 = f state0 e1 in
        let e2', state2 = f state1 e2 in
        (BinOp (e1', op, e2'), state2)
    | Assign (e1, e2) ->
        let e1', state1 = f state0 e1 in
        let e2', state2 = f state1 e2 in
        (Assign (e1', e2'), state2)
    | Array es ->
        let es', state' =
          List.fold_left
            (fun (es, state) e ->
              let e', state' = fo state e in
              (e' :: es, state'))
            ([], state0) es
        in
        (Array (List.rev es'), state')
    | ConditionalOp (e1, e2, e3) ->
        let e1', state1 = f state0 e1 in
        let e2', state2 = f state1 e2 in
        let e3', state3 = f state2 e3 in
        (ConditionalOp (e1', e2', e3'), state3)
    | AssignOp (e1, op, e2) ->
        let e1', state1 = f state0 e1 in
        let e2', state2 = f state1 e2 in
        (AssignOp (e1', op, e2'), state2)
    | Comma (e1, e2) ->
        let e1', state1 = f state0 e1 in
        let e2', state2 = f state1 e2 in
        (Comma (e1', e2'), state2)
    | VarDec vdecls ->
        let vdecls', state' =
          List.fold_left
            (fun (vdecls, state) (v, eo) ->
              let e', state' = fo state eo in
              ((v, e') :: vdecls, state'))
            ([], state0) vdecls
        in
        (VarDec (List.rev vdecls'), state')
    | RegExp _ -> raise (Failure "construct not supported yet")
    (***************)
    (* statements  *)
    (***************)
    | Script (b, es) ->
        let es' =
          List.map
            (fun e ->
              let e', _ = f init_state e in
              e')
            es
        in
        (Script (b, es'), state0)
    | Block es ->
        let es' =
          List.map
            (fun e ->
              let e', _ = f init_state e in
              e')
            es
        in
        (Block es', state0)
    | Skip -> (Skip, state0)
    | If (e, s1, s2) ->
        let e', state' = f state0 e in
        let s1', _ = f init_state s1 in
        let s2', _ = fo init_state s2 in
        (* Printf.printf "s1': %s\n"
           (JS_Parser.PrettyPrint.string_of_exp true s1'); *)
        (If (e', s1', s2'), state')
    | While (e, s) ->
        let e', state' = f state0 e in
        let s', _ = f init_state s in
        (While (e', s'), state')
    | DoWhile (s, e) ->
        let s', _ = f init_state s in
        let e', _ = f init_state e in
        (DoWhile (s', e'), state0)
    | Return e ->
        let e', state' = fo state0 e in
        (Return e', state')
    | Try (s1, None, s3) ->
        let s1', _ = f init_state s1 in
        let s3', _ = fo init_state s3 in
        (Try (s1', None, s3'), state0)
    | Try (s1, Some (x, s2), s3) ->
        let s1', _ = f init_state s1 in
        let s2', _ = f init_state s2 in
        let s3', _ = fo init_state s3 in
        (Try (s1', Some (x, s2'), s3'), state0)
    | Throw e ->
        let e', state' = f state0 e in
        (Throw e', state')
    | Continue lab -> (Continue lab, state0)
    | Break lab -> (Break lab, state0)
    | Label (lab, s) ->
        let s', _ = f init_state s in
        (Label (lab, s'), state0)
    | For (e1, e2, e3, s) ->
        let e1', state1 = fo state0 e1 in
        let e2', state2 = fo state1 e2 in
        let e3', state3 = fo state2 e3 in
        let s', _ = f init_state s in
        (For (e1', e2', e3', s'), state3)
    | Switch (e, s_cases) ->
        let e', state' = f state0 e in
        let s_cases' =
          List.map
            (fun (e, s) ->
              let e' =
                match e with
                | DefaultCase -> DefaultCase
                | Case e ->
                    let e', _ = f init_state e in
                    Case e'
              in
              let s', _ = f init_state s in
              (e', s'))
            s_cases
        in
        (Switch (e', s_cases'), state')
    | ForIn (e1, e2, s) ->
        let e1', state1 = f state0 e1 in
        let e2', state2 = f state1 e2 in
        let s', _ = f init_state s in
        (ForIn (e1', e2', s'), state2)
    | With (e, s) ->
        let e', state' = f state0 e in
        let s', _ = f init_state s in
        (With (e', s'), state')
    | Debugger -> (Debugger, state0)
  in

  f_transform exp new_exp_stx state next_state

(********************************************)
(********************************************)
(***         JavaScript Utils             ***)
(********************************************)
(********************************************)

let test_func_decl_in_block exp =
  let rec f in_block exp =
    let fo f e =
      match e with
      | None -> false
      | Some e -> f e
    in
    match exp.exp_stx with
    | Script (_, es) -> List.exists (f false) es
    (* Expressions *)
    | This
    | Var _
    | Num _
    | String _
    | Null
    | Bool _
    | RegExp _
    | Obj _
    | Array _
    | Unary_op _
    | BinOp _
    | Delete _
    | Assign _
    | AssignOp _
    | Comma _
    | Access _
    | CAccess _
    | ConditionalOp _
    | Call _
    | New _
    (* Statements *)
    | VarDec _
    | Skip
    | Continue _
    | Break _
    | Return _
    | Throw _
    | Debugger -> false
    (* Statements with sub-Statements *)
    | Block es -> List.exists (f true) es
    | If (_, s, so) -> f true s || fo (f true) so
    | While (_, s)
    | DoWhile (s, _)
    | For (_, _, _, s)
    | ForIn (_, _, s)
    | Label (_, s)
    | With (_, s) -> f true s
    | Switch (_, cs) -> List.exists (fun (_, s) -> f true s) cs
    | Try (s, sc, so) ->
        f true s || fo (fun (_, s) -> f true s) sc || fo (f true) so
    (* TODO: Ideally now the parser identifies these correctly, this test can be amended *)
    | FunctionExp _ | Function _ -> in_block
  in
  f true exp

let get_all_assigned_declared_identifiers exp =
  let rec fo is_lhs e =
    match e with
    | None -> []
    | Some e -> f is_lhs e
  and f is_lhs exp =
    match exp.exp_stx with
    (* Special Cases *)
    | Var v -> if is_lhs then [ v ] else []
    | VarDec vars ->
        flat_map
          (fun ve ->
            match ve with
            | v, None -> [ v ]
            | v, Some e -> v :: f false e)
          vars
    | Unary_op (op, e) -> (
        match op with
        | Pre_Decr | Post_Decr | Pre_Incr | Post_Incr -> f true e
        | _ -> [])
    | Delete e -> f true e
    | Assign (e1, e2) | AssignOp (e1, _, e2) -> f true e1 @ f false e2
    | Try (e1, eo2, eo3) ->
        f false e1
        @ Option.fold ~some:(fun (id, e) -> id :: f false e) ~none:[] eo2
        @ fo false eo3
    | ForIn (e1, e2, e3) -> f true e1 @ f false e2 @ f false e3
    | FunctionExp (_, n, vs, e) ->
        Option.fold ~some:(fun x -> [ x ]) ~none:[] n @ vs @ f false e
    | Function (_, n, vs, e) ->
        Option.fold ~some:(fun x -> [ x ]) ~none:[] n @ vs @ f false e
    (* Boring Cases *)
    | Num _
    | String _
    | Null
    | Bool _
    | RegExp _
    | This
    | Skip
    | Break _
    | Continue _
    | Debugger -> []
    | Throw e | Access (e, _) | Label (_, e) -> f false e
    | Return eo -> fo false eo
    | While (e1, e2)
    | DoWhile (e1, e2)
    | BinOp (e1, _, e2)
    | CAccess (e1, e2)
    | Comma (e1, e2)
    | With (e1, e2) -> f false e1 @ f false e2
    | ConditionalOp (e1, e2, e3) -> f false e1 @ f false e2 @ f false e3
    | If (e1, e2, eo3) -> f false e1 @ f false e2 @ fo false eo3
    | For (eo1, eo2, eo3, e4) ->
        fo false eo1 @ fo false eo2 @ fo false eo3 @ f false e4
    | Call (e1, e2s) | New (e1, e2s) ->
        f false e1 @ flat_map (fun e2 -> f false e2) e2s
    | Obj xs -> flat_map (fun (_, _, e) -> f false e) xs
    | Array es -> flat_map (fo false) es
    | Switch (e1, e2s) ->
        f false e1
        @ flat_map
            (fun (e2, e3) ->
              (match e2 with
              | Case e2 -> f false e2
              | DefaultCase -> [])
              @ f false e3)
            e2s
    | Block es | Script (_, es) -> flat_map (f is_lhs) es
  in

  f false exp

let var_decls_inner exp =
  let f_ac exp state _ ac =
    if not state then ac
    else
      match exp.exp_stx with
      | VarDec vars -> List.map (fun (v, _) -> v) vars @ ac
      | _ -> ac
  in
  let f_state exp state =
    match exp.exp_stx with
    | FunctionExp _ | Function _ -> false
    | _ -> state
  in
  js_fold f_ac f_state true exp

let var_decls exp =
  List.sort_uniq Stdlib.compare (var_decls_inner exp) @ [ "arguments" ]

let get_fun_decls exp =
  let f_ac exp _ _ ac =
    match exp.exp_stx with
    | Function (_, _, _, _) -> exp :: ac
    | _ -> ac
  in
  js_fold f_ac (fun _ y -> y) true exp

let get_names_of_named_function_expressions exp : string list =
  let f_ac exp _ _ ac =
    match exp.exp_stx with
    | FunctionExp (_, Some name, _, _) -> name :: ac
    | _ -> ac
  in
  js_fold f_ac (fun _ y -> y) true exp

let get_all_annots exp : JS_Parser.Syntax.annotation list =
  let f_ac exp _ _ ac = exp.exp_annot @ ac in
  js_fold f_ac (fun _ y -> y) true exp

let func_decls_in_elem exp : exp list =
  match exp.exp_stx with
  | Function (_, _, _, _) -> [ exp ]
  | _ -> []

let func_decls_in_exp exp : exp list =
  match exp.exp_stx with
  | Script (_, es) | Block es -> List.flatten (List.map func_decls_in_elem es)
  | _ -> func_decls_in_elem exp

let get_all_vars_f f_body f_args =
  let f_decls = func_decls_in_exp f_body in
  let fnames =
    List.map
      (fun f ->
        match f.exp_stx with
        | Function (_, Some name, _, _) -> name
        | _ ->
            raise
              (Failure
                 ("Must be function declaration "
                 ^ JS_Parser.PrettyPrint.string_of_exp true f)))
      f_decls
  in
  let vars = List.concat [ f_args; var_decls f_body; fnames ] in
  vars

let rec returns_empty_exp (e : JS_Parser.Syntax.exp) =
  let get_some e =
    match e with
    | None -> false
    | Some e -> returns_empty_exp e
  in
  let rec returns_empty_exp_list (el : JS_Parser.Syntax.exp list) =
    match el with
    | [] -> true
    | e :: el ->
        let reeel = returns_empty_exp_list el in
        if returns_empty_exp e then true else reeel
  in

  match e.exp_stx with
  | Null
  | Num _
  | String _
  | Bool _
  | Var _
  | Delete _
  | Unary_op _
  | BinOp _
  | Access _
  | New _
  | CAccess _
  | Assign _
  | AssignOp _
  | Comma _
  | ConditionalOp _
  | Obj _
  | Array _
  | RegExp _
  | FunctionExp _
  | Function _
  | Call _
  | This
  | Throw _
  | Return _
  | Debugger -> false
  | Label (_, e) | DoWhile (e, _) -> returns_empty_exp e
  | If (_, et, ee) ->
      let reeet = returns_empty_exp et in
      let reeee = get_some ee in
      if reeet then true else reeee
  | Try (et, ec, ef) ->
      let reeet = returns_empty_exp et in
      let reeec =
        match ec with
        | None -> false
        | Some (_, ec) -> returns_empty_exp ec
      in
      let reeef = get_some ef in
      if reeet then true else if reeec then true else reeef
  | Block el | Script (_, el) -> returns_empty_exp_list el
  | Switch (_, ese) ->
      let _, el = List.split ese in
      returns_empty_exp_list el
  | For _ | ForIn _ | While _ | VarDec _ | Break _ | Continue _ | With _ | Skip
    -> true

let is_stmt expr =
  match expr.exp_stx with
  (* Non-supported constructs *)
  | RegExp _ -> raise (Failure "JS Construct Not Supported")
  (* statement *)
  | Label _
  | If _
  | While _
  | DoWhile _
  | Skip
  | Break _
  | Continue _
  | Debugger
  | Throw _
  | Return _
  | Script _
  | Block _
  | VarDec _
  | For _
  | ForIn _
  | Try _
  | Switch _
  | Function _
  | With _ -> true
  (* expressions *)
  | _ -> false

(********************************************)
(********************************************)
(***     Char offsets to Line offsets     ***)
(********************************************)
(********************************************)

let generate_offset_lst str =
  let rec traverse_str ac_offset cur_str offset_lst =
    let new_line_index = try String.index cur_str '\n' with _ -> -1 in
    if new_line_index == -1 then offset_lst
    else
      let len = String.length cur_str in
      let new_str =
        try String.sub cur_str (new_line_index + 1) (len - new_line_index - 1)
        with _ -> ""
      in
      traverse_str
        (ac_offset + new_line_index + 1)
        new_str
        (offset_lst @ [ ac_offset + new_line_index + 1 ])
  in
  traverse_str 0 str []

let jsoffsetchar_to_jsoffsetline c_offset offset_list =
  let rec offsetchar_to_offsetline_aux offset_list cur_line =
    match offset_list with
    | [] -> cur_line
    | hd :: rest ->
        if c_offset < hd then cur_line
        else offsetchar_to_offsetline_aux rest (cur_line + 1)
  in
  offsetchar_to_offsetline_aux offset_list 1

let lift_flow_loc loc =
  let open JS_Parser.Loc in
  let open Location in
  let lift_pos p =
    let { line = pos_line; column = pos_column } = p in
    { pos_line; pos_column }
  in
  let { source; start; _end } = loc in
  let loc_source =
    match source with
    | None -> "(none)"
    | Some source -> file_key_to_string source
  in
  let loc_start = lift_pos start in
  let loc_end = lift_pos _end in
  { loc_source; loc_start; loc_end }
