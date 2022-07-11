open Gillian.Gil_syntax
open Compcert
open Csharpminor
open CConstants
module Logging = Gillian.Logging
module SS = Gillian.Utils.Containers.SS

let true_name = ValueTranslation.true_name

type context = {
  block_stack : string list;
  local_env : string list;
  gil_annot : Gil_logic_gen.gil_annots;
  exec_mode : ExecMode.t;
  loop_stack : string list;
}

(* Useful litte function, such a big expression for such a small thing *)
let nth x n = Expr.BinOp (Expr.PVar x, BinOp.LstNth, Expr.int n)

let trans_const =
  let tr = ValueTranslation.gil_of_compcert in
  function
  | Ointconst i -> tr (Values.Vint i)
  | Ofloatconst f -> tr (Values.Vfloat f)
  | Osingleconst f32 -> tr (Values.Vsingle f32)
  | Olongconst i64 -> tr (Values.Vlong i64)

let expr_of_chunk chunk =
  Expr.Lit (Literal.String (ValueTranslation.string_of_chunk chunk))

let internal_proc_of_unop uop =
  let open Cminor in
  match uop with
  | Olongofint -> UnOp_Functions.longofint
  | Ointoflong -> UnOp_Functions.intoflong
  | Ointoffloat -> UnOp_Functions.intoffloat
  | Ointofsingle -> UnOp_Functions.intofsingle
  | Olongofintu -> UnOp_Functions.longofintu
  | Olongoffloat -> UnOp_Functions.longoffloat
  | Olongofsingle -> UnOp_Functions.longofsingle
  | Olonguofsingle -> UnOp_Functions.longuofsingle
  | Ofloatofint -> UnOp_Functions.floatofint
  | Ofloatofintu -> UnOp_Functions.floatofintu
  | Ofloatofsingle -> UnOp_Functions.floatofsingle
  | Osingleoflongu -> UnOp_Functions.singleoflongu
  | Osingleofint -> UnOp_Functions.singleofint
  | Osingleoffloat -> UnOp_Functions.singleoffloat
  | Onegl -> UnOp_Functions.negl
  | Onegint -> UnOp_Functions.negint
  | Ocast8signed -> UnOp_Functions.cast8signed
  | Ocast8unsigned -> UnOp_Functions.cast8unsigned
  | Ocast16signed -> UnOp_Functions.cast16signed
  | Ocast16unsigned -> UnOp_Functions.cast16unsigned
  | _ ->
      failwith
        (Printf.sprintf "Unhandled unary operator : %s"
           (PrintCsharpminor.name_of_unop uop))

let trans_binop_expr ~fname binop te1 te2 =
  let call func =
    let gvar = Generators.gen_str ~fname Prefix.gvar in
    ( [
        Cmd.Call (gvar, Expr.Lit (Literal.String func), [ te1; te2 ], None, None);
      ],
      Expr.PVar gvar )
  in
  let open Cminor in
  match binop with
  (* Ocmpl *)
  | Ocmpl Cle -> call BinOp_Functions.cmpl_le
  | Ocmpl Cge -> call BinOp_Functions.cmpl_ge
  | Ocmpl Clt -> call BinOp_Functions.cmpl_lt
  | Ocmpl Ceq -> call BinOp_Functions.cmpl_eq
  (* OCmplu *)
  | Ocmplu Ceq -> call BinOp_Functions.cmplu_eq
  | Ocmplu Cne -> call BinOp_Functions.cmplu_ne
  | Ocmplu Cle -> call BinOp_Functions.cmplu_le
  | Ocmplu Cge -> call BinOp_Functions.cmplu_ge
  | Ocmplu Clt -> call BinOp_Functions.cmplu_lt
  | Ocmplu Cgt -> call BinOp_Functions.cmplu_gt
  (* OCmpfs *)
  | Ocmpfs Cge -> call BinOp_Functions.cmpfs_ge
  | Ocmpfs Cle -> call BinOp_Functions.cmpfs_le
  (* OCmpu *)
  | Ocmpu Cle -> call BinOp_Functions.cmpu_le
  | Ocmpu Cgt -> call BinOp_Functions.cmpu_gt
  | Ocmpu Ceq -> call BinOp_Functions.cmpu_eq
  | Ocmpu Cne -> call BinOp_Functions.cmpu_ne
  (* OCmp *)
  | Ocmp Cgt -> call BinOp_Functions.cmp_gt
  | Ocmp Cge -> call BinOp_Functions.cmp_ge
  | Ocmp Ceq -> call BinOp_Functions.cmp_eq
  | Ocmp Clt -> call BinOp_Functions.cmp_lt
  | Ocmp Cle -> call BinOp_Functions.cmp_le
  | Ocmp Cne -> call BinOp_Functions.cmp_ne
  (* Int ops *)
  | Oadd -> call BinOp_Functions.add
  | Osub -> call BinOp_Functions.sub
  | Omul -> call BinOp_Functions.mul
  | Odiv -> call BinOp_Functions.div
  | Oshl -> call BinOp_Functions.shl
  | Oshr -> call BinOp_Functions.shr
  | Omod -> call BinOp_Functions.mod_
  | Oand -> call BinOp_Functions.and_
  | Oor -> call BinOp_Functions.or_
  | Oxor -> call BinOp_Functions.xor
  (* Int unsgined ops *)
  | Oshru -> call BinOp_Functions.shru
  (* Long ops *)
  | Oaddl -> call BinOp_Functions.addl
  | Osubl -> call BinOp_Functions.subl
  | Omull -> call BinOp_Functions.mull
  | Odivl -> call BinOp_Functions.divl
  | Oandl -> call BinOp_Functions.andl
  | Oshll -> call BinOp_Functions.shll
  | Oorl -> call BinOp_Functions.orl
  | Oxorl -> call BinOp_Functions.xorl
  (* Long unsigned ops *)
  | Odivlu -> call BinOp_Functions.divlu
  | Omodlu -> call BinOp_Functions.modlu
  | Oshrlu -> call BinOp_Functions.shrlu
  (* Float ops *)
  | Oaddf -> call BinOp_Functions.addf
  | Odivf -> call BinOp_Functions.divf
  (* Single ops *)
  | Oaddfs -> call BinOp_Functions.addfs
  | Osubfs -> call BinOp_Functions.subfs
  | Omulfs -> call BinOp_Functions.mulfs
  | _ -> failwith "Unhandled"

let rec trans_expr ~fname ~local_env expr =
  let trans_expr = trans_expr ~fname ~local_env in
  let trans_binop_expr = trans_binop_expr ~fname in
  let gen_str = Generators.gen_str ~fname in
  let open Expr in
  match expr with
  | Evar id -> ([], PVar (true_name id))
  | Econst const -> ([], Lit (trans_const const))
  | Eload (chunk, expp) ->
      let cl, e = trans_expr expp in
      let gvar = gen_str Prefix.gvar in
      let loadv = Expr.Lit (Literal.String Internal_Functions.loadv) in
      let cmd =
        Cmd.Call (gvar, loadv, [ expr_of_chunk chunk; e ], None, None)
      in
      (cl @ [ cmd ], Expr.PVar gvar)
  | Eunop (uop, e) ->
      let cl, e = trans_expr e in
      let gvar = gen_str Prefix.gvar in
      let ip = internal_proc_of_unop uop in
      let call = Cmd.Call (gvar, Lit (Literal.String ip), [ e ], None, None) in
      (cl @ [ call ], PVar gvar)
  | Ebinop (binop, e1, e2) ->
      let leading_e1, te1 = trans_expr e1 in
      let leading_e2, te2 = trans_expr e2 in
      let leading_binop, te =
        try trans_binop_expr binop te1 te2
        with Failure _ ->
          failwith
            ("Binop isn't handled yet, cannot translate : "
            ^ PrintCminor.name_of_binop binop)
      in
      (leading_e1 @ leading_e2 @ leading_binop, te)
  | Eaddrof id when List.mem (true_name id) local_env ->
      let res = EList [ nth (true_name id) 0; Lit (Literal.Int Z.zero) ] in
      ([], res)
  | Eaddrof id ->
      let name = true_name id in
      let gvar_act = gen_str Prefix.gvar in
      let gvar_val = gen_str Prefix.gvar in
      let genvlookup = LActions.(str_ac (AGEnv GetSymbol)) in
      let cmd_act =
        Cmd.LAction (gvar_act, genvlookup, [ Expr.Lit (Literal.String name) ])
      in
      let zero = Expr.Lit (Literal.Int Z.zero) in
      let cmd_assign =
        Cmd.Assignment (gvar_val, Expr.EList [ nth gvar_act 1; zero ])
      in
      let res = EList [ nth gvar_val 0; Expr.zero_i ] in
      ([ cmd_act; cmd_assign ], res)

let annot_ctx ctx = Annot.make ~loop_info:ctx.loop_stack ()

(* let empty_annot = Annot.make () *)

let rec add_annots ~ctx ?first l =
  let annot = annot_ctx ctx in
  match l with
  | a :: r -> (annot, first, a) :: add_annots ~ctx r
  | [] -> []

let change_first_lab first_lab l =
  (* Changes the first lab, only if it is not already set *)
  match l with
  | (a, Some l, c) :: r -> (l, (a, Some l, c) :: r)
  | (a, None, c) :: r -> (first_lab, (a, Some first_lab, c) :: r)
  | [] -> failwith "Cannot change first label of an empty list of commands"

let trans_label lab =
  let id_ocaml = Camlcoq.P.to_int lab in
  let str_int = string_of_int id_ocaml in
  Prefix.user_lab ^ str_int

let make_free_cmd fname var_list =
  let zero = Expr.zero_i in
  let rec make_blocks = function
    | [] -> []
    | x :: r -> Expr.EList [ nth x 0; zero; nth x 1 ] :: make_blocks r
  in
  let freelist = Expr.Lit (Literal.String Internal_Functions.free_list) in
  let gvar = Generators.gen_str ~fname Prefix.gvar in
  (* If there's nothing to free, we just don't create the command *)
  match make_blocks var_list with
  | [] -> None
  | blocks ->
      Some (Cmd.Call (gvar, freelist, [ Expr.EList blocks ], None, None))

let make_symb_gen ~fname ~ctx assigned_id x type_string =
  let gen_str = Generators.gen_str ~fname in
  let assigned = true_name assigned_id in
  let str_x =
    match x with
    | Csharpminor.Evar idl
    | Csharpminor.Eload (_, Eaddrof idl)
    | Csharpminor.Eload (_, Evar idl) -> true_name idl
    | _ ->
        failwith
          (Format.asprintf "symb_int received invalid parameter %a"
             PrintCsharpminor.print_expr x)
  in
  let hash_x = "#" ^ str_x in
  let lvar_x = Expr.LVar hash_x in
  let lvar_val_string = gen_str Prefix.lvar in
  let lvar_val = Expr.LVar lvar_val_string in
  let assignment = Cmd.Assignment (assigned, lvar_x) in
  let specvar = Cmd.Logic (LCmd.SpecVar [ hash_x ]) in
  let assume_list =
    Cmd.Logic
      (LCmd.Assume
         (Eq (lvar_x, Expr.EList [ Lit (String type_string); lvar_val ])))
  in
  let assume_val_t =
    Cmd.Logic (LCmd.AssumeType (lvar_val_string, Type.IntType))
  in
  add_annots ~ctx [ assignment; specvar; assume_list; assume_val_t ]

let is_call name e =
  match e with
  | Csharpminor.Eaddrof l when String.equal (true_name l) name -> true
  | _ -> false

let is_assert_call = is_call Builtin_Functions.assert_f
let is_assume_call = is_call Builtin_Functions.assume_f
let is_printf_call = is_call "printf"
let last_invariant = ref None (* Dirty hack *)

let set_invariant l = last_invariant := Some l

let get_invariant () =
  let i =
    match !last_invariant with
    | None -> []
    | Some i -> [ i ]
  in
  last_invariant := None;
  i

let rec trans_stmt ~fname ~context stmt =
  let trans_stmt ?(context = context) = trans_stmt ~fname ~context in
  let make_symb_gen = make_symb_gen ~fname in
  (* Default context is the given context *)
  let trans_expr = trans_expr ~fname ~local_env:context.local_env in
  let gen_str = Generators.gen_str ~fname in
  match stmt with
  | Sskip -> [ (annot_ctx context, None, Cmd.Skip) ]
  | Sset (id, exp) ->
      let cmds, te = trans_expr exp in
      let var_name = true_name id in
      let ncmd = Cmd.Assignment (var_name, te) in
      let lab_ncmd = (annot_ctx context, None, ncmd) in
      add_annots ~ctx:context cmds @ [ lab_ncmd ]
  | Sseq (s1, Sskip) -> trans_stmt s1
  | Sseq (Sskip, s2) -> trans_stmt s2
  | Sseq (s1, s2) ->
      (* Making sure it's compiled in that order for the invariant to make sense *)
      let ts1 = trans_stmt s1 in
      let ts2 = trans_stmt s2 in
      ts1 @ ts2
  | Sifthenelse (exp, s1, s2) ->
      let then_lab = gen_str Prefix.then_lab in
      let else_lab = gen_str Prefix.else_lab in
      let endif_lab = gen_str Prefix.endif_lab in
      let leading_cmds, texp = trans_expr exp in
      let annot_leading_cmds = add_annots ~ctx:context leading_cmds in
      let then_lab, ts1 = change_first_lab then_lab (trans_stmt s1) in
      let else_lab, ts2 = change_first_lab else_lab (trans_stmt s2) in
      let bool_of_val =
        Expr.Lit (Literal.String Internal_Functions.bool_of_val)
      in
      let texb = gen_str Prefix.gvar in
      let bov = Cmd.Call (texb, bool_of_val, [ texp ], None, None) in
      let a_bov = (annot_ctx context, None, bov) in
      let guard = Cmd.GuardedGoto (PVar texb, then_lab, else_lab) in
      let goto_end = Cmd.Goto endif_lab in
      let end_cmd = Cmd.Skip in
      let lab_guard = (annot_ctx context, None, guard) in
      let lab_goto_end = (annot_ctx context, None, goto_end) in
      let lab_end_cmd = (annot_ctx context, Some endif_lab, end_cmd) in
      annot_leading_cmds @ [ a_bov; lab_guard ] @ ts1 @ [ lab_goto_end ] @ ts2
      @ [ lab_end_cmd ]
  | Sloop s ->
      let loop_lab = gen_str Prefix.loop_lab in
      let nctx = { context with loop_stack = loop_lab :: context.loop_stack } in
      let invariant = add_annots ~ctx:nctx (get_invariant ()) in
      let loop_lab, ts =
        change_first_lab loop_lab (invariant @ trans_stmt ~context:nctx s)
      in
      let goto_cmd = (annot_ctx nctx, None, Cmd.Goto loop_lab) in
      ts @ [ goto_cmd ]
  | Sblock s ->
      let block_lab = gen_str Prefix.end_block_lab in
      let nctx =
        { context with block_stack = block_lab :: context.block_stack }
      in
      (* Add the new label to the stack *)
      let skip_cmd = (annot_ctx context, Some block_lab, Cmd.Skip) in
      let ts = trans_stmt ~context:nctx s in
      ts @ [ skip_cmd ]
  | Sexit n ->
      let n_ocaml = Camlcoq.Nat.to_int n in
      let lab = List.nth context.block_stack n_ocaml in
      let goto_cmd = (annot_ctx context, None, Cmd.Goto lab) in
      [ goto_cmd ]
  | Sreturn rval_opt -> (
      let leading_cmds, rexpr =
        match rval_opt with
        | None -> ([], Expr.Lit Literal.Null)
        | Some e -> trans_expr e
      in
      let annotated_leading_cmds = add_annots ~ctx:context leading_cmds in
      let ret_assign =
        ( annot_ctx context,
          None,
          Cmd.Assignment (Gillian.Utils.Names.return_variable, rexpr) )
      in
      let freecmd_opt = make_free_cmd fname context.local_env in
      let return = (annot_ctx context, None, Cmd.ReturnNormal) in
      match freecmd_opt with
      | Some freecmd ->
          let annot_freecmd = (annot_ctx context, None, freecmd) in
          annotated_leading_cmds @ [ ret_assign; annot_freecmd; return ]
      | None -> annotated_leading_cmds @ [ ret_assign; return ])
  | Slabel (lab, s) ->
      (* If the translated thing already has a label, we add a skip before with the right label,
         otherwise, we put the label in the translated thing *)
      let gil_lab = trans_label lab in
      let gil_lab_or_already_exists, ts =
        change_first_lab gil_lab (trans_stmt s)
      in
      if not (String.equal gil_lab gil_lab_or_already_exists) then
        (annot_ctx context, Some gil_lab, Cmd.Skip) :: ts
      else ts
  | Sgoto lab ->
      let gil_lab = trans_label lab in
      [ (annot_ctx context, None, Cmd.Goto gil_lab) ]
  | Sstore (chunk, vaddr, v) ->
      let addr_eval_cmds, eaddr = trans_expr vaddr in
      let v_eval_cmds, ev = trans_expr v in
      let chunk_string = ValueTranslation.string_of_chunk chunk in
      let chunk_expr = Expr.Lit (Literal.String chunk_string) in
      let annot_addr_eval = add_annots ~ctx:context addr_eval_cmds in
      let annot_v_eval = add_annots ~ctx:context v_eval_cmds in
      let storev = Expr.Lit (Literal.String Internal_Functions.storev) in
      let gvar = gen_str Prefix.gvar in
      let cmd =
        Cmd.Call (gvar, storev, [ chunk_expr; eaddr; ev ], None, None)
      in
      annot_addr_eval @ annot_v_eval @ [ (annot_ctx context, None, cmd) ]
  | Scall (None, _, ex, [ e ]) when is_assert_call ex ->
      let cmds, egil = trans_expr e in
      let one = Expr.EList [ Lit (String VTypes.int_type); Expr.one_i ] in
      let form = Formula.Eq (egil, one) in
      let assert_cmd = Cmd.Logic (Assert form) in
      add_annots ~ctx:context (cmds @ [ assert_cmd ])
  | Scall (None, _, ex, [ e ]) when is_assume_call ex ->
      let cmds, egil = trans_expr e in
      let one = Expr.EList [ Lit (String VTypes.int_type); Expr.one_i ] in
      let form = Formula.Eq (egil, one) in
      let assert_cmd = Cmd.Logic (Assume form) in
      add_annots ~ctx:context (cmds @ [ assert_cmd ])
  | Scall (None, _, ex, args) when is_printf_call ex ->
      let cmds, egil = List.split (List.map trans_expr args) in
      let leftvar = gen_str Prefix.gvar in
      let cmd =
        Cmd.ECall
          ( leftvar,
            Lit (String CConstants.Internal_Functions.printf),
            egil,
            None )
      in
      (List.concat cmds |> add_annots ~ctx:context)
      @ [ (annot_ctx context, None, cmd) ]
  | Scall (optid, _, ex, lexp) ->
      let leftvar =
        match optid with
        | None -> gen_str Prefix.gvar
        | Some id -> true_name id
      in
      let leading_fn, fn_expr = trans_expr ex in
      let leadings_params, trans_params =
        List.split (List.map trans_expr lexp)
      in
      let leading_params = List.concat leadings_params in
      let fname_var = gen_str Prefix.gvar in
      let s_get_function_name =
        Expr.Lit (Literal.String Internal_Functions.get_function_name)
      in
      let get_fname =
        Cmd.Call (fname_var, s_get_function_name, [ fn_expr ], None, None)
      in
      let call_cmd =
        Cmd.Call (leftvar, Expr.PVar fname_var, trans_params, None, None)
      in
      add_annots ~ctx:context leading_fn
      @ add_annots ~ctx:context leading_params
      @ [
          (annot_ctx context, None, get_fname);
          (annot_ctx context, None, call_cmd);
        ]
  | Sswitch (is_long, guard, lab_stmts) ->
      let leading_guard, guard_expr = trans_expr guard in
      let num =
        if is_long then fun x ->
          Expr.Lit (ValueTranslation.gil_of_compcert (Compcert.Values.Vlong x))
        else fun x ->
          Expr.Lit (ValueTranslation.gil_of_compcert (Compcert.Values.Vint x))
      in
      let rec build_isdefault curr lbl =
        match lbl with
        | LSnil -> curr
        | LScons (None, _, r) -> build_isdefault curr r
        | LScons (Some l, _, r) ->
            let ne =
              Expr.UnOp (UnOp.UNot, Expr.BinOp (guard_expr, BinOp.Equal, num l))
            in
            build_isdefault (Expr.BinOp (ne, BinOp.BAnd, curr)) r
      in
      let rec make_switch had_default l_stmts =
        match l_stmts with
        | LSnil when had_default ->
            let lab = gen_str Prefix.endswitch_lab in
            ([], [ (annot_ctx context, Some lab, Cmd.Skip) ], lab, "")
        | LSnil ->
            let lab = gen_str Prefix.default_lab in
            ([], [ (annot_ctx context, Some lab, Cmd.Skip) ], lab, lab)
        | LScons (None, s, r) ->
            let switches, cases, next_lab, _ = make_switch true r in
            let default_lab, trans_case =
              change_first_lab (gen_str Prefix.default_lab) (trans_stmt s)
            in
            (switches, trans_case @ cases, next_lab, default_lab)
        | LScons (Some l, s, r) ->
            let switches, cases, next_lab, def_lab =
              make_switch had_default r
            in
            let g = Expr.BinOp (guard_expr, BinOp.Equal, num l) in
            let case_lab, trans_case =
              change_first_lab (gen_str Prefix.case_lab) (trans_stmt s)
            in
            let switch_lab = gen_str Prefix.switch_lab in
            let goto =
              ( annot_ctx context,
                Some switch_lab,
                Cmd.GuardedGoto (g, case_lab, next_lab) )
            in
            (goto :: switches, trans_case @ cases, switch_lab, def_lab)
      in
      let switches, cases, first_switch_lab, def_lab =
        make_switch false lab_stmts
      in
      let goto_default =
        Cmd.GuardedGoto
          ( build_isdefault (Expr.Lit (Literal.Bool true)) lab_stmts,
            def_lab,
            first_switch_lab )
      in
      add_annots ~ctx:context leading_guard
      @ ((annot_ctx context, None, goto_default) :: switches)
      @ cases
  | Sbuiltin (None, AST.EF_annot (_, s, _), []) -> (
      let string_lcmd =
        let buffer = Buffer.create 1000 in
        let () = List.iter (fun x -> Buffer.add_char buffer x) s in
        Buffer.contents buffer
      in
      let lexbuf = Lexing.from_string string_lcmd in
      let lcmd =
        try Annot_parser.logic_command_entry Annot_lexer.read lexbuf with
        | Annot_parser.Error ->
            let curr = lexbuf.Lexing.lex_curr_p in
            Fmt.failwith
              "Syntax error in annot\n%s\n\nUnexpected token %s at loc (%i, %i)"
              string_lcmd (Lexing.lexeme lexbuf) curr.pos_lnum
              (curr.pos_cnum - curr.pos_bol + 1)
        | exc ->
            Fmt.failwith "Syntax Error in annot (%s): \n%s"
              (Printexc.to_string exc) string_lcmd
      in
      let compiled =
        Gil_logic_gen.trans_lcmd ~fname ~ann:context.gil_annot lcmd
      in
      match compiled with
      | `Normal gil_lcmds ->
          let gil_lcmds =
            List.map
              (fun lc -> (annot_ctx context, None, Cmd.Logic lc))
              gil_lcmds
          in
          (* We should filter assert_s in verif, and assert_v in symb *)
          if ExecMode.concrete_exec context.exec_mode then [] else gil_lcmds
      | `Invariant inv ->
          let inv = Cmd.Logic (SL inv) in
          set_invariant inv;
          [])
  | Sbuiltin (_, AST.EF_annot_val _, _)
    when not (ExecMode.symbolic_exec context.exec_mode) ->
      failwith
        (Format.asprintf
           "The following statement looks like symbolic testing annotations to \
            me, but we're not in Symbolic testing mode :\n\
            %a"
           PrintCsharpminor.print_stmt stmt)
  | Sbuiltin (Some id, AST.EF_annot_val (_, s, _), [ x ])
    when String.equal
           (String.init (List.length s) (List.nth s))
           CConstants.Symbolic_Constr.symb_int ->
      make_symb_gen ~ctx:context id x CConstants.VTypes.int_type
  | Sbuiltin (Some id, AST.EF_annot_val (_, s, _), [ x ])
    when String.equal
           (String.init (List.length s) (List.nth s))
           CConstants.Symbolic_Constr.symb_long ->
      make_symb_gen ~ctx:context id x CConstants.VTypes.long_type
  | Sbuiltin (Some id, AST.EF_annot_val (_, s, _), [ x ])
    when String.equal
           (String.init (List.length s) (List.nth s))
           CConstants.Symbolic_Constr.symb_single ->
      make_symb_gen ~ctx:context id x CConstants.VTypes.single_type
  | Sbuiltin (Some id, AST.EF_annot_val (_, s, _), [ x ])
    when String.equal
           (String.init (List.length s) (List.nth s))
           CConstants.Symbolic_Constr.symb_float ->
      make_symb_gen ~ctx:context id x CConstants.VTypes.float_type
  | Sbuiltin (None, AST.EF_memcpy (sz, al), [ dst; src ]) ->
      let sz = ValueTranslation.int_of_z sz in
      let al = ValueTranslation.int_of_z al in
      let cmds_dst, dst = trans_expr dst in
      let cmds_src, src = trans_expr src in
      let temp = gen_str Prefix.gvar in
      let call =
        Cmd.Call
          ( temp,
            Expr.string Internal_Functions.ef_memcpy,
            [ Expr.int_z sz; Expr.int_z al; dst; src ],
            None,
            None )
      in
      add_annots ~ctx:context (cmds_dst @ cmds_src @ [ call ])
  | Sbuiltin (_optid, _exf, _params) as s ->
      failwith
        (Format.asprintf
           "Cannot compile builtin function %a : Not implemented yet"
           PrintCsharpminor.print_stmt s)

let empty_annot = Annot.make ()
let add_empty_annots l = List.map (fun a -> (empty_annot, None, a)) l

let alloc_var fname (name, sz) =
  let gvar = Generators.gen_str ~fname Prefix.gvar in
  let ocaml_size = ValueTranslation.gil_size_of_compcert sz in
  let expr_size = Expr.Lit (Int ocaml_size) in
  let alloc = LActions.(str_ac (AMem Alloc)) in
  let action_cmd = Cmd.LAction (gvar, alloc, [ Expr.zero_i; expr_size ]) in
  let assign_env =
    Cmd.Assignment (name, Expr.EList [ nth gvar 0; expr_size ])
  in
  [ (empty_annot, None, action_cmd); (empty_annot, None, assign_env) ]

let trans_function
    ?(gil_annot = Gil_logic_gen.empty)
    ?(exec_mode = ExecMode.Verification)
    filepath
    fname
    fdef =
  let { fn_sig = _; fn_params; fn_vars; fn_temps; fn_body } = fdef in
  (* Getting rid of the ids immediately *)
  let fn_vars = List.map (fun (id, sz) -> (true_name id, sz)) fn_vars in
  let context =
    {
      block_stack = [];
      local_env = fst (List.split fn_vars);
      gil_annot;
      exec_mode;
      loop_stack = [];
    }
  in
  let register_temps =
    List.map
      (fun temp ->
        ( empty_annot,
          None,
          Cmd.Assignment (true_name temp, Expr.Lit Literal.Undefined) ))
      fn_temps
  in
  let register_vars = List.concat (List.map (alloc_var fname) fn_vars) in
  let init_genv =
    if String.equal fname !Utils.Config.entry_point then
      let gvar = Generators.gen_str ~fname Prefix.gvar in
      let expr_fn =
        Expr.Lit (Literal.String CConstants.Internal_Functions.initialize_genv)
      in
      [ (empty_annot, None, Cmd.Call (gvar, expr_fn, [], None, None)) ]
    else []
  in
  let body = trans_stmt ~fname ~context fn_body in
  let body_with_registrations =
    init_genv @ register_vars @ register_temps @ body
  in
  let rec add_return b =
    match b with
    | [] | [ (_, _, Cmd.ReturnNormal) ] -> b
    | [ (a, b, c) ] ->
        [
          (a, b, c);
          (Annot.make (), None, Assignment ("ret", Expr.zero_i));
          (Annot.make (), None, ReturnNormal);
        ]
    | a :: b -> a :: add_return b
  in
  let body_with_reg_and_ret = add_return body_with_registrations in
  let params = List.map true_name fn_params in
  Proc.
    {
      proc_name = fname;
      proc_source_path = Some filepath;
      proc_internal = false;
      proc_body = Array.of_list body_with_reg_and_ret;
      proc_params = params;
      proc_spec = None;
    }

let set_global_function symbol target =
  let gvar = "u" in
  let fname = Expr.Lit (String Internal_Functions.glob_set_fun) in
  Cmd.Call
    (gvar, fname, [ Lit (String symbol); Lit (String target) ], None, None)

let set_global_var symbol target v =
  let symexpr = Expr.Lit (String symbol) in
  let target_expr = Expr.Lit (String target) in
  let sz =
    Expr.Lit
      (Int (ValueTranslation.int_of_z AST.(init_data_list_size v.gvar_init)))
  in
  let perm = Globalenvs.Genv.perm_globvar v in
  let perm_string =
    Expr.Lit (String (ValueTranslation.string_of_permission perm))
  in
  let init_data_list =
    List.map ValueTranslation.gil_init_data v.AST.gvar_init
  in
  let id_list_expr = Expr.Lit (Literal.LList init_data_list) in
  let setvar = CConstants.Internal_Functions.glob_set_var in
  Cmd.Call
    ( "u",
      Lit (String setvar),
      [ symexpr; target_expr; sz; id_list_expr; perm_string ],
      None,
      None )

(* Second part of the return tuple is:
   * false if it should be a function call
   * true if it should be an external call
*)
let intern_impl_of_extern_function ext_f =
  let open AST in
  match ext_f with
  | EF_malloc -> (CConstants.Internal_Functions.malloc, false)
  | EF_free -> (CConstants.Internal_Functions.free, false)
  | EF_memcpy _ -> (CConstants.Internal_Functions.memcpy, false)
  | EF_external ([ 'c'; 'a'; 'l'; 'l'; 'o'; 'c' ], _) ->
      (CConstants.Internal_Functions.calloc, false)
  | EF_external ([ 'm'; 'e'; 'm'; 'c'; 'p'; 'y' ], _) ->
      (CConstants.Internal_Functions.memcpy, false)
  | EF_external ([ 'm'; 'e'; 'm'; 's'; 'e'; 't' ], _) ->
      (CConstants.Internal_Functions.memset, false)
  | EF_external ([ 'm'; 'e'; 'm'; 'm'; 'o'; 'v'; 'e' ], _) ->
      (CConstants.Internal_Functions.memmove, false)
  | EF_external ([ 's'; 't'; 'r'; 'c'; 'm'; 'p' ], _) ->
      (CConstants.Internal_Functions.strcmp, false)
  | EF_external ([ 's'; 't'; 'r'; 'l'; 'e'; 'n' ], _) ->
      (CConstants.Internal_Functions.strlen, false)
  | EF_external ([ 's'; 't'; 'r'; 'c'; 'p'; 'y' ], _) ->
      (CConstants.Internal_Functions.strcpy, false)
  | EF_external ([ 'p'; 'r'; 'i'; 'n'; 't'; 'f' ], _) ->
      (CConstants.Internal_Functions.printf, true)
  | EF_external ([ 'r'; 'a'; 'n'; 'd' ], _) ->
      (CConstants.Internal_Functions.rand, false)
  | _ -> (CConstants.Internal_Functions.not_implemented, false)

let not_implemented func =
  String.equal
    (fst (intern_impl_of_extern_function func))
    CConstants.Internal_Functions.not_implemented

let is_builtin_func func_name =
  let builtins =
    "__builtin_debug" :: List.map fst C2C.builtins.builtin_functions
  in
  List.mem func_name builtins

let is_gil_func func_name exec_mode =
  ExecMode.symbolic_exec exec_mode
  && (String.equal func_name Builtin_Functions.assume_f
     || String.equal func_name Builtin_Functions.assert_f)

type symbol = { name : string; defined : bool }

let is_def_sym symbol = symbol.defined
let sym_name symbol = symbol.name

let mangle_symbol symbol filepath mangled_syms =
  let filename = Filename.basename (Filename.chop_extension filepath) in
  let mangled_sym = filename ^ "__" ^ symbol in
  Hashtbl.add mangled_syms symbol mangled_sym;
  mangled_sym

type compilation_data = {
  genv_pred_asrts : Asrt.t list;
  genv_init_cmds : string Cmd.t list;
  symbols : symbol list;
}

let rec trans_globdefs
    ?(gil_annot = Gil_logic_gen.empty)
    ?(exec_mode = ExecMode.Verification)
    ~clight_prog
    ~global_syms
    ~filepath
    ~mangled_syms
    globdefs =
  let open AST in
  let trans_globdefs =
    trans_globdefs ~clight_prog ~exec_mode ~gil_annot ~global_syms ~filepath
      ~mangled_syms
  in
  match globdefs with
  | [] -> ([], [], [], [], [])
  | (id, Gfun (Internal f)) :: r ->
      (* Internally-defined function (has either file or global scope) *)
      let init_asrts, init_acts, bi_specs, fs, syms = trans_globdefs r in
      let original_sym = true_name id in
      let has_global_scope = SS.mem original_sym global_syms in
      let symbol =
        if has_global_scope then original_sym
        else mangle_symbol original_sym filepath mangled_syms
      in
      let target = symbol in
      let new_cmd = set_global_function symbol target in
      let new_asrt = Gil_logic_gen.glob_fun_pred symbol target in
      let new_bi_specs =
        if ExecMode.biabduction_exec exec_mode then
          Gil_logic_gen.generate_bispec clight_prog symbol id f :: bi_specs
        else []
      in
      let new_syms =
        if has_global_scope then { name = symbol; defined = true } :: syms
        else syms
      in
      ( new_asrt :: init_asrts,
        new_cmd :: init_acts,
        new_bi_specs,
        trans_function ~gil_annot ~exec_mode filepath symbol f :: fs,
        new_syms )
  | (id, Gfun (External f)) :: r
    when (is_builtin_func (true_name id) && not_implemented f)
         || is_gil_func (true_name id) exec_mode ->
      (* Externally-defined, built-in function with no current implementation *)
      trans_globdefs r
  | (id, Gfun (External f)) :: r when not_implemented f ->
      (* Externally-defined, non-built-in function *)
      let init_asrts, init_acts, bi_specs, fs, syms = trans_globdefs r in
      let symbol = true_name id in
      let new_sym = { name = symbol; defined = false } in
      (init_asrts, init_acts, bi_specs, fs, new_sym :: syms)
  | (id, Gfun (External f)) :: r ->
      (* Externally-defined, built-in function with existing implementation *)
      let symbol = true_name id in
      let init_asrts, init_acts, bi_specs, fs, syms = trans_globdefs r in
      let target, is_ext_call = intern_impl_of_extern_function f in
      if not is_ext_call then
        let new_cmd = set_global_function symbol target in
        let new_asrt = Gil_logic_gen.glob_fun_pred symbol target in
        (new_asrt :: init_asrts, new_cmd :: init_acts, bi_specs, fs, syms)
      else (init_asrts, init_acts, bi_specs, fs, syms)
  | (id, Gvar v) :: r
    when Camlcoq.Z.to_int (init_data_list_size v.gvar_init) == 0 ->
      (* Externally-defined global variable *)
      let init_asrts, init_acts, bi_specs, fs, syms = trans_globdefs r in
      let symbol = true_name id in
      let new_sym = { name = symbol; defined = false } in
      (init_asrts, init_acts, bi_specs, fs, new_sym :: syms)
  | (id, Gvar v) :: r ->
      (* Internally-defined global variable (has either file or global scope) *)
      let init_asrts, init_acts, bi_specs, fs, syms = trans_globdefs r in
      let original_sym = true_name id in
      let has_global_scope = SS.mem original_sym global_syms in
      let symbol =
        if has_global_scope then original_sym
        else mangle_symbol original_sym filepath mangled_syms
      in
      let new_asrt =
        Constr.Others.glob_var_unallocated ~symb:symbol
          ~vname:(Expr.string original_sym)
      in
      let target = symbol in
      let new_cmd = set_global_var symbol target v in
      let new_syms =
        if has_global_scope then { name = symbol; defined = true } :: syms
        else syms
      in
      (new_asrt :: init_asrts, new_cmd :: init_acts, bi_specs, fs, new_syms)

let make_init_proc init_cmds =
  let end_cmds =
    [
      ( empty_annot,
        None,
        Cmd.Assignment
          (Gillian.Utils.Names.return_variable, Expr.Lit Literal.Undefined) );
      (empty_annot, None, Cmd.ReturnNormal);
    ]
  in
  let annot_init_cmds = add_empty_annots init_cmds in
  let all_cmds = annot_init_cmds @ end_cmds in
  Proc.
    {
      proc_name = CConstants.Internal_Functions.initialize_genv;
      proc_source_path = None;
      proc_internal = true;
      proc_params = [];
      proc_spec = None;
      proc_body = Array.of_list all_cmds;
    }

let trans_program
    ?(exec_mode = ExecMode.Verification)
    ?(gil_annot = Gil_logic_gen.empty)
    ~clight_prog
    ~filepath
    ~mangled_syms
    prog =
  let AST.{ prog_defs; prog_public; _ } = prog in
  let global_syms = SS.of_list (List.map true_name prog_public) in
  let init_asrts, init_acts, bi_specs, procedures, symbols =
    trans_globdefs ~clight_prog ~exec_mode ~gil_annot ~global_syms ~filepath
      ~mangled_syms prog_defs
  in
  let make_hashtbl get_name deflist =
    let hashtbl = Hashtbl.create 1 in
    let () =
      List.iter (fun def -> Hashtbl.add hashtbl (get_name def) def) deflist
    in
    hashtbl
  in
  let get_proc_name proc = proc.Proc.proc_name in
  ( Prog.
      {
        imports = [];
        lemmas = Hashtbl.create 1;
        preds = Hashtbl.create 1;
        only_specs = Hashtbl.create 1;
        macros = Hashtbl.create 1;
        bi_specs = make_hashtbl (fun p -> p.BiSpec.bispec_name) bi_specs;
        proc_names = List.map get_proc_name procedures;
        procs = make_hashtbl get_proc_name procedures;
        predecessors = Hashtbl.create 1;
      },
    { genv_pred_asrts = init_asrts; genv_init_cmds = init_acts; symbols } )

let annotate prog gil_annots =
  let () =
    List.iter
      (fun p -> Hashtbl.add prog.Prog.preds p.Pred.pred_name p)
      gil_annots.Gil_logic_gen.preds
  in
  let () =
    List.iter
      (fun s -> Hashtbl.add prog.Prog.only_specs s.Spec.spec_name s)
      gil_annots.Gil_logic_gen.onlyspecs
  in
  let () =
    List.iter
      (fun l -> Hashtbl.add prog.Prog.lemmas l.Lemma.lemma_name l)
      gil_annots.Gil_logic_gen.lemmas
  in
  let () =
    List.iter
      (fun spec ->
        match Hashtbl.find_opt prog.procs spec.Spec.spec_name with
        | None ->
            Logging.verbose (fun fmt ->
                fmt "Found spec but no declaration for '%s'" spec.spec_name)
        | Some proc ->
            Hashtbl.replace prog.procs spec.Spec.spec_name
              { proc with proc_spec = Some spec })
      gil_annots.specs
  in
  { prog with imports = prog.imports @ gil_annots.imports }

let get_compilation_data_from_only_specs ospecs =
  let cdata_of_ospec_name sname =
    let init_cmd = set_global_function sname sname in
    let init_asrt = Gil_logic_gen.glob_fun_pred sname sname in
    let symbol = { name = sname; defined = true } in
    (init_asrt, init_cmd, symbol)
  in
  List.fold_left
    (fun (iasrts, icmds, syms) Spec.{ spec_name; _ } ->
      let a, c, s = cdata_of_ospec_name spec_name in
      (a :: iasrts, c :: icmds, s :: syms))
    ([], [], []) ospecs

let trans_program_with_annots
    ~exec_mode
    ~clight_prog
    ~filepath
    ~mangled_syms
    prog
    annots =
  let gil_annot =
    if ExecMode.verification_exec exec_mode then
      Gil_logic_gen.trans_annots clight_prog annots filepath
    else if ExecMode.biabduction_exec exec_mode then
      Gil_logic_gen.gen_bi_preds clight_prog
    else Gil_logic_gen.empty
  in
  let non_annotated_prog, compilation_data =
    trans_program ~exec_mode ~gil_annot ~clight_prog ~filepath ~mangled_syms
      prog
  in
  (* Onlyspecs are considered to be defined *)
  let osa, osc, oss =
    get_compilation_data_from_only_specs gil_annot.Gil_logic_gen.onlyspecs
  in
  let compilation_data =
    {
      symbols = oss @ compilation_data.symbols;
      genv_pred_asrts = osa @ compilation_data.genv_pred_asrts;
      genv_init_cmds = osc @ compilation_data.genv_init_cmds;
    }
  in
  let annotated_prog = annotate non_annotated_prog gil_annot in
  (annotated_prog, compilation_data)
