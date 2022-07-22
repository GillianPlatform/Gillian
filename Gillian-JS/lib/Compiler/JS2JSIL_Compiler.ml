open Lexing
open JS2JSIL_Helpers
open Jslogic.JSLogicCommon
open JS_Utils
(* module Gil_syntax = Gillian.Gil_syntax *)

module Preprocess_GCmd = PreProcessing_Utils.M (struct
  type t = int Gil_syntax.Cmd.t

  let successors = Gil_syntax.Cmd.successors
end)

module SSubst = Gillian.Symbolic.Subst
open Gil_syntax.Expr
open Gil_syntax
open Jsil_syntax
open Jsil_syntax.LabCmd
module GProc = Gil_syntax.Proc
module GProg = Gil_syntax.Prog

let cc_tbl : cc_tbl_type = Hashtbl.create medium_tbl_size
let fun_tbl : fun_tbl_type = Hashtbl.create medium_tbl_size
let old_fun_tbl : pre_fun_tbl_type = Hashtbl.create medium_tbl_size
let vis_tbl : vis_tbl_type = Hashtbl.create medium_tbl_size

let if_verification a b =
  let cond = ExecMode.verification_exec !Config.current_exec_mode in
  if cond then a else b

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let power_list list n =
  let rec loop cur_list cur_n =
    if cur_n = 1 then cur_list
    else if cur_n > 1 then loop (cur_list @ list) (cur_n - 1)
    else raise (Failure "power list only for n > 1")
  in
  loop list n

let number_of_switches = ref 0

let fresh_switch_labels () =
  let b_cases_lab = fresh_b_cases_label () in
  let default_lab = fresh_default_label () in
  let end_switch = fresh_end_switch_label () in
  let fresh_end_case_label, _ =
    fresh_sth ("end_case_" ^ string_of_int !number_of_switches ^ "_")
  in
  number_of_switches := !number_of_switches + 1;
  (b_cases_lab, default_lab, end_switch, fresh_end_case_label)

let add_initial_label cmds lab metadata =
  match cmds with
  | [] -> [ (metadata, Some lab, LBasic Skip) ]
  | (_, Some _, _) :: _ -> (metadata, Some lab, LBasic Skip) :: cmds
  | (cmd_metadata, None, cmd) :: rest -> (cmd_metadata, Some lab, cmd) :: rest

let prefix_lcmds
    (lcmds : LCmd.t list)
    (invariant : (Asrt.t * string list) option)
    (cmds : (Annot.t * string option * LabCmd.t) list) :
    (Annot.t * string option * LabCmd.t) list =
  let lcmds =
    Option.fold
      ~some:(fun (inv, binders) ->
        lcmds @ [ LCmd.SL (Invariant (inv, binders)) ])
      ~none:lcmds invariant
  in
  let lcmds = List.map (fun lcmd -> LabCmd.LLogic lcmd) lcmds in
  match (lcmds, cmds) with
  | [], [] -> []
  | _, [] -> raise (Failure "DEATH. prefix_lcmds")
  | [], cmds -> cmds
  | first_lcmd :: rest_lcmds, (annot, Some lab, first_cmd) :: rest_cmds ->
      (annot, Some lab, first_lcmd)
      :: (List.map (fun lcmd -> (annot, None, lcmd)) rest_lcmds
         @ ((annot, None, first_cmd) :: rest_cmds))
  | lcmds, (annot, None, _) :: _ ->
      List.map (fun lcmd -> (annot, None, lcmd)) lcmds @ cmds

let is_list_type x = BinOp (UnOp (TypeOf, x), Equal, lit_typ ListType)
let is_vref x = BinOp (rtype x, Equal, lit_refv)
let is_oref x = BinOp (rtype x, Equal, lit_refo)
let is_ref x = BinOp (is_vref x, BOr, is_oref x)

let rec get_break_lab loop_list lab =
  match loop_list with
  | [] ->
      let msg =
        match lab with
        | None -> "breaking outside a loop"
        | Some lab ->
            Printf.sprintf "either breaking outside a loop or lab %s not found"
              lab
      in
      raise (Failure msg)
  | (_, lab_b, js_lab, valid_unlabelled) :: rest -> (
      match lab with
      | None -> (
          match valid_unlabelled with
          | true -> lab_b
          | false -> get_break_lab rest lab)
      | Some lab_str -> (
          match js_lab with
          | None -> get_break_lab rest lab
          | Some js_lab_str ->
              if lab_str = js_lab_str then lab_b else get_break_lab rest lab))

let rec get_continue_lab loop_list lab =
  match loop_list with
  | [] ->
      let msg =
        match lab with
        | None -> "continuing outside a loop"
        | Some lab ->
            Printf.sprintf
              "either continuing outside a loop or lab %s not found" lab
      in
      raise (Failure msg)
  | (lab_c, _, js_lab, valid_unlabelled) :: rest -> (
      match lab with
      | None -> (
          match lab_c with
          | Some lab_c -> (
              match valid_unlabelled with
              | true -> lab_c
              | false -> get_continue_lab rest lab)
          | None -> get_continue_lab rest lab)
      | Some lab_str -> (
          match js_lab with
          | None -> get_continue_lab rest lab
          | Some js_lab_str ->
              if lab_str = js_lab_str then
                match lab_c with
                | None -> get_continue_lab rest lab
                | Some lab_c -> lab_c
              else get_continue_lab rest lab))

let filter_cur_jumps
    (jumps : (string option * string * string) list)
    loop_lab
    include_no_lab =
  let rec filter_cur_jumps_iter jumps inner_jumps outer_jumps =
    match jumps with
    | [] -> (List.rev inner_jumps, List.rev outer_jumps)
    | (None, x, jump_lab) :: rest_jumps -> (
        match include_no_lab with
        | true ->
            filter_cur_jumps_iter rest_jumps (x :: inner_jumps) outer_jumps
        | false ->
            filter_cur_jumps_iter rest_jumps inner_jumps
              ((None, x, jump_lab) :: outer_jumps))
    | (Some lab, x, jump_lab) :: rest_jumps -> (
        match loop_lab with
        | None ->
            filter_cur_jumps_iter rest_jumps inner_jumps
              ((Some lab, x, jump_lab) :: outer_jumps)
        | Some loop_lab ->
            if loop_lab = lab then
              filter_cur_jumps_iter rest_jumps (x :: inner_jumps) outer_jumps
            else
              filter_cur_jumps_iter rest_jumps inner_jumps
                ((Some lab, x, jump_lab) :: outer_jumps))
  in
  filter_cur_jumps_iter jumps [] []

let add_none_labs cmds = List.map (fun cmd -> (None, cmd)) cmds

let add_skip_if_empty cmds x metadata =
  match x with
  | PVar _ -> (cmds, x)
  | Lit lit ->
      let x_r = fresh_var () in
      let cmd_ass_xr = LBasic (Assignment (x_r, Lit lit)) in
      (cmds @ [ (metadata, None, cmd_ass_xr) ], PVar x_r)
  | _ ->
      raise
        (Failure "The compiler must always generate a variable or a literal")

let make_var_ass_se () = LCall (var_se, lit_str syntaxErrorName, [], None, None)
let make_var_ass_te () = LCall (var_te, lit_str typeErrorName, [], None, None)

let make_var_ass_re () =
  LCall (var_re, lit_str referenceErrorName, [], None, None)

let add_final_var cmds x metadata =
  match x with
  | PVar x_name -> (cmds, x_name)
  | Lit lit ->
      let x_new = fresh_var () in
      let cmd_ass_new =
        (metadata, None, LBasic (Assignment (x_new, Lit lit)))
      in
      (cmds @ [ cmd_ass_new ], x_new)
  | _ -> raise (Failure "add_final_var: x needs to be a variable or a literal")

(*
Auxiliary Compilers
*)
let non_writable_ref_test x =
  (* (typeof (x) = $$v-reference_type) and ((field(x) = "eval") or (field(x) = "arguments"))  *)
  let left_e = is_vref x in
  let right_e =
    BinOp
      ( BinOp (field x, Equal, lit_str "eval"),
        BOr,
        BinOp (field x, Equal, Lit (String "arguments")) )
  in
  BinOp (left_e, BAnd, right_e)

let make_unresolvable_ref_test x =
  BinOp
    (BinOp (base x, Equal, Lit Null), BOr, BinOp (base x, Equal, Lit Undefined))

let make_get_value_call x err =
  (* x_v := getValue (x) with err *)
  match is_get_value_var x with
  | None ->
      let x_v = val_var_of_var x in
      ( x_v,
        LCall (x_v, Lit (String getValueName), [ x ], Some err, None),
        [ x_v ] )
  | Some x_v -> (x_v, LBasic Skip, [])

let make_to_number_call x x_v err =
  let x_n = number_var_of_var x in
  (x_n, LCall (x_n, Lit (String toNumberName), [ PVar x_v ], Some err, None))

let make_to_boolean_call x x_v err =
  let x_b = boolean_var_of_var x in
  (x_b, LCall (x_b, Lit (String toBooleanName), [ PVar x_v ], Some err, None))

let make_to_primitive_call x x_v err =
  let x_p = primitive_var_of_var x in
  (x_p, LCall (x_p, Lit (String toPrimitiveName), [ PVar x_v ], Some err, None))

let make_to_string_call x x_v err =
  let x_s = string_var_of_var x in
  (x_s, LCall (x_s, Lit (String toStringName), [ PVar x_v ], Some err, None))

let make_to_string_computed_call x x_v err =
  let x_s = string_var_of_var x in
  ( x_s,
    LCall (x_s, Lit (String toStringComputedName), [ PVar x_v ], Some err, None)
  )

let make_to_i32_call x x_v err =
  let x_i32 = i32_var_of_var x in
  (x_i32, LCall (x_i32, Lit (String toInt32Name), [ PVar x_v ], Some err, None))

let make_put_value_call x x_r err =
  let x_pv = fresh_var () in
  ( x_pv,
    LCall (x_pv, Lit (String putValueName), [ x; PVar x_r ], Some err, None) )

let make_dop_call x_obj prop x_desc b err =
  let x_dop = fresh_var () in
  ( x_dop,
    LCall
      ( x_dop,
        Lit (String defineOwnPropertyName),
        [ PVar x_obj; prop; PVar x_desc; Lit (Bool b) ],
        Some err,
        None ) )

let make_cae_call x err =
  let x_cae = fresh_var () in
  ( x_cae,
    LCall (x_cae, Lit (String checkAssignmentErrorsName), [ x ], Some err, None)
  )

let make_empty_ass () =
  let x = fresh_var () in
  let empty_ass = LBasic (Assignment (x, Lit Empty)) in
  (x, empty_ass)

let make_create_function_object_call x_sc fun_id params =
  let x_f = fresh_fun_var () in
  let processed_params = List.map (fun p -> Literal.String p) params in
  let cmd =
    LCall
      ( x_f,
        Lit (String createFunctionObjectName),
        [
          PVar x_sc;
          Lit (String fun_id);
          Lit (String fun_id);
          Lit (LList processed_params);
        ],
        None,
        None )
  in
  (x_f, cmd)

let translate_named_function_literal
    (top_level : bool)
    x_sc
    f_name
    f_id
    params
    index =
  (* x_f := create_function_object(x_sc, f_id, f_id, params) *)
  let x_f, cmd_cfoc = make_create_function_object_call x_sc f_id params in
  let cmd_cfoc = (None, cmd_cfoc) in

  (* x_er := l-nth(x_sc, index) *)
  let x_er = fresh_er_var () in
  let cmd_ass_xer =
    ( None,
      LBasic
        (Assignment
           (x_er, BinOp (PVar x_sc, LstNth, Lit (Num (float_of_int index))))) )
  in

  (* [x_er, f_name] := x_f *)
  (* [x_er, f_name] := {{ "d", x_f, true, true, false }} *)
  let cmd_f =
    if top_level then
      ( None,
        LBasic
          (Mutation
             ( PVar x_er,
               lit_str f_name,
               EList
                 [
                   Lit (String "d");
                   PVar x_f;
                   Lit (Bool true);
                   Lit (Bool true);
                   Lit (Bool false);
                 ] )) )
    else (None, LBasic (Mutation (PVar x_er, lit_str f_name, PVar x_f)))
  in

  let cmds = [ cmd_cfoc; cmd_ass_xer; cmd_f ] in
  (cmds, PVar x_f, [])

let translate_inc_dec x is_plus err =
  (*
              goto [ (typeof (x) = $$v-reference_type) and ((field(x) = "eval") or (field(x) = "arguments")) ] err next
       next:  x_v := getValue (x) with err
              x_n := i__toNumber (x_v) with err
              x_r := x_n +/- 1
              x_pv := putValue (x, x_r) with err
 *)
  (* goto [ (typeof (x) = $$v-reference_type) and ((field(x) = "eval") or (field(x) = "arguments")) ] err next *)
  let next0 = fresh_label () in
  let next1 = fresh_label () in

  let cmd_goto_islist = LGuardedGoto (is_list_type x, next1, next0) in
  let cmd_goto_legalass = LGuardedGoto (non_writable_ref_test x, err, next1) in

  (* next1:  x_v := getValue (x) with err *)
  let x_v, cmd_gv_x, _ = make_get_value_call x err in

  (* x_n := i__toNumber (x_v) with err *)
  let x_n, cmd_tn_x = make_to_number_call x x_v err in

  (* x_r := x_n +/- 1 *)
  let x_r = fresh_var () in
  let cmd_ass_xr =
    let (op : BinOp.t) = if is_plus then FPlus else FMinus in
    LBasic (Assignment (x_r, BinOp (PVar x_n, op, Lit (Num 1.))))
  in

  (* x_pv = putValue (x, x_r) with err4 *)
  let x_pv, cmd_pv_x = make_put_value_call x x_r err in

  let new_cmds =
    [
      (None, cmd_goto_islist);
      (*        goto [ typeof (x) = ListType ] next1 next0 )                                                                *)
      (Some next0, cmd_goto_legalass);
      (* next0: goto [ (typeof (x) = $$v-reference_type) and ((field(x) = "eval") or (field(x) = "arguments")) ] err next   *)
      (Some next1, cmd_gv_x);
      (* next1: x_v := i__getValue (x) with err                                                                             *)
      (None, cmd_tn_x);
      (*        x_n := i__toNumber (x_v) with err                                                                           *)
      (None, cmd_ass_xr);
      (*        x_r := x_n + 1                                                                                              *)
      (None, cmd_pv_x)
      (*        x_pv = i__putValue (x, x_r) with err                                                                        *);
    ]
  in
  let new_errs = [ var_se; x_v; x_n; x_pv ] in
  (new_cmds, new_errs, x_n, x_r)

let translate_multiplicative_binop x1 x2 x1_v x2_v aop err =
  let jsil_aop : BinOp.t =
    match aop with
    | JS_Parser.Syntax.Times -> FTimes
    | JS_Parser.Syntax.Div -> FDiv
    | JS_Parser.Syntax.Mod -> FMod
    | JS_Parser.Syntax.Minus -> FMinus
    | _ -> raise (Failure "Illegal binary operator - Impossible case")
  in

  (* x1_n := i__toNumber (x1_v) with err *)
  let x1_n, cmd_tn_x1 = make_to_number_call x1 x1_v err in
  (* x2_n := i__toNumber (x2_v) with err *)
  let x2_n, cmd_tn_x2 = make_to_number_call x2 x2_v err in
  (* x_r := x1_n * x2_n *)
  let x_r = fresh_var () in
  let cmd_ass_xr =
    LBasic (Assignment (x_r, BinOp (PVar x1_n, jsil_aop, PVar x2_n)))
  in

  let new_cmds =
    [
      (None, cmd_tn_x1);
      (*  x1_n := i__toNumber (x1_v) with err  *)
      (None, cmd_tn_x2);
      (*  x2_n := i__toNumber (x2_v) with err  *)
      (None, cmd_ass_xr) (*  x_r := x1_n * x2_n                   *);
    ]
  in
  let new_errs = [ x1_n; x2_n ] in
  (new_cmds, new_errs, x_r)

let translate_binop_plus x1 x2 x1_v x2_v err =
  (* x1_p := i__toPrimitive (x1_v) with err *)
  let x1_p, cmd_tp_x1 = make_to_primitive_call x1 x1_v err in

  (* x2_p := i__toPrimitive (x2_v) with err *)
  let x2_p, cmd_tp_x2 = make_to_primitive_call x2 x2_v err in

  (*  goto [((typeOf x1_p) = Str) or ((typeOf x2_p) = Str)] then else *)
  let then_lab = fresh_then_label () in
  let else_lab = fresh_else_label () in
  let end_lab = fresh_endif_label () in
  let goto_guard_left =
    BinOp (UnOp (TypeOf, PVar x1_p), Equal, Lit (Type StringType))
  in
  let goto_guard_right =
    BinOp (UnOp (TypeOf, PVar x2_p), Equal, Lit (Type StringType))
  in
  let goto_guard = BinOp (goto_guard_left, BOr, goto_guard_right) in
  let cmd_goto = LGuardedGoto (goto_guard, then_lab, else_lab) in

  (* then: x1_s := i__toString (x1_p) with err *)
  let x1_s, cmd_ts_x1 = make_to_string_computed_call x1 x1_p err in

  (* x2_s := i__toString (x2_p) with err *)
  let x2_s, cmd_ts_x2 = make_to_string_computed_call x2 x2_p err in

  (* x_rthen := x1_s ++ x2_s  *)
  let x_rthen = fresh_var () in
  let cmd_ass_xrthen =
    LBasic (Assignment (x_rthen, BinOp (PVar x1_s, StrCat, PVar x2_s)))
  in

  (* else: x1_n := i__toNumber (x1_p) with err *)
  let x1_n, cmd_tn_x1 = make_to_number_call x1 x1_p err in

  (* x2_n := i__toNumber (x2_p) with err *)
  let x2_n, cmd_tn_x2 = make_to_number_call x2 x2_p err in

  (* x_relse := x1_n + x2_n  *)
  let x_relse = fresh_var () in
  let cmd_ass_xrelse =
    LBasic (Assignment (x_relse, BinOp (PVar x1_n, FPlus, PVar x2_n)))
  in

  (* end:  x_r := PHI (x_rthen, x_relse) *)
  let x_r = fresh_var () in
  let cmd_ass_xr = LPhiAssignment [ (x_r, [ PVar x_rthen; PVar x_relse ]) ] in

  let new_cmds =
    [
      (None, cmd_tp_x1);
      (*       x1_p := i__toPrimitive (x1_v) with err                                                 *)
      (None, cmd_tp_x2);
      (*       x2_p := i__toPrimitive (x2_v) with err                                                 *)
      (None, cmd_goto);
      (*       goto [((typeOf x1_p) = Str) or ((typeOf x2_p) = Str)] then else    *)
      (Some then_lab, cmd_ts_x1);
      (* then: x1_s := i__toString (x1_p) with err                                                    *)
      (None, cmd_ts_x2);
      (*       x2_s := i__toString (x2_p) with err                                                    *)
      (None, cmd_ass_xrthen);
      (*       x_rthen := x1_s :: x2_s                                                                *)
      (None, LGoto end_lab);
      (*       goto end                                                                               *)
      (Some else_lab, cmd_tn_x1);
      (* else: x1_n := i__toNumber (x1_p) with err                                                    *)
      (None, cmd_tn_x2);
      (*       x2_n := i__toNumber (x2_p) with err                                                    *)
      (None, cmd_ass_xrelse);
      (*       x_relse := x1_n + x2_n                                                                 *)
      (Some end_lab, cmd_ass_xr)
      (* end:  x_r := PHI (x_rthen, x_relse)                                                          *);
    ]
  in
  let errs = [ x1_p; x2_p; x1_s; x2_s; x1_n; x2_n ] in
  (new_cmds, errs, x_r)

let translate_binop_comparison
    _
    _
    x1_v
    x2_v
    is_first_first
    flag_arg
    bool_undef
    err =
  (* x_ac := i__abstractComparison (x1_v, x2_v, flag_arg) with err  *)
  let x_ac = fresh_var () in
  let args =
    match is_first_first with
    | true -> [ PVar x1_v; PVar x2_v ]
    | false -> [ PVar x2_v; PVar x1_v ]
  in
  let cmd_ac =
    LCall
      ( x_ac,
        Lit (String abstractComparisonName),
        args @ [ Lit (Bool flag_arg) ],
        Some err,
        None )
  in

  (*  goto [ x_ac = undefined ] then end *)
  let then_lab = fresh_label () in
  let end_lab = fresh_label () in
  let cmd_goto =
    LGuardedGoto (BinOp (PVar x_ac, Equal, Lit Undefined), then_lab, end_lab)
  in

  (* x_r := PHI(x_ac, x_undef) *)
  let x_undef = fresh_var () in
  let x_r = fresh_var () in
  let cmd_ass_xr = LPhiAssignment [ (x_r, [ PVar x_ac; PVar x_undef ]) ] in

  let new_cmds =
    [
      (None, cmd_ac);
      (*        x_ac := i__abstractComparison (xi_v, xk_v, flag_arg) with err; where: i != k and i,k \in {1,2}  *)
      (None, cmd_goto);
      (*        goto [ x_ac = undefined ] then end                                                              *)
      (Some then_lab, LBasic (Assignment (x_undef, Lit (Bool bool_undef))));
      (* then:  x_undef := bool_undef                                                                           *)
      (Some end_lab, cmd_ass_xr)
      (* end:   x_r := PHI(x_ac, x_undef)                                                                       *);
    ]
  in
  let errs = [ x_ac ] in
  (new_cmds, errs, x_r)

let translate_bitwise_shift _ _ x1_v x2_v left_fun_name right_fun_name op err =
  (* x1_f := left_fun_name (x1_v) with err *)
  let x1_f = fresh_var () in
  let cmd_fc_x1 =
    LCall (x1_f, Lit (String left_fun_name), [ PVar x1_v ], Some err, None)
  in

  (* x2_f := right_fun_name (x2_v) with err *)
  let x2_f = fresh_var () in
  let cmd_fc_x2 =
    LCall (x2_f, Lit (String right_fun_name), [ PVar x2_v ], Some err, None)
  in

  (* x_r := x1_f op x2_f *)
  let x_r = fresh_var () in
  let cmd_ass_xr =
    LBasic (Assignment (x_r, BinOp (PVar x1_f, op, PVar x2_f)))
  in

  let new_cmds =
    [
      (None, cmd_fc_x1);
      (*  x1_f := left_fun_name (x1_v) with err     *)
      (None, cmd_fc_x2);
      (*  x2_f := right_fun_name (x2_v) with err    *)
      (None, cmd_ass_xr) (*  x_r := x1_f op x2_f                       *);
    ]
  in
  let errs = [ x1_f; x2_f ] in
  (new_cmds, errs, x_r)

let translate_binop_equality _ _ x1_v x2_v non_strict non_negated err =
  (* x_r1 := i__strictEqualityComparison/i__abstractEqualityComparison (x1_v, x2_v) with err *)
  let x_r1 = fresh_var () in
  let f_name =
    match non_strict with
    | true -> abstractEqualityComparisonName
    | false -> strictEqualityComparisonName
  in
  let cmd_ass_xr1 =
    LCall (x_r1, Lit (String f_name), [ PVar x1_v; PVar x2_v ], Some err, None)
  in

  let cmd_ass_xr2, ret =
    match non_negated with
    | true -> ([], x_r1)
    | false ->
        let x_r2 = fresh_var () in
        (* x_r2 := (not x_r1) *)
        ([ (None, LBasic (Assignment (x_r2, UnOp (UNot, PVar x_r1)))) ], x_r2)
  in

  let new_cmds = [ (None, cmd_ass_xr1) ] @ cmd_ass_xr2 in
  (new_cmds, [ x_r1 ], ret)

let translate_bitwise_bin_op x1 x2 x1_v x2_v bbop err =
  let bbop : BinOp.t =
    match bbop with
    | JS_Parser.Syntax.Bitand -> BitwiseAnd
    | JS_Parser.Syntax.Bitor -> BitwiseOr
    | JS_Parser.Syntax.Bitxor -> BitwiseXor
    | _ -> raise (Failure "Illegal bitwise operator")
  in

  (* x1_i32 := i__toInt32 (x1_v) with err3 *)
  let x1_i32, cmd_ti32_x1 = make_to_i32_call x1 x1_v err in

  (* x2_i32 := i__toInt32 (x2_v) with err4 *)
  let x2_i32, cmd_ti32_x2 = make_to_i32_call x2 x2_v err in

  (*  x_r := (x1_i32 bbop x2_i32) *)
  let x_r = fresh_var () in
  let cmd_ass_xr =
    LBasic (Assignment (x_r, BinOp (PVar x1_i32, bbop, PVar x2_i32)))
  in

  let new_cmds =
    [ (None, cmd_ti32_x1); (None, cmd_ti32_x2); (None, cmd_ass_xr) ]
  in
  let new_errs = [ x1_i32; x2_i32 ] in
  (new_cmds, new_errs, x_r)

let make_check_empty_test x_prev x_new =
  (* goto [x_new = empty] next1 next2
     next1: skip
     next2: x := PHI(x_new, x_previous)
  *)
  let x_prev, cmd_ass_xprev =
    match x_prev with
    | PVar x_prev -> (x_prev, [])
    | Lit lit ->
        let x_prev_var = fresh_var () in
        let cmd_ass_prev = (None, LBasic (Assignment (x_prev_var, Lit lit))) in
        (x_prev_var, [ cmd_ass_prev ])
    | _ ->
        raise
          (Failure
             "make_check_empty_test: x_prev needs to be either a literal or a \
              var")
  in

  let x_new, cmd_ass_new =
    match x_new with
    | PVar x_new -> (x_new, [])
    | Lit lit ->
        let x_new_var = fresh_var () in
        let cmd_ass_new = (None, LBasic (Assignment (x_new_var, Lit lit))) in
        (x_new_var, [ cmd_ass_new ])
    | _ ->
        raise
          (Failure
             "make_check_empty_test: x_new needs to be either a literal or a \
              var")
  in

  (* goto [x_new = empty] next1 next2 *)
  let next1 = fresh_next_label () in
  let next2 = fresh_next_label () in
  let cmd_goto =
    (None, LGuardedGoto (BinOp (PVar x_new, Equal, Lit Empty), next1, next2))
  in

  (* next1: skip  *)
  let cmd_skip = (Some next1, LBasic Skip) in

  (* next2: x := PHI(x_new, x_previous) *)
  let x = fresh_var () in
  let cmd_phi =
    (Some next2, LPhiAssignment [ (x, [ PVar x_new; PVar x_prev ]) ])
  in

  (cmd_ass_xprev @ cmd_ass_new @ [ cmd_goto; cmd_skip; cmd_phi ], x)

let make_loop_end cur_val_var prev_val_var break_vars end_lab cur_first =
  (*
      end_loop: x_ret_4 := PHI(break_vars, cur_val_var)
                goto [ x_ret_4 = empty ] next3 next4
      next3:    skip
      next4:    x_ret_5 := PHI(x_ret_4, prev_val_var)
*)
  let x_ret_4 = fresh_var () in
  let x_ret_5 = fresh_var () in
  let next3 = fresh_next_label () in
  let next4 = fresh_next_label () in

  (* x_ret_4 := PHI(cur_val_var, break_vars) *)
  let break_vars =
    match cur_first with
    | true -> cur_val_var :: break_vars
    | false -> break_vars @ [ cur_val_var ]
  in
  let phi_args = List.map (fun x -> PVar x) break_vars in
  let cmd_ass_ret4 = LPhiAssignment [ (x_ret_4, phi_args) ] in

  (* goto [ x_ret_4 = empty ] next3 next4 *)
  let cmd_goto =
    LGuardedGoto (BinOp (PVar x_ret_4, Equal, Lit Empty), next3, next4)
  in

  (* next4:    x_ret_5 := PHI(x_ret_4, prev_val_var) *)
  let cmd_ass_ret5 =
    LPhiAssignment [ (x_ret_5, [ PVar x_ret_4; PVar prev_val_var ]) ]
  in

  let cmds =
    [
      (Some end_lab, cmd_ass_ret4);
      (* end_loop:   x_ret_4 := PHI(cur_val_var, break_vars) *)
      (None, cmd_goto);
      (*             goto [ x_ret_4 = empty ] next3 next4  *)
      (Some next3, LBasic Skip);
      (* next3:      skip                                    *)
      (Some next4, cmd_ass_ret5)
      (* next4:      x_ret_5 := PHI(x_ret_4, prev_val_var)   *);
    ]
  in
  (cmds, x_ret_5)

let is_get_value_call cmd =
  match cmd with
  | LCall (_, Lit (String proc_name), _, _, _) -> proc_name = getValueName
  | _ -> false

let is_put_value_call cmd =
  match cmd with
  | LCall (_, Lit (String proc_name), _, _, _) -> proc_name = putValueName
  | _ -> false

let is_hasProperty_call cmd =
  match cmd with
  | LCall (_, Lit (String proc_name), _, _, _) -> proc_name = hasPropertyName
  | _ -> false

let get_args cmd =
  match cmd with
  | LCall (_, Lit (String _), args, _, _) -> Some args
  | _ -> None

let annotate_cmd_top_level metadata lcmd =
  let lab, (cmd : LabCmd.t) = lcmd in
  (metadata, lab, cmd)

let annotate_cmds_top_level metadata cmds =
  List.map (annotate_cmd_top_level metadata) cmds

(*
  *  translate_expr( tr_ctx, e) = cmds, e', x_is
     @param tr_ctx  translation context
     @param e javascript expression to compile
     @return (cmds, e', x_is) ->
        cmd  - the result of the compilation
        e'   - jsil expression that holds at runtime the outcome of the compilation
        x_is - the list of variables that may hold error values
  *)
let rec translate_expr tr_ctx e :
    (Annot.t * string option * LabCmd.t) list * Expr.t * string list =
  let f = translate_expr tr_ctx in

  let find_var_er_index v : int option =
    match tr_ctx.tr_use_cc with
    | false -> None
    | true ->
        let cur_var_tbl = get_scope_table cc_tbl tr_ctx.tr_er_fid in
        let fid_v = Hashtbl.find_opt cur_var_tbl v in
        Option.map
          (fun fid_v ->
            let fid_v_index =
              JS2JSIL_Preprocessing.get_vis_list_index tr_ctx.tr_vis_list fid_v
            in
            fid_v_index)
          fid_v
  in

  (* All the other commands must get the offsets and nothing else *)
  let js_loc = e.JS_Parser.Syntax.exp_loc in
  let metadata : Annot.t =
    Annot.make
      ~origin_loc:(JS_Utils.lift_flow_loc js_loc)
      ~loop_info:tr_ctx.tr_loops ()
  in
  let annotate_cmds = annotate_cmds_top_level metadata in
  let annotate_cmd cmd lab = annotate_cmd_top_level metadata (lab, cmd) in

  (* The first command must get the logic commands and the invariants *)
  let lcmds, call_lcmds, call_subst =
    JS2JSIL_Preprocessing.translate_lannots_in_exp cc_tbl vis_tbl old_fun_tbl
      tr_ctx.tr_fid tr_ctx.tr_sc_var false e
  in
  let invariant =
    JS2JSIL_Preprocessing.translate_invariant_in_exp cc_tbl vis_tbl old_fun_tbl
      tr_ctx.tr_fid tr_ctx.tr_sc_var e
  in
  let annotate_first_cmd = prefix_lcmds lcmds invariant in
  let annotate_first_call_cmd = prefix_lcmds call_lcmds None in

  (* Printf.printf "NOW NOW NOW lcmds: %s\n call_cmds: %s with the expression %s\n"
     (String.concat ", " (List.map JSIL_Print.string_of_lcmd lcmds))
     (String.concat ", " (List.map JSIL_Print.string_of_lcmd call_lcmds))
     (JS_Parser.PrettyPrint.string_of_exp true e); *)
  let compile_var_dec x e =
    let cmds_e, x_e, errs_e = f e in

    (* x_v := i__getValue (x) with err *)
    let x_v, cmd_gv_x, errs_x_v = make_get_value_call x_e tr_ctx.tr_err_lab in

    (* x_sf := l-nth(x_sc, index) *)
    let index = find_var_er_index x in
    let x_sf = fresh_var () in
    let cmd_xsf_ass =
      match index with
      | Some index ->
          LBasic
            (Assignment
               ( x_sf,
                 BinOp
                   (PVar tr_ctx.tr_sc_var, LstNth, lit_num (float_of_int index))
               ))
      | None ->
          LCall
            ( x_sf,
              lit_str dynamicScoper,
              [ PVar tr_ctx.tr_sc_var; lit_str x ],
              None,
              None )
    in

    (* x_ref := {{ v, x_sf, "x" }}  *)
    let x_ref = fresh_var () in
    let cmd_xref_ass =
      LBasic
        (Assignment
           ( x_ref,
             EList
               [
                 lit_refv; PVar x_sf; lit_str x; Lit (Bool tr_ctx.tr_strictness);
               ] ))
    in

    (* x_cae := i__checkAssignmentErrors (x_ref) with err *)
    let x_cae, cmd_cae = make_cae_call (PVar x_ref) tr_ctx.tr_err_lab in

    (* x_pv := i__putValue(x_ref, x_v) with err2 *)
    let x_pv, cmd_pv = make_put_value_call (PVar x_ref) x_v tr_ctx.tr_err_lab in
    let cmds =
      cmds_e
      @ annotate_cmds
          (add_none_labs
             [
               cmd_gv_x;
               (* x_v := i__getValue (x) with err          *)
               cmd_xsf_ass;
               (* x_sf := [x__scope, fid]                  *)
               cmd_xref_ass;
               (* x_ref := ref_v(x_sf, "x")                *)
               cmd_cae;
               cmd_pv (* x_pv := i__putValue(x_ref, x_v) with err *);
             ])
    in
    let errs = errs_e @ errs_x_v @ [ x_cae; x_pv ] in
    (cmds, x_ref, errs)
  in

  (*

  let compile_var_dec x e =
    let cmds_e, x_e, errs_e = f e in

    (* x_v := i__getValue (x) with err *)
    let x_v, cmd_gv_x, errs_x_v = make_get_value_call x_e tr_ctx.tr_err in

    (* x_sf := l-nth(x_sc, index) *)
    let index = find_var_er_index x in
    let x_sf = fresh_var () in
    let cmd_xsf_ass = LBasic (Assignment (x_sf, LstNth (Var tr_ctx.tr_sc_var, lit_num (float_of_int index)))) in

    (* x_ref := {{ v, x_sf, "x" }}  *)
    let x_ref = fresh_var () in
    let cmd_xref_ass = LBasic (Assignment (x_ref, EList [lit_refv; Var x_sf; lit_str x])) in

    (* x_cae := i__checkAssignmentErrors (x_ref) with err *)
    let x_cae, cmd_cae = make_cae_call (Var x_ref)  tr_ctx.tr_err in

    (* x_pv := i__putValue(x_ref, x_v) with err2 *)
    let x_pv, cmd_pv = make_put_value_call (Var x_ref) x_v tr_ctx.tr_err in
    let cmds = cmds_e @ (annotate_cmds (add_none_labs [
      cmd_gv_x;      (* x_v := i__getValue (x) with err          *)
      cmd_xsf_ass;   (* x_sf := [x__scope, fid]                  *)
      cmd_xref_ass;  (* x_ref := ref_v(x_sf, "x")                *)
      cmd_cae;
      cmd_pv         (* x_pv := i__putValue(x_ref, x_v) with err *)
    ])) in
    let errs = errs_e @ errs_x_v @ [ x_cae; x_pv ] in
    cmds, x_ref, errs in
*)
  let compile_var_dec_without_exp x =
    let _index = find_var_er_index x in

    (* x_sf := l-nth(x_sc, index) *)
    let index = find_var_er_index x in
    let x_sf = fresh_var () in
    let cmd_xsf_ass =
      match index with
      | Some index ->
          LBasic
            (Assignment
               ( x_sf,
                 BinOp
                   (PVar tr_ctx.tr_sc_var, LstNth, lit_num (float_of_int index))
               ))
      | None ->
          LCall
            ( x_sf,
              lit_str dynamicScoper,
              [ PVar tr_ctx.tr_sc_var; lit_str x ],
              None,
              None )
    in

    (* x_ref := {{ v, x_sf, "x" }}  *)
    let x_ref = fresh_var () in
    let cmd_xref_ass =
      LBasic
        (Assignment
           ( x_ref,
             EList
               [
                 lit_refv; PVar x_sf; lit_str x; Lit (Bool tr_ctx.tr_strictness);
               ] ))
    in

    (* x_cae := i__checkAssignmentErrors (x_ref) with err *)
    let x_cae, cmd_cae = make_cae_call (PVar x_ref) tr_ctx.tr_err_lab in

    let cmds =
      annotate_cmds
        (add_none_labs
           [
             cmd_xsf_ass;
             (* x_sf := [x__scope, v_fid]                          *)
             cmd_xref_ass;
             (* x_ref := ref_v(x_sf, "x")                          *)
             cmd_cae (* x_cae := i__checkAssignmentErrors (x_ref) with err *);
           ])
    in
    (x_ref, cmds, [ x_cae ])
  in

  let translate_bin_logical_operator e1 e2 lbop err =
    let cmds1, x1, errs1 = f e1 in
    let cmds2, x2, errs2 = f e2 in

    (* x1_v := i__getValue (x1) with err *)
    let x1_v, cmd_gv_x1, errs_x1_v = make_get_value_call x1 err in
    (* x1_b := i__toBoolean (x1_v) with err  *)
    let x1_b, cmd_tb_x1 = make_to_boolean_call x1 x1_v err in
    (* goto [x1_b] end next *)
    let next = fresh_next_label () in
    let end_lab = fresh_end_label () in
    let cmd_goto =
      match lbop with
      | JS_Parser.Syntax.And -> LGuardedGoto (PVar x1_b, next, end_lab)
      | JS_Parser.Syntax.Or -> LGuardedGoto (PVar x1_b, end_lab, next)
    in
    (* x2_v := i__getValue (x2) with err *)
    let x2_v, cmd_gv_x2, errs_x2_v = make_get_value_call x2 err in
    (* x_r := PHI(x1_v, x2_v) *)
    let x_r = fresh_var () in
    let cmd_ass_xr = LPhiAssignment [ (x_r, [ PVar x1_v; PVar x2_v ]) ] in

    let cmds2 = add_initial_label cmds2 next metadata in
    let cmds =
      annotate_first_cmd
        (cmds1
        @ annotate_cmds
            [
              (*         cmds1                                              *)
              (None, cmd_gv_x1);
              (*         x1_v := i__getValue (x1) with err                  *)
              (None, cmd_tb_x1);
              (*         x1_b := i__toBoolean (x1_v) with err               *)
              (None, cmd_goto)
              (*         goto [x1_b] next end                               *);
            ]
        @ cmds2
        @ annotate_cmds
            [
              (* next:   cmds2                                              *)
              (None, cmd_gv_x2);
              (*         x2_v := i__getValue (x2) with err                  *)
              (Some end_lab, cmd_ass_xr)
              (* end:    x_r := PHI(x1_v, x2_v)                             *);
            ])
    in
    let errs = errs1 @ errs_x1_v @ [ x1_b ] @ errs2 @ errs_x2_v in
    (cmds, PVar x_r, errs)
  in

  let translate_arg_list xes err =
    let cmds_args, x_args_gv, errs_args =
      List.fold_left
        (fun (cmds_args, x_args_gv, errs_args) e_arg ->
          let cmds_arg, x_arg, errs_arg = f e_arg in
          let x_arg_v, cmd_gv_arg, errs_xarg_v =
            make_get_value_call x_arg err
          in
          ( cmds_args @ cmds_arg @ [ annotate_cmd cmd_gv_arg None ],
            x_args_gv @ [ PVar x_arg_v ],
            errs_args @ errs_arg @ errs_xarg_v ))
        ([], [], []) xes
    in
    (cmds_args, x_args_gv, errs_args)
  in

  match e.JS_Parser.Syntax.exp_stx with
  | JS_Parser.Syntax.This ->
      (*
      Section 11.1.1 - The this Keyword
      C(this) =     x := __this
    *)
      let new_var = fresh_var () in
      let cmd = LBasic (Assignment (new_var, PVar var_this)) in
      let cmds = annotate_first_cmd [ annotate_cmd cmd None ] in
      (cmds, PVar new_var, [])
  | JS_Parser.Syntax.Var v -> (
      (*
     Section 11.1.2 - Identifier Reference
     Found in the closure clarification table: Phi(fid_1, x) = i
          x_1 := l-nth(x_sc, i);
          x_r := v-ref(x_1, "x")

    Not found in the closure clarification table: Phi(fid_1, x) = bot
          x_1 := o__hasProperty($lg, "x") with err;
          goto [x_1] then else
      then:   x_then := v-ref($lg, "x");
              goto end;
              (* SCOPE CHAIN RESOLUTION SHOULD RETURN THIS *)
      else:   x_else := v-ref(undefined, "x");
      end:    x_r = PHI(x_then, x_else)
    *)

      (* let translate_var_not_found v =
           (*  x_1 := o__hasProperty($lg, "x") with err *)
           let x_1 = fresh_var () in
             let cmd_ass_x1 = LCall (x_1, Lit (String hasPropertyName), [ Lit (Loc locGlobName); Lit (String v) ], Some tr_ctx.tr_err_lab) in

           (* goto [x_1] then else *)
           let then_lab = fresh_then_label () in
           let else_lab = fresh_else_label () in
           let end_lab = fresh_end_label () in
           let cmd_goto_unres_ref = LGuardedGoto (PVar x_1, then_lab, else_lab) in

           (* x_then := v-ref($lg, "x");   *)
           let x_then = fresh_var () in
           let cmd_ass_xthen = LBasic (Assignment (x_then, EList [lit_refv; lit_loc locGlobName; lit_str v]))  in

           (* x_then := v-ref(undefined, "x");  *)
           let x_else = fresh_var () in
           let cmd_ass_xelse = LBasic (Assignment (x_else, EList [lit_refv; Lit Undefined; lit_str v])) in

           (* x_r = PHI(x_then, x_else)  *)
           let x_r = fresh_var () in
           let cmd_ass_xr = LPhiAssignment [ (x_r, [ (PVar x_then); (PVar x_else) ]) ] in

           let cmds = [
             (None,          cmd_ass_x1);          (*       x_1 := o__hasProperty($lg, "x") with err    *)
             (None,          cmd_goto_unres_ref);  (*       goto [x_1] then else                        *)
             (Some then_lab, cmd_ass_xthen);       (* then: x_then := v-ref($lg, "x")                   *)
             (None,          LGoto end_lab);      (*       goto end                                    *)
             (Some else_lab, cmd_ass_xelse);       (* else: x_else := v-ref(undefined, "x")           *)
             (Some end_lab,  cmd_ass_xr)           (*       x_r = PHI(x_then, x_else)                   *)
           ] in
           let cmds = annotate_cmds cmds in
           (* List.iter (fun ({ line_offset; invariant; pre_logic_cmds; post_logic_cmds }, _, _) ->
             Printf.printf "Length: pre: %d \t post: %d\n" (List.length pre_logic_cmds) (List.length post_logic_cmds)) cmds; *)
           cmds, PVar x_r, [ x_1 ] in

         let translate_var_found v index =
           (* x_1 := l-nth(x_sc, index) *)
           let x_1 = fresh_var () in
           let cmd_ass_x1 = LBasic (Assignment (x_1, BinOp (PVar tr_ctx.tr_sc_var, LstNth, lit_num (float_of_int index)))) in

           (* x_r := {{ "v", x_1, "x" }} *)
           let x_r = fresh_var () in
           let cmd_ass_xret = LBasic (Assignment (x_r, EList [lit_refv; PVar x_1; lit_str v])) in

           let cmds = [
             (None, cmd_ass_x1);     (*   x_1 := l-nth(x_sc, index)    *)
             (None, cmd_ass_xret);   (*   x_r := v-ref(x_1, "x")       *)
           ] in
           let cmds = annotate_cmds cmds in
           cmds, PVar x_r, [] in *)
      let index = find_var_er_index v in
      match index with
      | (Some _ | None)
        when match index with
             | Some _ -> true
             | _ -> tr_ctx.tr_use_cc = false ->
          let x_1 = fresh_var () in
          let cmd_ass_x1 =
            match index with
            | Some index ->
                LBasic
                  (Assignment
                     ( x_1,
                       BinOp
                         ( PVar tr_ctx.tr_sc_var,
                           LstNth,
                           lit_num (float_of_int index) ) ))
            | None ->
                LCall
                  ( x_1,
                    lit_str dynamicScoper,
                    [ PVar tr_ctx.tr_sc_var; lit_str v ],
                    None,
                    None )
          in

          let x_r = fresh_var () in
          let cmd_ass_xret =
            LBasic
              (Assignment
                 ( x_r,
                   EList
                     [
                       lit_refv;
                       PVar x_1;
                       lit_str v;
                       Lit (Bool tr_ctx.tr_strictness);
                     ] ))
          in

          let cmds =
            [
              (None, cmd_ass_x1);
              (*   x_1 := l-nth(x_sc, index)    *)
              (None, cmd_ass_xret) (*   x_r := v-ref(x_1, "x")       *);
            ]
          in
          let cmds = annotate_cmds cmds in
          (cmds, PVar x_r, [])
      | _ ->
          (*  x_1 := o__hasProperty($lg, "x") with err *)
          let x_1 = fresh_var () in
          let cmd_ass_x1 =
            LCall
              ( x_1,
                Lit (String hasPropertyName),
                [ Lit (Loc locGlobName); Lit (String v) ],
                Some tr_ctx.tr_err_lab,
                None )
          in

          (* goto [x_1] then else *)
          let then_lab = fresh_then_label () in
          let else_lab = fresh_else_label () in
          let end_lab = fresh_end_label () in
          let cmd_goto_unres_ref =
            LGuardedGoto (PVar x_1, then_lab, else_lab)
          in

          (* x_then := v-ref($lg, "x");   *)
          let x_then = fresh_var () in
          let cmd_ass_xthen =
            LBasic
              (Assignment
                 ( x_then,
                   EList
                     [
                       lit_refv;
                       lit_loc locGlobName;
                       lit_str v;
                       Lit (Bool tr_ctx.tr_strictness);
                     ] ))
          in

          (* x_then := v-ref(undefined, "x");  *)
          let x_else = fresh_var () in
          let cmd_ass_xelse =
            LBasic
              (Assignment
                 ( x_else,
                   EList
                     [
                       lit_refv;
                       Lit Undefined;
                       lit_str v;
                       Lit (Bool tr_ctx.tr_strictness);
                     ] ))
          in

          (* x_r = PHI(x_then, x_else)  *)
          let x_r = fresh_var () in
          let cmd_ass_xr =
            LPhiAssignment [ (x_r, [ PVar x_then; PVar x_else ]) ]
          in

          let cmds =
            [
              (None, cmd_ass_x1);
              (*       x_1 := o__hasProperty($lg, "x") with err    *)
              (None, cmd_goto_unres_ref);
              (*       goto [x_1] then else                        *)
              (Some then_lab, cmd_ass_xthen);
              (* then: x_then := v-ref($lg, "x")                   *)
              (None, LGoto end_lab);
              (*       goto end                                    *)
              (Some else_lab, cmd_ass_xelse);
              (* else: x_else := v-ref(undefined, "x")           *)
              (Some end_lab, cmd_ass_xr)
              (*       x_r = PHI(x_then, x_else)                   *);
            ]
          in
          let cmds = annotate_cmds cmds in
          (* List.iter (fun ({ line_offset; invariant; pre_logic_cmds; post_logic_cmds }, _, _) ->
              Printf.printf "Length: pre: %d \t post: %d\n" (List.length pre_logic_cmds) (List.length post_logic_cmds)) cmds; *)
          (cmds, PVar x_r, [ x_1 ]))
  (*
   Section 11.1.3 - Literals
  *)
  | JS_Parser.Syntax.Null -> (annotate_first_cmd [], Lit Null, [])
  | JS_Parser.Syntax.Bool b -> (annotate_first_cmd [], Lit (Bool b), [])
  | JS_Parser.Syntax.String s ->
      let escaped_s = Str.global_replace (Str.regexp "\"") "\\\"" s in
      (annotate_first_cmd [], Lit (String escaped_s), [])
  | JS_Parser.Syntax.Num n -> (annotate_first_cmd [], Lit (Num n), [])
  (*
   Section 11.1.4 - Array Initialiser
  *)
  | JS_Parser.Syntax.Array eos ->
      (* xfvm := metadata (x_f_val)
         let xarrm = fresh_var () in
         let cmd_xarrm = annotate_cmd (LBasic (New (xarrm, None, Some (Lit Null)))) None in

         (* x_arr := new () *)
         let x_arr = fresh_obj_var () in
         let cmd_new_obj = annotate_cmd (LBasic (New (x_arr, None, Some (PVar xarrm)))) None in *)

      (* x_cdo := create_default_object (x_obj, $larr_proto, "Array") *)
      let x_cdo = fresh_obj_var () in
      let cmd_cdo_call =
        annotate_cmd
          (LCall
             ( x_cdo,
               Lit (String createDefaultObjectName),
               [ Lit (Loc locArrPrototype); Lit (String "Array") ],
               None,
               None ))
          None
      in

      (* [x_arr, "length"] := {{ "d", num, true, false, false }} *)
      let cmd_set_len num =
        annotate_cmd
          (LBasic
             (Mutation
                ( PVar x_cdo,
                  Lit (String "length"),
                  EList
                    [
                      Lit (String "d");
                      Lit (Num (float_of_int num));
                      Lit (Bool true);
                      Lit (Bool false);
                      Lit (Bool false);
                    ] )))
          None
      in

      let translate_array_property_definition x_obj e err num =
        let cmds, x, errs = f e in
        (* x_v := i__getValue (x) with err *)
        let x_v, cmd_gv_x, errs_x_v = make_get_value_call x err in

        (* x_desc := {{ "d", x_v, true, true, true}}  *)
        let x_desc = fresh_desc_var () in
        let cmd_ass_xdesc =
          LBasic
            (Assignment
               ( x_desc,
                 EList
                   [
                     Lit (String "d");
                     PVar x_v;
                     Lit (Bool true);
                     Lit (Bool true);
                     Lit (Bool true);
                   ] ))
        in

        let prop = Lit (String (string_of_int num)) in

        (* x_adop := a__defineOwnProperty(x_obj, toString(num), x_desc, true) with err *)
        let x_adop, cmd_adop_x = make_dop_call x_obj prop x_desc false err in

        let cmds =
          cmds
          @ annotate_cmds
              [
                (None, cmd_gv_x);
                (* x_v := i__getValue (x) with err                                            *)
                (None, cmd_ass_xdesc);
                (* x_desc := {{ "d", x_v, true, true, true}}                                     *)
                (None, cmd_adop_x)
                (* x_dop := a__defineOwnProperty(x_obj, toString(num), x_desc, true) with err *);
              ]
        in
        let errs = errs @ errs_x_v @ [ x_adop ] in
        (cmds, errs)
      in

      let cmds, errs, _ =
        List.fold_left
          (fun (cmds, errs, num) oe ->
            let new_cmds, new_errs =
              match oe with
              | None -> ([ cmd_set_len (num + 1) ], [])
              | Some e ->
                  translate_array_property_definition x_cdo e tr_ctx.tr_err_lab
                    num
            in
            (cmds @ new_cmds, errs @ new_errs, num + 1))
          ([], [], 0) eos
      in
      let cmds = annotate_first_cmd (cmd_cdo_call :: cmd_set_len 0 :: cmds) in
      (cmds, PVar x_cdo, errs)
  | JS_Parser.Syntax.Obj xs ->
      (*
     Section 11.1.5 - Object Initializer
     C({ pd_1, ..., pd_n } ) =
        x_cdo := create_default_object ($lobj_proto)
              C_pd(pd_1, x_obj)
        ...
        C_pd(pd_n, x_obj)

      pd := pn:e | get pn () { s } | set pn (x1, ..., xn) { s }

      pn := x | "x" | n

      C_pn(x) = "x";  C_pn("x") = "x"; C_pn (n) = num_to_string(n)

      C(e) = cmds, x
      ----------------------
      C_pd (pn:e) =   cmds
                      x_v := i__getValue (x) with err
                      x_desc := {{ "d", x_v, true, true, true}}
                      x_dop := o__defineOwnProperty(x_obj, C_pn(pn), x_desc, true) with err

      -----------------------
      C_pd ( get pn () { s }^getter_id ) =
                    x1 := copy_object (x_scope, {{main, fid1, ..., fidn }})
              x_f := create_function_object(x1, getter_id, {{}})
              x_desc := {{ "g", true, true, empty, empty, x_f, empty }}
              x_dop := o__defineOwnProperty(x_obj, C_pn(pn), x_desc, true) with err

      -----------------------
      C_pd ( set pn (x1, ..., xn) { s }^setter_id ) =
              x1 := copy_object (x_scope, {{main, fid1, ..., fidn }})
              x_f := create_function_object(x1, setter_id, {{x1, ..., xn}})
              x_desc := {{ "g", true, true, empty, empty, empty, x_f }}
              x_dop := o__defineOwnProperty(x_obj, C_pn(pn), x_desc, true) with err
    *)

      (* x_obj := create_default_object ($lobj_proto) *)
      let x_obj = fresh_var () in
      let cmd_cdo_call =
        annotate_cmd
          (LCall
             ( x_obj,
               Lit (String createDefaultObjectName),
               [ Lit (Loc locObjPrototype) ],
               None,
               None ))
          None
      in

      let translate_property_name pname =
        match pname with
        | JS_Parser.Syntax.PropnameId s | JS_Parser.Syntax.PropnameString s ->
            Lit (String s)
        | JS_Parser.Syntax.PropnameNum n -> UnOp (ToStringOp, Lit (Num n))
      in

      let translate_data_property_definition x_obj prop e err =
        let cmds, x, errs = f e in
        (* x_v := i__getValue (x) with err *)
        let x_v, cmd_gv_x, errs_x_v = make_get_value_call x err in

        (* x_desc := {{ "d", x_v, true, true, true}}  *)
        let x_desc = fresh_desc_var () in
        let cmd_ass_xdesc =
          LBasic
            (Assignment
               ( x_desc,
                 EList
                   [
                     Lit (String "d");
                     PVar x_v;
                     Lit (Bool true);
                     Lit (Bool true);
                     Lit (Bool true);
                   ] ))
        in

        (* x_dop := o__defineOwnProperty(x_obj, C_pn(pn), x_desc, true) with err *)
        let x_dop, cmd_dop_x = make_dop_call x_obj prop x_desc true err in

        let cmds =
          cmds
          @ annotate_cmds
              [
                (None, cmd_gv_x);
                (* x_v := i__getValue (x) with err                                          *)
                (None, cmd_ass_xdesc);
                (* x_desc := {{ "d", x_v, true, true, true}}                                   *)
                (None, cmd_dop_x)
                (* x_dop := o__defineOwnProperty(x_obj, C_pn(pn), x_desc, true) with err    *);
              ]
        in
        let errs = errs @ errs_x_v @ [ x_dop ] in
        (cmds, errs)
      in

      let translate_accessor_descriptor x_obj prop accessor is_getter err =
        let f_id =
          try JS2JSIL_Preprocessing.get_codename accessor
          with _ ->
            raise
              (Failure
                 "anonymous function literals should be annotated with their \
                  respective code names - Getter function")
        in
        let params =
          match accessor.JS_Parser.Syntax.exp_stx with
          | JS_Parser.Syntax.FunctionExp (_, _, params, _) -> params
          | _ -> raise (Failure "getters should be annonymous functions")
        in

        (* x_f := create_function_object(x_sc, f_id, f_id, params) *)
        let x_f, cmd_cfo =
          make_create_function_object_call tr_ctx.tr_sc_var f_id params
        in

        (* x_desc := {{ "g", true, true, empty, empty, x_f, empty }} *)
        (* x_desc := {{ "g", true, true, empty, empty, empty, x_f }} *)
        let x_desc = fresh_desc_var () in
        let desc_params =
          match is_getter with
          | true ->
              [
                Lit (String "g");
                Lit (Bool true);
                Lit (Bool true);
                Lit Empty;
                Lit Empty;
                PVar x_f;
                Lit Empty;
              ]
          | false ->
              [
                Lit (String "g");
                Lit (Bool true);
                Lit (Bool true);
                Lit Empty;
                Lit Empty;
                Lit Empty;
                PVar x_f;
              ]
        in
        let cmd_ass_xdesc = LBasic (Assignment (x_desc, EList desc_params)) in

        (* x_dop := o__defineOwnProperty(x_obj, C_pn(pn), x_desc, true) with err *)
        let x_dop, cmd_dop_x = make_dop_call x_obj prop x_desc true err in

        let cmds =
          annotate_cmds
            [
              (None, cmd_cfo);
              (* x_f := create_function_object(x_sc, f_id, f_id, params)                  *)
              (None, cmd_ass_xdesc);
              (* x_desc := x_desc := {{ "g", true, true, empty, empty, -, - }}          *)
              (None, cmd_dop_x)
              (* x_dop := o__defineOwnProperty(x_obj, C_pn(pn), x_desc, true) with err    *);
            ]
        in
        (cmds, [ x_dop ])
      in

      let cmds, errs =
        List.fold_left
          (fun (cmds, errs) (pname, ptype, e) ->
            let prop = translate_property_name pname in
            let new_cmds, new_errs =
              match ptype with
              | JS_Parser.Syntax.PropbodyVal ->
                  translate_data_property_definition x_obj prop e
                    tr_ctx.tr_err_lab
              | JS_Parser.Syntax.PropbodyGet ->
                  translate_accessor_descriptor x_obj prop e true
                    tr_ctx.tr_err_lab
              | JS_Parser.Syntax.PropbodySet ->
                  translate_accessor_descriptor x_obj prop e false
                    tr_ctx.tr_err_lab
            in
            (cmds @ new_cmds, errs @ new_errs))
          ([], []) xs
      in
      let cmds = annotate_first_cmd (cmd_cdo_call :: cmds) in
      (cmds, PVar x_obj, errs)
  | JS_Parser.Syntax.CAccess (e1, e2) ->
      (*
      Section 11.2.1 - Property Accessors
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1[e2]) =
        cmds1
        x1_v := i__getValue (x1) with err
        cmds2
        x2_v := i__getValue (x2) with err
        x_oc := i__checkObjectCoercible (x1_v) with err
        x2_s := i__toString (x2_v) with err
        x_r  := ref-o(x1_v, x4_v)
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in

      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      (* x_oc := i__checkObjectCoercible (x1_v) with err *)
      let x_oc = fresh_var () in
      let cmd_coc_x1 =
        LCall
          ( x_oc,
            Lit (String checkObjectCoercibleName),
            [ PVar x1_v ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      (* x2_s := i__toString (x2_v) with err *)
      let x2_s, cmd_ts_x2 =
        make_to_string_computed_call x2 x2_v tr_ctx.tr_err_lab
      in

      (*  x_r := ref-o(x1_v, x2_s) *)
      let x_r = fresh_var () in
      let cmd_ass_xr =
        LBasic
          (Assignment
             ( x_r,
               EList
                 [
                   lit_refo;
                   PVar x1_v;
                   PVar x2_s;
                   Lit (Bool tr_ctx.tr_strictness);
                 ] ))
      in

      let cmds =
        annotate_first_cmd
          (cmds1
          @ [
              (* cmds1                       *)
              annotate_cmd cmd_gv_x1 None
              (* x1_v := i__getValue (x1) with err                *);
            ]
          @ cmds2
          @ annotate_cmds
              [
                (* cmds2                                            *)
                (None, cmd_gv_x2);
                (* x2_v := i__getValue (x2) with err                *)
                (None, cmd_coc_x1);
                (* x_oc := i__checkObjectCoercible (x1_v) with err  *)
                (None, cmd_ts_x2);
                (* x2_s := i__toString (x2_v) with err              *)
                (None, cmd_ass_xr)
                (* x_r := ref-o(x1_v, xs_s)                         *);
              ])
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ [ x_oc; x2_s ] in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.Access (e, p) ->
      (*
      Section 11.2.1 - Property Accessors
      C(e) = cmds, x;
      C(e.p) =
        cmds1
        x_v := i__getValue (x) with err
        x_oc := i__checkObjectCoercible (x1_v) with err
        x_r  := ref-o(x1_v, "p")
     *)
      let cmds, x, errs = f e in

      (* x_v := i__getValue (x) with err *)
      let x_v, cmd_gv_x, errs_x_v = make_get_value_call x tr_ctx.tr_err_lab in

      (* x_oc := i__checkObjectCoercible (x_v) with err *)
      let x_oc = fresh_var () in
      let cmd_coc_x =
        LCall
          ( x_oc,
            Lit (String checkObjectCoercibleName),
            [ PVar x_v ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      (*  x_r := ref-o(x_v, "p") *)
      let x_r = fresh_var () in
      let cmd_ass_xr =
        LBasic
          (Assignment
             ( x_r,
               EList
                 [
                   lit_refo;
                   PVar x_v;
                   lit_str p;
                   Lit (Bool tr_ctx.tr_strictness);
                 ] ))
      in

      let cmds =
        annotate_first_cmd
          (cmds
          @ annotate_cmds
              [
                (* cmds                                             *)
                (None, cmd_gv_x);
                (* x_v := i__getValue (x) with err                  *)
                (None, cmd_coc_x);
                (* x_oc := i__checkObjectCoercible (x_v) with err   *)
                (None, cmd_ass_xr)
                (* x_r := ref-o(x_v, "p")                           *);
              ])
      in
      let errs = errs @ errs_x_v @ [ x_oc ] in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.New (e_f, xes) ->
      (*
      Section 11.2.2 - The new Operator
      C(e_f) = cmds_ef, x_f
      C(e_i) = cmds_ei, x_argi (for i = 1, ..., n)
      C(new e_f (e_1, ..., e_n) =
                  cmds_ef
                  x_f_val := i__getValue (x_f) with err;
                  cmds_e1
                  x_arg1_val := i__getValue (x_arg1) with err;
                  ...
                  cmds_en
                  x_argn_val := i__getValue (x_argn) with err;
                  goto [ typeOf(x_f_val) != Object] err next1;
          next1:  x_hp := [x_f_val, "@construct"];
                  goto [ x_hp = empty ] err next2;
          next2:  x_this := new ();
                  x_ref_prototype := ref-o(x_f_val, "prototype");
                  x_f_prototype := i__getValue(x_ref_prototype) with err;
                  goto [typeof (x_f_prototype) = Obj] then0 else0;
          then0:  x_f_prototype := $lobj_proto;
          else0:  x_cdo := i__createDefaultObject (x_this, x_f_prototype);
                  x_body := [x_f_val, "@construct"];
                  x_scope := [x_f_val, "@scope"];
                  x_r1 := x_body (x_scope, x_this, x_arg0_val, ..., x_argn_val) with err;
                  goto [typeOf(x_r1) = Obj ] next4 next3;
          next3:  skip
          next4:  x_r3 := PHI(x_r1, x_this)
    *)
      let cmds_ef, x_ef, errs_ef = f e_f in

      (* x_f_val := i__getValue (x_f) with err1;  *)
      let x_f_val, cmd_gv_f, errs_xf_val =
        make_get_value_call x_ef tr_ctx.tr_err_lab
      in

      let cmds_args, x_args_gv, errs_args =
        translate_arg_list xes tr_ctx.tr_err_lab
      in

      (* goto [ typeOf(x_f_val) != Object] err next1; err -> typeerror *)
      let next1 = fresh_next_label () in
      let goto_guard_expr =
        UnOp
          ( UNot,
            BinOp (UnOp (TypeOf, PVar x_f_val), Equal, Lit (Type ObjectType)) )
      in
      let cmd_goto_is_obj =
        LGuardedGoto (goto_guard_expr, tr_ctx.tr_err_lab, next1)
      in

      (* xfvm := metadata (x_f_val) *)
      let xfvm = fresh_var () in
      let cmd_xfvm = LBasic (MetaData (xfvm, PVar x_f_val)) in

      (* x_hp := [xfvm, "@construct"]; *)
      let x_hp = fresh_var () in
      let cmd_hf_construct =
        LBasic (HasField (x_hp, PVar xfvm, Lit (String _constructPropName)))
      in

      (* goto [ x_hp = empty ] err next2; *)
      let call = fresh_then_label () in
      let get_bt = fresh_then_label () in
      let jump = if_verification call get_bt in
      let cmd_goto_xhp = LGuardedGoto (PVar x_hp, jump, tr_ctx.tr_err_lab) in

      let x_bt = fresh_var () in
      let cmd_get_bt =
        LBasic (HasField (x_bt, PVar xfvm, Lit (String "@boundThis")))
      in

      let bind = fresh_else_label () in
      let goto_guard_expr = PVar x_bt in
      let cmd_bind_test = LGuardedGoto (goto_guard_expr, bind, call) in

      (* BIND *)
      let x_ba = fresh_var () in
      let cmd_get_ba =
        LBasic (Lookup (x_ba, PVar xfvm, Lit (String "@boundArguments")))
      in

      let x_tf = fresh_var () in
      let cmd_get_tf =
        LBasic (Lookup (x_tf, PVar xfvm, Lit (String "@targetFunction")))
      in

      (* x_bref_fprototype := ref-o(x_tf, "prototype");  *)
      let x_bref_fprototype = fresh_var () in
      let cmd_bass_xreffprototype =
        LBasic
          (Assignment
             ( x_bref_fprototype,
               EList
                 [
                   lit_refo;
                   PVar x_tf;
                   lit_str "prototype";
                   Lit (Bool tr_ctx.tr_strictness);
                 ] ))
      in

      (* x_bf_prototype := i__getValue(x_bref_prototype) with err; *)
      let x_bf_prototype, cmd_bgv_xreffprototype, errs_bf_prototype =
        make_get_value_call (PVar x_bref_fprototype) tr_ctx.tr_err_lab
      in

      let bthen1 = fresh_then_label () in
      let belse1 = fresh_else_label () in
      let goto_guard_expr =
        BinOp (UnOp (TypeOf, PVar x_bf_prototype), Equal, Lit (Type ObjectType))
      in
      let cmd_bis_object = LGuardedGoto (goto_guard_expr, belse1, bthen1) in

      let x_bwhyGodwhy = fresh_var () in
      let cmd_bset_proto =
        LBasic (Assignment (x_bwhyGodwhy, Lit (Loc locObjPrototype)))
      in

      let x_bprototype = fresh_var () in
      let cmd_bproto_phi =
        LPhiAssignment
          [ (x_bprototype, [ PVar x_bf_prototype; PVar x_bwhyGodwhy ]) ]
      in

      (* x_cdo := i__createDefaultObject (x_f_prototype); *)
      let x_bthis = fresh_this_var () in
      let cmd_bcdo_call =
        LCall
          ( x_bthis,
            Lit (String createDefaultObjectName),
            [ PVar x_bprototype ],
            None,
            None )
      in

      (* xtfm := metadata (x_tf) *)
      let xtfm = fresh_var () in
      let cmd_xtfm = LBasic (MetaData (xtfm, PVar x_tf)) in

      let x_bbody = fresh_body_var () in
      let cmd_bbody =
        LBasic (Lookup (x_bbody, PVar xtfm, Lit (String _constructPropName)))
      in

      let x_bfscope = fresh_fscope_var () in
      let cmd_bscope =
        LBasic (Lookup (x_bfscope, PVar xtfm, Lit (String _scopePropName)))
      in

      let x_params = fresh_var () in
      let cmd_append =
        LBasic
          (Assignment
             ( x_params,
               NOp
                 ( LstCat,
                   [
                     EList [ PVar x_bbody; PVar x_bfscope; PVar x_bthis ];
                     PVar x_ba;
                     EList x_args_gv;
                   ] ) ))
      in

      let x_bconstruct = fresh_var () in
      let cmd_bind =
        LApply (x_bconstruct, PVar x_params, Some tr_ctx.tr_err_lab)
      in

      (* goto [ x_bconstruct = empty ] next3 next4; *)
      let bnext3 = fresh_next_label () in
      let bnext4 = fresh_next_label () in
      let goto_guard_expr =
        BinOp (UnOp (TypeOf, PVar x_bconstruct), Equal, Lit (Type ObjectType))
      in
      let cmd_bgoto_test_type =
        LGuardedGoto (goto_guard_expr, bnext4, bnext3)
      in

      (* next3: skip; *)
      let cmd_bret_this = LBasic Skip in

      (* next4: x_rbind := PHI(x_bconstruct, x_bt) *)
      let x_rbind = fresh_var () in
      let cmd_bphi_final =
        LPhiAssignment [ (x_rbind, [ PVar x_bconstruct; PVar x_bthis ]) ]
      in

      (* SYNC *)
      let join = fresh_label () in
      let cmd_sync = LGoto join in

      (* x_ref_fprototype := ref-o(x_f_val, "prototype");  *)
      let x_ref_fprototype = fresh_var () in
      let cmd_ass_xreffprototype =
        LBasic
          (Assignment
             ( x_ref_fprototype,
               EList
                 [
                   lit_refo;
                   PVar x_f_val;
                   lit_str "prototype";
                   Lit (Bool tr_ctx.tr_strictness);
                 ] ))
      in

      (* x_f_prototype := i__getValue(x_ref_prototype) with err; *)
      let x_f_prototype, cmd_gv_xreffprototype, errs_xf_prototype =
        make_get_value_call (PVar x_ref_fprototype) tr_ctx.tr_err_lab
      in

      let then1 = fresh_then_label () in
      let else1 = fresh_else_label () in
      let goto_guard_expr =
        BinOp (UnOp (TypeOf, PVar x_f_prototype), Equal, Lit (Type ObjectType))
      in
      let cmd_is_object = LGuardedGoto (goto_guard_expr, else1, then1) in

      let x_whyGodwhy = fresh_var () in
      let cmd_set_proto =
        LBasic (Assignment (x_whyGodwhy, Lit (Loc locObjPrototype)))
      in

      let x_prototype = fresh_var () in
      let cmd_proto_phi =
        LPhiAssignment
          [ (x_prototype, [ PVar x_f_prototype; PVar x_whyGodwhy ]) ]
      in

      (* x_cdo := i__createDefaultObject (x_this, x_f_prototype); *)
      (* let x_cdo = fresh_var () in *)
      let x_this = fresh_this_var () in
      let cmd_cdo_call =
        LCall
          ( x_this,
            Lit (String createDefaultObjectName),
            [ PVar x_prototype ],
            None,
            None )
      in

      (* x_body := [xfvm, "@construct"];  *)
      let x_body = fresh_body_var () in
      let cmd_body =
        LBasic (Lookup (x_body, PVar xfvm, Lit (String _constructPropName)))
      in

      (* x_fscope := [xfvm, "@scope"]; *)
      let x_fscope = fresh_fscope_var () in
      let cmd_scope =
        LBasic (Lookup (x_fscope, PVar xfvm, Lit (String _scopePropName)))
      in

      (* x_r1 := x_body (x_scope, x_this, x_arg0_val, ..., x_argn_val) with err  *)
      let x_r1 = fresh_var () in
      let proc_args = PVar x_fscope :: PVar x_this :: x_args_gv in
      let cmd_proc_call =
        LCall (x_r1, PVar x_body, proc_args, Some tr_ctx.tr_err_lab, None)
      in

      (* goto [ x_r1 = empty ] next3 next4; *)
      let next3 = fresh_next_label () in
      let next4 = fresh_next_label () in
      let goto_guard_expr =
        BinOp (UnOp (TypeOf, PVar x_r1), Equal, Lit (Type ObjectType))
      in
      let cmd_goto_test_type = LGuardedGoto (goto_guard_expr, next4, next3) in

      (* next3: skip; *)
      let cmd_ret_this = LBasic Skip in

      (* next4: x_r2 := PHI(x_r1, x_this) *)
      let x_rcall = fresh_var () in
      let cmd_phi_final =
        LPhiAssignment [ (x_rcall, [ PVar x_r1; PVar x_this ]) ]
      in

      let x_final = fresh_var () in
      let cmd_phi_join =
        LPhiAssignment [ (x_final, [ PVar x_rbind; PVar x_rcall ]) ]
      in

      let cmds =
        annotate_first_cmd
          (cmds_ef
          @ [
              (*        cmds_ef                                                                 *)
              annotate_cmd cmd_gv_f None
              (*        x_f_val := i__getValue (x_f) with err                                   *);
            ]
          @ annotate_first_call_cmd
              (cmds_args
              @ annotate_cmds
                  [
                    (*        cmds_arg_i; x_arg_i_val := i__getValue (x_arg_i) with err               *)
                    (None, cmd_goto_is_obj);
                    (*        goto [ typeOf(x_f_val) != Object] err next1                             *)
                    (Some next1, cmd_xfvm);
                    (*        xfvm := metadata(x_f_val)                                               *)
                    (None, cmd_hf_construct);
                    (* next1: x_hp := [xfvm, "@construct"];                                           *)
                    (None, cmd_goto_xhp)
                    (*        goto [ x_hp = empty ] err next2                                         *);
                  ]
              @ if_verification []
                  (annotate_cmds
                     [
                       (* PREP *)
                       (Some get_bt, cmd_get_bt);
                       (*        x_bt := [xfvm, "@boundTarget"];                                         *)
                       (None, cmd_bind_test);
                       (*        goto [x_bt = empty] call bind                                           *)

                       (* BIND *)
                       (Some bind, cmd_get_ba);
                       (*         x_ba := [xfvm, "@boundArgs"];                                         *)
                       (None, cmd_get_tf);
                       (*         x_tf := [xfvm, "@targetFunction"];                                    *)
                       (None, cmd_bass_xreffprototype);
                       (*         x_bref_fprototype := ref-o(x_tf, "prototype")                         *)
                       (None, cmd_bgv_xreffprototype);
                       (*         x_bf_prototype := i__getValue(x_bref_prototype) with err              *)
                       (None, cmd_bis_object);
                       (*         goto [typeof (x_bf_prototype) = Obj] else1 then1;                     *)
                       (Some bthen1, cmd_bset_proto);
                       (* bthen1: x_bwhyGodwhy := $lobj_proto                                           *)
                       (Some belse1, cmd_bproto_phi);
                       (* belse1: x_bprototype := PHI (x_bf_prototype, x_bwhyGodwhy)                  *)
                       (None, cmd_bcdo_call);
                       (*         x_bcdo := create_default_object (x_bthis, x_bprototype)               *)
                       (None, cmd_xtfm);
                       (*         xtfm := metadata(x_tf)                                                *)
                       (None, cmd_bbody);
                       (*         x_bbody := [xtfm, "@construct"];                                      *)
                       (None, cmd_bscope);
                       (*         x_fscope := [xtfm, "@scope"]                                          *)
                       (None, cmd_append);
                       (*        SOMETHING ABOUT PARAMETERS                                             *)
                       (None, cmd_bind);
                       (*        MAGICAL FLATTENING CALL                                                *)
                       (None, cmd_bgoto_test_type);
                       (*        goto [typeOf(x_r1) = Obj ] next4 next3;                                *)
                       (Some bnext3, cmd_bret_this);
                       (* next3: skip                                                                   *)
                       (Some bnext4, cmd_bphi_final);
                       (* next4: x_rcall := PHI(x_r1, x_this)                                           *)
                       (None, cmd_sync)
                       (*        goto join                                                              *);
                     ])
              @ annotate_cmds
                  [
                    (Some call, cmd_ass_xreffprototype);
                    (*        x_ref_fprototype := ref-o(x_f_val, "prototype")                        *)
                    (None, cmd_gv_xreffprototype);
                    (*        x_f_prototype := i__getValue(x_ref_prototype) with err                 *)
                    (None, cmd_is_object);
                    (*        goto [typeof (x_f_prototype) = Obj] else1 then1;                       *)
                    (Some then1, cmd_set_proto);
                    (* then1:  x_whyGodwhy := $lobj_proto                                            *)
                    (Some else1, cmd_proto_phi);
                    (* else1: x_prototype := PHI (x_f_prototype, x_whyGodwhy)                      *)
                    (None, cmd_cdo_call);
                    (*        x_cdo := create_default_object (x_this, x_prototype)                   *)
                    (None, cmd_body);
                    (*        x_body := [xfvm, "@construct"]                                         *)
                    (None, cmd_scope);
                    (*        x_fscope := [xfvm, "@scope"]                                           *)
                    (None, cmd_proc_call);
                    (*        x_r1 := x_body (x_scope, x_this, x_arg0_val, ..., x_argn_val) with err *)
                    (None, cmd_goto_test_type);
                    (*        goto [typeOf(x_r1) = Obj ] next4 next3;                                *)
                    (Some next3, cmd_ret_this);
                    (* next3: skip                                                                   *)
                    (Some next4, cmd_phi_final)
                    (* next4: x_rcall := PHI(x_r1, x_this)                                           *);
                  ]
              @ if_verification []
                  (annotate_cmds
                     [
                       (Some join, cmd_phi_join)
                       (*        x_final := PHI (x_rbind, x_rcall);                                       *);
                     ])))
      in
      let errs =
        errs_ef @ errs_xf_val @ errs_args @ [ var_te; var_te ]
        @ if_verification [] (errs_bf_prototype @ [ x_bconstruct ])
        @ errs_xf_prototype @ [ x_r1 ]
      in
      (cmds, PVar (if_verification x_rcall x_final), errs)
  (*
      * The constructs for symbolic execution...
    **)
  | JS_Parser.Syntax.Call (e_f, xes)
    when e_f.JS_Parser.Syntax.exp_stx
         = JS_Parser.Syntax.Var js_symbolic_constructs.js_assert -> (
      match List.map (fun xe -> xe.JS_Parser.Syntax.exp_stx) xes with
      | [ JS_Parser.Syntax.String assert_arg_str ] ->
          let e' = Parsing.parse_expr_from_string assert_arg_str in
          let xs = Expr.pvars e' in
          let subst = SSubst.init [] in

          let cmds, errs =
            List.fold_left
              (fun (cmds, errs) x ->
                let new_cmds, x_expr, new_errs =
                  f { e with JS_Parser.Syntax.exp_stx = JS_Parser.Syntax.Var x }
                in

                let x_v, cmd_gv_x, errs_x_v =
                  make_get_value_call x_expr tr_ctx.tr_err_lab
                in
                SSubst.put subst (PVar x) (PVar x_v);
                ( cmds @ new_cmds @ [ annotate_cmd cmd_gv_x None ],
                  errs @ new_errs @ errs_x_v ))
              ([], []) (SS.elements xs)
          in

          let le = SSubst.subst_in_expr subst ~partial:true e' in
          let asrt =
            match Formula.lift_logic_expr le with
            | Some (asrt_b, _) -> asrt_b
            | _ ->
                raise
                  (Failure
                     (Printf.sprintf "Invalid assert. Could not lift\n%s"
                        ((Fmt.to_to_string Expr.pp) le)))
          in
          let cmd = (metadata, None, LLogic (LCmd.Assert asrt)) in

          (cmds @ [ cmd ], Lit Empty, errs)
      | es ->
          let msg =
            String.concat "\n"
              (List.map
                 (fun e -> JS_Parser.PrettyPrint.string_of_exp_syntax_1 e true)
                 es)
          in
          raise (Failure (Printf.sprintf "Invalid assert:\n%s" msg)))
  | JS_Parser.Syntax.Call (e_f, xes)
    when e_f.JS_Parser.Syntax.exp_stx
         = JS_Parser.Syntax.Var js_symbolic_constructs.js_assume -> (
      match List.map (fun xe -> xe.JS_Parser.Syntax.exp_stx) xes with
      | [ JS_Parser.Syntax.String assume_arg_str ] ->
          let e' = Parsing.parse_expr_from_string assume_arg_str in
          let xs = Expr.pvars e' in
          let subst = SSubst.init [] in
          let cmds, errs =
            List.fold_left
              (fun (cmds, errs) x ->
                let new_cmds, x_expr, new_errs =
                  f { e with JS_Parser.Syntax.exp_stx = JS_Parser.Syntax.Var x }
                in
                let x_v, cmd_gv_x, errs_x_v =
                  make_get_value_call x_expr tr_ctx.tr_err_lab
                in
                SSubst.put subst (PVar x) (PVar x_v);
                ( cmds @ new_cmds @ [ annotate_cmd cmd_gv_x None ],
                  errs @ new_errs @ errs_x_v ))
              ([], []) (SS.elements xs)
          in

          let le = SSubst.subst_in_expr subst ~partial:true e' in
          let asrt =
            match Formula.lift_logic_expr le with
            | Some (asrt_b, _) -> asrt_b
            | _ ->
                raise
                  (Failure
                     (Printf.sprintf "Invalid assume. Could not lift\n%s"
                        ((Fmt.to_to_string Expr.pp) le)))
          in
          let cmd = (metadata, None, LLogic (LCmd.Assume asrt)) in

          (cmds @ [ cmd ], Lit Empty, errs)
      | _ -> raise (Failure "Invalid assume"))
  | JS_Parser.Syntax.Call (e_f, xes)
    when e_f.JS_Parser.Syntax.exp_stx
         = JS_Parser.Syntax.Var js_symbolic_constructs.js_symb ->
      let () =
        match xes with
        | [] -> ()
        | _ -> raise (Failure "Invalid symbolic")
      in
      let x_v = fresh_var () ^ "_v" in
      let cmd1 = (metadata, None, LLogic (LCmd.FreshSVar x_v)) in
      let x_v = PVar x_v in
      let cmd2 =
        (metadata, None, LLogic (LCmd.Assume (Not (Eq (x_v, Lit Empty)))))
      in
      let cmd3 =
        (metadata, None, LLogic (LCmd.Assume (Not (Eq (x_v, Lit Nono)))))
      in
      let cmd4 =
        ( metadata,
          None,
          LLogic
            (LCmd.Assume (Not (Eq (UnOp (TypeOf, x_v), Lit (Type ListType)))))
        )
      in
      ([ cmd1; cmd2; cmd3; cmd4 ], x_v, [])
  | JS_Parser.Syntax.Call (e_f, xes)
    when e_f.JS_Parser.Syntax.exp_stx
         = JS_Parser.Syntax.Var js_symbolic_constructs.js_symb_number ->
      let () =
        match xes with
        | [] -> ()
        | _ -> raise (Failure "Invalid symb_number")
      in
      let x_v = fresh_var () ^ "_v" in
      let cmd1 = (metadata, None, LLogic (FreshSVar x_v)) in
      let x_v = PVar x_v in
      let cmd2 =
        (metadata, None, LLogic (LCmd.AssumeType (x_v, Type.NumberType)))
      in
      ([ cmd1; cmd2 ], x_v, [])
  | JS_Parser.Syntax.Call (e_f, xes)
    when e_f.JS_Parser.Syntax.exp_stx
         = JS_Parser.Syntax.Var js_symbolic_constructs.js_symb_string ->
      let () =
        match xes with
        | [] -> ()
        | _ -> raise (Failure "Invalid symb_string")
      in
      let x_v = fresh_var () ^ "_v" in
      let cmd1 = (metadata, None, LLogic (FreshSVar x_v)) in
      let x_v = PVar x_v in
      let cmd2 =
        (metadata, None, LLogic (LCmd.AssumeType (x_v, Type.StringType)))
      in
      ([ cmd1; cmd2 ], x_v, [])
  | JS_Parser.Syntax.Call (e_f, xes)
    when e_f.JS_Parser.Syntax.exp_stx
         = JS_Parser.Syntax.Var js_symbolic_constructs.js_symb_bool ->
      let () =
        match xes with
        | [] -> ()
        | _ -> raise (Failure "Invalid symb_bool")
      in
      let x_v = fresh_var () ^ "_v" in
      let cmd1 = (metadata, None, LLogic (FreshSVar x_v)) in
      let x_v = PVar x_v in
      let cmd2 =
        (metadata, None, LLogic (LCmd.AssumeType (x_v, Type.BooleanType)))
      in
      ([ cmd1; cmd2 ], x_v, [])
  | JS_Parser.Syntax.Call (e_f, xes)
    when Gillian.Utils.(ExecMode.biabduction_exec !Config.current_exec_mode)
         &&
         match e_f.JS_Parser.Syntax.exp_stx with
         | JS_Parser.Syntax.Var bi_annot
           when List.mem bi_annot JS2JSIL_Helpers.reserved_biannots -> true
         | _ -> false ->
      let bi_annot =
        match e_f.JS_Parser.Syntax.exp_stx with
        | JS_Parser.Syntax.Var bi_annot -> bi_annot
        | _ -> raise (Failure "JS2JSIL: Impossible")
      in

      Logging.print_to_all ("BiAnnot: " ^ bi_annot);

      let cmds_args, proc_args, errs_args =
        translate_arg_list xes tr_ctx.tr_err_lab
      in

      let x_rcall = fresh_var () in
      let cmd_proc_call =
        LCall
          ( x_rcall,
            Lit (String bi_annot),
            proc_args,
            Some tr_ctx.tr_err_lab,
            None )
      in

      ( cmds_args @ [ (metadata, None, cmd_proc_call) ],
        PVar x_rcall,
        errs_args @ [ x_rcall ] )
  | JS_Parser.Syntax.Call (e_f, xes) ->
      (*
      Section 11.2.3 - Function call
      C(e_f) = cmds_ef, x_f
      C(e_i) = cmds_ei, x_argi (for i = 1, ..., n)
      C(e_f(e_1, ..., e_n) =
                  cmds_ef
                  x_f_val := i__getValue (x_f) with err;
                  cmds_e1
                  x_arg1_val := i__getValue (x_arg1) with err;
                  ...
                  cmds_en
                  x_argn_val := i__getValue (x_argn) with err;
                  goto [ typeOf(x_f_val) != Object] err next1;
          next1:  x_ic := isCallable(x_f_val);
                  goto [ x_ic ] next2 err;
          next2:  goto [ typeOf(x_f) = ObjReference ] then else;
          then1:  x_this := base(x_f);
                  goto end;
          else1:  x_this := undefined;
          end1:   xfvm := metadata(x_f_val);
                  x_body := [xfvm, "@call"];
                  x_scope := [xfvm, "@scope"];
                  x_r1 := x_body (x_scope, x_this, x_arg0_val, ..., x_argn_val) with err;
                  goto [ x_r1 = empty ] next3 next4;
          next3:  x_r2 := undefined;
          next4:  x_r3 := PHI(x_r1, x_r2)
    *)
      let cmds_ef, x_ef, errs_ef = f e_f in

      (* x_f_val := i__getValue (x_f) with err1;  *)
      let x_f_val, cmd_gv_f, errs_xf_val =
        make_get_value_call x_ef tr_ctx.tr_err_lab
      in

      let cmds_args, x_args_gv, errs_args =
        translate_arg_list xes tr_ctx.tr_err_lab
      in

      (* goto [ typeOf(x_f_val) != Object] err next1; err -> typeerror *)
      let next1 = fresh_next_label () in
      let goto_guard_expr =
        UnOp
          ( UNot,
            BinOp (UnOp (TypeOf, PVar x_f_val), Equal, Lit (Type ObjectType)) )
      in
      let cmd_goto_is_obj =
        LGuardedGoto (goto_guard_expr, tr_ctx.tr_err_lab, next1)
      in

      (* next1: x_ic := isCallable(x_f_val); *)
      let x_ic = fresh_var () in
      let cmd_ic =
        LCall (x_ic, Lit (String isCallableName), [ PVar x_f_val ], None, None)
      in

      (* goto [ x_ic ] getbt err; -> typeerror *)
      let call = fresh_label () in
      let get_bt = fresh_next_label () in
      let jump = if_verification call get_bt in
      let cmd_goto_is_callable =
        LGuardedGoto (PVar x_ic, jump, tr_ctx.tr_err_lab)
      in

      let xfvm = fresh_var () in
      let cmd_xfvm = LBasic (MetaData (xfvm, PVar x_f_val)) in

      let x_ibt = fresh_var () in
      let cmd_get_ibt =
        LBasic (HasField (x_ibt, PVar xfvm, Lit (String "@boundThis")))
      in

      let bind = fresh_else_label () in
      let goto_guard_expr = PVar x_ibt in
      let cmd_bind_test = LGuardedGoto (goto_guard_expr, bind, call) in

      (* BIND *)
      let x_bt = fresh_var () in
      let cmd_get_bt =
        LBasic (Lookup (x_bt, PVar xfvm, Lit (String "@boundThis")))
      in

      let x_ba = fresh_var () in
      let cmd_get_ba =
        LBasic (Lookup (x_ba, PVar xfvm, Lit (String "@boundArguments")))
      in

      let x_tf = fresh_var () in
      let cmd_get_tf =
        LBasic (Lookup (x_tf, PVar xfvm, Lit (String "@targetFunction")))
      in

      (* xtfm := metadata (x_tf) *)
      let xtfm = fresh_var () in
      let cmd_xtfm = LBasic (MetaData (xtfm, PVar x_tf)) in

      let x_bbody = fresh_body_var () in
      let cmd_bbody =
        LBasic (Lookup (x_bbody, PVar xtfm, Lit (String _callPropName)))
      in

      let x_bfscope = fresh_fscope_var () in
      let cmd_bscope =
        LBasic (Lookup (x_bfscope, PVar xtfm, Lit (String _scopePropName)))
      in

      let x_params = fresh_var () in
      let cmd_append =
        LBasic
          (Assignment
             ( x_params,
               NOp
                 ( LstCat,
                   [
                     EList [ PVar x_bbody; PVar x_bfscope; PVar x_bt ];
                     PVar x_ba;
                     EList x_args_gv;
                   ] ) ))
      in

      let x_rbind = fresh_var () in
      let cmd_bind = LApply (x_rbind, PVar x_params, Some tr_ctx.tr_err_lab) in

      (* SYNC *)
      let join = fresh_label () in
      let cmd_sync = LGoto join in

      (* x_body := [x_f_val, "@call"]; *)
      let x_body = fresh_body_var () in
      let cmd_body =
        LBasic (Lookup (x_body, PVar xfvm, Lit (String _callPropName)))
      in

      (* EVAL *)
      let then_eval = fresh_then_label () in
      let else_eval = fresh_else_label () in
      let cmd_are_we_doing_eval =
        LGuardedGoto
          ( BinOp (PVar x_body, Equal, Lit (String "ExecuteEval")),
            then_eval,
            else_eval )
      in

      let x_ecall = fresh_var () in
      let proc_args =
        PVar JS2JSIL_Helpers.var_sc_first :: PVar JS2JSIL_Helpers.var_this
        :: Lit (Bool tr_ctx.tr_strictness) :: x_args_gv
      in
      let cmd_execute_eval =
        LECall (x_ecall, PVar "ExecuteEval", proc_args, Some tr_ctx.tr_err_lab)
      in

      (* x_fscope := [x_f_val, "@scope"]; *)
      let x_fscope = fresh_fscope_var () in
      let cmd_scope =
        LBasic (Lookup (x_fscope, PVar xfvm, Lit (String _scopePropName)))
      in

      (* join: goto [ typeOf(x_f) = ObjReference ] then else;  *)
      let then_lab_1 = fresh_then_label () in
      let then_lab_2 = fresh_then_label () in
      let else_lab = fresh_else_label () in
      let end_lab = fresh_endif_label () in

      let cmd_goto_obj_ref_1 =
        LGuardedGoto (is_list_type x_ef, then_lab_1, else_lab)
      in
      let cmd_goto_obj_ref_2 =
        LGuardedGoto (is_oref x_ef, then_lab_2, else_lab)
      in

      (* then: x_then_this := base(x_f); *)
      let x_this_then = fresh_this_var () in
      let cmd_this_base = LBasic (Assignment (x_this_then, base x_ef)) in

      (*  goto end; *)
      let cmd_goto_end = LGoto end_lab in

      (* else: x_else_this := undefined; *)
      let x_this_else = fresh_this_var () in
      let cmd_this_undefined =
        LBasic (Assignment (x_this_else, Lit Undefined))
      in

      (* end: x_this := PHI(x_then_this, x_else_this) *)
      let x_this = fresh_this_var () in
      let cmd_ass_xthis =
        LPhiAssignment [ (x_this, [ PVar x_this_then; PVar x_this_else ]) ]
      in

      (* x_r1 := x_body (x_scope, x_this, x_arg0_val, ..., x_argn_val) with err  *)
      let x_rcall = fresh_var () in
      let proc_args = PVar x_fscope :: PVar x_this :: x_args_gv in
      let cmd_proc_call =
        LCall
          (x_rcall, PVar x_body, proc_args, Some tr_ctx.tr_err_lab, call_subst)
      in

      let x_r1 = fresh_var () in
      let cmd_phi_join =
        LPhiAssignment [ (x_r1, [ PVar x_rbind; PVar x_ecall; PVar x_rcall ]) ]
      in

      (* goto [ x_r1 = empty ] next3 next4;
         let next3 = fresh_next_label () in
         let next4 = fresh_next_label () in
         let goto_guard_expr = BinOp (PVar (if_verification x_rcall x_r1), Equal, Lit Empty) in
         let cmd_goto_test_empty = LGuardedGoto (goto_guard_expr, next3, next4) in

         (* next3: x_r2 := undefined; *)
         let x_r2 = fresh_var () in
         let cmd_ret_undefined = LBasic (Assignment (x_r2, Lit Undefined)) in

         (* next4: x_r3 := PHI(x_r1, x_r2) *)
         let x_r3 = fresh_var () in
         let cmd_phi_final = LPhiAssignment [ (x_r3, [ PVar (if_verification x_rcall x_r1); PVar x_r2 ]) ] in *)
      let cmds =
        annotate_first_cmd
          (cmds_ef
          @ [
              (*        cmds_ef                                                                   *)
              annotate_cmd cmd_gv_f None
              (*        x_f_val := i__getValue (x_f) with err                                     *);
            ]
          @ annotate_first_call_cmd
              (cmds_args
              @ annotate_cmds
                  [
                    (*        cmds_arg_i; x_arg_i_val := i__getValue (x_arg_i) with err                 *)
                    (None, cmd_goto_is_obj);
                    (*        goto [ typeOf(x_f_val) != Object] err next1                               *)
                    (Some next1, cmd_xfvm);
                    (* next1: xfvm := metadata(x_f_val)                                               *)
                    (None, cmd_ic);
                    (*        x_ic := isCallable(x_f_val)                                               *)
                    (None, cmd_goto_is_callable)
                    (*        goto [ x_ic ] getbt err; -> typeerror                                     *);
                  ]
              @ if_verification []
                  (annotate_cmds
                     [
                       (* PREP *)
                       (Some get_bt, cmd_get_ibt);
                       (*        x_bt := [xfvm, "@boundTarget"];                                      *)
                       (None, cmd_bind_test);
                       (*        goto [x_bt = empty] call bind                                           *)

                       (* BIND *)
                       (Some bind, cmd_get_bt);
                       (*        x_bt := [xfvm, "@boundThis"];                                          *)
                       (None, cmd_get_ba);
                       (*        x_ba := [xfvm, "@boundArgs"];                                          *)
                       (None, cmd_get_tf);
                       (*        x_tf := [xfvm, "@targetFunction"];                                     *)
                       (None, cmd_xtfm);
                       (*        xtfm := metadata(x_tf)                                                  *)
                       (None, cmd_bbody);
                       (*        x_bbody := [xtfm, "@call"];                                               *)
                       (None, cmd_bscope);
                       (*        x_fscope := [xtfm, "@scope"]                                              *)
                       (None, cmd_append);
                       (*        SOMETHING ABOUT PARAMETERS                                                *)
                       (None, cmd_bind);
                       (*        MAGICAL FLATTENING CALL                                                   *)
                       (None, cmd_sync)
                       (*        goto join                                                                 *);
                     ])
              (* CALL *)
              @ annotate_cmds [ (Some call, cmd_body) ]
              @ if_verification []
                  (annotate_cmds
                     [
                       (None, cmd_are_we_doing_eval);
                       (Some then_eval, cmd_execute_eval);
                       (None, cmd_sync);
                     ])
              @ annotate_cmds
                  [
                    (Some else_eval, cmd_scope);
                    (*        x_fscope := [xfvm, "@scope"]                                              *)
                    (None, cmd_goto_obj_ref_1);
                    (* next2: goto [ typeOf(x_f) = ObjReference ] then else                             *)
                    (Some then_lab_1, cmd_goto_obj_ref_2);
                    (Some then_lab_2, cmd_this_base);
                    (* then:  x_then_this := base(x_f)                                                  *)
                    (None, cmd_goto_end);
                    (*        goto end                                                                  *)
                    (Some else_lab, cmd_this_undefined);
                    (* else:  x_else_this := undefined                                                  *)
                    (Some end_lab, cmd_ass_xthis);
                    (* end:   x_this := PHI(x_then_this, x_else_this)                                   *)
                    (None, cmd_proc_call)
                    (*        x_rcall := x_body (x_scope, x_this, x_arg0_val, ..., x_argn_val) with err *);
                  ]
              @ if_verification []
                  (annotate_cmds
                     [
                       (* JOIN *)
                       (Some join, cmd_phi_join)
                       (*        x_r1 := PHI (x_rbind, x_rcall);                                           *);
                     ])
                (* @ annotate_cmds [
                     (None,           cmd_goto_test_empty);  (*        goto [ x_r1 = empty ] next3 next4                                       *)
                     (Some next3,     cmd_ret_undefined);    (* next3: x_r2 := undefined                                                       *)
                     (Some next4,     cmd_phi_final)         (* next4: x_r3 := PHI(x_r1, x_r2)                                                 *)
                   ] *)))
      in
      let errs =
        errs_ef @ errs_xf_val @ errs_args @ [ var_te; var_te ]
        @ if_verification [] [ x_rbind; x_ecall ]
        @ [ x_rcall ]
      in
      (cmds, PVar (if_verification x_rcall x_r1), errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Post_Incr, e) ->
      (*
      Section: 11.3.1
      C(e) = cmds, x

      C(e++) =
          cmds
            goto [ (typeof (x) = $$v-reference_type) and ((field(x) = "eval") or (field(x) = "arguments")) ] err next
       next:  x_v := i__getValue (x) with err
          x_n := i__toNumber (x_v) with err
          x_r := x_n + 1
          x_pv := putValue (x, x_r) with err;
     *)
      let cmds, x, errs = f e in
      let new_cmds, new_errs, x_v, _ =
        translate_inc_dec x true tr_ctx.tr_err_lab
      in
      let new_cmds = annotate_cmds new_cmds in
      (annotate_first_cmd (cmds @ new_cmds), PVar x_v, errs @ new_errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Post_Decr, e) ->
      (*
      Section: 11.3.2
      C(e) = cmds, x

      C(e--) =
          cmds
              goto [ (typeof (x) = $$v-reference_type) and ((field(x) = "eval") or (field(x) = "arguments")) ] err next
       next:  x_v := i__getValue (x) with err
          x_n := i__toNumber (x_v) with err
          x_r := x_n - 1
          x_pv := putValue (x, x_r) with err
     *)
      let cmds, x, errs = f e in
      let new_cmds, new_errs, x_v, _ =
        translate_inc_dec x false tr_ctx.tr_err_lab
      in
      let new_cmds = annotate_cmds new_cmds in
      (annotate_first_cmd (cmds @ new_cmds), PVar x_v, errs @ new_errs)
  (* TODO: Adjust for non-strict *)
  | JS_Parser.Syntax.Delete e ->
      (*
      Section: 11.4.1
      C(e) = cmds, x
      C(delete e) =
             cmds
             goto [ (is-ref x) ] next1 next4
      next1: goto [ ((base(x) = null) or (base(x) = undefined)) ] err next2
      next2: goto [ (typeOf x) = $$v-reference_type ] err next3
      next3: x_obj := toObject(base(x)) with err
           x_r1 := deleteProperty(x_obj, field(x), true) with err
           goto next5
      next4: x_r2 := true
      next5: x_r := PHI(x_r1; x_r2)
        *)
      let cmds, x, errs = f e in

      (* goto [ (typeOf x) <: $$reference_type ] next1 next4 *)
      let next1 = fresh_next_label () in
      let next2 = fresh_next_label () in
      let next3 = fresh_next_label () in
      let next4 = fresh_next_label () in
      let next5 = fresh_next_label () in
      let next6 = fresh_next_label () in
      let goto_guard_1 = is_list_type x in
      let goto_guard_2 = is_ref x in
      let cmd_goto_isref_1 = LGuardedGoto (goto_guard_1, next1, next5) in
      let cmd_goto_isref_2 = LGuardedGoto (goto_guard_2, next2, next5) in

      (* next1: goto [ ((base(x) = null) or (base(x) = undefined)) ] err next2 *)
      let cmd_goto_is_resolvable_ref =
        LGuardedGoto (make_unresolvable_ref_test x, tr_ctx.tr_err_lab, next3)
      in

      (* next2: goto [ (typeOf x) = $$v-reference_type ] err next3 *)
      let goto_guard = is_vref x in
      let cmd_goto_is_vref =
        LGuardedGoto (goto_guard, tr_ctx.tr_err_lab, next4)
      in

      (* next3: x_obj := toObject(base(x)) err *)
      let x_obj = fresh_obj_var () in
      let cmd_to_obj =
        LCall
          (x_obj, lit_str toObjectName, [ base x ], Some tr_ctx.tr_err_lab, None)
      in

      (* x_r1 := deleteProperty(x_obj, field(x), true) with err *)
      let x_r1 = fresh_var () in
      let cmd_delete =
        LCall
          ( x_r1,
            lit_str deletePropertyName,
            [ PVar x_obj; field x; Lit (Bool tr_ctx.tr_strictness) ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      let x_r2 = fresh_var () in
      let x_r = fresh_var () in
      let cmds =
        annotate_first_cmd
          (cmds
          @ annotate_cmds
              [
                (*        cmds                                                                     *)
                (None, cmd_goto_isref_1);
                (*        goto [ (typeOf x) <: $$reference_type ] next1 next5                      *)
                (Some next1, cmd_goto_isref_2);
                (* next1: goto [ (typeOf x) <: $$reference_type ] next2 next3                      *)
                (Some next2, cmd_goto_is_resolvable_ref);
                (* next2: goto [ ((base(x_e) = null) or (base(x_e) = undefined)) ] err next3       *)
                (Some next3, cmd_goto_is_vref);
                (* next3: goto [ (typeOf x) = $$v-reference_type ] err next4                       *)
                (Some next4, cmd_to_obj);
                (* next4: x_obj := toObject(base(x)) err3                                          *)
                (None, cmd_delete);
                (*        x_r1 := deleteProperty(x_obj, field(x), true) with err                   *)
                (None, LGoto next6);
                (*        goto next6                                                               *)
                (Some next5, LBasic (Assignment (x_r2, Lit (Bool true))));
                (* next5: x_r2 := true                                                             *)
                (Some next6, LPhiAssignment [ (x_r, [ PVar x_r1; PVar x_r2 ]) ])
                (* next6: x_r := PHI(x_r1, x_r2)                                                   *);
              ])
      in
      let errs = errs @ [ var_se; var_se; x_obj; x_r1 ] in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Void, e) ->
      (* Section: 11.4.2
           C(e) = cmds, x
         C(void e) =
               cmds
                 x_v := getValue (x) with err
             x_r := undefined
      *)
      let cmds, x, errs = f e in
      (* x_v := getValue (x) with err *)
      let _, cmd_gv_x, errs_x_v = make_get_value_call x tr_ctx.tr_err_lab in
      let x_r = fresh_var () in
      let cmds =
        annotate_first_cmd
          (cmds
          @ annotate_cmds
              [
                (*  cmds                                *)
                (None, cmd_gv_x);
                (*  x_v := getValue (x) with err        *)
                (None, LBasic (Assignment (x_r, Lit Undefined)))
                (*  x_r := undefined                  *);
              ])
      in
      let errs = errs @ errs_x_v in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.TypeOf, e) ->
      (* Section: 11.4.3
         C(e) =  cmds, x
         C(typeof e) =
                 cmds
                         goto [ is-ref(typeof (x)) ] next1 next5
                next1:   goto [ is-ref(typeof (x)) ] next2 next5
              next2:   goto [ ((base(x) = null) or (base(x) = undefined)) ] next3 next4
              next3:   x1 := undefined
                   goto next4
              next4:   x2 := getValue (x) with err
              next5:   x3 := PHI (x, x1, x2)
                   x_r := i__typeOf (x3) with err
      *)
      let cmds, x, errs = f e in
      let cmds, x_name = add_final_var cmds x metadata in

      (* goto [ is-ref(typeof (x)) ] next1 next4 *)
      let next1 = fresh_next_label () in
      let next2 = fresh_next_label () in
      let next3 = fresh_next_label () in
      let next4 = fresh_next_label () in
      let next5 = fresh_next_label () in
      let cmd_goto_ref_1 = LGuardedGoto (is_list_type x, next1, next5) in
      let cmd_goto_ref_2 = LGuardedGoto (is_ref x, next2, next5) in

      (* goto [ ((base(x_e) = null) or (base(x_e) = undefined)) ] next3 next4 *)
      let cmd_goto_unres_ref =
        LGuardedGoto (make_unresolvable_ref_test x, next3, next4)
      in

      (* x2 := getValue (x) with err *)
      let x1 = fresh_var () in
      let x2 = fresh_var () in
      let cmd_gv_x =
        LCall
          (x2, Lit (String getValueName), [ x ], Some tr_ctx.tr_err_lab, None)
      in

      (* x_r := i__typeOf (x3) with err *)
      let x3 = fresh_var () in
      let x_r = fresh_var () in
      let cmd_ass_xr =
        LCall
          ( x_r,
            Lit (String jsTypeOfName),
            [ PVar x3 ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      let cmds =
        annotate_first_cmd
          (cmds
          @ annotate_cmds
              [
                (*             cmds                                                  *)
                (None, cmd_goto_ref_1);
                (*             goto [ typeof (x) <: $$reference-type ] next1 next5   *)
                (Some next1, cmd_goto_ref_2);
                (*             goto [ typeof (x) <: $$reference-type ] next2 next5   *)
                (Some next2, cmd_goto_unres_ref);
                (* next2:      goto [ base(x) = undefined] next2 next3               *)
                (Some next3, LBasic (Assignment (x1, Lit Undefined)));
                (* next3:      x1 := undefined                                       *)
                (None, LGoto next5);
                (*             goto next4                                            *)
                (Some next4, cmd_gv_x);
                (* next4:      x2 := getValue (x) with err                           *)
                ( Some next5,
                  LPhiAssignment
                    [ (x3, [ PVar x_name; PVar x_name; PVar x1; PVar x2 ]) ] );
                (* next5:      x3 := PHI (x, x1, x2)                                 *)
                (None, cmd_ass_xr)
                (*             x_r := i__typeOf (x3) with err                        *);
              ])
      in
      let errs = errs @ [ x2; x_r ] in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Pre_Incr, e) ->
      (*
      Section: 11.4.4
      C(e) = cmds, x
      C(++e) =          cmds
                        goto [ (typeof (x) = $$v-reference_type) and ((field(x) = "eval") or (field(x) = "arguments")) ] err next
                 next:  x_v := i__getValue (x) with err
                x_n := i__toNumber (x_v) with err
                x_r := x_n + 1
                x_pv := i__putValue (x, x_r) with err
     *)
      let cmds, x, errs = f e in
      let new_cmds, new_errs, _, x_r =
        translate_inc_dec x true tr_ctx.tr_err_lab
      in
      let new_cmds = annotate_cmds new_cmds in
      (annotate_first_cmd (cmds @ new_cmds), PVar x_r, errs @ new_errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Pre_Decr, e) ->
      (*
         Section: 11.4.5
         C(e) = cmds, x
      C(--e) =          cmds
                        goto [ (typeof (x) = $$v-reference_type) and ((field(x) = "eval") or (field(x) = "arguments")) ] err next
                 next:  x_v := getValue (x) with err
                        x_n := i__toNumber (x_v) with err
                        x_r := x_n - 1
                        x_pv := i__putValue (x, x_r) with err
       *)
      let cmds, x, errs = f e in
      let new_cmds, new_errs, _, x_r =
        translate_inc_dec x false tr_ctx.tr_err_lab
      in
      let new_cmds = annotate_cmds new_cmds in
      (annotate_first_cmd (cmds @ new_cmds), PVar x_r, errs @ new_errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Positive, e) ->
      (*
      Section: 11.4.6
      C(e) = cmds, x
      C(+e) =  cmds
               x_v := i__getValue (x) with err
               x_n := i__toNumber (x_v) with err
     *)
      let cmds, x, errs = f e in

      (* x_v := i__getValue (x) with err *)
      let x_v, cmd_gv_x, errs_x_v = make_get_value_call x tr_ctx.tr_err_lab in

      (* x_n := i__toNumber (x_v) with err *)
      let x_n, cmd_tn_x = make_to_number_call x x_v tr_ctx.tr_err_lab in

      let cmds =
        annotate_first_cmd
          (cmds
          @ annotate_cmds
              [
                (*  cmds                                *)
                (None, cmd_gv_x);
                (*  x_v := i__getValue (x) with err     *)
                (None, cmd_tn_x) (*  x_n := i__toNumber (x_v) with err   *);
              ])
      in
      let errs = errs @ errs_x_v @ [ x_n ] in
      (cmds, PVar x_n, errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Negative, e) ->
      (*
      Section: 11.4.7
      C(e) = cmds, x
      C(-e) =        cmds
                     x_v := i__getValue (x) with err
                     x_n := i__toNumber (x_v) with err
                     x_r := (negative x_n)
     *)
      let cmds, x, errs = f e in

      (* x_v := getValue (x) with err *)
      let x_v, cmd_gv_x, errs_x_v = make_get_value_call x tr_ctx.tr_err_lab in

      (* x_n := i__toNumber (x_v) with err *)
      let x_n, cmd_tn_x = make_to_number_call x x_v tr_ctx.tr_err_lab in

      (* x_r := (negative x_n) *)
      let x_r = fresh_var () in
      let cmd_ass_xr =
        LBasic (Assignment (x_r, UnOp (FUnaryMinus, PVar x_n)))
      in

      let cmds =
        annotate_first_cmd
          (cmds
          @ annotate_cmds
              [
                (*            cmds                                *)
                (None, cmd_gv_x);
                (* x_v := i__getValue (x) with err    *)
                (None, cmd_tn_x);
                (* x_n := i__toNumber (x_v) with err  *)
                (None, cmd_ass_xr) (* x_r := (negative x_n)              *);
              ])
      in
      let errs = errs @ errs_x_v @ [ x_n ] in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Bitnot, e) ->
      (*
      Section: 11.4.8
      C(e) = cmds, x
      C(~e) =        cmds
                     x_v := i__getValue (x) with err
                     x_n := i__toNumber (x_v) with err
                     x_i32 := (num_to_int32 x_n)
                     x_r := (! x_i32)
     *)
      let cmds, x, errs = f e in

      (* x_v := i__getValue (x) with err *)
      let x_v, cmd_gv_x, errs_x_v = make_get_value_call x tr_ctx.tr_err_lab in

      (* x_n := i__toNumber (x_v) with err *)
      let x_n, cmd_tn_x = make_to_number_call x x_v tr_ctx.tr_err_lab in

      let x_r = fresh_var () in
      let x_i32 = fresh_var () in
      let cmds =
        annotate_first_cmd
          (cmds
          @ annotate_cmds
              [
                (*  cmds                                *)
                (None, cmd_gv_x);
                (*  x_v := i__getValue (x) with err     *)
                (None, cmd_tn_x);
                (*  x_n := i__toNumber (x_v) with err   *)
                (None, LBasic (Assignment (x_i32, UnOp (ToInt32Op, PVar x_n))));
                (*  x_i32 := (num_to_int32 x_n)         *)
                (None, LBasic (Assignment (x_r, UnOp (BitwiseNot, PVar x_i32))))
                (*  x_r := (! x_i32)                    *);
              ])
      in
      let errs = errs @ errs_x_v @ [ x_n ] in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.Unary_op (JS_Parser.Syntax.Not, e) ->
      (*
      Section: 11.4.9
      C(e)  =  cmds, x
      C(!e) =  cmds
               x_v := i__getValue (x) with err
               x_b := i__toBoolean (x_v) with err
               x_r := not x_b
     *)
      let cmds, x, errs = f e in

      (* x_v := i__getValue (x) with err1 *)
      let x_v, cmd_gv_x, errs_x_v = make_get_value_call x tr_ctx.tr_err_lab in

      (* x_b := i__toBoolean (x_v) with err2 *)
      let x_b, cmd_tb_x = make_to_boolean_call x x_v tr_ctx.tr_err_lab in

      (*  x_r := (not x_b)   *)
      let x_r = fresh_var () in
      let cmd_xr_ass = LBasic (Assignment (x_r, UnOp (UNot, PVar x_b))) in

      let cmds =
        annotate_first_cmd
          (cmds
          @ annotate_cmds
              [
                (* cmds                               *)
                (None, cmd_gv_x);
                (* x_v := i__getValue (x) with err    *)
                (None, cmd_tb_x);
                (* x_b := i__toBoolean (x_v) with err *)
                (None, cmd_xr_ass) (* x_r := (not x_b)                   *);
              ])
      in
      let errs = errs @ errs_x_v @ [ x_b ] in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp (e1, JS_Parser.Syntax.Arith aop, e2)
    when aop = JS_Parser.Syntax.Times
         || aop = JS_Parser.Syntax.Div || aop = JS_Parser.Syntax.Mod
         || aop = JS_Parser.Syntax.Minus ->
      (* Sections 11.5 + 11.6.2
         C(e1) = cmds1, x1; C(e2) = cmds2, x2
         C(e1 * e2) =  cmds1
                       x1_v := i__getValue (x1) with err
                       cmds2
                       x2_v := i__getValue (x2) with err
                       x1_n := i__toNumber (x1_v) with err
                       x2_n := i__toNumber (x2_v) with err
                       x_r := x1_n * x2_n
      *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_multiplicative_binop x1 x2 x1_v x2_v aop tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ annotate_cmds ([ (None, cmd_gv_x2) ] @ new_cmds))
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp (e1, JS_Parser.Syntax.Arith JS_Parser.Syntax.Plus, e2)
    ->
      (*
      Section 11.6.1
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 + e2) =          cmds1
                            x1_v := i__getValue (x1) with err
                            cmds2
                            x2_v := i__getValue (x2) with err
                            x1_p := i__toPrimitive (x1_v) with err
                            x2_p := i__toPrimitive (x2_v) with err
                            goto [((typeOf x1_p) = Str) or ((typeOf x2_p) = Str)] then else
                      then: x1_s := i__toString (x1_p) with err
                            x2_s := i__toString (x2_p) with err
                            x_rthen := x1_s :: x2_s
                            goto end
                      else: x1_n := i__toNumber (x1_p) with err
                            x2_n := i__toNumber (x2_p) with err
                            x_relse := x1_n + x2_n
                      end:  x_r := PHI (x_rthen, x_relse)
    *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_binop_plus x1 x2 x1_v x2_v tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp (e1, JS_Parser.Syntax.Arith JS_Parser.Syntax.Lsh, e2)
    ->
      (*
      Section 11.7.1
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 << e2) =    cmds1
                       x1_v := i__getValue (x1) with err
                       cmds2
                       x2_v := i__getValue (x2) with err
                       x1_i32 := i__toInt32 (x1_v) with err
                       x2_ui32 := i__toUInt32 (x2_v) with err
                       x_r := x1_i32 << x2_ui32
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_bitwise_shift x1 x2 x1_v x2_v toInt32Name toUInt32Name
          LeftShift tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp (e1, JS_Parser.Syntax.Arith JS_Parser.Syntax.Rsh, e2)
    ->
      (*
      Section 11.7.2
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 >> e2) =    cmds1
                       x1_v := i__getValue (x1) with err
                       cmds2
                       x2_v := i__getValue (x2) with err
                       x1_i32 := i__toInt32 (x1_v) with err
                       x2_ui32 := i__toUInt32 (x2_v) with err
                       x_r := x1_i32 >> x2_ui32
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_bitwise_shift x1 x2 x1_v x2_v toInt32Name toUInt32Name
          SignedRightShift tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp (e1, JS_Parser.Syntax.Arith JS_Parser.Syntax.Ursh, e2)
    ->
      (*
      Section 11.7.3
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 >>> e2) =   cmds1
                       x1_v := i__getValue (x1) with err
                       cmds2
                       x2_v := i__getValue (x2) with err
                       x1_ui32 := i__toUInt32 (x1_v) with err
                       x2_ui32 := i__toUInt32 (x2_v) with err
                       x_r := x1_ui32 >>> x2_ui32
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_bitwise_shift x1 x2 x1_v x2_v toUInt32Name toUInt32Name
          UnsignedRightShift tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.Lt, e2) ->
      (*
      Section 11.8.1
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 < e2) =             cmds1
                               x1_v := i__getValue (x1) with err
                               cmds2
                               x2_v := i__getValue (x2) with err
                               x_ac := i__abstractComparison (x1_v, x2_v, true) with err
                               goto [ x_ac = undefined ] then end
                        then:  x_undef := false
                        end:   x_r := PHI(x_ac, x_undef)
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_binop_comparison x1 x2 x1_v x2_v true true false
          tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.Gt, e2) ->
      (*
      Section 11.8.2
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 > e2) =             cmds1
                               x1_v := i__getValue (x1) with err
                               cmds2
                               x2_v := i__getValue (x2) with err
                               x_ac := i__abstractComparison (x2_v, x1_v, false) with err
                               goto [ x_ac = undefined ] then end
                        then:  x_undef := false
                        end:   x_r := PHI(x_ac, x_undef)
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_binop_comparison x1 x2 x1_v x2_v false false false
          tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.Le, e2) ->
      (*
      Section 11.8.3
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 <= e2) =
          cmds1
            x1_v := i__getValue (x1) with err
            cmds2
          x2_v := i__getValue (x2) with err
          x_ac := i__abstractComparison (x2_v, x1_v, false) with err
          goto [ x_ac = undefined] then end
       then:  x_undef := true
       end:   x_r1 := PHI(x_ac, x_undef)
          x_r2 := (not x_r1)
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r1 =
        translate_binop_comparison x1 x2 x1_v x2_v false false true
          tr_ctx.tr_err_lab
      in
      let x_r2 = fresh_var () in
      let new_cmd = LBasic (Assignment (x_r2, UnOp (UNot, PVar x_r1))) in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds
          @ [ annotate_cmd new_cmd None ])
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r2, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.Ge, e2) ->
      (*
      Section 11.8.4
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 >= e2) =
        cmds1
        x1_v := i__getValue (x1) with err
        cmds2
        x2_v := i__getValue (x2) with err
        x_ac := i__abstractComparison (x1_v, x2_v, true) with err
          goto [ x_ac = undefined] then end
     then:  x_undef := true
     end:   x_r1 := PHI(x_ac, x_undef)
        x_r2 := (not x_r1)
      *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r1 =
        translate_binop_comparison x1 x2 x1_v x2_v true true true
          tr_ctx.tr_err_lab
      in
      let x_r2 = fresh_var () in
      let new_cmd = LBasic (Assignment (x_r2, UnOp (UNot, PVar x_r1))) in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds
          @ [ annotate_cmd new_cmd None ])
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r2, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.InstanceOf, e2) ->
      (*
      Section 11.8.6
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 instanceof e2) =
        cmds1
          x1_v := i__getValue (x1) with err
        cmds2
        x2_v := i__getValue (x2) with err
        goto [ (typeOf x2_v) = Obj ] next1 err
    next1:  x_cond := [x2_v, "@hasInstance"];
        goto [ x_cond = empty ] err next2
    next2:  x_hi := [x2_v, "@hasInstance"]
        x_r := x_hi (x2_v, x1_v) with err
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in

      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      (* goto [ (typeOf x2_v) = Obj ] next1 err *)
      let next1 = fresh_label () in
      let cmd_goto_ot =
        LGuardedGoto
          ( BinOp (UnOp (TypeOf, PVar x2_v), Equal, Lit (Type ObjectType)),
            next1,
            tr_ctx.tr_err_lab )
      in

      (* get the metadata *)
      let x2vm = fresh_var () in
      let cmd_x2vm = LBasic (MetaData (x2vm, PVar x2_v)) in

      (* next1: x_cond := hasField (x2_v, "@hasInstance")  *)
      let x_cond = fresh_var () in
      let cmd_hasfield =
        LBasic (Lookup (x_cond, PVar x2vm, Lit (String "@class")))
      in

      (* goto [ x_cond = empty ] err next2 *)
      let next2 = fresh_label () in
      let cmd_goto_xcond =
        LGuardedGoto
          ( BinOp (PVar x_cond, Equal, Lit (String "Function")),
            next2,
            tr_ctx.tr_err_lab )
      in

      (* x_r := x_hi (x2_v, x1_v) with err *)
      let x_r = fresh_var () in
      let cmd_ass_xr =
        LCall
          ( x_r,
            Lit (String "hasInstance"),
            [ PVar x2_v; PVar x1_v ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      let cmds =
        annotate_first_cmd
          (cmds1
          @ [
              (*        cmds1                                     *)
              annotate_cmd cmd_gv_x1 None
              (*        x1_v := i__getValue (x1) with err         *);
            ]
          @ cmds2
          @ annotate_cmds
              [
                (*        cmds2                                     *)
                (None, cmd_gv_x2);
                (*        x2_v := i__getValue (x2) with err         *)
                (None, cmd_goto_ot);
                (*        goto [ (typeOf x2_v) = Obj ] next1 err    *)
                (Some next1, cmd_x2vm);
                (* next1: x2vm := metadata(x2_v)                    *)
                (None, cmd_hasfield);
                (*        x_cond := hasField (x2_v, "@hasInstance") *)
                (None, cmd_goto_xcond);
                (*        goto [ x_cond = empty ] err next2         *)
                (Some next2, cmd_ass_xr)
                (*        x_r := x_hi (x2_v, x1_v) with err         *);
              ])
      in
      let errs =
        errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ [ var_te; var_te; x_r ]
      in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.In, e2) ->
      (*
      Section 11.8.7
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 in e2) =
          cmds1
            x1_v := i__getValue (x1) with err
          cmds2
          x2_v := i__getValue (x2) with err
          goto [ (typeOf x2_v) = Obj ] next1 err
      next1:  x1_s := i__toString (x1_v) with err
          x_r := o__hasProperty (x2_v, x1_s) with err
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in

      (* x2_v := getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      (* goto [ (typeOf x2_v) = Obj ] next1 err *)
      let next1 = fresh_label () in
      let cmd_goto_ot =
        LGuardedGoto
          ( BinOp (UnOp (TypeOf, PVar x2_v), Equal, Lit (Type ObjectType)),
            next1,
            tr_ctx.tr_err_lab )
      in

      (* next1: x1_s := i__toString (x1_v) with err   *)
      let x1_s, cmd_ts_x1 = make_to_string_call x1 x1_v tr_ctx.tr_err_lab in

      (*  x_r := o__hasProperty (x2_v, x1_s) with err   *)
      let x_r = fresh_var () in
      let cmd_ass_xr =
        LCall
          ( x_r,
            Lit (String hasPropertyName),
            [ PVar x2_v; PVar x1_s ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      let cmds =
        annotate_first_cmd
          (cmds1
          @ [
              (*         cmds1                                             *)
              annotate_cmd cmd_gv_x1 None
              (*         x1_v := getValue (x1) with err                    *);
            ]
          @ cmds2
          @ annotate_cmds
              [
                (*         cmds2                                             *)
                (None, cmd_gv_x2);
                (*         x2_v := getValue (x2) with err                    *)
                (None, cmd_goto_ot);
                (*         goto [ (typeOf x2_v) = Obj ] next1 err  *)
                (Some next1, cmd_ts_x1);
                (* next1:  x1_s := i__toString (x1_v) with err               *)
                (None, cmd_ass_xr)
                (*         x_r := o__hasProperty (x2_v, x1_s) with err       *);
              ])
      in
      let errs =
        errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ [ var_te; x1_s; x_r ]
      in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.Equal, e2) ->
      (*
      Section 11.9.1
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 == e2) =
        cmds1
        x1_v := i__getValue (x1) with err
        cmds2
        x2_v := i__getValue (x2) with err
        x_r := i__abstractEqualityComparison (x1_v, x2_v) with err
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_binop_equality x1 x2 x1_v x2_v true true tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.NotEqual, e2) ->
      (*
      Section 11.9.2
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 != e2) =
        cmds1
          x1_v := i__getValue (x1) with err
        cmds2
        x2_v := i__getValue (x2) with err
        x_r1 := i__abstractEqualityComparison (x1_v, x2_v) with err
        x_r2 := (not x_r1)
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_binop_equality x1 x2 x1_v x2_v true false tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.TripleEqual, e2) ->
      (*
      Section 11.9.4
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 === e2) =
        cmds1
        x1_v := i__getValue (x1) with err
        cmds2
        x2_v := i__getValue (x2) with err
        x_r := i__strictEqualityComparison (x1_v, x2_v) with err
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_binop_equality x1 x2 x1_v x2_v false true tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp
      (e1, JS_Parser.Syntax.Comparison JS_Parser.Syntax.NotTripleEqual, e2) ->
      (*
      Section 11.9.5
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 !== e2) =
          cmds1
        x1_v := i__getValue (x1) with err
        cmds2
        x2_v := i__getValue (x2) with err
        x_r1 := i__strictEqualityComparison (x1_v, x2_v) with err
        x_r2 := (not x_r1)
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_binop_equality x1 x2 x1_v x2_v false false tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp (e1, JS_Parser.Syntax.Arith bbop, e2)
    when bbop = JS_Parser.Syntax.Bitand
         || bbop = JS_Parser.Syntax.Bitor
         || bbop = JS_Parser.Syntax.Bitxor ->
      (*
      Section 11.10
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 == e2) =
        cmds1
        x1_v := i__getValue (x1) with err
        cmds2
        x2_v := i__getValue (x2) with err
        x1_i32 := i__toInt32 (x1_v) with err
        x2_i32 := i__toInt32 (x2_v) with err
        x_r := (x1_i32 bbop x2_i32)
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        translate_bitwise_bin_op x1 x2 x1_v x2_v bbop tr_ctx.tr_err_lab
      in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ [ annotate_cmd cmd_gv_x1 None ]
          @ cmds2
          @ [ annotate_cmd cmd_gv_x2 None ]
          @ annotate_cmds new_cmds)
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.BinOp (e1, JS_Parser.Syntax.Boolean lbop, e2) ->
      (*
      Section 11.11
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 && e2) =
          cmds1
          x1_v := i__getValue (x1) with err1
          x1_b := i__toBoolean (x1_v) with err2
            goto [x1_b] next end
        next: cmds2
          x2_v := i__getValue (x2) with err3
      end:  x_r := PHI(x1_v, x2_v)
     *)
      translate_bin_logical_operator e1 e2 lbop tr_ctx.tr_err_lab
  | JS_Parser.Syntax.ConditionalOp (e1, e2, e3) ->
      (*
      Section 11.12
      C(e1) = cmds1, x1; C(e2) = cmds2, x2; C(e3) = cmds3, x3
      C(e1 ? e2 : e3) =
          cmds1
          x1_v := i__getValue (x1) with err
          x1_b := i__toBoolean (x1_v) with err
            goto [x1_b] then else
       then:  cmds2
          x2_v := i__getValue (x2) with err
          goto end_if
       else:  cmds3
          x3_v := i__getValue (x3) with err
          end_if:  x_r := PHI(x2_v, x3_v)
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in
      let cmds3, x3, errs3 = f e3 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x1_b := i__toBoolean (x1_v) with err  *)
      let x1_b, cmd_tb_x1 = make_to_boolean_call x1 x1_v tr_ctx.tr_err_lab in
      (* goto [x1_b] then else *)
      let then_lab = fresh_then_label () in
      let else_lab = fresh_else_label () in
      let end_if_lab = fresh_endif_label () in
      let cmd_goto = LGuardedGoto (PVar x1_b, then_lab, else_lab) in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in
      (* x3_v := i__getValue (x3) with err *)
      let x3_v, cmd_gv_x3, errs_x3_v =
        make_get_value_call x3 tr_ctx.tr_err_lab
      in
      (* x_r := PHI(x2_v, x3_v) *)
      let x_r = fresh_var () in
      let cmd_ass_xr = LPhiAssignment [ (x_r, [ PVar x2_v; PVar x3_v ]) ] in

      let cmds2 = add_initial_label cmds2 then_lab metadata in
      let cmds3 = add_initial_label cmds3 else_lab metadata in
      let cmds =
        annotate_first_cmd
          (cmds1
          @ annotate_cmds
              [
                (*         cmds1                                              *)
                (None, cmd_gv_x1);
                (*         x1_v := i__getValue (x1) with err                  *)
                (None, cmd_tb_x1);
                (*         x1_b := i__toBoolean (x1_v) with err               *)
                (None, cmd_goto)
                (*         goto [x1_b] then else                              *);
              ]
          @ cmds2
          @ annotate_cmds
              [
                (* then:   cmds2                                              *)
                (None, cmd_gv_x2);
                (*         x2_v := i__getValue (x2) with err                  *)
                (None, LGoto end_if_lab)
                (*         goto end_if                                        *);
              ]
          @ cmds3
          @ annotate_cmds
              [
                (* else:   cmds3                                              *)
                (None, cmd_gv_x3);
                (*         x3_v := i__getValue (x3) with err                  *)
                (Some end_if_lab, cmd_ass_xr)
                (* end_if: x_r := PHI(x2_v, x3_v)                             *);
              ])
      in

      let errs =
        errs1 @ errs_x1_v @ [ x1_b ] @ errs2 @ errs_x2_v @ errs3 @ errs_x3_v
      in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.Assign (e1, e2) ->
      (*
      Section 11.13.1 - Simple Assignment
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1 = e2) =      cmds1
                        cmds2
                        x2_v := i__getValue (x2) with err
                        x_cae := i__checkAssignmentErrors (x1) with err
                        x_pv = i__putValue (x1, x2_v) with err
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      (*  x_cae := i__checkAssignmentErrors (x1) with err *)
      let x_cae, cmd_cae_x1 = make_cae_call x1 tr_ctx.tr_err_lab in

      (* x_pv = i__putValue (x1, x2_v) with err *)
      let x_pv, cmd_put_value = make_put_value_call x1 x2_v tr_ctx.tr_err_lab in

      let cmds =
        annotate_first_cmd
          (cmds1 (*   cmds1                                           *) @ cmds2
          @ annotate_cmds
              [
                (*   cmds2                                           *)
                (None, cmd_gv_x2);
                (*   x2_v := i__getValue (x2) with err               *)
                (None, cmd_cae_x1);
                (*   x_cae := i__checkAssertionErrors (x1) with err  *)
                (None, cmd_put_value)
                (*   x_pv := i__putValue (x1, x2_v) with err         *);
              ])
      in

      let cmds = cmds in
      let errs = errs1 @ errs2 @ errs_x2_v @ [ x_cae; x_pv ] in
      (cmds, PVar x2_v, errs)
  | JS_Parser.Syntax.AssignOp (e1, op, e2) ->
      (*
      Section 11.13.1 - Compound Assignment
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C_op(x1_v, x2_v) = cmds, x
      C(e1 op= e2) =    cmds1
                        x1_v := i__getValue (x1) with err
                        cmds2
                        x2_v := i__getValue (x2) with err
                        cmds
                        x_cae := i__checkAssignmentErrors (x1) with err
                next:   x_pv = putValue (x1, x) with err
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in
      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let new_cmds, new_errs, x_r =
        match op with
        | JS_Parser.Syntax.Plus ->
            translate_binop_plus x1 x2 x1_v x2_v tr_ctx.tr_err_lab
        | JS_Parser.Syntax.Minus
        | JS_Parser.Syntax.Times
        | JS_Parser.Syntax.Div
        | JS_Parser.Syntax.Mod ->
            translate_multiplicative_binop x1 x2 x1_v x2_v op tr_ctx.tr_err_lab
        | JS_Parser.Syntax.Ursh ->
            translate_bitwise_shift x1 x2 x1_v x2_v toUInt32Name toUInt32Name
              SignedRightShift tr_ctx.tr_err_lab
        | JS_Parser.Syntax.Lsh ->
            translate_bitwise_shift x1 x2 x1_v x2_v toInt32Name toUInt32Name
              LeftShift tr_ctx.tr_err_lab
        | JS_Parser.Syntax.Rsh ->
            translate_bitwise_shift x1 x2 x1_v x2_v toInt32Name toUInt32Name
              UnsignedRightShift tr_ctx.tr_err_lab
        | JS_Parser.Syntax.Bitand
        | JS_Parser.Syntax.Bitor
        | JS_Parser.Syntax.Bitxor ->
            translate_bitwise_bin_op x1 x2 x1_v x2_v op tr_ctx.tr_err_lab
      in

      (* x_cae := i__checkAssertionErrors (x1) with err *)
      let x_cae, cmd_cae_x1 = make_cae_call x1 tr_ctx.tr_err_lab in

      (* x_pv = i__putValue (x1, x_r) with err *)
      let x_pv, cmd_pv = make_put_value_call x1 x_r tr_ctx.tr_err_lab in

      let cmds =
        annotate_first_cmd
          (cmds1
          @ [
              (*    cmds1                                           *)
              annotate_cmd cmd_gv_x1 None
              (*    x1_v := i__getValue (x1) with err               *);
            ]
          @ cmds2
          @ [
              (*    cmds2                                           *)
              annotate_cmd cmd_gv_x2 None
              (*    x2_v := i__getValue (x2) with err               *);
            ]
          @ annotate_cmds
              (new_cmds
              @ [
                  (*    new_cmds                                        *)
                  (None, cmd_cae_x1);
                  (*    x_cae := i__checkAssertionErrors (x1) with err  *)
                  (None, cmd_pv)
                  (*    x_pv = putValue (x1, x2_v) with err             *);
                ]))
      in
      let errs =
        errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ new_errs @ [ x_cae; x_pv ]
      in
      (cmds, PVar x_r, errs)
  | JS_Parser.Syntax.Comma (e1, e2) ->
      (*
      Section 11.14 - Comma Operator
      C(e1) = cmds1, x1; C(e2) = cmds2, x2
      C(e1, e2) =    cmds1
                     x1_v := i__getValue (x1) with err
                     cmds2
                     x2_v := i__getValue (x2) with err
     *)
      let cmds1, x1, errs1 = f e1 in
      let cmds2, x2, errs2 = f e2 in

      (* x1_v := getValue (x1) with err *)
      let _, cmd_gv_x1, errs_x1_v = make_get_value_call x1 tr_ctx.tr_err_lab in

      (* x2_v := getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      let cmds =
        annotate_first_cmd
          (cmds1
          @ [
              (*       cmds1                                *)
              annotate_cmd cmd_gv_x1 None
              (*       x1_v := i__getValue (x1) with err    *);
            ]
          @ cmds2
          @ [
              (*       cmds2                                *)
              annotate_cmd cmd_gv_x2 None
              (*       x2_v := i__getValue (x2) with err    *);
            ])
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v in
      (cmds, PVar x2_v, errs)
  | JS_Parser.Syntax.FunctionExp (strictness, None, params, _) ->
      (*
       Section 13
       x_f := create_function_object(x_sc, f_id, f_id, params)
    *)
      let tr_ctx =
        JS2JSIL_Helpers.update_tr_ctx ?strictness:(Some strictness) tr_ctx
      in

      let f_id =
        try JS2JSIL_Preprocessing.get_codename e
        with _ ->
          raise
            (Failure
               "anonymous function literals should be annotated with their \
                respective code names")
      in
      let x_f, cmd =
        make_create_function_object_call tr_ctx.tr_sc_var f_id params
      in
      let cmds = annotate_first_cmd [ annotate_cmd cmd None ] in
      (cmds, PVar x_f, [])
  | JS_Parser.Syntax.FunctionExp (strictness, Some f_name, params, _)
  | JS_Parser.Syntax.Function (strictness, Some f_name, params, _) ->
      let tr_ctx =
        JS2JSIL_Helpers.update_tr_ctx ?strictness:(Some strictness) tr_ctx
      in
      let f_id =
        try JS2JSIL_Preprocessing.get_codename e
        with _ ->
          raise
            (Failure
               "named function literals should be annotated with their \
                respective code names")
      in

      (* x_f_outer_er := new ();  *)
      let x_f_outer_er_meta = fresh_var () in
      let x_f_outer_er = fresh_er_var () in

      let cmd_ass_xfouter_meta =
        LBasic (New (x_f_outer_er_meta, None, Some (Lit Null)))
      in
      let cmd_ass_xfouter =
        LBasic (New (x_f_outer_er, None, Some (PVar x_f_outer_er_meta)))
      in

      (* [x_er, "@er"] := true *)
      let cmd_er_flag =
        LBasic
          (Mutation
             ( PVar x_f_outer_er_meta,
               Lit (String _erFlagPropName),
               Lit (Bool true) ))
      in

      (* x_sc_f := x_sc @ {{ x_f_outer_er }}  *)
      let x_sc_f = fresh_scope_chain_var () in
      let cmd_xscf_ass =
        LBasic
          (Assignment
             ( x_sc_f,
               NOp
                 (LstCat, [ PVar tr_ctx.tr_sc_var; EList [ PVar x_f_outer_er ] ])
             ))
      in

      (* x_f := create_function_object(x_sc_f, f_id, params) *)
      let x_f, cmd_fun_constr =
        make_create_function_object_call x_sc_f f_id params
      in

      (* [x_f_outer_er, f] := x_f *)
      let cmd_fname_updt =
        LBasic (Mutation (PVar x_f_outer_er, Lit (String f_name), PVar x_f))
      in

      (* x_cae := i__checkAssignmentErrors (ref-v(x_f_outer_er, "f")) with err *)
      let x_cae = fresh_var () in
      let cmd_cae =
        LCall
          ( x_cae,
            lit_str checkAssignmentErrorsName,
            [
              EList
                [
                  lit_refv;
                  PVar x_f_outer_er;
                  lit_str f_name;
                  Lit (Bool tr_ctx.tr_strictness);
                ];
            ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      let cmds =
        [
          (None, cmd_ass_xfouter_meta);
          (None, cmd_ass_xfouter);
          (*  x_f_outer_er := new ()                                                *)
          (None, cmd_er_flag);
          (None, cmd_xscf_ass);
          (*  x_sc_f := x_f_outer_er :: x_sc                                        *)
          (None, cmd_fun_constr);
          (*  x_f := create_function_object(x_sc_f, f_id, params)                   *)
          (None, cmd_fname_updt);
          (*  [x_f_outer_er, f] := x_f                                              *)
          (None, cmd_cae)
          (*  x_cae := i__checkAssignmentErrors (ref-v(x_f_outer_er, "f")) with err *);
        ]
      in

      let cmds = annotate_first_cmd (annotate_cmds cmds) in
      (cmds, PVar x_f, [ (* x_t; *) x_cae ])
  | JS_Parser.Syntax.VarDec decs ->
      let rec loop decs cmds errs =
        match decs with
        | [] ->
            raise
              (Failure
                 "no empty variable declaration lists in expression contexts")
        | [ (v, eo) ] -> (
            match eo with
            | None ->
                let x, new_cmds, new_errs = compile_var_dec_without_exp v in
                (x, cmds @ new_cmds, errs @ new_errs)
            | Some e ->
                let new_cmds, x, new_errs = compile_var_dec v e in
                (x, cmds @ new_cmds, errs @ new_errs))
        | (v, eo) :: rest_decs -> (
            match eo with
            | None -> loop rest_decs cmds errs
            | Some e ->
                let new_cmds, _, new_errs = compile_var_dec v e in
                loop rest_decs (cmds @ new_cmds) (errs @ new_errs))
      in
      let x, cmds, errs = loop decs [] [] in
      (annotate_first_cmd cmds, PVar x, errs)
  | JS_Parser.Syntax.RegExp (_, _) ->
      raise (Failure "Not implemented: RegExp literal")
  | x ->
      raise
        (Failure
           (Printf.sprintf "Unhandled expression %s at %s"
              (JS_Parser.PrettyPrint.string_of_exp_syntax x)
              __LOC__))

and translate_statement tr_ctx e =
  let fe = translate_expr tr_ctx in
  let _f = translate_statement tr_ctx in

  let f_previous loop_list previous lab e =
    let new_tr_ctx = update_tr_ctx ~loop_list ~previous ~lab tr_ctx in
    translate_statement new_tr_ctx e
  in

  let find_var_er_index v =
    match tr_ctx.tr_use_cc with
    | false -> None
    | true ->
        let cur_var_tbl = get_scope_table cc_tbl tr_ctx.tr_er_fid in
        let fid_v = Hashtbl.find_opt cur_var_tbl v in
        Option.map
          (fun fid_v ->
            let fid_v_index =
              JS2JSIL_Preprocessing.get_vis_list_index tr_ctx.tr_vis_list fid_v
            in
            fid_v_index)
          fid_v
  in

  (* All the other commands must get the offsets and nothing else *)
  let origin_loc = JS_Utils.lift_flow_loc e.JS_Parser.Syntax.exp_loc in
  let metadata : Annot.t =
    Annot.make ~origin_loc ~loop_info:tr_ctx.tr_loops ()
  in
  let annotate_cmds = annotate_cmds_top_level metadata in
  let annotate_cmd cmd lab = annotate_cmd_top_level metadata (lab, cmd) in

  (* The first command must get the logic commands and the invariants *)
  let lcmds, _, _ =
    JS2JSIL_Preprocessing.translate_lannots_in_exp cc_tbl vis_tbl old_fun_tbl
      tr_ctx.tr_fid tr_ctx.tr_sc_var true e
  in
  let invariant =
    JS2JSIL_Preprocessing.translate_invariant_in_exp cc_tbl vis_tbl old_fun_tbl
      tr_ctx.tr_fid tr_ctx.tr_sc_var e
  in
  let annotate_first_cmd = prefix_lcmds lcmds invariant in

  let compile_var_dec x e =
    let index = find_var_er_index x in
    let cmds_e, x_e, errs_e = fe e in

    (* x_v := i__getValue (x) with err *)
    let x_v, cmd_gv_x, errs_x_v = make_get_value_call x_e tr_ctx.tr_err_lab in

    (* x_sf := l-nth(x__sc, index)  *)
    let x_sf = fresh_var () in
    let cmd_xsf_ass =
      match index with
      | Some index ->
          LBasic
            (Assignment
               ( x_sf,
                 BinOp
                   (PVar tr_ctx.tr_sc_var, LstNth, lit_num (float_of_int index))
               ))
      | None ->
          LCall
            ( x_sf,
              lit_str dynamicScoper,
              [ PVar tr_ctx.tr_sc_var; lit_str x ],
              Some tr_ctx.tr_err_lab,
              None )
    in

    (* x_ref := {{ "v", x_sf, "x" }}  *)
    let x_ref = fresh_var () in
    let cmd_xref_ass =
      LBasic
        (Assignment
           ( x_ref,
             EList
               [
                 lit_refv; PVar x_sf; lit_str x; Lit (Bool tr_ctx.tr_strictness);
               ] ))
    in

    (* x_cae := i__checkAssignmentErrors (x_ref) with err *)
    let x_cae = fresh_var () in
    let cmd_cae =
      LCall
        ( x_cae,
          Lit (String checkAssignmentErrorsName),
          [ PVar x_ref ],
          Some tr_ctx.tr_err_lab,
          None )
    in

    (* x_pv := i__putValue(x_ref, x_v) with err2 *)
    let x_pv, cmd_pv = make_put_value_call (PVar x_ref) x_v tr_ctx.tr_err_lab in
    let cmds =
      cmds_e
      @ annotate_cmds
          [
            (None, cmd_gv_x);
            (* x_v := i__getValue (x) with err                      *)
            (None, cmd_xsf_ass);
            (* x_sf := l-nth(x_sc, index)                           *)
            (None, cmd_xref_ass);
            (* x_ref := {{ "v", x_sf, "x" }}                        *)
            (None, cmd_cae);
            (* x_cae := i__checkAssignmentErrors (x_ref) with err   *)
            (None, cmd_pv)
            (* x_pv := i__putValue(x_ref, x_v) with err             *);
          ]
    in
    let errs = errs_e @ errs_x_v @ [ x_cae; x_pv ] in
    (cmds, x_ref, errs)
  in

  let create_final_phi_cmd cmds x errs rets breaks conts break_label js_lab =
    let cur_breaks, outer_breaks = filter_cur_jumps breaks js_lab false in
    match cur_breaks with
    | [] -> (cmds, x, errs, rets, breaks, conts)
    | _ ->
        let x_name, cmd_new_x =
          match x with
          | PVar x_name -> (x_name, [])
          | Lit lit ->
              let x_name = fresh_var () in
              let cmd_new_x =
                annotate_cmd (LBasic (Assignment (x_name, Lit lit))) None
              in
              (x_name, [ cmd_new_x ])
          | _ ->
              raise
                (Failure
                   "translate. Block: the result of the compilation must be a \
                    variable or a literal")
        in
        let x_ret = fresh_var () in
        let phi_args = cur_breaks @ [ x_name ] in
        let phi_args = List.map (fun x -> PVar x) phi_args in
        let cmd_ass_phi =
          annotate_cmd (LPhiAssignment [ (x_ret, phi_args) ]) break_label
        in
        ( cmds @ cmd_new_x @ [ cmd_ass_phi ],
          PVar x_ret,
          errs,
          rets,
          outer_breaks,
          conts )
  in

  (*
   *  When breaking or continuing inside a try-catch-finally block, the code of the finally
   *  always gets executed before breaking or continuing. Hence, the cont_break_list needs to be
   *  rewritten so that continues and breaks are redirected to the code of the finally.
   *  this procedure returns the new cont_break_list together with a mapping that associates
   *  the newly created labels with their original values.
   *)
  let rename_cont_break_list cont_break_list finally_lab_gen =
    let jumps_mapping = Hashtbl.create 1 in
    (* Printf.printf "I am creating a jumps mapping for a try catch finally\n"; *)
    let rec rename_cont_break_list_iter
        cont_break_list
        (new_cont_break_list :
          (string option * string * string option * bool) list) =
      match cont_break_list with
      | [] -> List.rev new_cont_break_list
      | (None, break_lab, js_lab, is_valid_unlabelled) :: rest ->
          let new_finally_lab = finally_lab_gen () in
          (* Printf.printf "Creating a mapping from %s to %s\n" break_lab new_finally_lab; *)
          Hashtbl.add jumps_mapping new_finally_lab break_lab;
          rename_cont_break_list_iter rest
            ((None, new_finally_lab, js_lab, is_valid_unlabelled)
            :: new_cont_break_list)
      | (Some cont_lab, break_lab, js_lab, is_valid_unlabelled) :: rest ->
          let new_finally_lab1 = finally_lab_gen () in
          let new_finally_lab2 = finally_lab_gen () in
          Hashtbl.add jumps_mapping new_finally_lab1 cont_lab;
          Hashtbl.add jumps_mapping new_finally_lab2 break_lab;
          (* Printf.printf "Creating a mapping from %s to %s and %s to %s\n" break_lab new_finally_lab1 cont_lab new_finally_lab2; *)
          rename_cont_break_list_iter rest
            (( Some new_finally_lab1,
               new_finally_lab2,
               js_lab,
               is_valid_unlabelled )
            :: new_cont_break_list)
    in
    let new_cont_break_list = rename_cont_break_list_iter cont_break_list [] in
    (new_cont_break_list, jumps_mapping)
  in

  (*
   *  We create a finally block for each break/continue that occurs inside
   *  the try and catch blocks of the try-catch-finally.
   *)
  let make_finally_break_blocks jump_list jumps_mapping e tcf_lab _ =
    let rec make_finally_blocks_iter
        jump_list
        finally_blocks
        cur_break_vars
        errs
        rets
        outer_breaks
        inner_breaks
        conts =
      match jump_list with
      | [] ->
          ( finally_blocks,
            cur_break_vars,
            errs,
            rets,
            outer_breaks,
            inner_breaks,
            conts )
      | (js_lab, var, jump) :: rest -> (
          try
            let original_jump = Hashtbl.find jumps_mapping jump in
            (* TODO: figure out if the next line should be uncommented and used *)
            (* let new_loop_list = (None, end_label, tcf_lab, false) :: tr_ctx.tr_loop_list in *)
            let new_tr_ctx =
              update_tr_ctx ~loop_list:tr_ctx.tr_loop_list ~lab:None
                ~previous:None tr_ctx
            in
            let cmds_cur, _, errs_cur, rets_cur, breaks_cur, conts_cur =
              translate_statement new_tr_ctx e
            in
            let cmds_cur = add_initial_label cmds_cur jump metadata in
            let new_finally_block =
              cmds_cur @ [ annotate_cmd (LGoto original_jump) None ]
            in
            let cur_inner_breaks, cur_outer_breaks =
              filter_cur_jumps breaks_cur tcf_lab false
            in
            make_finally_blocks_iter rest
              (finally_blocks @ new_finally_block)
              cur_break_vars (errs @ errs_cur) (rets @ rets_cur)
              (outer_breaks @ cur_outer_breaks
              @ [ (js_lab, var, original_jump) ])
              (inner_breaks @ cur_inner_breaks)
              (conts @ conts_cur)
          with _ ->
            if (not (tcf_lab = None)) && tcf_lab = js_lab then
              make_finally_blocks_iter rest finally_blocks
                (cur_break_vars @ [ var ])
                errs rets outer_breaks inner_breaks conts
            else
              raise
                (Failure
                   (Printf.sprintf
                      "make_finally_break_blocks: unknown jump %s\n" jump)))
    in
    make_finally_blocks_iter jump_list [] [] [] [] [] [] []
  in

  let make_finally_cont_blocks jump_list jumps_mapping e tcf_lab end_label =
    let rec make_finally_blocks_iter
        jump_list
        finally_blocks
        errs
        rets
        outer_breaks
        inner_breaks
        conts =
      match jump_list with
      | [] -> (finally_blocks, errs, rets, outer_breaks, inner_breaks, conts)
      | (js_lab, var, jump) :: rest -> (
          try
            (* Printf.printf ("I am processing a continue!!! \n"); *)
            let original_jump = Hashtbl.find jumps_mapping jump in
            let new_loop_list =
              (None, end_label, tcf_lab, false) :: tr_ctx.tr_loop_list
            in
            let new_ctx =
              update_tr_ctx ~loop_list:new_loop_list ~lab:None ~previous:None
                tr_ctx
            in
            let cmds_cur, _, errs_cur, rets_cur, breaks_cur, conts_cur =
              translate_statement new_ctx e
            in
            let cmds_cur = add_initial_label cmds_cur jump metadata in
            let new_finally_block =
              cmds_cur @ [ annotate_cmd (LGoto original_jump) None ]
            in
            let cur_inner_breaks, cur_outer_breaks =
              filter_cur_jumps breaks_cur tcf_lab false
            in
            make_finally_blocks_iter rest
              (finally_blocks @ new_finally_block)
              (errs @ errs_cur) (rets @ rets_cur)
              (outer_breaks @ cur_outer_breaks)
              (inner_breaks @ cur_inner_breaks)
              (conts @ conts_cur @ [ (js_lab, var, original_jump) ])
          with _ ->
            raise
              (Failure
                 (Printf.sprintf "make_finally_cont_blocks: unknown jump %s\n"
                    jump)))
    in
    make_finally_blocks_iter jump_list [] [] [] [] [] []
  in

  let make_try_catch_cmds e1 (x, e2) catch_id =
    (*
                  cmds1
                  goto finally
        err1:     x_err := PHI(errs1)
                  x_er := new ()
                  x_cae := i__checkAssignmentErrors (ref-v(x_er, "x")) with err2
                  [x_er, "x"] := x_err
                  x_sc_new := x_sc @ {{ x_er }}
                  cmds2
                  goto finally
        err2:     x_ret_1 := PHI(x_cae, errs2)
                  goto err
        finally:  x_ret_2 := PHI(breaks1, x_1, breaks2, x_2)
    *)
    let new_err1, new_err2, finally, end_label, _, _ = fresh_tcf_vars () in
    let new_loop_list =
      (None, finally, tr_ctx.tr_js_lab, false) :: tr_ctx.tr_loop_list
    in
    let new_ctx_1 =
      update_tr_ctx ~loop_list:new_loop_list ~previous:None ~lab:None
        ~err:new_err1 tr_ctx
    in
    let cmds1, x1, errs1, rets1, breaks1, conts1 =
      translate_statement new_ctx_1 e1
    in
    let cmds1, x1_v = add_final_var cmds1 x1 metadata in

    let x_sc_new = fresh_scope_chain_var () in
    let new_ctx_2 =
      update_tr_ctx ~loop_list:new_loop_list ~previous:None ~lab:None
        ~err:new_err2
        ~vis_list:(tr_ctx.tr_vis_list @ [ catch_id ])
        ~er_fid:catch_id ~sc_var:x_sc_new tr_ctx
    in
    let cmds2, x2, errs2, rets2, breaks2, conts2 =
      translate_statement new_ctx_2 e2
    in
    let cmds2, x2_v = add_final_var cmds2 x2 metadata in

    let cur_breaks1, outer_breaks1 =
      filter_cur_jumps breaks1 tr_ctx.tr_js_lab false
    in
    let cur_breaks2, outer_breaks2 =
      filter_cur_jumps breaks2 tr_ctx.tr_js_lab false
    in

    (* x_err := PHI(errs1) *)
    let x_err = fresh_err_var () in
    let phi_args1 = List.map (fun x -> PVar x) errs1 in
    let cmd_ass_xerr = LPhiAssignment [ (x_err, phi_args1) ] in

    (* x_f_outer_er := new ();  *)
    let x_er_meta = fresh_var () in
    let x_er = fresh_er_var () in

    let cmd_ass_xer_meta = LBasic (New (x_er_meta, None, Some (Lit Null))) in
    let cmd_ass_xer = LBasic (New (x_er, None, Some (PVar x_er_meta))) in

    (* [x_er, "@er"] := true *)
    let cmd_er_flag =
      LBasic
        (Mutation (PVar x_er_meta, Lit (String _erFlagPropName), Lit (Bool true)))
    in

    (* x_cae := i__checkAssignmentErrors (ref-v(x_er, "x")) with err2 *)
    let x_cae = fresh_var () in
    let cmd_cae =
      LCall
        ( x_cae,
          Lit (String checkAssignmentErrorsName),
          [
            EList
              [
                lit_refv; PVar x_er; lit_str x; Lit (Bool tr_ctx.tr_strictness);
              ];
          ],
          Some new_err2,
          None )
    in

    (* [x_er, "x"] := x_err *)
    let cmd_mutate_x =
      LBasic (Mutation (PVar x_er, Lit (String x), PVar x_err))
    in

    (* x_sc_new := x_sc @ {{ x_er }}  *)
    let cmd_sc_updt =
      LBasic
        (Assignment
           ( x_sc_new,
             NOp (LstCat, [ PVar tr_ctx.tr_sc_var; EList [ PVar x_er ] ]) ))
    in

    (* err2:     x_ret_1 := PHI(x_cae, errs2) *)
    let x_ret_1 = fresh_var () in
    let phi_args2 = List.map (fun x -> PVar x) (x_cae :: errs2) in
    let cmd_ass_xret1 = LPhiAssignment [ (x_ret_1, phi_args2) ] in

    (* x_ret_2 := PHI(cur_breaks1, x_1, cur_breaks2, x_2) *)
    let x_ret_2 = fresh_var () in
    let phi_args3 = cur_breaks1 @ [ x1_v ] @ cur_breaks2 @ [ x2_v ] in
    let phi_args3 = List.map (fun x -> PVar x) phi_args3 in
    let cmd_ass_xret2 = LPhiAssignment [ (x_ret_2, phi_args3) ] in

    let cmds =
      cmds1
      @ annotate_cmds
          [
            (None, LGoto finally);
            (Some new_err1, cmd_ass_xerr);
            (None, cmd_ass_xer_meta);
            (None, cmd_ass_xer);
            (None, cmd_er_flag);
            (None, cmd_cae);
            (None, cmd_mutate_x);
            (None, cmd_sc_updt);
          ]
      @ cmds2
      @ annotate_cmds
          [
            (None, LGoto finally);
            (Some new_err2, cmd_ass_xret1);
            (None, LGoto tr_ctx.tr_err_lab);
            (Some finally, cmd_ass_xret2);
          ]
    in

    ( cmds,
      x_ret_2,
      [ x_ret_1 ],
      rets1 @ rets2,
      outer_breaks1 @ outer_breaks2,
      conts1 @ conts2,
      end_label )
  in

  let make_try_catch_cmds_with_finally e1 (x, e2) catch_id e3 =
    (*
                  cmds1
                  goto finally
        err1:     x_err := PHI(errs1)
                  x_er := new ()
                  x_cae := i__checkAssignmentErrors (ref-v(x_er, "x")) with err
                  [x_er, "x"] := x_err
                  [x_scope, "cid"] := x_er
                  cmds2
                  goto finally
        err2:     x_ret_1 := PHI(x_cae; errs2)
                  cmds_finally
                  goto err
        finally:  x_ret_2 := PHI(breaks_1, x_1, breaks_2, x_2)
                  cmds_finally
                  goto end
        ret_tcf:  x_ret_3 := PHI(rets1, rets2)
                  cmds_finally
                  goto ret_label
                  break_cont_ret_finally_blocks_1
                  break_cont_ret_finally_blocks_2
        end:      x_ret_4 := PHI(breaks_finally, x_ret_2)
    *)
    let new_err1, new_err2, finally, end_label, abnormal_finally, tcf_ret =
      fresh_tcf_vars ()
    in
    let new_loop_list, jumps_mapping =
      rename_cont_break_list tr_ctx.tr_loop_list abnormal_finally
    in

    let new_loop_list =
      (None, finally, tr_ctx.tr_js_lab, false) :: new_loop_list
    in

    let new_ctx_1 =
      update_tr_ctx ~loop_list:new_loop_list ~lab:None ~previous:None
        ~ret_lab:tcf_ret ~err:new_err1 tr_ctx
    in
    let cmds1, x1, errs1, rets1, breaks1, conts1 =
      translate_statement new_ctx_1 e1
    in
    let cmds1, x1_v = add_final_var cmds1 x1 metadata in

    let x_sc_new = fresh_scope_chain_var () in
    let new_ctx_2 =
      update_tr_ctx ~loop_list:new_loop_list ~lab:None ~previous:None
        ~ret_lab:tcf_ret ~err:new_err2
        ~vis_list:(tr_ctx.tr_vis_list @ [ catch_id ])
        ~er_fid:catch_id ~sc_var:x_sc_new tr_ctx
    in
    let cmds2, x2, errs2, rets2, breaks2, conts2 =
      translate_statement new_ctx_2 e2
    in
    let cmds2, x2_v = add_final_var cmds2 x2 metadata in

    let new_loop_list =
      (None, end_label, tr_ctx.tr_js_lab, false) :: tr_ctx.tr_loop_list
    in
    let new_ctx_3 =
      update_tr_ctx ~loop_list:new_loop_list ~lab:None ~previous:None tr_ctx
    in
    let cmds3_1, _, errs3_1, rets3_1, breaks3_1, conts3_1 =
      translate_statement new_ctx_3 e3
    in
    let cmds3_2, _, errs3_2, rets3_2, breaks3_2, conts3_2 =
      translate_statement new_ctx_3 e3
    in
    let cmds3_3, _, errs3_3, rets3_3, breaks3_3, conts3_3 =
      translate_statement new_ctx_3 e3
    in

    let inner_breaks3_1, outer_breaks3_1 =
      filter_cur_jumps breaks3_1 tr_ctx.tr_js_lab false
    in
    let inner_breaks3_2, outer_breaks3_2 =
      filter_cur_jumps breaks3_2 tr_ctx.tr_js_lab false
    in
    let inner_breaks3_3, outer_breaks3_3 =
      filter_cur_jumps breaks3_3 tr_ctx.tr_js_lab false
    in

    let ( finally_cmds_breaks1,
          cur_break_vars_1,
          errs_b1,
          rets_b1,
          outer_breaks_b1,
          inner_breaks_b1,
          conts_b1 ) =
      make_finally_break_blocks breaks1 jumps_mapping e3 tr_ctx.tr_js_lab
        end_label
    in
    let ( finally_cmds_conts1,
          errs_c1,
          rets_c1,
          outer_breaks_c1,
          inner_breaks_c1,
          conts_c1 ) =
      make_finally_cont_blocks conts1 jumps_mapping e3 tr_ctx.tr_js_lab
        end_label
    in

    let ( finally_cmds_breaks2,
          cur_break_vars_2,
          errs_b2,
          _,
          outer_breaks_b2,
          inner_breaks_b2,
          conts_b2 ) =
      make_finally_break_blocks breaks2 jumps_mapping e3 tr_ctx.tr_js_lab
        end_label
    in
    let ( finally_cmds_conts2,
          errs_c2,
          _,
          outer_breaks_c2,
          inner_breaks_c2,
          conts_c2 ) =
      make_finally_cont_blocks conts2 jumps_mapping e3 tr_ctx.tr_js_lab
        end_label
    in

    (* x_err := PHI(errs1) *)
    let x_err = fresh_err_var () in
    let phi_args1 = List.map (fun x -> PVar x) errs1 in
    let cmd_ass_xerr = LPhiAssignment [ (x_err, phi_args1) ] in

    (* x_f_outer_er := new ();  *)
    let x_er_meta = fresh_var () in
    let x_er = fresh_er_var () in

    let cmd_ass_xer_meta = LBasic (New (x_er_meta, None, Some (Lit Null))) in
    let cmd_ass_xer = LBasic (New (x_er, None, Some (PVar x_er_meta))) in

    (* [x_er, "@er"] := true *)
    let cmd_er_flag =
      LBasic
        (Mutation (PVar x_er_meta, Lit (String _erFlagPropName), Lit (Bool true)))
    in

    (* x_cae := i__checkAssignmentErrors (ref-v(x_er, "x")) with err2 *)
    let x_cae = fresh_var () in
    let cmd_cae =
      LCall
        ( x_cae,
          Lit (String checkAssignmentErrorsName),
          [
            EList
              [
                lit_refv; PVar x_er; lit_str x; Lit (Bool tr_ctx.tr_strictness);
              ];
          ],
          Some new_err2,
          None )
    in

    (* [x_er, "x"] := x_err *)
    let cmd_mutate_x =
      LBasic (Mutation (PVar x_er, Lit (String x), PVar x_err))
    in

    (* x_sc_new := x_sc @ {{ x_er }} *)
    let cmd_sc_updt =
      LBasic
        (Assignment
           ( x_sc_new,
             NOp (LstCat, [ PVar tr_ctx.tr_sc_var; EList [ PVar x_er ] ]) ))
    in

    (* err2:     x_ret_1 := PHI(x_cae, errs2) *)
    let x_ret_1 = fresh_var () in
    let phi_args2 = List.map (fun x -> PVar x) (x_cae :: errs2) in
    let cmd_ass_xret1 = LPhiAssignment [ (x_ret_1, phi_args2) ] in

    (* x_ret_2 := PHI(cur_breaks1, x_1, cur_breaks2, x_2, x_ret_1) *)
    let x_ret_2 = fresh_var () in
    let phi_args3 = cur_break_vars_1 @ [ x1_v ] @ cur_break_vars_2 @ [ x2_v ] in
    let phi_args3 = List.map (fun x -> PVar x) phi_args3 in
    let cmd_ass_xret2 = LPhiAssignment [ (x_ret_2, phi_args3) ] in

    (* x_ret_3 := PHI(rets1, rets2)  *)
    let x_ret_3 = fresh_var () in
    let phi_args4 = rets1 @ rets2 in
    let phi_args4 = List.map (fun x -> PVar x) phi_args4 in
    let cmd_ass_xret3 = LPhiAssignment [ (x_ret_3, phi_args4) ] in

    (* x_ret_4 := PHI(inner_breaks_finally, x_ret_2) *)
    let x_ret_4 = fresh_var () in
    let phi_args5 =
      inner_breaks3_1 @ inner_breaks3_2 @ [ x_ret_2 ] @ inner_breaks3_3
      @ inner_breaks_b1 @ inner_breaks_c1 @ inner_breaks_b2 @ inner_breaks_c2
    in
    let phi_args5 = List.map (fun x -> PVar x) phi_args5 in
    let cmd_ass_xret4 = LPhiAssignment [ (x_ret_4, phi_args5) ] in

    let ret_label = tr_ctx.tr_ret_lab in
    let errs =
      errs3_1 @ [ x_ret_1 ] @ errs3_2 @ errs3_3 @ errs_b1 @ errs_c1 @ errs_b2
      @ errs_c2
    in
    let rets =
      rets3_1 @ rets3_2 @ rets3_3 @ [ x_ret_3 ] @ rets_b1 @ rets_c1 @ errs_b2
      @ errs_c2
    in
    let breaks =
      outer_breaks3_1 @ outer_breaks3_2 @ outer_breaks3_3 @ outer_breaks_b1
      @ outer_breaks_c1 @ outer_breaks_b2 @ outer_breaks_c2
    in
    let conts =
      conts3_1 @ conts3_2 @ conts3_3 @ conts_b1 @ conts_c1 @ conts_b2 @ conts_c2
    in

    let cmds =
      cmds1
      @ annotate_cmds
          [
            (*            cmds1                                                            *)
            (None, LGoto finally);
            (*            goto finally                                                     *)
            (Some new_err1, cmd_ass_xerr);
            (*  err1:     x_err := PHI(errs1)                                              *)
            (None, cmd_ass_xer_meta);
            (None, cmd_ass_xer);
            (*            x_er := new ()                                                   *)
            (None, cmd_er_flag);
            (None, cmd_cae);
            (*            x_cae := i__checkAssignmentErrors (ref-v(x_er, "x")) with err2   *)
            (None, cmd_mutate_x);
            (*            [x_er, "x"] := x_err                                             *)
            (None, cmd_sc_updt)
            (*            x_sc_new := x_er :: x_sc                                         *);
          ]
      @ cmds2
      @ annotate_cmds
          [
            (*            cmds2                                                            *)
            (None, LGoto finally);
            (*            goto finally                                                     *)
            (Some new_err2, cmd_ass_xret1)
            (*  err2:     x_ret_1 := PHI(x_cae, errs2)                                     *);
          ]
      @ cmds3_1
      @ annotate_cmds
          [
            (*            cmds3_1                                                          *)
            (None, LGoto tr_ctx.tr_err_lab);
            (*            goto err                                                         *)
            (Some finally, cmd_ass_xret2)
            (*  finally:  x_ret_2 := PHI(cur_breaks1, x_1, cur_breaks2, x_2)               *);
          ]
      @ cmds3_2
      @ annotate_cmds
          [
            (*            cmds3_2                                                          *)
            (None, LGoto end_label);
            (*            goto end                                                         *)
            (Some tcf_ret, cmd_ass_xret3)
            (*  tcf_ret:  x_ret_3 := PHI(rets1, rets2)                                     *);
          ]
      @ cmds3_3
      @ annotate_cmds
          [
            (*            cmds3_3                                                          *)
            (None, LGoto ret_label)
            (*            goto ret_label                                                   *);
          ]
      @ finally_cmds_breaks1 @ finally_cmds_conts1
      (*            break_cont_finally_blocks_1                                      *)
      @ finally_cmds_breaks2
      @ finally_cmds_conts2
      @ [
          (*            break_cont_finally_blocks_2                                      *)
          annotate_cmd cmd_ass_xret4 (Some end_label)
          (*  end:      x_ret_4 := PHI(x_ret_2, inner_breaks_finally)                    *);
        ]
    in
    (cmds, PVar x_ret_4, errs, rets, breaks, conts)
  in

  let make_try_finally_cmds e1 e3 =
    (*
            cmds1
                  goto finally
        err1:     x_err := PHI(errs1)
            cmds_finally
            goto err
      finally:    x_ret_1 := PHI(breaks_1, x_1)
                cmds_finally
                      goto end
      ret_tcf:  x_ret_2 := PHI(rets1)
                cmds_finally
            goto ret_label
            break_cont_ret_finally_blocks_1
      end:      x_ret_3 := PHI(x_ret_1, breaks_finally)
    *)
    let new_err1, _, finally, end_label, abnormal_finally, tcf_ret =
      fresh_tcf_vars ()
    in
    let new_loop_list, jumps_mapping =
      rename_cont_break_list tr_ctx.tr_loop_list abnormal_finally
    in

    let new_loop_list =
      (None, finally, tr_ctx.tr_js_lab, false) :: new_loop_list
    in
    let new_ctx_1 =
      update_tr_ctx ~loop_list:new_loop_list ~err:new_err1 ~lab:None
        ~previous:None ~ret_lab:tcf_ret tr_ctx
    in
    let cmds1, x1, errs1, rets1, breaks1, conts1 =
      translate_statement new_ctx_1 e1
    in
    let cmds1, x1_v = add_final_var cmds1 x1 metadata in

    let new_loop_list =
      (None, end_label, tr_ctx.tr_js_lab, false) :: tr_ctx.tr_loop_list
    in
    let new_ctx_3 =
      update_tr_ctx ~loop_list:new_loop_list ~lab:None ~previous:None tr_ctx
    in
    let cmds3_1, _, errs3_1, rets3_1, breaks3_1, conts3_1 =
      translate_statement new_ctx_3 e3
    in
    let cmds3_2, _, errs3_2, rets3_2, breaks3_2, conts3_2 =
      translate_statement new_ctx_3 e3
    in
    let cmds3_3, _, errs3_3, rets3_3, breaks3_3, conts3_3 =
      translate_statement new_ctx_3 e3
    in
    let inner_breaks3_1, outer_breaks3_1 =
      filter_cur_jumps breaks3_1 tr_ctx.tr_js_lab false
    in
    let inner_breaks3_2, outer_breaks3_2 =
      filter_cur_jumps breaks3_2 tr_ctx.tr_js_lab false
    in
    let inner_breaks3_3, outer_breaks3_3 =
      filter_cur_jumps breaks3_3 tr_ctx.tr_js_lab false
    in

    let ( finally_cmds_breaks1,
          cur_break_vars_1,
          errs_b1,
          rets_b1,
          outer_breaks_b1,
          inner_breaks_b1,
          conts_b1 ) =
      make_finally_break_blocks breaks1 jumps_mapping e3 tr_ctx.tr_js_lab
        end_label
    in
    let ( finally_cmds_conts1,
          errs_c1,
          rets_c1,
          outer_breaks_c1,
          inner_breaks_c1,
          conts_c1 ) =
      make_finally_cont_blocks conts1 jumps_mapping e3 tr_ctx.tr_js_lab
        end_label
    in

    (* x_err := PHI(errs1) *)
    let x_err = fresh_err_var () in
    let phi_args1 = List.map (fun x -> PVar x) errs1 in
    let cmd_ass_xerr = LPhiAssignment [ (x_err, phi_args1) ] in

    (* x_ret_1 := PHI(cur_breaks1, x_1) *)
    let x_ret_1 = fresh_var () in
    let phi_args = cur_break_vars_1 @ [ x1_v ] in
    let phi_args = List.map (fun x -> PVar x) phi_args in
    let cmd_ass_xret1 = LPhiAssignment [ (x_ret_1, phi_args) ] in

    (* x_ret_2 := PHI(rets1)  *)
    let x_ret_2 = fresh_var () in
    let phi_args = rets1 in
    let phi_args = List.map (fun x -> PVar x) phi_args in
    let cmd_ass_xret2 = LPhiAssignment [ (x_ret_2, phi_args) ] in

    (* x_ret_3 := PHI(inner_breaks_finally, x_ret_1) *)
    let x_ret_3 = fresh_var () in
    let phi_args =
      inner_breaks3_1 @ inner_breaks3_2 @ [ x_ret_1 ] @ inner_breaks3_3
      @ inner_breaks_b1 @ inner_breaks_c1
    in
    let phi_args = List.map (fun x -> PVar x) phi_args in
    let cmd_ass_xret3 = LPhiAssignment [ (x_ret_3, phi_args) ] in

    let ret_label = tr_ctx.tr_ret_lab in
    let errs = errs3_1 @ [ x_err ] @ errs3_2 @ errs3_3 @ errs_b1 @ errs_c1 in
    let rets = rets3_1 @ rets3_2 @ rets3_3 @ [ x_ret_2 ] @ rets_b1 @ rets_c1 in
    let breaks =
      outer_breaks3_1 @ outer_breaks3_2 @ outer_breaks3_3 @ outer_breaks_b1
      @ outer_breaks_c1
    in
    let conts = conts3_1 @ conts3_2 @ conts3_3 @ conts_b1 @ conts_c1 in

    let cmds =
      cmds1
      @ annotate_cmds
          [
            (*            cmds1                                                       *)
            (None, LGoto finally);
            (*            goto finally                                                *)
            (Some new_err1, cmd_ass_xerr)
            (*  err1:     x_err := PHI(errs1)                                         *);
          ]
      @ cmds3_1
      @ annotate_cmds
          [
            (*            cmds3_1                                                     *)
            (None, LGoto tr_ctx.tr_err_lab);
            (*            goto err                                                    *)
            (Some finally, cmd_ass_xret1)
            (*  finally:  x_ret_1 := PHI(breaks_1, x_1)                               *);
          ]
      @ cmds3_2
      @ annotate_cmds
          [
            (*            cmds3_2                                                     *)
            (None, LGoto end_label);
            (*            goto end                                                    *)
            (Some tcf_ret, cmd_ass_xret2)
            (*  tcf_ret:  x_ret_2 := PHI(rets1)                                       *);
          ]
      @ cmds3_3
      @ annotate_cmds
          [
            (*            cmds3_3                                                     *)
            (None, LGoto ret_label)
            (*            goto ret_label                                              *);
          ]
      @ finally_cmds_breaks1 @ finally_cmds_conts1
      @ [
          (*            break_cont_finally_blocks_1                                 *)
          annotate_cmd cmd_ass_xret3 (Some end_label)
          (*  end:      x_ret_3 := PHI(inner_breaks_finally, x_ret_2)               *);
        ]
    in
    (cmds, PVar x_ret_3, errs, rets, breaks, conts)
  in

  (* Statements **)
  match e.JS_Parser.Syntax.exp_stx with
  | JS_Parser.Syntax.Script (_, es) | JS_Parser.Syntax.Block es ->
      (*
     Section 12.1 - Block

     C_iter({}) = [], empty

     C(stmts) = cmds, x
     C(stmt) = cmds', x'
     CanBeEmpty(stmt)
     -------------------------
     C_iter(stmts; stmt) =      cmds
                      cmds'
                  goto [x' = empty] next end
               next:  skip
               end:   x'' := PHI(x', x)



     C_iter(stmts) = cmds, x
     C_iter(stmt) = cmds', x'
     !CanBeEmpty(stmt)
     -------------------------
     C_iter(stmts; stmt) =  cmds
                cmds'


     C_iter (stmts) = cmds, x
     -------------------------------
     C(Block stmts) = cmds
                      x_ret := PHI (break_vars, x)
     *)
      let break_label, new_loop_list =
        match tr_ctx.tr_js_lab with
        | None -> (None, tr_ctx.tr_loop_list)
        | Some _ ->
            let break_label = fresh_break_label () in
            ( Some break_label,
              (None, break_label, tr_ctx.tr_js_lab, false)
              :: tr_ctx.tr_loop_list )
      in

      let rec loop es bprevious cmds_ac errs_ac rets_ac breaks_ac conts_ac =
        match es with
        | [] -> ([], Lit Empty, [], [], [], [])
        | [ e ] -> (
            let cmds_e, x_e, errs_e, rets_e, breaks_e, conts_e =
              f_previous new_loop_list bprevious None e
            in
            match (returns_empty_exp e, bprevious) with
            | true, Some x_previous ->
                let new_cmds, x_r = make_check_empty_test x_previous x_e in
                let new_cmds = annotate_cmds new_cmds in
                ( cmds_ac @ cmds_e @ new_cmds,
                  PVar x_r,
                  errs_ac @ errs_e,
                  rets_ac @ rets_e,
                  breaks_ac @ breaks_e,
                  conts_ac @ conts_e )
            | _, _ ->
                ( cmds_ac @ cmds_e,
                  x_e,
                  errs_ac @ errs_e,
                  rets_ac @ rets_e,
                  breaks_ac @ breaks_e,
                  conts_ac @ conts_e ))
        | e :: rest_es -> (
            let cmds_e, x_e, errs_e, rets_e, breaks_e, conts_e =
              f_previous new_loop_list bprevious None e
            in
            match (returns_empty_exp e, bprevious) with
            | true, Some x_previous ->
                let new_cmds, x_r = make_check_empty_test x_previous x_e in
                let new_cmds = annotate_cmds new_cmds in
                loop rest_es (Some (PVar x_r))
                  (cmds_ac @ cmds_e @ new_cmds)
                  (errs_ac @ errs_e) (rets_ac @ rets_e) (breaks_ac @ breaks_e)
                  (conts_ac @ conts_e)
            | _, _ ->
                loop rest_es (Some x_e) (cmds_ac @ cmds_e) (errs_ac @ errs_e)
                  (rets_ac @ rets_e) (breaks_ac @ breaks_e) (conts_ac @ conts_e)
            )
      in

      let cmds, x, errs, rets, breaks, conts =
        loop es tr_ctx.tr_previous [] [] [] [] []
      in
      create_final_phi_cmd cmds x errs rets breaks conts break_label
        tr_ctx.tr_js_lab
  | JS_Parser.Syntax.VarDec decs ->
      (*
     Section 12.2 - Variable Statement

      vdec ::= x | x = e
      vdecs ::= vdec, vdecs  | []


      C_dec (x) = []

      C(e) = cmds, x
      --------------------------
      C_dec(x = e) = cmds
                     x_v := i__getValue(x) with err
               x_sf := [x__scope, fid]
               x_ref := ref_v(x_sf, "x")
               x_pv := i__putValue(x_ref, x_v) with err

      C_dec(vdec) = cmds1
      C_dec(vdecs) = cmds2
      --------------------------
      C_dec (vdec, vdecs) = cmds1
                            cmds2

      C_dec ([]) = []

      C_dec(vdecs) = cmds
      --------------------------
      C(var vdecs) = cmds
                    x := empty

     *)
      let rec loop decs cmds errs =
        match decs with
        | [] ->
            let x, empty_ass = make_empty_ass () in

            (x, cmds @ [ annotate_cmd empty_ass None ], errs)
        | (v, eo) :: rest_decs -> (
            match eo with
            | None -> loop rest_decs cmds errs
            | Some e ->
                let new_cmds, _, new_errs = compile_var_dec v e in
                loop rest_decs (cmds @ new_cmds) (errs @ new_errs))
      in
      let x, cmds, errs = loop decs [] [] in
      let cmds = annotate_first_cmd cmds in
      (cmds, PVar x, errs, [], [], [])
  | JS_Parser.Syntax.Skip
  (* Section 12.3 - Empty Statement *)
  | JS_Parser.Syntax.Debugger ->
      (* Section 12.15 - Debugger Statement **)
      ([], Lit Empty, [], [], [], [])
  | JS_Parser.Syntax.Num _
  | JS_Parser.Syntax.String _
  | JS_Parser.Syntax.Null
  | JS_Parser.Syntax.Bool _
  | JS_Parser.Syntax.Var _
  | JS_Parser.Syntax.This
  | JS_Parser.Syntax.Delete _
  | JS_Parser.Syntax.Comma _
  | JS_Parser.Syntax.Unary_op _
  | JS_Parser.Syntax.BinOp _
  | JS_Parser.Syntax.Access _
  | JS_Parser.Syntax.Call _
  | JS_Parser.Syntax.Assign _
  | JS_Parser.Syntax.AssignOp _
  | JS_Parser.Syntax.FunctionExp _
  | JS_Parser.Syntax.New _
  | JS_Parser.Syntax.Obj _
  | JS_Parser.Syntax.Array _
  | JS_Parser.Syntax.CAccess _
  | JS_Parser.Syntax.ConditionalOp _ ->
      (*
     Section 12.4 - Expression Statement
     *)
      let cmds_e, x_e, errs_e = fe e in
      let x_e_v, cmd_gv_xe, errs_x_e_v =
        make_get_value_call x_e tr_ctx.tr_err_lab
      in
      let cmds = cmds_e @ [ annotate_cmd cmd_gv_xe None ] in
      let cmds = annotate_first_cmd cmds in
      (cmds, PVar x_e_v, errs_e @ errs_x_e_v, [], [], [])
  | JS_Parser.Syntax.If (e1, e2, e3) ->
      (*
     Section 12.5 - If Statement
       *  C(e1) = cmds1, x1; C(e2) = cmds2, x2; C(e3) = cmds3, x3
       *
       *  C(if (e1) { e2 } else { e3 }) =
             cmds1
             x1_v := i__getValue (x1) with err
             x1_b := i__toBoolean (x1_b) with err
             goto [x1_b] then else
      then:  cmds2
             goto endif
      else:  cmds3
      endif: x_if := PHI(x2, x3)
       *)
      let break_label, new_loop_list =
        match tr_ctx.tr_js_lab with
        | None -> (None, tr_ctx.tr_loop_list)
        | Some _ ->
            let break_label = fresh_break_label () in
            ( Some break_label,
              (None, break_label, tr_ctx.tr_js_lab, false)
              :: tr_ctx.tr_loop_list )
      in

      let cmds1, x1, errs1 = fe e1 in
      let cmds2, x2, errs2, rets2, breaks2, conts2 =
        f_previous new_loop_list None None e2
      in
      let cmds3, x3, errs3, rets3, breaks3, conts3 =
        match e3 with
        | None ->
            let x3, cmd3 = make_empty_ass () in
            ([ annotate_cmd cmd3 None ], PVar x3, [], [], [], [])
        | Some e3 -> f_previous new_loop_list None None e3
      in

      (* x1_v := getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 tr_ctx.tr_err_lab
      in

      (* x1_b := toBoolean (x1_v) with err *)
      let x1_b, cmd_tb_x1 = make_to_boolean_call x1 x1_v tr_ctx.tr_err_lab in

      (* goto [x1_b] then else *)
      let then_lab = fresh_then_label () in
      let else_lab = fresh_else_label () in
      let end_lab = fresh_end_label () in
      let cmd_goto_if = LGuardedGoto (PVar x1_b, then_lab, else_lab) in

      let cmds2 = add_initial_label cmds2 then_lab metadata in
      let cmds3 = add_initial_label cmds3 else_lab metadata in

      let cmds2, x2 = add_skip_if_empty cmds2 x2 metadata in
      let cmds3, x3 = add_skip_if_empty cmds3 x3 metadata in

      (* goto end *)
      let cmd_goto_endif = LGoto end_lab in

      (* end: x_if := PHI(x2, x3) *)
      let x2_name, x3_name =
        match (x2, x3) with
        | PVar x2_name, PVar x3_name -> (x2_name, x3_name)
        | _, _ ->
            raise
              (Failure
                 "the compilation of the then and else parts of the an if \
                  statement must generate a variable each")
      in
      let x_if = fresh_var () in
      let cmd_end_if =
        LPhiAssignment [ (x_if, [ PVar x2_name; PVar x3_name ]) ]
      in

      let cmds =
        cmds1
        @ annotate_cmds
            [
              (*       cmds1                               *)
              (None, cmd_gv_x1);
              (*       x1_v := getValue (x1) with err      *)
              (None, cmd_tb_x1);
              (*       x1_b := toBoolean (x1_v) with err   *)
              (None, cmd_goto_if)
              (*       goto [x1_b] then else               *);
            ]
        @ cmds2
        @ annotate_cmds
            [
              (* then: cmds2                               *)
              (None, cmd_goto_endif)
              (*       goto end                            *);
            ]
        @ cmds3
        @ annotate_cmds
            [
              (* else: cmds3                               *)
              (Some end_lab, cmd_end_if)
              (* end:  x_if := PHI(x3, x2)                 *);
            ]
      in
      let errs = errs1 @ errs_x1_v @ [ x1_b ] @ errs2 @ errs3 in

      let cmds = annotate_first_cmd cmds in
      let cmds, x, errs, rets, breaks, conts =
        ( cmds,
          PVar x_if,
          errs,
          rets2 @ rets3,
          breaks2 @ breaks3,
          conts2 @ conts3 )
      in
      create_final_phi_cmd cmds x errs rets breaks conts break_label
        tr_ctx.tr_js_lab
  | JS_Parser.Syntax.DoWhile (e1, e2) ->
      (*
     Section 12.6.1 - The do-while Statement
       *  C(e1) = cmds1, x1; C(e2) = cmds2, x2
       *
       *  C(do { e1 } while (e2) ) =
                x_ret_0 := empty
      head:     x_ret_1 := PHI(x_ret_0, x_ret_3)
                cmds1
                x1_v := i__getValue (x1) with err
      cont:     x_ret_2 := PHI(cont_vars, x1_v)
                goto [ not (x_ret_2 = empty) ] next1 next2
      next1:    skip
      next2:    x_ret_3 := PHI(x_ret_1, x_ret_2)
      guard:    cmds2
                x2_v := i__getValue (x2) with err
                x2_b := i__toBoolean (x2_v) with err
                goto [x2_b] head end_loop
      end_loop: x_ret_4 := PHI(break_vars, x_ret_3)
                goto [ x_ret_4 = empty ] next3 next4
      next3:    skip
      next4:    x_ret_5 := PHI(x_ret_4, x_ret_1)
       *)
      let head, guard, _, cont, end_loop = fresh_loop_vars () in

      let new_loop_list =
        (Some cont, end_loop, tr_ctx.tr_js_lab, true) :: tr_ctx.tr_loop_list
      in
      let new_ctx =
        update_tr_ctx ~previous:None ~lab:None ~loop_list:new_loop_list tr_ctx
      in
      let cmds1, x1, errs1, rets1, breaks1, conts1 =
        translate_statement new_ctx e2
      in
      let cmds2, x2, errs2 = fe e1 in
      let cmds2 = add_initial_label cmds2 guard metadata in

      let cur_breaks, outer_breaks =
        filter_cur_jumps breaks1 tr_ctx.tr_js_lab true
      in
      let cur_conts, outer_conts =
        filter_cur_jumps conts1 tr_ctx.tr_js_lab true
      in

      (* x_ret_0 := empty *)
      let x_ret_0, cmd_ass_ret_0 = make_empty_ass () in

      (* x_ret_1 := PHI(x_ret_0, x_ret_3)  *)
      let x_ret_1 = fresh_var () in
      let x_ret_2 = fresh_var () in
      let x_ret_3 = fresh_var () in
      let cmd_ass_ret_1 =
        LPhiAssignment [ (x_ret_1, [ PVar x_ret_0; PVar x_ret_3 ]) ]
      in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 new_ctx.tr_err_lab
      in

      (* x_ret_2 := PHI(cont_vars, x1_v) *)
      let cur_conts = cur_conts @ [ x1_v ] in
      let cur_conts = List.map (fun x -> PVar x) cur_conts in
      let cmd_ass_ret_2 = LPhiAssignment [ (x_ret_2, cur_conts) ] in

      (*  goto [ not (x_ret_2 = empty) ] next1 next2 *)
      let next1 = fresh_next_label () in
      let next2 = fresh_next_label () in
      let expr_goto_guard = BinOp (PVar x_ret_2, Equal, Lit Empty) in
      let expr_goto_guard = UnOp (UNot, expr_goto_guard) in
      let cmd_goto_empty_test = LGuardedGoto (expr_goto_guard, next1, next2) in

      (* x_ret_3 := PHI(x_ret_1, x_ret_2)  *)
      let cmd_ass_ret_3 =
        LPhiAssignment [ (x_ret_3, [ PVar x_ret_1; PVar x_ret_2 ]) ]
      in

      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 new_ctx.tr_err_lab
      in

      (* x2_b := i__toBoolean (x2_v) with err *)
      let x2_b, cmd_tb_x2 = make_to_boolean_call x2 x2_v new_ctx.tr_err_lab in

      (* goto [x2_b] head end_loop *)
      let cmd_dowhile_goto = LGuardedGoto (PVar x2_b, head, end_loop) in

      let cmds_end_loop, x_ret_5 =
        make_loop_end x_ret_3 x_ret_1 cur_breaks end_loop false
      in

      let cmds =
        annotate_cmds
          [
            (None, cmd_ass_ret_0);
            (*              x_ret_0 := empty                           *)
            (Some head, cmd_ass_ret_1)
            (* head:        x_ret_1 := PHI(x_ret_0, x_ret_3)             *);
          ]
        @ cmds1
        @ annotate_cmds
            [
              (*              cmds1                                        *)
              (None, cmd_gv_x1);
              (*              x1_v := i__getValue (x1) with err            *)
              (Some cont, cmd_ass_ret_2);
              (* cont:        x_ret_2 := PHI(cont_vars, x1_v)              *)
              (None, cmd_goto_empty_test);
              (*              goto [ not (x_ret_2 = empty) ] next1 next2 *)
              (Some next1, LBasic Skip);
              (* next1:       skip                                         *)
              (Some next2, cmd_ass_ret_3)
              (* next2:       x_ret_3 := PHI(x_ret_1, x_ret_2)             *);
            ]
        @ cmds2
        @ annotate_cmds
            [
              (* guard:       cmds2                                        *)
              (None, cmd_gv_x2);
              (*              x2_v := i__getValue (x2) with err            *)
              (None, cmd_tb_x2);
              (*              x2_b := i__toBoolean (x2_v) with err         *)
              (None, cmd_dowhile_goto)
              (*              goto [x2_b] head end                         *);
            ]
        @ annotate_cmds cmds_end_loop
      in
      let errs = errs1 @ errs_x1_v @ errs2 @ errs_x2_v @ [ x2_b ] in
      let cmds = annotate_first_cmd cmds in
      (cmds, PVar x_ret_5, errs, rets1, outer_breaks, outer_conts)
  | JS_Parser.Syntax.While (e1, e2) ->
      (*
     Section 12.6.2 - The while Statement
       *  C(e1) = cmds1, x1; C(e2) = cmds2, x2
       *
       *  C(while (e1) { e2 } ) =
                x_ret_0 := empty
      head:     x_ret_1 := PHI(x_ret_0, x_ret_3)
                cmds1
                x1_v := i__getValue (x1) with err
                x1_b := i__toBoolean (x1_b) with err
                goto [x1_b] body end_loop
      body:     cmds2
                x2_v := i__getValue (x2) with err
      cont:     x_ret_2 := PHI(cont_vars, x2_v)
                goto [not (x_ret_2 = empty)] next1 next2
      next1:    skip;
      next2:    x_ret_3 := PHI(x_ret_1, x_ret_2)
                goto head
      end_loop: x_ret_4 := PHI(x_ret_1, break_vars)
                goto [ x_ret_4 = empty ] next3 next4
      next3:    skip
      next4:    x_ret_5 := PHI(x_ret_4, x_ret_1)
       *)
      let head, _, body, cont, end_loop = fresh_loop_vars () in

      let new_loop_id = fresh_loop_identifier () in
      let new_loops = new_loop_id :: tr_ctx.tr_loops in
      let new_ctx = update_tr_ctx ~loops:new_loops tr_ctx in

      let cmds1, x1, errs1 = translate_expr new_ctx e1 in

      let new_loop_list =
        (Some cont, end_loop, tr_ctx.tr_js_lab, true) :: tr_ctx.tr_loop_list
      in
      let new_ctx =
        update_tr_ctx ~previous:None ~lab:None ~loop_list:new_loop_list new_ctx
      in

      let cmds2, x2, errs2, rets2, breaks2, conts2 =
        translate_statement new_ctx e2
      in

      let cur_breaks, outer_breaks =
        filter_cur_jumps breaks2 tr_ctx.tr_js_lab true
      in
      let cur_conts, outer_conts =
        filter_cur_jumps conts2 tr_ctx.tr_js_lab true
      in

      (* x_ret_0 := empty *)
      let x_ret_0, cmd_ass_ret_0 = make_empty_ass () in
      let x_ret_1 = fresh_var () in

      (* x_ret_1 := PHI(x_ret_0, x_ret_3) *)
      let x_ret_2 = fresh_var () in
      let x_ret_3 = fresh_var () in
      let cmd_ass_ret_1 =
        LPhiAssignment [ (x_ret_1, [ PVar x_ret_0; PVar x_ret_3 ]) ]
      in

      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, errs_x1_v =
        make_get_value_call x1 new_ctx.tr_err_lab
      in

      (* x1_b := i__toBoolean (x1_v) with err *)
      let x1_b, cmd_tb_x1 = make_to_boolean_call x1 x1_v new_ctx.tr_err_lab in

      (* goto [x1_b] body endwhile  *)
      let cmd_goto_while = LGuardedGoto (PVar x1_b, body, end_loop) in

      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 new_ctx.tr_err_lab
      in

      (* x_ret_2 := PHI(cont_vars, x2_v) *)
      let cur_conts = cur_conts @ [ x2_v ] in
      let cur_conts = List.map (fun x -> PVar x) cur_conts in
      let cmd_ass_ret_2 = LPhiAssignment [ (x_ret_2, cur_conts) ] in

      (* goto [not (x_ret_2 = empty)] next1 next2 *)
      let next1 = fresh_next_label () in
      let next2 = fresh_next_label () in
      let expr_goto_guard = BinOp (PVar x_ret_2, Equal, Lit Empty) in
      let expr_goto_guard = UnOp (UNot, expr_goto_guard) in
      let cmd_goto_empty_test = LGuardedGoto (expr_goto_guard, next1, next2) in

      (* x_ret_3 := PHI(x_ret_1, x_ret_2) *)
      let cmd_ass_ret_3 =
        LPhiAssignment [ (x_ret_3, [ PVar x_ret_1; PVar x_ret_2 ]) ]
      in

      let cmds_end_loop, x_ret_5 =
        make_loop_end x_ret_1 x_ret_1 cur_breaks end_loop true
      in

      (* Place invariant exactly where it's supposed to be *)
      let head_cmds =
        match invariant with
        | Some (a, binders) ->
            [
              (Some head, LabCmd.LLogic (LCmd.SL (Invariant (a, binders))));
              (None, cmd_ass_ret_1);
            ]
        | None -> [ (Some head, cmd_ass_ret_1) ]
      in

      let cmds2 = add_initial_label cmds2 body metadata in

      (* Set up the new annotation *)
      let metadata = Annot.set_loop_info metadata new_loops in
      let annotate_loop_cmds = annotate_cmds_top_level metadata in

      let cmds =
        (* This command is not in the loop *)
        annotate_cmds [ (None, cmd_ass_ret_0) ]
        (* But these from here on in are *)
        @ annotate_loop_cmds head_cmds
        @ cmds1
        @ annotate_loop_cmds
            [
              (*           cmds1                                      *)
              (None, cmd_gv_x1);
              (*           x1_v := i__getValue (x1) with err          *)
              (None, cmd_tb_x1);
              (*           x1_b := i__toBoolean (x1_b) with err       *)
              (None, cmd_goto_while)
              (*           goto [x1_b] body endwhile                  *);
            ]
        @ cmds2
        @ annotate_loop_cmds
            [
              (* body:     cmds2                                      *)
              (None, cmd_gv_x2);
              (*           x2_v := i__getValue (x2) with err          *)
              (Some cont, cmd_ass_ret_2);
              (* cont:     x_ret_2 := PHI(cont_vars, x2_v)            *)
              (None, cmd_goto_empty_test);
              (*           goto [not (x_ret_2 = empty)] next1 next2 *)
              (Some next1, LBasic Skip);
              (* next1:    skip                                       *)
              (Some next2, cmd_ass_ret_3);
              (* next2:    x_ret_3 := PHI(x_ret_1, x_ret_2)           *)
              (None, LGoto head)
              (*           goto head                                  *);
            ]
        (* These commands are out of the loop *)
        @ annotate_cmds cmds_end_loop
      in
      let errs = errs1 @ errs_x1_v @ [ x1_b ] @ errs2 @ errs_x2_v in
      (* Non-invariant commands go before all commands *)
      let cmds = prefix_lcmds lcmds None cmds in
      (cmds, PVar x_ret_5, errs, rets2, outer_breaks, outer_conts)
  | JS_Parser.Syntax.ForIn (e1, e2, e3) ->
      (*
     Section 12.6.4
       *  C(e_lhs) = cmds1, x1; C(e_obj) = cmds2, x2; C(e_stmt) = cmds3, x3
       *
       *  C( for (e1 in e2) { e3 } ) =
              cmds2                                     1.  Understand what the object is
          x2_v := i__getValue (x2) with err                     2.  and get its value
          x_ret_0 := empty                              5.  Set V to empty
          goto [(x2_v = null) or
          (x2_v = undefined)] next6 next0;                      3.  If the object is null or undefined, we're done
      next0:  x4 := "i__toObject" (x2_v) with err                     4.  Otherwise, convert whatever we have to an object
          xlf := "i__getAllEnumerableFields" (x4)  with err               Put all of its enumerable properties (protochain included) in xlf
          xf  := getFields (xlf)                              Get all of those properties
          len := l-len (xf)                               Get the number of properties
          x_c := 0;                                   Initialise counter
      head:   x_ret_1 := PHI(x_ret_0, x_ret_3)                        Setup return value
          x_c_1 := PSI(x_c, x_c_2);                           Setup counter
          goto [x_c_1 < len] body end_loop                      6.  Are we done?
      body:   xp := l-nth (xf, x_c_1)                               6a. Get the nth property
          xl := [xlf, xp];                                  6a. Get the location of where it should be
          xhf := hasField (xl, xp)                                        6a. Understand if it's still there!
          goto [xhf] next1 next3                                6a. And jump accordingly
      next1:  cmds1                                       6b. Evaluate lhs
          x5 := "i__putValue" (x1, xp) with err                       6c. Put it in, put it in
          cmds3                                       6d. Evaluate the statement
          x3_v = "i__getValue" (x3) with err
      cont:   x_ret_2 := PHI(cont_vars, x3_v)
          goto [ not (x_ret_2 = empty) ] next2 next3
      next2:    skip
      next3:  x_ret_3 := PHI(x_ret_1, x_ret_2)
      next4:  x_c_2 := x_c_1 + 1
          goto head
      end_loop: x_ret_4 := PHI(x_ret_1, x_ret_1, break_vars)
              goto [ x_ret_4 = empty ] next5 next6
      next5:  skip
      next6:  x_ret_5 := PHI(x_ret_0, x_ret_1, x_ret_4)

      errs: errs2, x2_v, x4, xlf, errs1, x5, errs3, x3_v
       *)
      let cmds1, x1, errs1 = fe e1 in
      let cmds2, x2, errs2 = fe e2 in
      let head, _, body, cont, end_loop = fresh_loop_vars () in
      let new_loop_list =
        (Some cont, end_loop, tr_ctx.tr_js_lab, true) :: tr_ctx.tr_loop_list
      in
      let new_ctx =
        update_tr_ctx ~previous:None ~lab:None ~loop_list:new_loop_list tr_ctx
      in
      let cmds3, x3, errs3, rets3, breaks3, conts3 =
        translate_statement new_ctx e3
      in

      let cur_breaks, outer_breaks =
        filter_cur_jumps breaks3 tr_ctx.tr_js_lab true
      in
      let cur_conts, outer_conts =
        filter_cur_jumps conts3 tr_ctx.tr_js_lab true
      in

      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 tr_ctx.tr_err_lab
      in

      (*  x_ret_0 := empty *)
      let x_ret_0 = fresh_var () in
      let x_ret_1 = fresh_var () in
      let x_ret_2 = fresh_var () in
      let x_ret_3 = fresh_var () in
      let x_ret_4 = fresh_var () in
      let x_ret_5 = fresh_var () in
      let cmd_ass_xret0 = LBasic (Assignment (x_ret_0, Lit Empty)) in

      (* goto [(x2_v = null) or (x2_v = undefined)] next6 next0;  *)
      let next0 = fresh_next_label () in
      let next1 = fresh_next_label () in
      let next2 = fresh_next_label () in
      let next3 = fresh_next_label () in
      let next4 = fresh_next_label () in
      let next5 = fresh_next_label () in
      let next6 = fresh_next_label () in
      let expr_goto_guard =
        BinOp
          ( BinOp (PVar x2_v, Equal, Lit Null),
            BOr,
            BinOp (PVar x2_v, Equal, Lit Undefined) )
      in
      let cmd_goto_null_undef = LGuardedGoto (expr_goto_guard, next6, next0) in

      (* x4 := "i__toObject" (x2_v) with err   *)
      let x4 = fresh_var () in
      let cmd_to_obj_call =
        LCall
          ( x4,
            Lit (String toObjectName),
            [ PVar x2_v ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      (* xlf := "i__getAllEnumerableFields" (x4)  with err  *)
      let xlf = fresh_var () in
      let cmd_get_enum_fields =
        LCall
          ( xlf,
            Lit (String getEnumFieldsName),
            [ PVar x4 ],
            Some tr_ctx.tr_err_lab,
            None )
      in

      (* xf  := getFields (xlf)  *)
      let xf = fresh_var () in
      let cmd_xfr_ass = LBasic (GetFields (xf, PVar xlf)) in

      (* let xf = fresh_var () in
         let cmd_xf_ass = LBasic (Assignment (xf, UnOp (SetToList, PVar xfr))) in *)

      (* len := l-len (xf)   *)
      let len = fresh_var () in
      let cmd_ass_len = LBasic (Assignment (len, UnOp (LstLen, PVar xf))) in

      (* x_c := 0 *)
      let x_c = fresh_var () in
      let cmd_ass_xc = LBasic (Assignment (x_c, Lit (Num 0.))) in

      (*   x_ret_1 := PHI(x_ret_0, x_ret_3)  *)
      let x_c_1 = fresh_var () in
      let x_c_2 = fresh_var () in
      let cmd_ass_xret1 =
        LPhiAssignment
          [
            (x_ret_1, [ PVar x_ret_0; PVar x_ret_3 ]);
            (x_c_1, [ PVar x_c; PVar x_c_2 ]);
          ]
      in

      (* goto [x_c_1 < len] body end_loop  *)
      let cmd_goto_len =
        LGuardedGoto (BinOp (PVar x_c_1, FLessThan, PVar len), body, end_loop)
      in

      (* xp := l-nth (xf, x_c_1)  *)
      let xp = fresh_var () in
      let cmd_ass_xp =
        LBasic (Assignment (xp, BinOp (PVar xf, LstNth, PVar x_c_1)))
      in

      (* xl := [xlf, xp]; *)
      let xl = fresh_var () in
      let cmd_ass_xl = LBasic (Lookup (xl, PVar xlf, PVar xp)) in

      (*  xhf := hasField (xl, xp) *)
      let xhf = fresh_var () in
      let cmd_ass_hf = LBasic (HasField (xhf, PVar xl, PVar xp)) in

      (* goto [xhf] next1 next3  *)
      let cmd_goto_xhf = LGuardedGoto (PVar xhf, next1, next3) in

      (* x5 := "i__putValue" (x1, xp) with err   *)
      let x5, cmd_pv_x1 = make_put_value_call x1 xp tr_ctx.tr_err_lab in

      (* x3_v = "i__getValue" (x3) with err *)
      let x3_v, cmd_gv_x3, errs_x3_v =
        make_get_value_call x3 tr_ctx.tr_err_lab
      in

      (* x_ret_2 := PHI(cont_vars, x3_v) *)
      let phi_args = cur_conts @ [ x3_v ] in
      let phi_args = List.map (fun x -> PVar x) phi_args in
      let cmd_phi_cont = LPhiAssignment [ (x_ret_2, phi_args) ] in

      (* goto [ not (x_ret_2 = empty) ] next2 next3 *)
      let expr_goto_guard = BinOp (PVar x_ret_2, Equal, Lit Empty) in
      let expr_goto_guard = UnOp (UNot, expr_goto_guard) in
      let cmd_goto_xret2 = LGuardedGoto (expr_goto_guard, next2, next3) in

      (* x_ret_3 := PHI(x_ret_1, x_ret_2) *)
      let cmd_phi_xret3 =
        LPhiAssignment
          [ (x_ret_3, [ PVar x_ret_1; PVar x_ret_1; PVar x_ret_2 ]) ]
      in

      (* x_c_2 := x_c_1 + 1 *)
      let cmd_ass_incr =
        LBasic (Assignment (x_c_2, BinOp (PVar x_c_1, FPlus, Lit (Num 1.))))
      in

      (*  x_ret_4 := PHI(x_ret_1, break_vars)  *)
      let phi_args = x_ret_1 :: cur_breaks in
      let phi_args = List.map (fun x -> PVar x) phi_args in
      let cmd_phi_xret4 = LPhiAssignment [ (x_ret_4, phi_args) ] in

      (* goto [ x_ret_4 = empty ] next5 next6 *)
      let cmd_goto_xret4_empty =
        LGuardedGoto (BinOp (PVar x_ret_4, Equal, Lit Empty), next5, next6)
      in

      (* x_ret_5 := PHI(x_ret_0, x_ret_1, x_ret_4) *)
      let cmd_phi_xret5 =
        LPhiAssignment
          [ (x_ret_5, [ PVar x_ret_0; PVar x_ret_1; PVar x_ret_4 ]) ]
      in

      let cmds1 = add_initial_label cmds1 next1 metadata in
      let cmds =
        cmds2
        @ annotate_cmds
            [
              (*           cmds2                                                        *)
              (None, cmd_gv_x2);
              (*           x2_v := i__getValue (x2) with err                            *)
              (None, cmd_ass_xret0);
              (*           x_ret_0 := empty                                         *)
              (None, cmd_goto_null_undef);
              (*           goto [(x2_v = null) or (x2_v = undefined)] next6 next0   *)
              (Some next0, cmd_to_obj_call);
              (* next0:    x4 := "i__toObject" (x2_v) with err                      *)
              (None, cmd_get_enum_fields);
              (*           xlf := "i__getAllEnumerableFields" (x4)  with err            *)
              (None, cmd_xfr_ass);
              (* (None,          cmd_xf_ass);   *)
              (*           xf  := getFields (xlf)                                       *)
              (None, cmd_ass_len);
              (*           len := l-len (xf)                                            *)
              (None, cmd_ass_xc);
              (*           x_c := 0                                                     *)
              (Some head, cmd_ass_xret1);
              (* head:     x_ret_1 := PHI(x_ret_0, x_ret_3)                           *)
              (None, cmd_goto_len);
              (*           goto [x_c_1 < len] body end_loop                           *)
              (Some body, cmd_ass_xp);
              (* body:     xp := l-nth (xf, x_c_1)                                    *)
              (None, cmd_ass_xl);
              (*           xl := [xlf, xp]                                              *)
              (None, cmd_ass_hf);
              (*           xhf := hasField (xl, xp)                                    *)
              (None, cmd_goto_xhf)
              (*           goto [xhf] next1 next4                                     *);
            ]
        @ cmds1
        @ annotate_cmds
            [
              (* next1:    cmds1                                                        *)
              (None, cmd_pv_x1)
              (*           x5 := "i__putValue" (x1, xp) with err                      *);
            ]
        @ cmds3
        @ annotate_cmds
            [
              (*           cmds3                                                        *)
              (None, cmd_gv_x3);
              (*           x3_v = "i__getValue" (x3) with err                           *)
              (Some cont, cmd_phi_cont);
              (* cont:     x_ret_2 := PHI(cont_vars, x3_v)                              *)
              (None, cmd_goto_xret2);
              (*           goto [ not (x_ret_2 = empty) ] next2 next3                 *)
              (Some next2, LBasic Skip);
              (* next2:    skip                                                         *)
              (Some next3, cmd_phi_xret3);
              (* next3:    x_ret_3 := PHI(x_ret_1, x_ret_1, x_ret_2)                    *)
              (Some next4, cmd_ass_incr);
              (* next4:    x_c_2 := x_c_1 + 1                                           *)
              (None, LGoto head);
              (*           goto head                                                    *)
              (Some end_loop, cmd_phi_xret4);
              (* end_loop: x_ret_4 := PHI(x_ret_1, break_vars)                          *)
              (None, cmd_goto_xret4_empty);
              (*           goto [ x_ret_4 = empty ] next5 next6                       *)
              (Some next5, LBasic Skip);
              (* next5:    skip                                                         *)
              (Some next6, cmd_phi_xret5)
              (* next6:    x_ret_5 := PHI(x_ret_0, x_ret_1, x_ret_4)                    *);
            ]
      in
      let errs =
        errs2 @ errs_x2_v @ [ x4; xlf ] @ errs1 @ [ x5 ] @ errs3 @ errs_x3_v
      in
      let cmds = annotate_first_cmd cmds in
      (cmds, PVar x_ret_5, errs, rets3, outer_breaks, outer_conts)
  | JS_Parser.Syntax.For (e1, e2, e3, e4) ->
      (*
     Section 12.6.3
       *  C(e1) = cmds1, x1; C(e2) = cmds2, x2; C(e3) = cmds3, _; C(e4) = cmds4, x4
       *
       *  C( for(e1; e2; e3) { e4 } ) =
              cmds1
          x1_v := i__getValue (x1) with err
          x_ret_0 := empty
      head:   x_ret_1 := PHI(x_ret_0, x_ret_3)
          cmds2
              x2_v := i__getValue (x2) with err
          x2_b := i__toBoolean (x2_v) with err
          goto [x2_b] body end_loop
      body:   cmds4
          x4_v := i__getValue (x4) with err
      cont:   x_ret_2 := PHI(cont_vars, x4_v)
          goto [ not (x_ret_2 = empty) ] next1 next2
        next1:  skip
      next2:  x_ret_3 := PHI(x_ret_1, x_ret_2)
              cmds3
          goto head
      end_loop: x_ret_4 := PHI(x_ret_1, break_vars)
              goto [ x_ret_4 = empty ] next3 next4
      next3:  skip
      next4:  x_ret_5 := PHI(x_ret_4, x_ret_1)
       *)
      let cmds1, x1, errs1 =
        match e1 with
        | Some e1 -> fe e1
        | None ->
            let x1_v, cmd_ass_x1v = make_empty_ass () in
            ([ annotate_cmd cmd_ass_x1v None ], PVar x1_v, [])
      in
      (* x1_v := i__getValue (x1) with err *)
      let x1_v, cmd_gv_x1, _ = make_get_value_call x1 tr_ctx.tr_err_lab in

      (* x_ret_0 := empty  *)
      let x_ret_0, cmd_ass_ret_0 = make_empty_ass () in

      let cmds1, errs1 =
        ( cmds1
          @ [ annotate_cmd cmd_gv_x1 None; annotate_cmd cmd_ass_ret_0 None ],
          errs1 @ [ x1_v ] )
      in

      let head, _, _, cont, end_loop = fresh_loop_vars () in

      let new_loop_list =
        (Some cont, end_loop, tr_ctx.tr_js_lab, true) :: tr_ctx.tr_loop_list
      in

      let new_loop_id = fresh_loop_identifier () in
      let new_loops = new_loop_id :: tr_ctx.tr_loops in
      let new_ctx =
        update_tr_ctx ~previous:None ~lab:None ~loop_list:new_loop_list
          ~loops:new_loops tr_ctx
      in
      let fe = translate_expr new_ctx in

      (* Set up the new annotation *)
      let metadata = Annot.set_loop_info metadata new_loops in
      let annotate_cmd = annotate_cmd_top_level metadata in
      let annotate_cmds = annotate_cmds_top_level metadata in

      let cmds2, x2, errs2 =
        match e2 with
        | Some e2 -> fe e2
        | None ->
            let x2 = fresh_var () in
            let cmd_ass_x2 =
              annotate_cmd (None, LBasic (Assignment (x2, Lit (Bool true))))
            in
            ([ cmd_ass_x2 ], PVar x2, [])
      in

      let cmds3, _, errs3 =
        match e3 with
        | Some e3 -> fe e3
        | None ->
            let x3_v, cmd_ass_x3v = make_empty_ass () in
            ([ annotate_cmd (None, cmd_ass_x3v) ], PVar x3_v, [])
      in

      let cmds4, x4, errs4, rets4, breaks4, conts4 =
        translate_statement new_ctx e4
      in

      let cur_breaks, outer_breaks =
        filter_cur_jumps breaks4 tr_ctx.tr_js_lab true
      in
      let cur_conts, outer_conts =
        filter_cur_jumps conts4 tr_ctx.tr_js_lab true
      in

      (* head:     x_ret_1 := PHI(x_ret_0, x_ret_3)  *)
      let x_ret_1 = fresh_var () in
      let x_ret_2 = fresh_var () in
      let x_ret_3 = fresh_var () in
      let cmd_ass_ret_1 =
        LPhiAssignment [ (x_ret_1, [ PVar x_ret_0; PVar x_ret_3 ]) ]
      in

      (* x2_v := i__getValue (x2) with err *)
      let x2_v, cmd_gv_x2, errs_x2_v =
        make_get_value_call x2 new_ctx.tr_err_lab
      in

      (* x2_b := i__toBoolean (x2_v) with err2 *)
      let x2_b, cmd_tb_x2 = make_to_boolean_call x2 x2_v new_ctx.tr_err_lab in

      (* goto [x2_b] body end_loop *)
      let body = fresh_loop_body_label () in
      let cmd_for_goto = LGuardedGoto (PVar x2_b, body, end_loop) in

      (* x4_v := i__getValue (x4) with err *)
      let x4_v, cmd_gv_x4, errs_x4_v =
        make_get_value_call x4 new_ctx.tr_err_lab
      in

      (* cont:     x_ret_2 := PHI(cont_vars, x4_v)  *)
      let cur_conts = cur_conts @ [ x4_v ] in
      let cur_conts = List.map (fun x -> PVar x) cur_conts in
      let cmd_ass_ret_2 = LPhiAssignment [ (x_ret_2, cur_conts) ] in

      (*  goto [ not (x_ret_2 = empty) ] next1 next2  *)
      let next1 = fresh_next_label () in
      let next2 = fresh_next_label () in
      let expr_goto_guard = BinOp (PVar x_ret_2, Equal, Lit Empty) in
      let expr_goto_guard = UnOp (UNot, expr_goto_guard) in
      let cmd_goto_empty_test = LGuardedGoto (expr_goto_guard, next1, next2) in

      (* next2:    x_ret_3 := PHI(x_ret_1, x_ret_2) *)
      let cmd_ass_ret_3 =
        LPhiAssignment [ (x_ret_3, [ PVar x_ret_1; PVar x_ret_2 ]) ]
      in

      let cmds_end_loop, x_ret_5 =
        make_loop_end x_ret_1 x_ret_1 cur_breaks end_loop true
      in

      let cmds4 = add_initial_label cmds4 body metadata in

      (* Place invariant exactly where it's supposed to be *)
      let head_cmds =
        match invariant with
        | Some (a, binders) ->
            [
              (Some head, LabCmd.LLogic (LCmd.SL (Invariant (a, binders))));
              (None, cmd_ass_ret_1);
            ]
        | None -> [ (Some head, cmd_ass_ret_1) ]
      in

      let cmds =
        cmds1 @ annotate_cmds head_cmds @ cmds2
        @ annotate_cmds
            [
              (*              cmds2                                        *)
              (None, cmd_gv_x2);
              (*              x2_v := i__getValue (x2) with err            *)
              (None, cmd_tb_x2);
              (*              x2_b := i__toBoolean (x2_v) with err         *)
              (None, cmd_for_goto)
              (*              goto [x2_b] body end                         *);
            ]
        @ cmds4
        @ annotate_cmds
            [
              (* body:        cmds4                                        *)
              (None, cmd_gv_x4);
              (*              x4_v := i__getValue (x4) with err            *)
              (Some cont, cmd_ass_ret_2);
              (* cont:        x_ret_2 := PHI(cont_vars, x4_v)              *)
              (None, cmd_goto_empty_test);
              (*              goto [ not (x_ret_2 = empty) ] next1 next2 *)
              (Some next1, LBasic Skip);
              (* next1:       skip                                         *)
              (Some next2, cmd_ass_ret_3)
              (* next2:       x_ret_3 := PHI(x_ret_1, x_ret_2)             *);
            ]
        @ cmds3
        @ annotate_cmds
            [
              (*              cmds3                                        *)
              (None, LGoto head)
              (*              goto head                                    *);
            ]
        @ annotate_cmds cmds_end_loop
      in
      let errs =
        errs1 @ errs2 @ errs_x2_v @ [ x2_b ] @ errs4 @ errs_x4_v @ errs3
      in
      let cmds = prefix_lcmds lcmds None cmds in
      (cmds, PVar x_ret_5, errs, rets4, outer_breaks, outer_conts)
  | JS_Parser.Syntax.Return e -> (
      (*
      Section 12.9

      C(return) =
            x_r := undefined;
            goto ret_lab

      C(e) = cmds, x
      ---------------------------
      C(return e) =
        cmds
        x_r := i__getValue(x) with err
        goto ret_lab
    *)
      (* When we hit a return, we automatically exit all loops *)
      let new_ctx = update_tr_ctx ~loops:[] tr_ctx in
      let metadata = Annot.set_loop_info metadata [] in
      let annotate_cmd cmd lab = annotate_cmd_top_level metadata (lab, cmd) in
      match e with
      | None ->
          let x_r = fresh_var () in
          (* x_r := undefined *)
          let cmd_xr_ass =
            annotate_cmd (LBasic (Assignment (x_r, Lit Undefined))) None
          in
          (* goto lab_ret *)
          let cmd_goto_ret = annotate_cmd (LGoto new_ctx.tr_ret_lab) None in
          let cmds = [ cmd_xr_ass; cmd_goto_ret ] in
          let cmds = annotate_first_cmd cmds in
          (cmds, PVar x_r, [], [ x_r ], [], [])
      | Some e ->
          let cmds, x, errs = translate_expr new_ctx e in
          (* x_r := i__getValue(x) with err *)
          let x_r, cmd_gv_x, errs_x_r =
            make_get_value_call x new_ctx.tr_err_lab
          in
          let cmd_gv_x = annotate_cmd cmd_gv_x None in
          (* goto ret_lab *)
          let cmd_goto_ret = annotate_cmd (LGoto new_ctx.tr_ret_lab) None in
          let cmds = cmds @ [ cmd_gv_x; cmd_goto_ret ] in
          let cmds = annotate_first_cmd cmds in
          (cmds, PVar x_r, errs @ errs_x_r, [ x_r ], [], []))
  | JS_Parser.Syntax.Continue lab ->
      (*
      Section 12.7

      find_continue_lab (lab) = jsil_lab
      ---------------------------
      C(continue lab) =
            x_r := empty;
            goto jsil_lab

      next_continue_lab () = jsil_lab
      ---------------------------
      C(continue) =
            x_r := empty;
            goto jsil_lab
    *)
      let x_r, cmd_ret =
        match tr_ctx.tr_previous with
        | None ->
            let x_r, cmd = make_empty_ass () in
            (x_r, [ annotate_cmd cmd None ])
        | Some (Lit lit) ->
            let x_r = fresh_var () in
            let cmd = LBasic (Assignment (x_r, Lit lit)) in
            (x_r, [ annotate_cmd cmd None ])
        | Some (PVar x) -> (x, [])
        | Some _ ->
            raise
              (Failure
                 "Continue: The return of the compilation must be either a \
                  variable or a literal")
      in

      (* goto lab_c *)
      let lab_c = get_continue_lab tr_ctx.tr_loop_list lab in
      let cmd_goto = [ annotate_cmd (LGoto lab_c) None ] in

      let cmds = cmd_ret @ cmd_goto in
      let cmds = annotate_first_cmd cmds in
      (cmds, PVar x_r, [], [], [], [ (lab, x_r, lab_c) ])
  | JS_Parser.Syntax.Break lab ->
      (*
        Section 12.8
          x_r := empty;
          goto lab_r
    *)
      let x_r, cmd_ret =
        match tr_ctx.tr_previous with
        | None ->
            let x_r, cmd = make_empty_ass () in
            (x_r, [ annotate_cmd cmd None ])
        | Some (Lit lit) ->
            let x_r = fresh_var () in
            let cmd = LBasic (Assignment (x_r, Lit lit)) in
            (x_r, [ annotate_cmd cmd None ])
        | Some (PVar x) -> (x, [])
        | Some _ ->
            raise
              (Failure
                 "Continue: The return of the compilation must be either a \
                  variable or a literal")
      in

      (* goto lab_r *)
      let lab_r = get_break_lab tr_ctx.tr_loop_list lab in
      let cmd_goto = [ annotate_cmd (LGoto lab_r) None ] in

      let cmds = cmd_ret @ cmd_goto in
      let cmds = annotate_first_cmd cmds in
      (cmds, PVar x_r, [], [], [ (lab, x_r, lab_r) ], [])
  | JS_Parser.Syntax.Label (js_lab, e) ->
      (* Section 12.12 *)
      let new_ctx = update_tr_ctx ~lab:(Some js_lab) tr_ctx in
      translate_statement new_ctx e
  | JS_Parser.Syntax.Throw e ->
      (*
     Section 12.13 - The throw statement

     C(e) = cmds, x
     ----------------------------
     C(throw e) =
        cmds
            x_v := i__getValue (x) with err
        goto err
    *)
      let cmds, x, errs = fe e in
      let x_v, cmd_gv_x, errs_x_v = make_get_value_call x tr_ctx.tr_err_lab in
      let cmd_gv_x = [ annotate_cmd cmd_gv_x None ] in

      (* goto err  *)
      let cmd_goto = [ annotate_cmd (LGoto tr_ctx.tr_err_lab) None ] in

      let cmds =
        cmds (*  cmds                            *) @ cmd_gv_x
        @ (*  x_v := i__getValue (x) with err *)
        cmd_goto
      in

      (*  goto err                        *)
      let cmds = annotate_first_cmd cmds in
      (cmds, Lit Empty, errs @ errs_x_v @ [ x_v ], [], [], [])
  | JS_Parser.Syntax.Try (e1, Some (x, e2), Some e3) ->
      (*
      Section 12.14 - The try Statement
     C(e1) = cmds1, x1; C(e2) = cmds2, x2; C(e3) = cmds3, x3
     -----------------------------------------------------------
      C(try { e1 } catch^{cid}(x) { e2 } finally { e3 } =
              cmds1
                    goto finally
          err1:     x_err := PHI(errs1)
                  x_er := new ()
              [x_er, "x"] := x_err
              [x_scope, "cid"] := x_er
              cmds2
              goto finally
        err2:     x_ret_1 := PHI(errs2)
        finally:    x_ret_2 := PHI(breaks1, x_1, breaks2, x_2, x_ret_1)
                    cmds3
          end:        x_ret_3 := PHI(breaks3, x_ret_2)
     *)
      let catch_id =
        try JS2JSIL_Preprocessing.get_codename e
        with _ ->
          raise
            (Failure
               "catch statements must be annotated with their respective code \
                names - try - catch - finally")
      in
      let cmds, x, errs, rets, breaks, conts =
        make_try_catch_cmds_with_finally e1 (x, e2) catch_id e3
      in
      let cmds = annotate_first_cmd cmds in
      (cmds, x, errs, rets, breaks, conts)
  | JS_Parser.Syntax.Try (e1, None, Some e3) ->
      (*
     Section 12.14 - The try Statement
     C(e1) = cmds1, x1; C(e3) = cmds3, x3
     -----------------------------------------------------------
      C(try { e1 } finally { e3 } =
              cmds1
              goto finally
        err:      x_ret_1 := PHI(errs1)
        finally:  x_ret_2 := PHI(cur_breaks1, x_1, x_ret_1)
                  cmds3
        end:        x_ret_3 := PHI(cur_breaks3, x_ret_2)
     *)
      let cmds, x, errs, rets, breaks, conts = make_try_finally_cmds e1 e3 in
      let cmds = annotate_first_cmd cmds in
      (cmds, x, errs, rets, breaks, conts)
  | JS_Parser.Syntax.Try (e1, Some (x, e2), None) ->
      (*
     Section 12.14 - The try Statement
     C(e1) = cmds1, x1; C(e2) = cmds2, x2;
     -----------------------------------------------------------
      C(try { e1 } catch^{cid}(x) { e2 } =
            cmds1
                  goto finally
        err:      x_err := PHI(errs1)
                x_er := new ()
            [x_er, "x"] := x_err
            [x_scope, "cid"] := x_er
            cmds2
      finally:    x_ret_1 := PHI(breaks1, x_1, breaks2, x_2)
     *)
      let catch_id =
        try JS2JSIL_Preprocessing.get_codename e
        with _ ->
          raise
            (Failure
               "catch statements must be annotated with their respective code \
                names - try - catch - finally")
      in
      let cmds12, x_ret_1, errs12, rets12, breaks12, conts12, _ =
        make_try_catch_cmds e1 (x, e2) catch_id
      in
      let cmds = annotate_first_cmd cmds12 in
      (cmds, PVar x_ret_1, errs12, rets12, breaks12, conts12)
  | JS_Parser.Syntax.Switch (e, xs) -> (
      (*
      Section

      a_case = e_c, e_s
      C(e_c) = cmds1, x1
      C(e_s) = cmds2, x2
      --------------------------------------------------------
      C_case ( a_case, x_prev_found, x_switch_guard ) =
                    goto [ not x_prev_found ] next1 next2
        next1:      cmds1
                    x1_v := getValue (x1) with err
              goto [ x1_v = x_switch_guard ] next2 end_case
                    cmds2
        end_case:   x_found := PHI(x_false, x_true)
                    x_case := PSI(x_prev_case, x_2)



      C_case ( a_case ) = cmds1, x_prev_1
      C_a_cases ( a_cases ) = cmds2, x_prev_2
      --------------------------------------------------------
      C_cases ( a_case :: a_cases, x_prev, x_switch_guard ) =
                cmds1
            cmds2


      C(s) = cmds_def, x_def
      C(b_stmt_i) = cmds_i, x_i, for all b_stmt_i \in b_stmts
      ---------------------------------------------------------
      C_default ( s, b_stmts, x_found_b, breaks_a) =
                      cmds_def
                goto [ not (x_found_b) ] next end_switch
          next:         cmds_1
                      ...
                cmds_n
          end_switch:   x_r := PHI(breaks_ab, breaks_def, x_def, breaks_b, x_n)



      C(e) = cmds_guard, x_guard
      C_cases (a_cases, x_found, x_guard_v) = cmds_a, x_found_a, x_a
      C_cases (b_cases, x_found_a, x_guard_v) = cmds_b, x_found_b, x_b
      C_defautl (default_case, b_stmts(b_cases), x_found_b) = cmds_default
      ------------------------------------------------------
      C(switch(e) { a_cases, default_case, b_cases} =
                    cmds_guard
              x_guard_v := i__getValue (x_guard) with err
              cmds_a
              goto [ x_found_a ] default b_cases
        b_cases:  cmds_b
        default:  x_found_b := PHI(x_false, x_false, x_true)
                    cmds_default

     *)
      let compile_case
          e
          s
          x_prev_found
          x_prev_case
          x_switch_guard
          end_switch
          _
          fresh_end_case_label =
        let x_found = fresh_found_var () in
        let next1 = fresh_next_label () in
        let next2 = fresh_next_label () in

        let new_loop_list =
          (None, end_switch, tr_ctx.tr_js_lab, true) :: tr_ctx.tr_loop_list
        in
        let new_ctx =
          update_tr_ctx ~previous:None ~lab:None ~loop_list:new_loop_list tr_ctx
        in
        let cmds1, x1, errs1, _, _, _ = translate_statement new_ctx e in
        let cmds1 = add_initial_label cmds1 next1 metadata in

        let cmds2, x2, errs2, rets2, breaks2, conts2 =
          translate_statement new_ctx s
        in
        let cmds2, x2 = add_final_var cmds2 x2 metadata in
        let cmds2 = add_initial_label cmds2 next2 metadata in

        (* goto [ not x_prev_found ] next1 next2 *)
        let cmd_goto_1 =
          LGuardedGoto (UnOp (UNot, PVar x_prev_found), next1, next2)
        in

        (* x1_v := getValue (x1) with err *)
        let x1_v, cmd_gv_x1, errs_x1_v =
          make_get_value_call x1 tr_ctx.tr_err_lab
        in

        (* goto [ x1_v = x_switch_guard ] next2 end_case *)
        (* let next1 = fresh_next_label () in *)
        let end_case = fresh_end_case_label () in
        let cmd_goto_2 =
          LGuardedGoto
            (BinOp (PVar x1_v, Equal, PVar x_switch_guard), next2, end_case)
        in

        (* x_found_2 := PHI(x_false, x_true)  *)
        let x_case = fresh_case_var () in
        let cmd_ass_xfound =
          LPhiAssignment
            [
              (x_found, [ Lit (Bool false); Lit (Bool true) ]);
              (x_case, [ PVar x_prev_case; PVar x2 ]);
            ]
        in

        let cmds =
          annotate_cmds
            [
              (None, cmd_goto_1)
              (*           goto [ not x_prev_found ] next1 next2          *);
            ]
          @ cmds1
          @ annotate_cmds
              [
                (* next1:    cmds1                                          *)
                (None, cmd_gv_x1);
                (*           x1_v := getValue (x1) with err                 *)
                (None, cmd_goto_2)
                (*           goto [ x1_v = x_switch_guard ] next2 end_case  *);
              ]
          @ cmds2
          @ annotate_cmds
              [
                (* next2:    cmds2                                          *)
                (Some end_case, cmd_ass_xfound)
                (* end_case: x_found := PHI(x_false, x_true)                *);
              ]
        in
        let errs = errs1 @ errs_x1_v @ errs2 in
        (cmds, x_case, errs, rets2, breaks2, conts2, x_found)
      in

      let compile_default
          s
          b_stmts
          x_old_b
          x_found_b
          end_switch
          js_lab
          cur_breaks_ab =
        let new_loop_list =
          (None, end_switch, js_lab, true) :: tr_ctx.tr_loop_list
        in
        let new_ctx =
          update_tr_ctx ~loop_list:new_loop_list ~previous:None ~lab:None tr_ctx
        in
        let f_default = translate_statement new_ctx in

        let cmds_def, x_def, errs_def, rets_def, breaks_def, conts_def =
          f_default s
        in
        let cmds_def, x_def = add_final_var cmds_def x_def metadata in
        let cmds_b, x_b, errs_b, rets_b, breaks_b, conts_b =
          List.fold_left
            (fun (cmds_ac, _, errs_ac, rets_ac, breaks_ac, conts_ac) b_stmt ->
              let ( cur_b_cmds,
                    x_b,
                    cur_b_errs,
                    cur_b_rets,
                    cur_b_breaks,
                    cur_b_conts ) =
                f_default b_stmt
              in
              let cur_b_cmds, x_b = add_final_var cur_b_cmds x_b metadata in
              ( cmds_ac @ cur_b_cmds,
                x_b,
                errs_ac @ cur_b_errs,
                rets_ac @ cur_b_rets,
                breaks_ac @ cur_b_breaks,
                conts_ac @ cur_b_conts ))
            ([], x_def, [], [], [], [])
            b_stmts
        in

        let cur_breaks_b, outer_breaks_b =
          filter_cur_jumps breaks_b js_lab true
        in
        let cur_breaks_def, outer_breaks_def =
          filter_cur_jumps breaks_def js_lab true
        in

        (* goto [ not (x_found_b) ] next end_switch *)
        let next = fresh_next_label () in
        let cmd_goto =
          LGuardedGoto (UnOp (UNot, PVar x_found_b), next, end_switch)
        in
        let cmds_def = add_initial_label cmds_def next metadata in

        (* x_r := PHI(breaks_ab, x_ab, breaks_def, breaks_b, x_b) *)
        let x_r = fresh_var () in
        let phi_args : string list =
          cur_breaks_ab @ [ x_old_b ] @ cur_breaks_def @ cur_breaks_b @ [ x_b ]
        in
        let phi_args = List.map (fun x -> PVar x) phi_args in
        let cmd_ass_xr = LPhiAssignment [ (x_r, phi_args) ] in

        let cmds =
          [
            annotate_cmd cmd_goto None
            (*             goto [ not (x_found_b) ] next end_switch                *);
          ]
          @ cmds_def
          (* next:       cmds_def                                                *)
          @ cmds_b
          @ [
              (*             b_cmds                                                  *)
              annotate_cmd cmd_ass_xr (Some end_switch)
              (* end_switch: x_r := PHI(breaks_ab, x_ab, breaks_def, breaks_b, x_b)  *);
            ]
        in
        ( cmds,
          x_r,
          errs_def @ errs_b,
          rets_def @ rets_b,
          outer_breaks_def @ outer_breaks_b,
          conts_def @ conts_b )
      in

      let filter_cases cases =
        List.fold_left
          (fun (a_cases, def, b_cases) case ->
            match (case, def) with
            | (JS_Parser.Syntax.Case e, s), None ->
                ((e, s) :: a_cases, def, b_cases)
            | (JS_Parser.Syntax.DefaultCase, s), None ->
                (a_cases, Some s, b_cases)
            | (JS_Parser.Syntax.Case e, s), Some _ ->
                (a_cases, def, (e, s) :: b_cases)
            | (JS_Parser.Syntax.DefaultCase, _), Some _ ->
                raise (Failure "No two defaults for the same try"))
          ([], None, []) cases
      in

      let a_cases, def, b_cases = filter_cases xs in
      let a_cases, b_cases = (List.rev a_cases, List.rev b_cases) in
      let b_cases_lab, default_lab, end_switch, fresh_end_case_label =
        fresh_switch_labels ()
      in
      let x_found_init = fresh_found_var () in

      let cmds_guard, x_guard, errs_guard = fe e in
      (* x_guard_v := i__getValue (x_guard) with err  *)
      let x_guard_v, cmd_gv_xguardv, errs_x_guard_v =
        make_get_value_call x_guard tr_ctx.tr_err_lab
      in
      let cmd_gv_xguardv = annotate_cmd cmd_gv_xguardv None in
      (* x_found := false *)
      let cmd_x_found_init =
        annotate_cmd (LBasic (Assignment (x_found_init, Lit (Bool false)))) None
      in
      (* x_init_val := empty *)
      let x_init = fresh_var () in
      let cmd_val_init =
        annotate_cmd (LBasic (Assignment (x_init, Lit Empty))) None
      in

      let cmds_as, x_as, errs_as, rets_as, breaks_as, conts_as, x_found_as =
        List.fold_left
          (fun (cmds_ac, x_ac, errs_ac, rets_ac, breaks_ac, conts_ac, x_found_ac)
               (e, s) ->
            let cmds_a, x_a, errs_a, rets_a, breaks_a, conts_a, x_found_a =
              compile_case e s x_found_ac x_ac x_guard_v end_switch
                tr_ctx.tr_js_lab fresh_end_case_label
            in
            ( cmds_ac @ cmds_a,
              x_a,
              errs_ac @ errs_a,
              rets_ac @ rets_a,
              breaks_ac @ breaks_a,
              conts_ac @ conts_a,
              x_found_a ))
          ([], x_init, [], [], [], [], x_found_init)
          a_cases
      in
      let cmds_as =
        cmds_guard
        @ [ cmd_gv_xguardv; cmd_x_found_init; cmd_val_init ]
        @ cmds_as
      in
      let errs_as = errs_guard @ errs_x_guard_v @ errs_as in

      let cmds_bs, x_bs, errs_bs, rets_bs, breaks_bs, conts_bs, x_found_bs =
        List.fold_left
          (fun (cmds_bc, x_bc, errs_bc, rets_bc, breaks_bc, conts_bc, x_found_bc)
               (e, s) ->
            let cmds_b, x_b, errs_b, rets_b, breaks_b, conts_b, x_found_b =
              compile_case e s x_found_bc x_bc x_guard_v end_switch
                tr_ctx.tr_js_lab fresh_end_case_label
            in
            ( cmds_bc @ cmds_b,
              x_b,
              errs_bc @ errs_b,
              rets_bc @ rets_b,
              breaks_bc @ breaks_b,
              conts_bc @ conts_b,
              x_found_b ))
          ([], x_as, [], [], [], [], x_found_as)
          b_cases
      in

      match (b_cases, def) with
      | [], None ->
          (*  end_switch: x_r := PHI(breaks_a, x_a) *)
          let x_r = fresh_var () in
          let cur_breaks_as, outer_breaks_as =
            filter_cur_jumps breaks_as tr_ctx.tr_js_lab true
          in
          let phi_args = cur_breaks_as @ [ x_as ] in
          let phi_args = List.map (fun x -> PVar x) phi_args in
          let cmd_end_switch =
            annotate_cmd (LPhiAssignment [ (x_r, phi_args) ]) (Some end_switch)
          in
          let cmds = cmds_as @ [ cmd_end_switch ] in
          let cmds = annotate_first_cmd cmds in
          (cmds, PVar x_r, errs_as, rets_as, outer_breaks_as, conts_as)
      | [], Some def ->
          let new_loop_list =
            (None, end_switch, tr_ctx.tr_js_lab, true) :: tr_ctx.tr_loop_list
          in
          let new_ctx =
            update_tr_ctx ~previous:None ~lab:None ~loop_list:new_loop_list
              tr_ctx
          in
          let f_default = translate_statement new_ctx in
          let cmds_def, x_def, errs_def, rets_def, breaks_def, conts_def =
            f_default def
          in
          let cmds_def, x_def = add_final_var cmds_def x_def metadata in

          (*  end_switch: x_r := PHI(breaks_a, breaks_def, x_def) *)
          let x_r = fresh_var () in
          let cur_breaks_as, outer_breaks_as =
            filter_cur_jumps breaks_as new_ctx.tr_js_lab true
          in
          let cur_breaks_def, outer_breaks_def =
            filter_cur_jumps breaks_def new_ctx.tr_js_lab true
          in
          let phi_args = cur_breaks_as @ cur_breaks_def @ [ x_def ] in
          let phi_args = List.map (fun x -> PVar x) phi_args in
          let cmd_end_switch =
            annotate_cmd (LPhiAssignment [ (x_r, phi_args) ]) (Some end_switch)
          in
          let cmds = cmds_as @ cmds_def @ [ cmd_end_switch ] in
          let cmds = annotate_first_cmd cmds in
          ( cmds,
            PVar x_r,
            errs_as @ errs_def,
            rets_as @ rets_def,
            outer_breaks_as @ outer_breaks_def,
            conts_as @ conts_def )
      | _, Some def ->
          let b_stmts = List.map fst b_cases in
          let cmds_bs = add_initial_label cmds_bs b_cases_lab metadata in

          (* goto [ x_found_a ] default b_cases *)
          let cmd_goto_xfounda =
            annotate_cmd
              (LGuardedGoto (PVar x_found_as, default_lab, b_cases_lab))
              None
          in

          (* default:   x_found_b := PHI(x_false, x_found_b) *)
          let x_found_b = fresh_xfoundb_var () in
          let cmd_ass_xfoundb =
            annotate_cmd
              (LPhiAssignment
                 [ (x_found_b, [ Lit (Bool false); PVar x_found_bs ]) ])
              (Some default_lab)
          in

          let cur_breaks_as, outer_breaks_as =
            filter_cur_jumps breaks_as tr_ctx.tr_js_lab true
          in
          let cur_breaks_bs, outer_breaks_bs =
            filter_cur_jumps breaks_bs tr_ctx.tr_js_lab true
          in
          let cur_breaks_ab = cur_breaks_as @ cur_breaks_bs in

          let cmds_def, x_def, errs_def, rets_def, outer_breaks_def, conts_def =
            compile_default def b_stmts x_bs x_found_b end_switch
              tr_ctx.tr_js_lab cur_breaks_ab
          in
          let cmds =
            cmds_as @ [ cmd_goto_xfounda ] @ cmds_bs @ [ cmd_ass_xfoundb ]
            @ cmds_def
          in
          let cmds = annotate_first_cmd cmds in
          ( cmds,
            PVar x_def,
            errs_as @ errs_bs @ errs_def,
            rets_as @ rets_bs @ rets_def,
            outer_breaks_as @ outer_breaks_bs @ outer_breaks_def,
            conts_as @ conts_bs @ conts_def )
      | _, _ -> raise (Failure "no b cases with no default"))
  | JS_Parser.Syntax.Function _ -> ([], Lit Empty, [], [], [], [])
  | JS_Parser.Syntax.With (_, _) ->
      raise (Failure "Not implemented: with (this should not happen)")
  | JS_Parser.Syntax.RegExp (_, _) ->
      raise (Failure "Not implemented: RegExp literal")
  | Try (_, None, None) -> raise (Failure "Try with missing parts")

let make_final_cmd vars final_lab final_var origin_loc =
  let cmd_final =
    match vars with
    | [] -> LBasic Skip
    | [ x ] -> LBasic (Assignment (final_var, PVar x))
    | _ ->
        let vars = List.map (fun x_r -> PVar x_r) vars in
        LPhiAssignment [ (final_var, vars) ]
  in
  (Annot.make ~origin_loc (), Some final_lab, cmd_final)

let translate_fun_decls (top_level : bool) (sc_var : string) (cur_index : int) e
    =
  let f_decls = func_decls_in_exp e in
  let hoisted_fdecls =
    List.fold_left
      (fun ac f_decl ->
        let f_name, f_params =
          match f_decl.JS_Parser.Syntax.exp_stx with
          | JS_Parser.Syntax.Function (_, Some f_name, f_params, _) ->
              (f_name, f_params)
          | _ -> raise (Failure "expected function declaration")
        in
        let f_id = JS2JSIL_Preprocessing.get_codename f_decl in
        let f_cmds, _, _ =
          translate_named_function_literal top_level sc_var f_name f_id f_params
            cur_index
        in
        ac @ f_cmds)
      [] f_decls
  in
  hoisted_fdecls

let generate_main e strictness spec : EProc.t =
  let origin_loc = JS_Utils.lift_flow_loc e.JS_Parser.Syntax.exp_loc in
  let annotate_cmd cmd lab = (Annot.make ~origin_loc (), lab, cmd) in

  let new_var = fresh_var () in
  let setup_heap_ass =
    annotate_cmd
      (LCall (new_var, Lit (String setupHeapName), [], None, None))
      None
  in
  let sc_var_main = JS2JSIL_Helpers.var_scope in

  (* x_sc := {{ $lg }} *)
  let init_scope_chain_ass =
    annotate_cmd
      (LBasic (Assignment (sc_var_main, Lit (LList [ Loc locGlobName ]))))
      None
  in

  let sc_var_main = JS2JSIL_Helpers.var_sc_first in

  (* x_sc_fst := {{ $lg }} *)
  let init_scope_chain_ass_again =
    annotate_cmd
      (LBasic (Assignment (sc_var_main, Lit (LList [ Loc locGlobName ]))))
      None
  in

  (* __this := $lg *)
  let this_ass =
    annotate_cmd (LBasic (Assignment (var_this, Lit (Loc locGlobName)))) None
  in

  (* global vars init assignments: [$lg, y] := {{ "d", undefined, true, true, true }} *)
  let global_var_asses =
    List.map
      (fun global_v ->
        annotate_cmd
          (LBasic
             (Mutation
                ( Lit (Loc locGlobName),
                  Lit (String global_v),
                  Lit
                    (LList
                       [
                         String "d"; Undefined; Bool true; Bool true; Bool false;
                       ]) )))
          None)
      (var_decls e)
  in

  (* x__te := TypeError () *)
  let cmd_ass_te = make_var_ass_te () in
  let cmd_ass_te = annotate_cmd cmd_ass_te None in

  (* x__te := SyntaxError () *)
  let cmd_ass_se = make_var_ass_se () in
  let cmd_ass_se = annotate_cmd cmd_ass_se None in

  (* x__re := ReferenceError () *)
  let cmd_ass_re = make_var_ass_re () in
  let cmd_ass_re = annotate_cmd cmd_ass_re None in

  let ctx = make_translation_ctx main_fid [ main_fid ] sc_var_main strictness in
  let cmds_hoist_fdecls = translate_fun_decls true sc_var_main 0 e in
  let cmds_hoist_fdecls =
    annotate_cmds_top_level (Annot.make ~origin_loc ()) cmds_hoist_fdecls
  in

  let cmds_e, x_e, errs, _, _, _ = translate_statement ctx e in

  (* List.iter (fun ({ line_offset; invariant; pre_logic_cmds; post_logic_cmds }, _, _) ->
     Printf.printf "Length: pre: %d \t post: %d\n" (List.length pre_logic_cmds) (List.length post_logic_cmds)) cmds_e; *)

  (* x_ret := x_e *)
  let ret_ass =
    annotate_cmd (LBasic (Assignment (Names.return_variable, x_e))) None
  in

  let x_ignore = fresh_var () in
  let cmd_del_te =
    annotate_cmd
      (LCall
         ( x_ignore,
           Lit (String JS2JSIL_Helpers.deleteObjAndMetadata),
           [ PVar var_te ],
           None,
           None ))
      None
  in
  let cmd_del_se =
    annotate_cmd
      (LCall
         ( x_ignore,
           Lit (String JS2JSIL_Helpers.deleteObjAndMetadata),
           [ PVar var_se ],
           None,
           None ))
      None
  in
  let cmd_del_re =
    annotate_cmd
      (LCall
         ( x_ignore,
           Lit (String JS2JSIL_Helpers.deleteObjAndMetadata),
           [ PVar var_re ],
           None,
           None ))
      None
  in

  (* lab_ret: skip *)
  let lab_ret_cmd = annotate_cmd LReturnNormal (Some ctx.tr_ret_lab) in

  let cmd_err_phi_node =
    make_final_cmd errs ctx.tr_err_lab Names.return_variable origin_loc
  in
  let lab_err_cmd = annotate_cmd LReturnError None in
  let global_err_asrt = annotate_cmd (LLogic (LCmd.Assert False)) None in
  let err_cmds =
    if !Javert_utils.Js_config.cosette then
      [ cmd_err_phi_node; global_err_asrt; lab_err_cmd ]
    else [ cmd_err_phi_node; lab_err_cmd ]
  in

  let main_cmds =
    [
      setup_heap_ass; init_scope_chain_ass; init_scope_chain_ass_again; this_ass;
    ]
    @ global_var_asses
    @ [ cmd_ass_te; cmd_ass_se; cmd_ass_re ]
    @ cmds_hoist_fdecls @ cmds_e
    @ [ ret_ass; cmd_del_te; cmd_del_se; cmd_del_re; lab_ret_cmd ]
    @ err_cmds
  in
  { name = main_fid; body = Array.of_list main_cmds; params = []; spec }

let generate_proc_eval new_fid ?use_cc e strictness vis_fid : EProc.t =
  let origin_loc = JS_Utils.lift_flow_loc e.JS_Parser.Syntax.exp_loc in
  let annotate_cmd cmd lab = (Annot.make ~origin_loc (), lab, cmd) in
  let annotate_cmds cmds =
    List.map (fun (lab, cmd) -> (Annot.make ~origin_loc (), lab, cmd)) cmds
  in
  let var_sc_proc = JS2JSIL_Helpers.var_sc_first in

  (* x_er_m := new (null)   *)
  (* x_er   := new (x_er_m) *)
  let x_er = JS2JSIL_Helpers.var_er in
  let x_erm = JS2JSIL_Helpers.var_er_metadata in
  let cmd_er_m_creation =
    annotate_cmd (LBasic (New (x_erm, None, Some (Lit Null)))) None
  in
  let cmd_er_creation =
    annotate_cmd (LBasic (New (x_er, None, Some (PVar x_erm)))) None
  in

  (* [x_er, "@er"] := true *)
  let cmd_er_flag =
    annotate_cmd
      (LBasic
         (Mutation (PVar x_erm, Lit (String _erFlagPropName), Lit (Bool true))))
      None
  in

  (* [x_er, decl_var_i] := undefined *)
  let new_fid_vars = var_decls e in
  let cmds_decls =
    List.map
      (fun decl_var ->
        let cmd =
          LBasic (Mutation (PVar x_er, Lit (String decl_var), Lit Undefined))
        in
        annotate_cmd cmd None)
      new_fid_vars
  in

  (* x_sc_0 = x_scope @ {{ x_er }}  *)
  let cmd_ass_er_to_sc =
    annotate_cmd
      (LBasic
         (Assignment
            ( var_sc_proc,
              NOp (LstCat, [ PVar var_scope; EList [ PVar var_er ] ]) )))
      None
  in

  (* x__te := TypeError () *)
  let cmd_ass_te = make_var_ass_te () in
  let cmd_ass_te = annotate_cmd cmd_ass_te None in
  (* x__te := SyntaxError () *)
  let cmd_ass_se = make_var_ass_se () in
  let cmd_ass_se = annotate_cmd cmd_ass_se None in

  let ctx = make_translation_ctx new_fid vis_fid var_sc_proc strictness in
  let ctx = update_tr_ctx ?use_cc ctx in

  let cmds_hoist_fdecls =
    translate_fun_decls false var_sc_proc (List.length vis_fid - 1) e
  in
  let cmds_hoist_fdecls = annotate_cmds cmds_hoist_fdecls in
  let cmds_e, x_e, errs, _, _, _ = translate_statement ctx e in

  (* x__re := ReferenceError () *)
  let cmd_ass_re = make_var_ass_re () in
  let cmd_ass_re = annotate_cmd cmd_ass_re None in

  let xe_v, cmd_gv_xe, errs_xe_v = make_get_value_call x_e ctx.tr_err_lab in
  let cmd_gv_xe = annotate_cmd cmd_gv_xe None in

  (* goto [x_new = empty] next1 next2 *)
  let next1 = fresh_next_label () in
  let next2 = fresh_next_label () in
  let cmd_is_empty =
    annotate_cmd
      (LGuardedGoto (BinOp (PVar xe_v, Equal, Lit Empty), next1, next2))
      None
  in
  (* next1: skip  *)
  let x_undef = fresh_var () in
  let cmd_undef =
    annotate_cmd (LBasic (Assignment (x_undef, Lit Undefined))) (Some next1)
  in

  (* next2: x := PHI(x_new, x_previous) *)
  let x_final = fresh_var () in
  let cmd_phi =
    annotate_cmd
      (LPhiAssignment [ (x_final, [ PVar xe_v; PVar x_undef ]) ])
      (Some next2)
  in

  (* x_ret := x_e *)
  let ret_ass =
    annotate_cmd
      (LBasic (Assignment (Names.return_variable, PVar x_final)))
      None
  in

  (* lab_ret: skip *)
  let lab_ret_cmd = annotate_cmd LReturnNormal None in

  (* lab_err: x_error := PHI(errs, x_fake_ret) *)
  let cmd_error_phi =
    make_final_cmd (errs @ errs_xe_v) ctx.tr_err_lab Names.return_variable
      origin_loc
  in
  let lab_err_cmd = annotate_cmd LReturnError None in

  let fid_cmds =
    [ cmd_er_m_creation; cmd_er_creation; cmd_er_flag ]
    @ cmds_decls
    @ [ cmd_ass_er_to_sc; cmd_ass_te; cmd_ass_se; cmd_ass_re ]
    @ cmds_hoist_fdecls @ cmds_e
    @ [
        cmd_gv_xe;
        cmd_is_empty;
        cmd_undef;
        cmd_phi;
        ret_ass;
        lab_ret_cmd;
        cmd_error_phi;
        lab_err_cmd;
      ]
  in
  {
    name = new_fid;
    body = Array.of_list fid_cmds;
    params = [ var_scope; var_this ];
    spec = None;
  }

let generate_proc ?use_cc e fid params strictness vis_fid spec : EProc.t =
  let origin_loc = JS_Utils.lift_flow_loc e.JS_Parser.Syntax.exp_loc in
  let annotate_cmd cmd lab = (Annot.make ~origin_loc (), lab, cmd) in

  let var_sc_proc = JS2JSIL_Helpers.var_sc_first in

  let ctx = make_translation_ctx fid vis_fid var_sc_proc strictness in
  let ctx = update_tr_ctx ?use_cc ctx in

  let new_ctx =
    {
      ctx with
      tr_ret_lab = "pre_" ^ ctx.tr_ret_lab;
      tr_err_lab = "pre_" ^ ctx.tr_err_lab;
    }
  in
  let cmds_hoist_fdecls =
    translate_fun_decls false var_sc_proc (List.length vis_fid - 1) e
  in
  let cmds_hoist_fdecls =
    annotate_cmds_top_level (Annot.make ~origin_loc ()) cmds_hoist_fdecls
  in

  (* x_er_m := new (null)   *)
  (* x_er   := new (x_er_m) *)
  let cmd_er_m_creation =
    annotate_cmd (LBasic (New (var_er_metadata, None, Some (Lit Null)))) None
  in
  let cmd_er_creation =
    annotate_cmd (LBasic (New (var_er, None, Some (PVar var_er_metadata)))) None
  in

  (* [x_er, "@er"] := true *)
  let cmd_er_flag =
    annotate_cmd
      (LBasic
         (Mutation
            (PVar var_er_metadata, Lit (String _erFlagPropName), Lit (Bool true))))
      None
  in

  (* [x_er, "arg_i"] := x_{i+2} *)
  let cmds_params =
    List.map
      (fun param ->
        let cmd =
          LBasic (Mutation (PVar var_er, Lit (String param), PVar param))
        in
        annotate_cmd cmd None)
      params
  in

  (* [x_er, decl_var_i] := undefined *)
  let cmds_decls =
    List.map
      (fun decl_var ->
        let cmd =
          LBasic (Mutation (PVar var_er, Lit (String decl_var), Lit Undefined))
        in
        annotate_cmd cmd None)
      (var_decls e)
  in

  (*
      CREATING THE ARGUMENTS OBJECT:
      x_argList_pre := args;
      x_argList_act := cdr (cdr (x_argList_pre));
      x_args := "create_arguments_object" (x_argList_act) with err;
      [x_er, "arguments"] := x_args;
  *)
  let x_argList_pre = fresh_var () in
  let x_argList_act = fresh_var () in
  let cmds_arg_obj =
    [
      (Annot.make ~origin_loc (), None, LArguments x_argList_pre);
      ( Annot.make ~origin_loc (),
        None,
        LBasic
          (Assignment (x_argList_act, UnOp (Cdr, UnOp (Cdr, PVar x_argList_pre))))
      );
      ( Annot.make ~origin_loc (),
        None,
        LCall
          ( var_args,
            Lit (String createArgsName),
            [ PVar x_argList_act ],
            None,
            None ) );
      ( Annot.make ~origin_loc (),
        None,
        LBasic (Mutation (PVar var_er, Lit (String "arguments"), PVar var_args))
      );
    ]
  in

  (* x_sc_0 = x_scope @ {{ x_er }} *)
  let cmd_ass_er_to_sc =
    annotate_cmd
      (LBasic
         (Assignment
            ( var_sc_proc,
              NOp (LstCat, [ PVar var_scope; EList [ PVar var_er ] ]) )))
      None
  in

  (* x__te := TypeError () *)
  let cmd_ass_te = make_var_ass_te () in
  let cmd_ass_te = annotate_cmd cmd_ass_te None in
  (* x__te := SyntaxError () *)
  let cmd_ass_se = make_var_ass_se () in
  let cmd_ass_se = annotate_cmd cmd_ass_se None in
  (* x__re := ReferenceError () *)
  let cmd_ass_re = make_var_ass_re () in
  let cmd_ass_re = annotate_cmd cmd_ass_re None in

  let cmds_e, _, errs, rets, _, _ = translate_statement new_ctx e in

  (* List.iter (fun ({ line_offset; invariant; pre_logic_cmds; post_logic_cmds }, _, _) ->
     Printf.printf "Length: pre: %d \t post: %d\n" (List.length pre_logic_cmds) (List.length post_logic_cmds)) cmds_e; *)

  (* x_dr := undefined *)
  let x_dr = fresh_var () in
  let cmd_dr_ass =
    annotate_cmd (LBasic (Assignment (x_dr, Lit Undefined))) None
  in
  let rets = rets @ [ x_dr ] in

  (* x_scope_final := x_scope *)
  let cmd_xscope_final =
    annotate_cmd
      (LBasic (Assignment (JS2JSIL_Helpers.var_scope_final, PVar var_sc_proc)))
      None
  in

  (* pre_lab_ret: x_return := PHI(...) *)
  let cmd_return_phi =
    make_final_cmd rets new_ctx.tr_ret_lab Names.return_variable origin_loc
  in

  let x_ignore = fresh_var () in
  let cmd_del_te =
    annotate_cmd
      (LCall
         ( x_ignore,
           Lit (String JS2JSIL_Helpers.deleteObjAndMetadata),
           [ PVar var_te ],
           None,
           None ))
      None
  in
  let cmd_del_se =
    annotate_cmd
      (LCall
         ( x_ignore,
           Lit (String JS2JSIL_Helpers.deleteObjAndMetadata),
           [ PVar var_se ],
           None,
           None ))
      None
  in
  let cmd_del_re =
    annotate_cmd
      (LCall
         ( x_ignore,
           Lit (String JS2JSIL_Helpers.deleteObjAndMetadata),
           [ PVar var_re ],
           None,
           None ))
      None
  in
  (* let cmd_del_args  = annotate_cmd (LCall (x_ignore, Lit (String JS2JSIL_Helpers.deleteObjAndMetadata), [ PVar var_args ], None)) None in *)
  let cmd_del_errs =
    annotate_cmd
      (LCall
         ( x_ignore,
           Lit (String JS2JSIL_Helpers.deleteErrorObjects),
           [ PVar Names.return_variable; PVar var_te; PVar var_se; PVar var_re ],
           None,
           None ))
      None
  in
  let cmd_ret_final = annotate_cmd LReturnNormal None in

  (*
  let cmds_restore_er_ret = generate_proc_er_restoring_code fid x_er_old ctx.tr_ret_lab in
  let cmds_restore_er_ret = annotate_cmds cmds_restore_er_ret in *)
  let errs = errs in
  let cmd_error_phi =
    make_final_cmd errs new_ctx.tr_err_lab Names.return_variable origin_loc
  in
  let cmd_err_final = annotate_cmd LReturnError None in

  let fid_cmds =
    [ cmd_er_m_creation; cmd_er_creation; cmd_er_flag ]
    @ cmds_decls @ cmds_params
    @ if_verification [] cmds_arg_obj
    @ [ cmd_ass_er_to_sc; cmd_ass_te; cmd_ass_se; cmd_ass_re ]
    @ cmds_hoist_fdecls @ cmds_e
    @ [ cmd_dr_ass; cmd_return_phi; cmd_del_te; cmd_del_se; cmd_del_re ]
    @ if_verification [] []
    @ [ cmd_xscope_final; cmd_ret_final; cmd_error_phi; cmd_xscope_final ]
    @ if_verification [] [ cmd_del_errs ]
    @ [ cmd_err_final ]
  in
  {
    name = fid;
    body = Array.of_list fid_cmds;
    params = var_scope :: var_this :: params;
    spec;
  }

(**** EVAL ****)
let js2jsil_eval
    (prog : ('a, int) Gillian.Gil_syntax.Prog.t)
    fid_parent
    strictness
    e =
  let prog, which_pred = (prog.procs, prog.predecessors) in

  let e, fid_eval, vislist_eval, eval_fun_tbl =
    JS2JSIL_Preprocessing.preprocess_eval cc_tbl vis_tbl strictness e fid_parent
      []
  in

  Hashtbl.iter
    (fun f_id (_, f_params, f_body, f_strictness, _) ->
      Option.iter
        (fun f_body ->
          let proc =
            if f_id = fid_eval then
              generate_proc_eval fid_eval ?use_cc:(Some false) e f_strictness
                vislist_eval
            else
              let vislist =
                try Hashtbl.find vis_tbl f_id
                with _ ->
                  let msg =
                    Printf.sprintf
                      "EV: Function %s not found in visibility table" f_id
                  in
                  raise (Failure msg)
              in
              let f_params =
                match f_params with
                | "x__scope" :: "x__this" :: rest -> rest
                | "x__scope" :: rest -> rest
                | _ -> f_params
              in
              generate_proc f_body f_id f_params f_strictness vislist None
          in
          L.verbose (fun m -> m "Eval proc to execute:@\n%a@\n" EProc.pp proc);
          let proc' = JSIL2GIL.jsil2core_proc proc in
          let proc'' = GProc.indexed_of_labeled proc' in
          Hashtbl.add prog f_id proc'';
          Preprocess_GCmd.extend_which_pred which_pred proc''.proc_body
            proc.name)
        f_body)
    eval_fun_tbl;

  let proc_eval =
    try Hashtbl.find prog fid_eval
    with _ -> raise (Failure "no eval proc was created")
  in
  proc_eval.proc_name

(* FUNCTION CONSTRUCTOR *)
let js2jsil_function_constructor_prop
    (prog : ('a, int) GProg.t)
    _
    params
    strictness
    e =
  let prog, which_pred = (prog.procs, prog.predecessors) in

  let _, new_fid, _, new_fun_tbl =
    JS2JSIL_Preprocessing.preprocess_eval cc_tbl vis_tbl strictness e
      !Config.entry_point params
  in

  Hashtbl.iter
    (fun f_id (_, f_params, f_body, f_strictness, _) ->
      Option.iter
        (function
          | f_body ->
              let proc =
                let vis_fid =
                  try Hashtbl.find vis_tbl f_id
                  with _ ->
                    let msg =
                      Printf.sprintf "Function %s not found in visibility table"
                        f_id
                    in
                    raise (Failure msg)
                in
                let f_params =
                  match f_params with
                  | "x__scope" :: "x__this" :: rest -> rest
                  | "x__scope" :: rest -> rest
                  | _ -> f_params
                in
                generate_proc f_body f_id f_params f_strictness vis_fid None
              in
              L.verbose (fun m ->
                  m "Function constructor proc to execute:@\n%a@\n" EProc.pp
                    proc);
              let proc' = JSIL2GIL.jsil2core_proc proc in
              let proc'' = GProc.indexed_of_labeled proc' in
              Hashtbl.replace prog f_id proc'';
              Preprocess_GCmd.extend_which_pred which_pred proc''.proc_body
                proc.name)
        f_body)
    new_fun_tbl;
  let proc_fun_constr =
    try Hashtbl.find prog new_fid
    with _ -> raise (Failure "no function constructor proc was created")
  in
  proc_fun_constr

let compute_imports (for_verification : bool) : string list =
  if for_verification then js2jsil_logic_imports
  else if ExecMode.biabduction_exec !Config.current_exec_mode then
    js2jsil_imports_bi
  else if ExecMode.symbolic_exec !Config.current_exec_mode then
    js2jsil_imports_cosette
  else js2jsil_imports

let js2jsil ~filename e for_verification =
  let e, only_specs, predicates, ids, imports =
    JS2JSIL_Preprocessing.preprocess cc_tbl fun_tbl vis_tbl e
  in
  let procedures = Hashtbl.create medium_tbl_size in

  Hashtbl.iter
    (fun f_id (_, f_params, f_body, f_strictness, spec) ->
      Option.fold
        ~some:(fun f_body ->
          (* print_normal (Printf.sprintf "Procedure %s is recursive?! %b" f_id f_rec); *)
          let proc =
            if f_id = main_fid then generate_main e f_strictness spec
            else
              let vis_fid =
                try Hashtbl.find vis_tbl f_id
                with _ ->
                  let msg =
                    Printf.sprintf "Function %s not found in visibility table"
                      f_id
                  in
                  raise (Failure msg)
              in
              generate_proc f_body f_id f_params f_strictness vis_fid spec
          in
          Hashtbl.add procedures f_id proc)
        ~none:() f_body)
    fun_tbl;

  let imports =
    List.map
      (fun (path, ver) ->
        (Filename.concat (Filename.dirname filename) path, ver))
      imports
    @ List.map (fun x -> (x, false)) (compute_imports for_verification)
  in

  (* TODO: Populate table with actual lemmas *)
  let lemmas = Lemma.init_tbl () in
  let macros = Macro.init_tbl () in
  let bispecs = BiSpec.init_tbl () in
  ( EProg.init imports lemmas predicates only_specs procedures macros bispecs
      (ids @ [ !Config.entry_point ]),
    cc_tbl,
    vis_tbl )
