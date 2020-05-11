%{
open Containers
open Parser_state

let normalised_lvar_r = Str.regexp "##NORMALISED_LVAR"
%}

(***** Token definitions *****)
(* Type literals *)
%token UNDERSCORE
%token UNDEFTYPELIT
%token NULLTYPELIT
%token EMPTYTYPELIT
%token NONETYPELIT
%token BOOLTYPELIT
%token INTTYPELIT
%token NUMTYPELIT
%token STRTYPELIT
%token OBJTYPELIT
%token LISTTYPELIT
%token TYPETYPELIT
%token SETTYPELIT
(* Constants *)
%token MIN_FLOAT
%token MAX_FLOAT
%token RANDOM
%token PI
%token UTCTIME
%token LOCALTIME
(* Literals *)
%token UNDEFINED
%token NULL
%token EMPTY
%token TRUE
%token FALSE
%token <float> FLOAT
%token NAN
%token INFINITY
%token <string> STRING
%token <string> LOC
%token LSTNIL
%token LSTOPEN
%token LSTCLOSE
(* PVariables *)
%token <string> VAR
(* Binary operators *)
%token EQ

%token FLT
%token FGT
%token FLE
%token FGE
%token FPLUS
%token FMINUS
%token FTIMES
%token FDIV
%token FMOD

%token ILT
%token IGT
%token ILE
%token IGE
%token IPLUS
%token IMINUS
%token ITIMES
%token IDIV
%token IMOD

%token SLT
%token AND
%token OR
%token BITWISEAND
%token BITWISEOR
%token BITWISEXOR
%token LEFTSHIFT
%token SIGNEDRIGHTSHIFT
%token UNSIGNEDRIGHTSHIFT
%token BITWISEANDL
%token BITWISEORL
%token BITWISEXORL
%token LEFTSHIFTL
%token SIGNEDRIGHTSHIFTL
%token UNSIGNEDRIGHTSHIFTL
%token M_ATAN2
%token M_POW
%token LSTCAT
%token LSTREV
%token STRCAT
(* Unary operators *)
(* Unary minus uses the same token as binary minus: FMINUS *)
%token NOT
%token BITWISENOT
%token M_ISNAN
%token M_ABS
%token M_ACOS
%token M_ASIN
%token M_ATAN
%token M_CEIL
%token M_COS
%token M_EXP
%token M_FLOOR
%token M_LOG
%token M_ROUND
%token M_SGN
%token M_SIN
%token M_SQRT
%token M_TAN
%token TOSTRING
%token TOINT
%token TOUINT16
%token TOINT32
%token TOUINT32
%token TONUMBER
%token CAR
%token CDR
%token SETTOLIST
%token LSTLEN
%token STRLEN
(* Expression keywords *)
%token TYPEOF
%token ASSUME
%token ASSERT
%token SEPASSERT
%token SEPAPPLY
%token INVARIANT
%token ASSUME_TYPE
%token SPEC_VAR
%token LSTNTH
%token LSTSUB
%token STRNTH
%token BIND
%token EXISTENTIALS
%token BRANCH
%token USESUBST
(* Command keywords  *)
%token SKIP
%token DEFEQ
%token ARGUMENTS
%token GOTO
%token WITH
%token APPLY
%token PHI
%token RETURN
%token THROW
%token EXTERN
(* Logic variables *)
%token <string> LVAR
(* Logical expressions *)
%token LNONE
%token <string> ALOC
(* Logic assertions *)
%token OASSERT
%token CASSERT
%token LAND
%token LOR
%token LNOT
%token LTRUE
%token LFALSE
%token LEQUAL
%token LLESSTHAN
%token LLESSTHANEQUAL
%token LSLESSTHAN
%token LEMP
(*%token LEXISTS *)
%token LFORALL
%token LTYPES
(* Logic predicates *)
%token PURE
%token PRED
(* Logic commands *)
%token OLCMD
%token CLCMD
%token FOLD
%token UNFOLD
%token UNFOLDALL
%token RECUNFOLD
%token LIF
%token LTHEN
%token LELSE
(* Procedure specification keywords *)
%token ONLY
%token SPEC
%token BISPEC
%token LEMMA
%token VARIANT
%token NORMAL
%token ERROR
%token FAIL
(* Procedure definition keywords *)
%token PROC
(* Others *)
%token IMPORT
%token MACRO
%token VERIFY
(* Directives *)
%token NO_PATH
%token INTERNAL
%token INTERNAL_FILE
(* Separators *)
%token DOT
%token COMMA
%token COLON
%token SCOLON
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token CLBRACKET
%token CRBRACKET
(* SETS *)
%token SETUNION
%token SETINTER
%token SETDIFF
%token SETMEM
%token SETSUB
%token LSETMEM
%token LSETSUB
%token SETOPEN
%token SETCLOSE
(* EOF *)
%token EOF

(***** Precedence of operators *****)
(* The later an operator is listed, the higher precedence it is given. *)
(* Logic operators have lower precedence *)
%nonassoc DOT
%left LOR
%left LAND
%left separating_conjunction
%right LNOT
%nonassoc LEQUAL LLESSTHAN LLESSTHANEQUAL LSLESSTHAN
%nonassoc SETMEM SETSUB LSETMEM LSETSUB
(* Program operators have higher precedence.*)
(* Based on JavaScript:
   https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Operators/Operator_Precedence *)
%left OR
%left AND
%nonassoc EQ
%nonassoc FLT FLE FGT FGE ILT ILE IGT IGE SLT
%left LEFTSHIFT SIGNEDRIGHTSHIFT UNSIGNEDRIGHTSHIFT LEFTSHIFTL SIGNEDRIGHTSHIFTL UNSIGNEDRIGHTSHIFTL
%left BITWISEOR BITWISEXOR BITWISEAND BITWISEXORL BITWISEORL BITWISEANDL
%left FPLUS FMINUS IPLUS IMINUS
%left FTIMES FDIV FMOD ITIMES IDIV IMOD M_POW
%left M_ATAN2 STRCAT SETDIFF

%nonassoc binop_prec
%nonassoc unop_prec

(***** Types and entry points *****)
%type <Literal.t>    lit_target
%type <Type.t>       type_target
%type <Constant.t>   constant_target
%type <UnOp.t>       unop_target
%type <BinOp.t>      binop_target
%type <NOp.t>        nop_target
%type <Formula.t>    pure_assertion_target

%type <(Annot.t, string) Prog.t> gmain_target
%type <Expr.t> top_level_expr_target

%type <Spec.st> g_sspec_target
%type <Asrt.t>  top_level_g_assertion_target
%type <string * string list> lab_spec_target

%start gmain_target
%start top_level_expr_target
%start top_level_g_assertion_target
%%

(********************************)
(********* Common Stuff *********)
(********************************)

import_target:
  IMPORT; imports = separated_nonempty_list(COMMA, STRING); SCOLON { imports }
;

import_verify_target:
  IMPORT; VERIFY; imports = separated_nonempty_list(COMMA, STRING); SCOLON { imports }
;

proc_head_target:
  PROC; proc_name = VAR; LBRACE; param_list = separated_list(COMMA, VAR); RBRACE
    { (proc_name, param_list) }
;

use_subst_target:
  | USESUBST; LBRACKET; lab=VAR; FMINUS; subst = separated_list(COMMA, lvar_le_pair_target); RBRACKET
      { (lab, subst) }
  | USESUBST; LBRACKET; lab=VAR RBRACKET
      { (lab, []) }
;

lvar_le_pair_target:
  lv = LVAR; COLON; e=expr_target { (lv, e )}
;

phi_target:
  v = VAR; COLON; args = separated_list(COMMA, expr_target)
    { (v, args) }

call_with_target:
  WITH; i=VAR { i }
;

(*
  [def1_id: #x1, #x2, #x3 ]
  [def1_id]
*)
assertion_id_target:
  | LBRACKET; v=VAR; RBRACKET
      { (v, []) }
  | LBRACKET; v=VAR; COLON; lvars=separated_nonempty_list(COMMA, LVAR); RBRACKET
      { (v, lvars) }
;

pred_param_target:
  (* Program variable with in-parameter status and optional type *)
  | in_param = option(FPLUS); v = VAR; t = option(preceded(COLON, type_target))
    { let in_param = Option.fold ~some:(fun _ -> true) ~none:false in_param in
      (v, t), in_param }
;

pred_head_target:
  name = VAR; LBRACE; params = separated_list(COMMA, pred_param_target); RBRACE;
  { (* Register the predicate declaration in the syntax checker *)
    let num_params = List.length params in
    let params, ins = List.split params in
    let param_names, _ = List.split params in
    let ins = List.map Option.get (List.filter (fun x -> x <> None) (List.mapi (fun i is_in -> if is_in then Some i else None) ins)) in
    let ins = if (List.length ins > 0) then ins else (List.mapi (fun i _ -> i) param_names) in
    (* register_predicate name num_params; *)
    (* enter_predicate params; *)
    (name, num_params, params, ins)
  }
;

(*******************************)
(********* Expressions *********)
(*******************************)

expr_target:
(* literal *)
  | lit=lit_target { Lit lit }
(* Logic variable *)
  | lvar = logic_variable_target
    { lvar }
(* Abstract locations are *normally* computed on normalisation *)
  | ALOC
    { ALoc $1 }
(* Program variable (including the special variable "ret") *)
  | pvar = program_variable_target
    { pvar }
(* e binop e *)
  | e1=expr_target; bop=binop_target; e2=expr_target
      { BinOp (e1, bop, e2) } %prec binop_prec
  | e1=expr_target; FGT; e2=expr_target
      { BinOp (e2, FLessThan, e1) }
  | e1=expr_target; FGE; e2=expr_target
      { BinOp (e2, FLessThanEqual, e1) }
(* unop e *)
    | uop=unop_target; e=expr_target
     { UnOp (uop, e) } %prec unop_prec
(* - e *)
(* Unary negation has the same precedence as logical not, not as binary negation. *)
  | IMINUS; e=expr_target
     { UnOp (IUnaryMinus, e) } %prec unop_prec
  | FMINUS; e=expr_target
     { UnOp (FUnaryMinus, e) } %prec unop_prec
(* {{ e, ..., e }} *)
  | LSTOPEN; exprlist = separated_nonempty_list(COMMA, expr_target); LSTCLOSE
     { EList exprlist }
(* -{- e, ..., e -}- *)
  | SETOPEN; exprlist = separated_list(COMMA, expr_target); SETCLOSE
     { ESet (Expr.Set.elements (Expr.Set.of_list exprlist)) }
(* l-nth (list, n) *)
  | LSTNTH; LBRACE; e1=expr_target; COMMA; e2=expr_target; RBRACE
     { BinOp (e1, LstNth, e2) }
  | LSTSUB; LBRACE; e1=expr_target; COMMA; e2=expr_target; COMMA; e3 = expr_target; RBRACE
    { LstSub (e1, e2, e3) }
(* nop le *)
  | nop=nop_target; LBRACE; les=separated_list(COMMA, expr_target); RBRACE
     {
        let les =
          match (nop : NOp.t) with
          | SetInter
          | SetUnion -> Expr.Set.elements (Expr.Set.of_list les)
          | _ -> les in
        NOp (nop, les)
     }
(* s-nth (string, n) *)
  | STRNTH; LBRACE; e1=expr_target; COMMA; e2=expr_target; RBRACE
     { BinOp (e1, StrNth, e2) }
(* (e) *)
    | LBRACE; e=expr_target; RBRACE
    { e }
(* Ignore variable *)
  | UNDERSCORE
    { LVar (LVar.alloc ()) }
;

top_level_expr_target:
  e = expr_target; EOF { e }
;

var_and_le_target:
  | LBRACE; lvar = LVAR; DEFEQ; le = expr_target; RBRACE;
    { (lvar, le) }
;


(***********************)
(********* GIL *********)
(***********************)

gmain_target:
  internal = option(INTERNAL_FILE);
  imports = option(import_target);
  imports_to_verify = option(import_verify_target);
  g_prog = gdeclaration_target;
  EOF
    {
      internal_file := Option.is_some internal;
      let imports = List.map (fun path -> (path, false))
        (Option.value ~default:[] imports)
      in
      let imports_to_verify = List.map (fun path -> (path, true))
        (Option.value ~default:[] imports_to_verify)
      in
      Prog.update_imports g_prog (imports @ imports_to_verify);
    }
;

gdeclaration_target:
  | prog = gdeclaration_target; lemma = g_lemma_target
    { Prog.add_lemma prog lemma }

  | lemma = g_lemma_target
    { let prog = Prog.create () in Prog.add_lemma prog lemma }

  | prog = gdeclaration_target; pred = g_pred_target
    { Prog.add_pred prog pred }

  | pred = g_pred_target
    { let prog = Prog.create () in Prog.add_pred prog pred }

  | prog = gdeclaration_target; proc = gproc_target
    { Prog.add_proc prog proc }

  | proc = gproc_target
    { let prog = Prog.create () in Prog.add_proc prog proc }

  | prog = gdeclaration_target; macro = g_macro_target
    { Prog.add_macro prog macro }

  | macro = g_macro_target
    { let prog = Prog.create () in Prog.add_macro prog macro }

  | prog = gdeclaration_target; ospec = g_only_spec_target
    { Prog.add_ospec prog ospec  }

  | ospec = g_only_spec_target
    { let prog = Prog.create () in Prog.add_ospec prog ospec }

  | prog = gdeclaration_target; bispec = g_bi_spec_target
    { Prog.add_bispec prog bispec }

  | bispec = g_bi_spec_target
    { let prog = Prog.create () in Prog.add_bispec prog bispec }
;

(* [spec;] proc xpto (x, y) { cmd_list }; *)
gproc_target:
  no_path = option(NO_PATH);
  internal = option(INTERNAL);
  proc_spec = option(g_spec_target);
  proc_head = proc_head_target;
  CLBRACKET;
  cmd_list = gcmd_list_target;
  CRBRACKET;
  SCOLON
    {
      let proc_name, proc_params = proc_head in
      let () =
        if Option.is_some no_path then
          procs_with_no_paths := SS.add proc_name !procs_with_no_paths
      in
      Proc.
        {
          proc_name;
          proc_source_path = None;
          proc_internal = Option.is_some internal;
          proc_body = Array.of_list cmd_list;
          proc_params;
          proc_spec;
        }
    }
;

gcmd_list_target:
  gcmd_list = separated_nonempty_list(SCOLON, gcmd_with_label)
    {
      List.map
        (fun (lab, gcmd) ->
          let annot : Annot.t = Annot.init () in
          annot, lab, gcmd)
    gcmd_list
  }
;

gcmd_with_label:
  | cmd = gcmd_target
    { None, cmd }
  | lab = VAR; COLON; cmd = gcmd_target
  { Some lab, cmd }
;

(*** GIL commands ***)
gcmd_target:
(* skip *)
  | SKIP { Skip }
(* x := [laction](e1, ..., en) *)
  | v=VAR; DEFEQ; LBRACKET; laction=VAR; RBRACKET; LBRACE; es=separated_list(COMMA, expr_target); RBRACE
    { LAction(v, laction, es) }
(* x := e *)
  | v=VAR; DEFEQ; e=expr_target
    { Assignment (v, e) }
(* goto i *)
  | GOTO; i=VAR
    { Goto i }
(* goto [e] i j *)
  | GOTO LBRACKET; e=expr_target; RBRACKET; i=VAR; j=VAR
    { GuardedGoto (e, i, j) }
(* x := e(e1, ..., en) with j use_subst [bla - #x: bla, #y: ble] *)
  | v=VAR; DEFEQ; e=expr_target;
    LBRACE; es=separated_list(COMMA, expr_target); RBRACE; oi = option(call_with_target); subst = option(use_subst_target)
    {
      Call (v, e, es, oi, subst)
    }
(* x := e(e1, ..., en) with j *)
  | v=VAR; DEFEQ; EXTERN; pname=VAR;
    LBRACE; es=separated_list(COMMA, expr_target); RBRACE; oi = option(call_with_target)
    {
      ECall (v, PVar pname, es, oi) }
(* x := apply (e1, ..., en) with j *)
  | v=VAR; DEFEQ; APPLY;
    LBRACE; es=expr_target; RBRACE; oi = option(call_with_target)
    { Apply (v, es, oi) }
(* x := args *)
  | v = VAR; DEFEQ; ARGUMENTS
    { Arguments v }
(* x := PHI(e1, e2, ... en); *)
  | PHI; LBRACE; phi_args =separated_list(SCOLON, phi_target); RBRACE
    {
      match phi_args with
      | [] -> raise (Failure "EMPTY PHI")
      | _  -> PhiAssignment phi_args
    }
(* return *)
  | RETURN { ReturnNormal }
  | THROW  { ReturnError  }
(* Logic command *)
  | lcmd = g_logic_cmd_target
    { Cmd.Logic lcmd }
(* fail *)
  | FAIL; LBRACKET; v=VAR; RBRACKET; LBRACE; es=separated_list(COMMA, expr_target); RBRACE
      { Cmd.Fail (v, es) }
;


g_only_spec_target:
(* only <spec> *)
  ONLY; spec = g_spec_target
  { spec }
;

g_spec_target:
(* spec xpto (x, y) [[ assertion ]] [[ post1; ...; postn ]] NORMAL|ERROR *)
  SPEC; spec_head = spec_head_target;
  spec_sspecs = separated_nonempty_list(SCOLON, g_sspec_target)
  { let (spec_name, spec_params) = spec_head in
    let spec_normalised = !Config.previously_normalised in
    let spec_to_verify = true in
    let spec : Spec.t = { spec_name; spec_params; spec_sspecs; spec_normalised; spec_to_verify } in
    spec
  }
;

g_spec_line:
  OASSERT; a = g_assertion_target; CASSERT { let a' : Asrt.t = a in a' }
;

g_mult_spec_line:
  OASSERT; asrts = separated_list(SCOLON, g_assertion_target); CASSERT
    { let asrts' : Asrt.t list = asrts in asrts'  }
;

g_sspec_target:
(* [spec_name: #bla, #ble, #bli] [[ .... ]] [[ .... ]] Normal *)
  | option(lab_spec_target) g_spec_line g_mult_spec_line NORMAL
    { { ss_pre = $2; ss_posts = $3; ss_flag = Normal; ss_to_verify = true; ss_label = $1 } }
(* [[ .... ]] [[ .... ]] Error *)
  | lab_spec = option(lab_spec_target); ss_pre = g_spec_line; ss_posts = g_mult_spec_line; ERROR
  {
    let spec : Spec.st = { ss_pre; ss_posts; ss_flag = Error; ss_to_verify = true; ss_label = lab_spec} in
    spec
  }
;

top_level_g_assertion_target:
  a = g_assertion_target; EOF { a }

g_assertion_target:
(* P * Q *)
(* The precedence of the separating conjunction is not the same as the arithmetic product *)
  | left_ass=g_assertion_target; FTIMES; right_ass=g_assertion_target
    { Asrt.Star (left_ass, right_ass) } %prec separating_conjunction
(* <GA>(es; es) *)
  | FLT; v=VAR; FGT; LBRACE; es1=separated_list(COMMA, expr_target); SCOLON; es2=separated_list(COMMA, expr_target); RBRACE
    { Asrt.GA (v, es1, es2) }
(* emp *)
  | LEMP;
    { Asrt.Emp }
(* x(e1, ..., en) *)
  | name = VAR; LBRACE; params = separated_list(COMMA, expr_target); RBRACE
    { (* validate_pred_assertion (name, params); *)
      Asrt.Pred (name, params)
    }
(* types (type_pairs) *)
  | LTYPES; LBRACE; type_pairs = separated_list(COMMA, type_env_pair_target); RBRACE
    { Asrt.Types type_pairs }
(* (P) *)
  | LBRACE; g_assertion_target; RBRACE
    { $2 }
(* pure *)
  | pure_assertion_target
    { Asrt.Pure $1 }
;

g_macro_target:
  MACRO; head = macro_head_def_target; COLON; macro_definition = separated_list(SCOLON, g_logic_cmd_target)
  {
    let (macro_name, macro_params) = head in
    let macro : Macro.t = { macro_name; macro_params; macro_definition } in
    macro
  }
;

g_named_assertion_target:
  id = option(assertion_id_target); a = g_assertion_target
  { (id, a) }
;

(* TODO: Check that the assertions are only predicates, or deal with full assertions in the execution *)
g_logic_cmd_target:
(* fold x(e1, ..., en) *)
  | FOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; fold_info = option(unfold_info_target)
    { SL (Fold (name, les, fold_info)) }

(* unfold x(e1, ..., en) [ def with #x := le1 and ... ] *)
  | UNFOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; unfold_info = option(unfold_info_target)
    { SL (Unfold (name, les, unfold_info, false)) }

(* unfold* x(e1, ..., en) [ def with #x := le1 and ... ] *)
  | RECUNFOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; unfold_info = option(unfold_info_target)
    { SL (Unfold (name, les, unfold_info, true)) }

(* unfold_all x *)
  | UNFOLDALL; name = VAR
    { SL (GUnfold name) }

(* invariant (a) [existentials: x, y, z] *)
  | INVARIANT; LBRACE; a = g_assertion_target; RBRACE; existentials = option(existentials_target)
    { SL (Invariant (a, Option.value ~default:[ ] existentials)) }

(* apply lemma_name(args) [bind: x, y ] *)
   | SEPAPPLY; lemma_name = VAR; LBRACE; params = separated_list(COMMA, expr_target); RBRACE; binders = option(binders_target)
    {
      let binders = Option.value ~default:[] binders in
      SL (ApplyLem (lemma_name, params, binders))
    }

(* assert_* (a) [bind: x, y, z] *)
  | SEPASSERT; LBRACE; a = g_assertion_target; RBRACE; binders = option(binders_target)
    { SL (SepAssert (a, Option.value ~default:[ ] binders)) }

(* if(le) { lcmd* } else { lcmd* } *)
  | LIF; LBRACE; le=expr_target; RBRACE; LTHEN; CLBRACKET;
      then_lcmds = separated_list(SCOLON, g_logic_cmd_target);
      CRBRACKET; LELSE; CLBRACKET;
      else_lcmds = separated_list(SCOLON, g_logic_cmd_target);
       CRBRACKET;
    { If (le, then_lcmds, else_lcmds)}

(* if(e) { lcmd* } *)
  | LIF; LBRACE; le=expr_target; RBRACE; LTHEN; CLBRACKET;
      then_lcmds = separated_list(SCOLON, g_logic_cmd_target);
      CRBRACKET;
    { If (le, then_lcmds, [])}

  | macro = macro_head_target;
    { let (name, params) = macro in Macro (name, params) }

(* assert (a) *)
  | ASSERT; LBRACE; a = pure_assertion_target; RBRACE
    { Assert a }

(* assume (a) *)
  | ASSUME; LBRACE; a = pure_assertion_target; RBRACE
    { Assume a }

(* assume_type (x, t) *)
  | ASSUME_TYPE; LBRACE; x=LVAR; COMMA; t=type_target; RBRACE
    { AssumeType (x, t) }

(* spec_var (x, t) *)
  | SPEC_VAR; LBRACE; xs = separated_list(COMMA, LVAR); RBRACE
    { SpecVar xs }

(* branch (fo) *)
  | BRANCH; LBRACE; fo = pure_assertion_target; RBRACE
     { Branch fo }
;

(* pred name (arg1, ..., argn) : def1, ..., defn ; *)
g_pred_target:
  no_path = option(NO_PATH);
  internal = option(INTERNAL);
  pure = option(PURE);
  PRED;
  pred_head = pred_head_target;
  COLON;
  pred_definitions = separated_nonempty_list(COMMA, g_named_assertion_target);
  SCOLON
  {
    let pred_pure = Option.is_some pure in
    let (pred_name, pred_num_params, pred_params, pred_ins) = pred_head in
    let () =
      if Option.is_some no_path then
        preds_with_no_paths := SS.add pred_name !preds_with_no_paths
    in
    let pred_normalised = !Config.previously_normalised in
    Pred.
      {
        pred_name;
        pred_source_path = None;
        pred_internal = Option.is_some internal;
        pred_num_params;
        pred_params;
        pred_ins;
        pred_definitions;
        pred_pure;
        pred_normalised;
      }
  }
;

lemma_variant_target:
  VARIANT LBRACE; variant = expr_target; RBRACE
  { variant }

lemma_head_target:
  lemma_name = VAR; LBRACE; lemma_params = separated_list(COMMA, VAR); RBRACE
  {
    (lemma_name, lemma_params)
  }

g_lemma_target:
  (* lemma xpto (x, y)
     variant(x)
     [[ pre ]]
     [[ post ]] [existentials: a, b, c]
     [* proof_body *] *)
  LEMMA; lemma_head = lemma_head_target; lemma_variant = option(lemma_variant_target);
    lemma_hyp = g_spec_line; lemma_concs = g_mult_spec_line; lemma_existentials=option(existentials_target); lemma_proof = option(g_lemma_proof_target);
  {
      let (lemma_name, lemma_params) = lemma_head in
      let lemma_existentials = Option.value ~default:[] lemma_existentials in
      let lemma : Lemma.t = { lemma_name; lemma_params; lemma_hyp; lemma_concs; lemma_variant; lemma_proof; lemma_existentials} in
      lemma
  }
;

g_lemma_proof_target:
  OLCMD; proof = separated_list(SCOLON, g_logic_cmd_target); CLCMD;
  { proof }
;

g_bi_spec_target:
(* bispec xpto (x, y) : [[ assertion; assertion... ]] *)
  BISPEC; spec_head = spec_head_target; COLON; asrts = g_mult_spec_line
  {
    let (name, params) = spec_head in
    let is_normalised = !Config.previously_normalised in
    let bi_spec : BiSpec.t =
      {
        bispec_name       = name;
        bispec_params     = params;
        bispec_pres       = asrts;
        bispec_normalised = is_normalised
      } in
    bi_spec
  }
;


macro_head_target:
 | name = VAR; LBRACE; params = separated_list(COMMA, expr_target); RBRACE
   { (name, params) }
;

(* <spec_name: #bla, #ble, #bli> *)
lab_spec_target:
  | FLT; sspec_name = VAR; COLON; lvars = separated_list (COMMA, LVAR); FGT
    { (sspec_name, lvars) }
  | FLT; sspec_name = VAR; FGT
    { (sspec_name, []) }
;

spec_head_target:
  spec_name = VAR; LBRACE; spec_params = separated_list(COMMA, VAR); RBRACE
  { (* enter_specs spec_params; *)
    (spec_name, spec_params)
  }
;

macro_head_def_target:
 | name = VAR; LBRACE; params = separated_list(COMMA, VAR); RBRACE
   { (name, params) }
;

(* [ def with #x := le1 and ... ] *)
unfold_info_target:
  | LBRACKET; id = VAR; WITH; var_les = separated_list(AND, var_and_le_target); RBRACKET
    { (id, var_les) }
;

binders_target:
  | LBRACKET; BIND; COLON; xs = separated_list(COMMA, LVAR); RBRACKET
    { xs }
;

existentials_target:
  | LBRACKET; EXISTENTIALS; COLON; xs = separated_list(COMMA, LVAR); RBRACKET
    { xs }
;

pure_assertion_target:
(* P /\ Q *)
  | left_ass=pure_assertion_target; LAND; right_ass=pure_assertion_target
    { And (left_ass, right_ass) }
(* P \/ Q *)
  | left_ass=pure_assertion_target; LOR; right_ass=pure_assertion_target
    { Or (left_ass, right_ass) }
(* ! Q *)
  | LNOT; ass=pure_assertion_target
    { Not (ass) }
(* true *)
  | LTRUE
    { True }
(* false *)
  | LFALSE
    { False }
(* E == E *)
  | left_expr=expr_target; LEQUAL; right_expr=expr_target
    { Eq (left_expr, right_expr) }
(* E <# E *)
  | left_expr=expr_target; LLESSTHAN; right_expr=expr_target
    { Less (left_expr, right_expr) }
(* E <=# E *)
  | left_expr=expr_target; LLESSTHANEQUAL; right_expr=expr_target
    { LessEq (left_expr, right_expr) }
(* E s<# E *)
  | left_expr=expr_target; LSLESSTHAN; right_expr=expr_target
    { StrLess (left_expr, right_expr) }
(* E --e-- E *)
  | left_expr=expr_target; LSETMEM; right_expr=expr_target
    { SetMem (left_expr, right_expr) }
(* E --s-- E *)
  | left_expr=expr_target; LSETSUB; right_expr=expr_target
    { SetSub (left_expr, right_expr) }
(* forall X, Y, Z . P *)
  | LFORALL; vars = separated_nonempty_list(COMMA, lvar_type_target); DOT; ass = pure_assertion_target
    { ForAll (vars, ass) }
(* (P) *)
  | LBRACE; f=pure_assertion_target; RBRACE
    { f }
;



lvar_type_target:
  | lvar = just_logic_variable_target; COLON; the_type = type_target
    { (lvar, Some the_type) }
  | lvar = just_logic_variable_target;
    { (lvar, None) }



type_env_pair_target:
  | lvar = logic_variable_target; COLON; the_type=type_target
    { (lvar, the_type) }
  | pvar = program_variable_target; COLON; the_type=type_target
    { (pvar, the_type) }
;

logic_variable_target:
  v = LVAR
  {
    let v_imported = Str.replace_first normalised_lvar_r "_lvar_n" v in
    (* Prefixed with _n_ to avoid clashes *)
    LVar v_imported }
;

just_logic_variable_target:
  v = LVAR
  { (* validate_lvar v; *) v }

program_variable_target:
  | v = VAR
    { (* let _ = validate_pvar v in *) Expr.PVar v }
;

(********* COMMON *********)

lit_target:
  | UNDEFINED                 { Undefined }
  | NULL                      { Null }
  | EMPTY                     { Empty }
  | constant_target           { Constant $1 }
  | TRUE                      { Bool true }
  | FALSE                     { Bool false }
  | FLOAT                     { Num $1 }
  | NAN                       { Num nan }
  | INFINITY                  { Num infinity }
  | STRING                    { String $1 }
  | LOC                       { Loc $1 }
  | type_target               { Type $1 }
  | LSTNIL                    { LList [] }
  | LSTOPEN LSTCLOSE          { LList [] }
  | LNONE                     { Nono }
;

nop_target:
  | SETUNION { NOp.SetUnion }
  | SETINTER { NOp.SetInter }
  | LSTCAT   { NOp.LstCat   }
;

binop_target:
  | EQ                  { Equal }
  | ILT                 { ILessThan }
  | ILE                 { ILessThanEqual }
  | IPLUS               { IPlus }
  | IMINUS              { IMinus }
  | ITIMES              { ITimes }
  | IDIV                { IDiv }
  | IMOD                { IMod }
  | FLT                 { FLessThan }
  | FLE                 { FLessThanEqual }
  | FPLUS               { FPlus }
  | FMINUS              { FMinus }
  | FTIMES              { FTimes }
  | FDIV                { FDiv }
  | FMOD                { FMod }
  | SLT                 { SLessThan }
  | AND                 { BAnd }
  | OR                  { BOr }
  | BITWISEAND          { BitwiseAnd }
  | BITWISEOR           { BitwiseOr}
  | BITWISEXOR          { BitwiseXor }
  | LEFTSHIFT           { LeftShift }
  | SIGNEDRIGHTSHIFT    { SignedRightShift }
  | UNSIGNEDRIGHTSHIFT  { UnsignedRightShift }
  | BITWISEANDL         { BitwiseAndL }
  | BITWISEORL          { BitwiseOrL }
  | BITWISEXORL         { BitwiseXorL }
  | LEFTSHIFTL          { LeftShiftL }
  | SIGNEDRIGHTSHIFTL   { SignedRightShiftL }
  | UNSIGNEDRIGHTSHIFTL { UnsignedRightShiftL }
  | M_ATAN2             { M_atan2 }
  | M_POW               { M_pow }
  | STRCAT              { StrCat }
  | SETDIFF             { SetDiff }
  | SETMEM              { BSetMem }
  | SETSUB              { BSetSub }
;

unop_target:
  (* Unary minus defined in (l)expr_target *)
  | NOT         { UNot }
  | BITWISENOT  { BitwiseNot }
  | M_ISNAN     { M_isNaN }
  | M_ABS       { M_abs }
  | M_ACOS      { M_acos }
  | M_ASIN      { M_asin }
  | M_ATAN      { M_atan }
  | M_CEIL      { M_ceil }
  | M_COS       { M_cos }
  | M_EXP       { M_exp }
  | M_FLOOR     { M_floor }
  | M_LOG       { M_log }
  | M_ROUND     { M_round }
  | M_SGN       { M_sgn }
  | M_SIN       { M_sin }
  | M_SQRT      { M_sqrt }
  | M_TAN       { M_tan }
  | TOSTRING    { ToStringOp }
  | TOINT       { ToIntOp }
  | TOUINT16    { ToUint16Op }
  | TOINT32     { ToInt32Op }
  | TOUINT32    { ToUint32Op }
  | TONUMBER    { ToNumberOp }
  | TYPEOF      { TypeOf }
  | CAR         { Car }
  | CDR         { Cdr }
  | LSTLEN      { LstLen }
  | LSTREV      { LstRev }
  | STRLEN      { StrLen }
  | SETTOLIST   { SetToList }
;

constant_target:
  | MIN_FLOAT { Min_float }
  | MAX_FLOAT { Max_float }
  | RANDOM    { Random }
  | PI        { Pi }
  | UTCTIME   { UTCTime }
  | LOCALTIME { LocalTime }
;

type_target:
  | UNDEFTYPELIT { UndefinedType }
  | NULLTYPELIT  { NullType }
  | EMPTYTYPELIT { EmptyType }
  | NONETYPELIT  { NoneType }
  | BOOLTYPELIT  { BooleanType }
  | INTTYPELIT   { IntType }
  | NUMTYPELIT   { NumberType }
  | STRTYPELIT   { StringType }
  | OBJTYPELIT   { ObjectType }
  | LISTTYPELIT  { ListType }
  | TYPETYPELIT  { TypeType }
  | SETTYPELIT   { SetType }
;

