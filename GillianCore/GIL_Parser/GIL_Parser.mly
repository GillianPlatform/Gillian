%{
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
%token <Z.t> INTEGER
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
%token INTTONUM
%token NUMTOINT
(* Expression keywords *)
%token TYPEOF
%token ASSUME
%token ASSERT
%token SEPASSERT
%token INVARIANT
%token ASSUME_TYPE
%token LSTNTH
%token LSTSUB
%token STRNTH
%token BIND
%token EXISTENTIALS
%token BRANCH
%token USESUBST
%token HIDES
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
%token ILLESSTHAN
%token ILLESSTHANEQUAL
%token FLLESSTHAN
%token FLLESSTHANEQUAL
%token LSLESSTHAN
%token LEMP
(*%token LEXISTS *)
%token LFORALL
%token LTYPES
(* Logic predicates *)
%token ABSTRACT
%token PURE
%token PRED
%token NOUNFOLD
%token FACTS
(* Logic commands *)
%token OLCMD
%token CLCMD
%token FOLD
%token UNFOLD
%token UNFOLDALL
%token RECUNFOLD
%token SYMBEXEC
%token LIF
%token LTHEN
%token LELSE
%token FRESH_SVAR
(* Procedure specification keywords *)
%token AXIOMATIC
%token INCOMPLETE
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
%nonassoc LEQUAL ILLESSTHAN ILLESSTHANEQUAL FLLESSTHAN FLLESSTHANEQUAL LSLESSTHAN
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
%start lit_target
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

proc_name:
  | proc_name = VAR { proc_name }
  | proc_name = STRING { proc_name }

proc_head_target:
  PROC; proc_name = proc_name; LBRACE; param_list = separated_list(COMMA, VAR); RBRACE
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
  | lit=lit_target { Expr.Lit lit }
(* Logic variable *)
  | lvar = logic_variable_target
    { lvar }
(* Abstract locations are *normally* computed on normalisation *)
  | ALOC
    { Expr.ALoc $1 }
(* Program variable (including the special variable "ret") *)
  | pvar = program_variable_target
    { pvar }
(* e binop e *)
  | e1=expr_target; bop=binop_target; e2=expr_target
      { Expr.BinOp (e1, bop, e2) } %prec binop_prec
  | e1=expr_target; FGT; e2=expr_target
      { Expr.BinOp (e2, FLessThan, e1) }
  | e1=expr_target; FGE; e2=expr_target
      { Expr.BinOp (e2, FLessThanEqual, e1) }
  | e1=expr_target; IGT; e2=expr_target
      { Expr.BinOp (e2, ILessThan, e1) }
  | e1=expr_target; IGE; e2=expr_target
      { Expr.BinOp (e2, ILessThanEqual, e1) }
(* unop e *)
    | uop=unop_target; e=expr_target
     { Expr.UnOp (uop, e) } %prec unop_prec
(* - e *)
(* Unary negation has the same precedence as logical not, not as binary negation. *)
  | IMINUS; e=expr_target
     { Expr.UnOp (IUnaryMinus, e) } %prec unop_prec
  | FMINUS; e=expr_target
     { Expr.UnOp (FUnaryMinus, e) } %prec unop_prec
(* {{ e, ..., e }} *)
  | LSTOPEN; exprlist = separated_nonempty_list(COMMA, expr_target); LSTCLOSE
     { Expr.EList exprlist }
(* -{- e, ..., e -}- *)
  | SETOPEN; exprlist = separated_list(COMMA, expr_target); SETCLOSE
     { Expr.ESet (Expr.Set.elements (Expr.Set.of_list exprlist)) }
(* l-nth (list, n) *)
  | LSTNTH; LBRACE; e1=expr_target; COMMA; e2=expr_target; RBRACE
     { Expr.BinOp (e1, LstNth, e2) }
  | LSTSUB; LBRACE; e1=expr_target; COMMA; e2=expr_target; COMMA; e3 = expr_target; RBRACE
    { Expr.LstSub (e1, e2, e3) }
(* nop le *)
  | nop=nop_target; LBRACE; les=separated_list(COMMA, expr_target); RBRACE
     {
        let les =
          match (nop : NOp.t) with
          | SetInter
          | SetUnion -> Expr.Set.elements (Expr.Set.of_list les)
          | _ -> les in
        Expr.NOp (nop, les)
     }
(* s-nth (string, n) *)
  | STRNTH; LBRACE; e1=expr_target; COMMA; e2=expr_target; RBRACE
     { Expr.BinOp (e1, StrNth, e2) }
(* (e) *)
    | LBRACE; e=expr_target; RBRACE
    { e }
(* Ignore variable *)
  | UNDERSCORE
    { Expr.LVar (LVar.alloc ()) }
;

top_level_expr_target:
  e = expr_target; EOF { e }
;

var_and_le_target:
  | LBRACE; lvar = LVAR; DEFEQ; le = expr_target; RBRACE;
    { (lvar, le) }
;

var_and_var_target:
  | LBRACE; lvar1 = LVAR; DEFEQ; lvar2 = LVAR; RBRACE;
    { (lvar1, lvar2) }
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
    { gcmd_list }
;

gcmd_with_label:
  | cmd = gcmd_with_annot
    { let annot, cmd = cmd in annot, None, cmd }
  | lab = VAR; COLON; cmd = gcmd_with_annot
    { let annot, cmd = cmd in  annot, Some lab, cmd }
;

gcmd_with_annot:
  | cmd = gcmd_target
    {
      let open Location in
      let open Lexing in
      let loc_start : Location.position =
        {
          pos_line = $startpos.pos_lnum;
          pos_column = $startpos.pos_cnum - $startpos.pos_bol;
        }
      in
      let loc_end : Location.position =
        {
          pos_line = $endpos.pos_lnum;
          pos_column = $endpos.pos_cnum - $endpos.pos_bol;
        }
      in
      let origin_loc : Location.t =
        {
          loc_start;
          loc_end;
          loc_source = $startpos.pos_fname;
        }
      in
      let annot : Annot.t = Annot.make ~origin_loc ()
      in annot, cmd
    };
(*** GIL commands ***)
gcmd_target:
(* skip *)
  | SKIP { Cmd.Skip }
(* x := [laction](e1, ..., en) *)
  | v=VAR; DEFEQ; LBRACKET; laction=VAR; RBRACKET; LBRACE; es=separated_list(COMMA, expr_target); RBRACE
    { Cmd.LAction(v, laction, es) }
(* x := e *)
  | v=VAR; DEFEQ; e=expr_target
    { Cmd.Assignment (v, e) }
(* goto i *)
  | GOTO; i=VAR
    { Cmd.Goto i }
(* goto [e] i j *)
  | GOTO LBRACKET; e=expr_target; RBRACKET; i=VAR; j=VAR
    { Cmd.GuardedGoto (e, i, j) }
(* x := e(e1, ..., en) with j use_subst [bla - #x: bla, #y: ble] *)
  | v=VAR; DEFEQ; e=expr_target;
    LBRACE; es=separated_list(COMMA, expr_target); RBRACE; oi = option(call_with_target); subst = option(use_subst_target)
    {
      Cmd.Call (v, e, es, oi, subst)
    }
(* x := e(e1, ..., en) with j *)
  | v=VAR; DEFEQ; EXTERN; pname=VAR;
    LBRACE; es=separated_list(COMMA, expr_target); RBRACE; oi = option(call_with_target)
    {
      Cmd.ECall (v, PVar pname, es, oi) }
(* x := apply (e1, ..., en) with j *)
  | v=VAR; DEFEQ; APPLY;
    LBRACE; es=expr_target; RBRACE; oi = option(call_with_target)
    { Cmd.Apply (v, es, oi) }
(* x := args *)
  | v = VAR; DEFEQ; ARGUMENTS
    { Cmd.Arguments v }
(* x := PHI(e1, e2, ... en); *)
  | PHI; LBRACE; phi_args =separated_list(SCOLON, phi_target); RBRACE
    {
      match phi_args with
      | [] -> raise (Failure "EMPTY PHI")
      | _  -> Cmd.PhiAssignment phi_args
    }
(* return *)
  | RETURN { Cmd.ReturnNormal }
  | THROW  { Cmd.ReturnError  }
(* Logic command *)
  | lcmd = g_logic_cmd_target
    { Cmd.Logic lcmd }
(* fail *)
  | FAIL; LBRACKET; v=VAR; RBRACKET; LBRACE; es=separated_list(COMMA, expr_target); RBRACE
      { Cmd.Fail (v, es) }
;


g_only_spec_target:
(* only <spec> *)
  AXIOMATIC; spec = g_spec_target
  {
    let open Spec in
    let new_sspecs = List.map (fun (sspec : st) -> { sspec with ss_to_verify = false }) spec.spec_sspecs in
    { spec with spec_sspecs = new_sspecs; spec_to_verify = false }
  }
;

g_spec_target:
(* (incomplete) spec xpto (x, y) [[ assertion ]] [[ post1; ...; postn ]] NORMAL|ERROR *)
  incomplete = option(INCOMPLETE); SPEC; spec_head = spec_head_target;
  spec_sspecs = separated_nonempty_list(SCOLON, g_sspec_target)
  { let (spec_name, spec_params) = spec_head in
    let spec_normalised = !Config.previously_normalised in
    let spec_to_verify = true in
    let spec_incomplete = Option.is_some incomplete in
    let spec : Spec.t = { spec_name; spec_params; spec_sspecs; spec_normalised; spec_incomplete; spec_to_verify } in
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
  | option(lab_spec_target) g_spec_line g_mult_spec_line option(variant_target) NORMAL
    { Spec.{ ss_pre = $2; ss_posts = $3; ss_variant = $4; ss_flag = Normal; ss_to_verify = true; ss_label = $1 } }
(* [[ .... ]] [[ .... ]] Error *)
  | lab_spec = option(lab_spec_target); ss_pre = g_spec_line; ss_posts = g_mult_spec_line; ss_variant = option(variant_target); ERROR
  {
    let spec : Spec.st = { ss_pre; ss_posts; ss_variant; ss_flag = Error; ss_to_verify = true; ss_label = lab_spec} in
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

hides_vars:
  | LBRACKET; HIDES; COLON; hides = separated_list(COMMA, LVAR); RBRACKET;
    { hides }

g_named_assertion_target:
  id = option(assertion_id_target); a = g_assertion_target; hides = option(hides_vars)
  { (id, a, Option.value hides ~default:[]) }
;

g_logic_cmd_target:
(* fold x(e1, ..., en) *)
  | FOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; fold_info = option(logic_bindings_target)
    { LCmd.SL (Fold (name, les, fold_info)) }

(* unfold x(e1, ..., en) [ def with #x := le1 and ... ] *)
  | UNFOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; unfold_info = option(unfold_info_target)
    { LCmd.SL (Unfold (name, les, unfold_info, false)) }

(* unfold* x(e1, ..., en) [ def with #x := le1 and ... ] *)
  | RECUNFOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; unfold_info = option(unfold_info_target)
    { LCmd.SL (Unfold (name, les, unfold_info, true)) }

(* unfold_all x *)
  | UNFOLDALL; name = VAR
    { LCmd.SL (GUnfold name) }

  | SYMBEXEC { LCmd.SL SymbExec }

(* invariant (a) [existentials: x, y, z] *)
  | INVARIANT; LBRACE; a = g_assertion_target; RBRACE; binders = option(binders_target)
    { LCmd.SL (Invariant (a, Option.value ~default:[ ] binders)) }

(* assert_* (a) [bind: x, y, z] *)
  | SEPASSERT; LBRACE; a = g_assertion_target; RBRACE; binders = option(binders_target)
    { LCmd.SL (SepAssert (a, Option.value ~default:[ ] binders)) }

(* apply lemma_name(args) [bind: x, y ] *)
   | APPLY; lemma_name = VAR; LBRACE; params = separated_list(COMMA, expr_target); RBRACE; binders = option(binders_target)
    {
      let binders = Option.value ~default:[] binders in
      LCmd.SL (ApplyLem (lemma_name, params, binders))
    }

(* if(le) { lcmd* } else { lcmd* } *)
  | LIF; LBRACE; le=expr_target; RBRACE; LTHEN; CLBRACKET;
      then_lcmds = separated_list(SCOLON, g_logic_cmd_target);
      CRBRACKET; LELSE; CLBRACKET;
      else_lcmds = separated_list(SCOLON, g_logic_cmd_target);
       CRBRACKET;
    { LCmd.If (le, then_lcmds, else_lcmds)}

(* if(e) { lcmd* } *)
  | LIF; LBRACE; le=expr_target; RBRACE; LTHEN; CLBRACKET;
      then_lcmds = separated_list(SCOLON, g_logic_cmd_target);
      CRBRACKET;
    { LCmd.If (le, then_lcmds, [])}

  | macro = macro_head_target;
    { let (name, params) = macro in LCmd.Macro (name, params) }

(* assert (a) *)
  | ASSERT; LBRACE; a = pure_assertion_target; RBRACE
    { LCmd.Assert a }

(* assume (a) *)
  | ASSUME; LBRACE; a = pure_assertion_target; RBRACE
    { LCmd.Assume a }

(* assume_type (x, t) *)
  | ASSUME_TYPE; LBRACE; e=expr_target; COMMA; t=type_target; RBRACE
    { LCmd.AssumeType (e, t) }


  (* x := e *)
  | v=VAR; DEFEQ; FRESH_SVAR; LBRACE; RBRACE
    { LCmd.FreshSVar (v) }

(* branch (fo) *)
  | BRANCH; LBRACE; fo = pure_assertion_target; RBRACE
     { LCmd.Branch fo }
;

g_pred_def_target:
  COLON; defs = separated_nonempty_list(COMMA, g_named_assertion_target)
  { defs }

g_pred_facts_target:
  FACTS; COLON; facts = separated_nonempty_list(AND, pure_assertion_target); SCOLON
  { facts }

(* pred name (arg1, ..., argn) : def1, ..., defn ; *)
g_pred_target:
  no_path = option(NO_PATH);
  internal = option(INTERNAL);
  abstract = option(ABSTRACT);
  pure = option(PURE);
  nounfold = option(NOUNFOLD);
  PRED;
  pred_head = pred_head_target;
  pred_definitions = option(g_pred_def_target);
  SCOLON
  pred_facts=option(g_pred_facts_target);
  {
    let pred_abstract = Option.is_some abstract in
    let pred_pure = Option.is_some pure in
    let pred_nounfold = pred_abstract || Option.is_some nounfold in
    let (pred_name, pred_num_params, pred_params, pred_ins) = pred_head in
    let pred_definitions = Option.value ~default:[] pred_definitions in
    let () = if (pred_abstract <> (pred_definitions = [])) then
      raise (Failure (Format.asprintf "Malformed predicate %s: either abstract with definition or non-abstract without definition." pred_name))
    in
    let () =
      if Option.is_some no_path then
        preds_with_no_paths := SS.add pred_name !preds_with_no_paths
    in
    let pred_normalised = !Config.previously_normalised in
    let pred_facts = Option.value ~default:[] pred_facts in
    Pred.
      {
        pred_name;
        pred_source_path = None;
        pred_internal = Option.is_some internal;
        pred_num_params;
        pred_params;
        pred_ins;
        pred_definitions;
        pred_facts;
        pred_pure;
        pred_abstract;
        pred_nounfold;
        pred_normalised;
      }
  }
;

variant_target:
  VARIANT LBRACE; variant = expr_target; RBRACE
  { variant }

hides_target:
  HIDES; COLON; hides = separated_list(COMMA, LVAR)
    { hides }

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
  no_path = option(NO_PATH);
  internal = option(INTERNAL);
  LEMMA;
  lemma_head = lemma_head_target;
  lemma_variant = option(variant_target);
  lemma_hides = option(hides_target);
  lemma_hyp = g_spec_line;
  lemma_concs = g_mult_spec_line;
  lemma_existentials = option(existentials_target);
  lemma_proof = option(g_lemma_proof_target);
  {
    (* FIXME: can only read one spec right now *)
    let lemma_name, lemma_params = lemma_head in
    let () =
      if Option.is_some no_path then
        lemmas_with_no_paths := SS.add lemma_name !lemmas_with_no_paths
    in
    let lemma_existentials = Option.value ~default:[] lemma_existentials in
    let spec = Lemma.{
      lemma_hyp;
      lemma_concs;
      lemma_spec_variant = lemma_variant;
      lemma_spec_hides = lemma_hides
    } in
    Lemma.
      {
        lemma_name;
        lemma_source_path = None;
        lemma_internal = Option.is_some internal;
        lemma_params;
        lemma_specs = [ spec ];
        lemma_variant;
        lemma_proof;
        lemma_existentials;
      }
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
logic_bindings_target:
  | LBRACKET; id = VAR; WITH; var_les = separated_list(AND, var_and_le_target); RBRACKET
    { (id, var_les) }
;

(* [bind: (#x := le1) and ... ] *)
unfold_info_target:
  | LBRACKET; BIND; COLON; var_les = separated_list(AND, var_and_var_target); RBRACKET
    { var_les }
;

lvar_or_pvar:
  | LVAR { $1 }
  | VAR  { $1 }

binders_target:
  | LBRACKET; BIND; COLON; xs = separated_list(COMMA, lvar_or_pvar); RBRACKET
    { xs }
;

existentials_target:
  | LBRACKET; EXISTENTIALS; COLON; xs = separated_list(COMMA, LVAR); RBRACKET
    { xs }
;

pure_assertion_target:
(* P /\ Q *)
  | left_ass=pure_assertion_target; LAND; right_ass=pure_assertion_target
    { Formula.And (left_ass, right_ass) }
(* P \/ Q *)
  | left_ass=pure_assertion_target; LOR; right_ass=pure_assertion_target
    { Formula.Or (left_ass, right_ass) }
(* ! Q *)
  | LNOT; ass=pure_assertion_target
    { Formula.Not (ass) }
(* true *)
  | LTRUE
    { Formula.True }
(* false *)
  | LFALSE
    { Formula.False }
(* E == E *)
  | left_expr=expr_target; LEQUAL; right_expr=expr_target
    { Formula.Eq (left_expr, right_expr) }
(* E i<# E *)
  | left_expr=expr_target; ILLESSTHAN; right_expr=expr_target
    { Formula.ILess (left_expr, right_expr) }
(* E <# E *)
  | left_expr=expr_target; FLLESSTHAN; right_expr=expr_target
    { Formula.FLess (left_expr, right_expr) }
(* E i<=# E *)
  | left_expr=expr_target; ILLESSTHANEQUAL; right_expr=expr_target
    { Formula.ILessEq (left_expr, right_expr) }
(* E <=# E *)
  | left_expr=expr_target; FLLESSTHANEQUAL; right_expr=expr_target
    { Formula.FLessEq (left_expr, right_expr) }
(* E s<# E *)
  | left_expr=expr_target; LSLESSTHAN; right_expr=expr_target
    { Formula.StrLess (left_expr, right_expr) }
(* E --e-- E *)
  | left_expr=expr_target; LSETMEM; right_expr=expr_target
    { Formula.SetMem (left_expr, right_expr) }
(* E --s-- E *)
  | left_expr=expr_target; LSETSUB; right_expr=expr_target
    { Formula.SetSub (left_expr, right_expr) }
(* forall X, Y, Z . P *)
  | LFORALL; vars = separated_nonempty_list(COMMA, lvar_type_target); DOT; ass = pure_assertion_target
    { Formula.ForAll (vars, ass) }
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
    Expr.LVar v_imported }
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
  | UNDEFINED                 { Literal.Undefined }
  | NULL                      { Literal.Null }
  | EMPTY                     { Literal.Empty }
  | constant_target           { Literal.Constant $1 }
  | TRUE                      { Literal.Bool true }
  | FALSE                     { Literal.Bool false }
  | FLOAT                     { Literal.Num $1 }
  | n = INTEGER               { Literal.Int n }
  | NAN                       { Literal.Num nan }
  | INFINITY                  { Literal.Num infinity }
  | STRING                    { Literal.String $1 }
  | LOC                       { Literal.Loc $1 }
  | type_target               { Literal.Type $1 }
  | LSTNIL                    { Literal.LList [] }
  | LSTOPEN LSTCLOSE          { Literal.LList [] }
  | LNONE                     { Literal.Nono }
;

nop_target:
  | SETUNION { NOp.SetUnion }
  | SETINTER { NOp.SetInter }
  | LSTCAT   { NOp.LstCat   }
;

binop_target:
  | EQ                  { BinOp.Equal }
  | ILT                 { BinOp.ILessThan }
  | ILE                 { BinOp.ILessThanEqual }
  | IPLUS               { BinOp.IPlus }
  | IMINUS              { BinOp.IMinus }
  | ITIMES              { BinOp.ITimes }
  | IDIV                { BinOp.IDiv }
  | IMOD                { BinOp.IMod }
  | FLT                 { BinOp.FLessThan }
  | FLE                 { BinOp.FLessThanEqual }
  | FPLUS               { BinOp.FPlus }
  | FMINUS              { BinOp.FMinus }
  | FTIMES              { BinOp.FTimes }
  | FDIV                { BinOp.FDiv }
  | FMOD                { BinOp.FMod }
  | SLT                 { BinOp.SLessThan }
  | AND                 { BinOp.BAnd }
  | OR                  { BinOp.BOr }
  | BITWISEAND          { BinOp.BitwiseAnd }
  | BITWISEOR           { BinOp.BitwiseOr}
  | BITWISEXOR          { BinOp.BitwiseXor }
  | LEFTSHIFT           { BinOp.LeftShift }
  | SIGNEDRIGHTSHIFT    { BinOp.SignedRightShift }
  | UNSIGNEDRIGHTSHIFT  { BinOp.UnsignedRightShift }
  | BITWISEANDL         { BinOp.BitwiseAndL }
  | BITWISEORL          { BinOp.BitwiseOrL }
  | BITWISEXORL         { BinOp.BitwiseXorL }
  | LEFTSHIFTL          { BinOp.LeftShiftL }
  | SIGNEDRIGHTSHIFTL   { BinOp.SignedRightShiftL }
  | UNSIGNEDRIGHTSHIFTL { BinOp.UnsignedRightShiftL }
  | M_ATAN2             { BinOp.M_atan2 }
  | M_POW               { BinOp.M_pow }
  | STRCAT              { BinOp.StrCat }
  | SETDIFF             { BinOp.SetDiff }
  | SETMEM              { BinOp.BSetMem }
  | SETSUB              { BinOp.BSetSub }
;

unop_target:
  (* Unary minus defined in (l)expr_target *)
  | NOT         { UnOp.UNot }
  | BITWISENOT  { UnOp.BitwiseNot }
  | M_ISNAN     { UnOp.M_isNaN }
  | M_ABS       { UnOp.M_abs }
  | M_ACOS      { UnOp.M_acos }
  | M_ASIN      { UnOp.M_asin }
  | M_ATAN      { UnOp.M_atan }
  | M_CEIL      { UnOp.M_ceil }
  | M_COS       { UnOp.M_cos }
  | M_EXP       { UnOp.M_exp }
  | M_FLOOR     { UnOp.M_floor }
  | M_LOG       { UnOp.M_log }
  | M_ROUND     { UnOp.M_round }
  | M_SGN       { UnOp.M_sgn }
  | M_SIN       { UnOp.M_sin }
  | M_SQRT      { UnOp.M_sqrt }
  | M_TAN       { UnOp.M_tan }
  | TOSTRING    { UnOp.ToStringOp }
  | TOINT       { UnOp.ToIntOp }
  | TOUINT16    { UnOp.ToUint16Op }
  | TOINT32     { UnOp.ToInt32Op }
  | TOUINT32    { UnOp.ToUint32Op }
  | TONUMBER    { UnOp.ToNumberOp }
  | TYPEOF      { UnOp.TypeOf }
  | CAR         { UnOp.Car }
  | CDR         { UnOp.Cdr }
  | LSTLEN      { UnOp.LstLen }
  | LSTREV      { UnOp.LstRev }
  | STRLEN      { UnOp.StrLen }
  | SETTOLIST   { UnOp.SetToList }
  | INTTONUM { UnOp.IntToNum }
  | NUMTOINT { UnOp.NumToInt }
;

constant_target:
  | MIN_FLOAT { Constant.Min_float }
  | MAX_FLOAT { Constant.Max_float }
  | RANDOM    { Constant.Random }
  | PI        { Constant.Pi }
  | UTCTIME   { Constant.UTCTime }
  | LOCALTIME { Constant.LocalTime }
;

type_target:
  | UNDEFTYPELIT { Type.UndefinedType }
  | NULLTYPELIT  { Type.NullType }
  | EMPTYTYPELIT { Type.EmptyType }
  | NONETYPELIT  { Type.NoneType }
  | BOOLTYPELIT  { Type.BooleanType }
  | INTTYPELIT   { Type.IntType }
  | NUMTYPELIT   { Type.NumberType }
  | STRTYPELIT   { Type.StringType }
  | OBJTYPELIT   { Type.ObjectType }
  | LISTTYPELIT  { Type.ListType }
  | TYPETYPELIT  { Type.TypeType }
  | SETTYPELIT   { Type.SetType }
;

