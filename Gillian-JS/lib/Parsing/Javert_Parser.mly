%{
open Utils.Containers
open Utils
open Gillian.Gil_syntax
open! Jsil_syntax
open Jslogic

let normalised_lvar_r = Str.regexp "##NORMALISED_LVAR"
%}

(***** Token definitions *****)
(*  JS Logic Literals *)
%token SCOPELEXPR
%token SCOPE
%token THIS
%token CLOSURE
%token SCSCOPE
%token OCHAINS
%token SCHAIN
%token UNDERSCORE
(* Type literals *)
%token UNDEFTYPELIT
%token NULLTYPELIT
%token EMPTYTYPELIT
%token NONETYPELIT
%token BOOLTYPELIT
%token NUMTYPELIT
%token STRTYPELIT
%token OBJTYPELIT
%token LISTTYPELIT
%token TYPETYPELIT
%token SETTYPELIT
(* Constants *)
%token MIN_FLOAT
%token MAX_FLOAT
%token EPSILON
%token MAX_SAFE_INTEGER
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
(* Filenames *)
%token <string> FILENAME
(* Binary operators *)
%token EQUAL
%token LESSTHAN
%token GREATERTHAN
%token LESSTHANEQUAL
%token GREATERTHANEQUAL
%token LESSTHANSTRING
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token MOD
%token AND
%token OR
%token BITWISEAND
%token BITWISEOR
%token BITWISEXOR
%token LEFTSHIFT
%token SIGNEDRIGHTSHIFT
%token UNSIGNEDRIGHTSHIFT
%token M_ATAN2
%token M_POW
%token LSTCONS
%token LSTCAT
%token LSTREV
%token STRCAT
(* Unary operators *)
(* Unary minus uses the same token as binary minus: MINUS *)
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
%token INVARIANT
%token ASSUME_TYPE
%token FRESH_SVAR
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
%token NEW
%token DELETE
%token DELETEOBJ
%token HASFIELD
%token GETFIELDS
%token METADATA
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
%token LLESSTHANSTRING
%token LARROW
%token LEMP
%token EMPTYFIELDS
(*%token LEXISTS *)
%token LFORALL
%token LTYPES
%token LMETADATA
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
%token FLASH
%token RECUNFOLD
%token LIF
%token LTHEN
%token LELSE
(* Procedure specification keywords *)
%token AXIOMATIC
%token INCOMPLETE
%token SPEC
%token BISPEC
%token LEMMA
%token VARIANT
%token NORMAL
%token ERROR
(* JS axiomatic spec specifics *)
%token JSOS
(* Procedure definition keywords *)
%token PROC
(* Others *)
%token IMPORT
%token MACRO
(* Separators *)
%token DOT
%token COMMA
%token COLON
%token SCOLON
(*%token DOT*)
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
(* End-Of-File *)
%token EOF

(***** Precedence of operators *****)
(* The later an operator is listed, the higher precedence it is given. *)
(* Logic operators have lower precedence *)
%nonassoc DOT
%left LOR
%left LAND
%left separating_conjunction
%right LNOT
%nonassoc LEQUAL LLESSTHAN LLESSTHANEQUAL LLESSTHANSTRING LARROW
%nonassoc SETMEM SETSUB LSETMEM LSETSUB
(* Program operators have higher precedence.*)
(* Based on JavaScript:
   https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Operators/Operator_Precedence *)
%left OR
%left AND
%left BITWISEOR
%left BITWISEXOR
%left BITWISEAND
%nonassoc EQUAL
%nonassoc LESSTHAN LESSTHANSTRING LESSTHANEQUAL GREATERTHAN GREATERTHANEQUAL
%nonassoc LSTCONS
%left LEFTSHIFT SIGNEDRIGHTSHIFT UNSIGNEDRIGHTSHIFT
%left PLUS MINUS
%left TIMES DIV MOD M_POW
%left M_ATAN2 STRCAT SETDIFF

%nonassoc binop_prec
%nonassoc unop_prec

(* Common *)
%type <Type.t>       type_target
%type <Constant.t>   constant_target
%type <Literal.t>    lit_target

(* JSIL *)
%type <UnOp.t>                     unop_target
%type <BinOp.t>                    binop_target
%type <NOp.t>                      nop_target
%type <Expr.t>                     expr_target
%type <Gillian.Gil_syntax.Formula.t>   pure_assertion_target
%type <Jsil_syntax.EProg.t>               jsil_main_target

%start jsil_main_target

(* JS *)
%type <Jslogic.JSAsrt.pt>      js_pure_assertion_target
%type <Jslogic.JSPred.t>       js_pred_target
%type <Jslogic.JSSpec.t>       js_only_spec_target
%type <Jslogic.JSLCmd.t list>  js_logic_cmds_target
%type <Jslogic.JSAsrt.t list>  top_level_js_assertion_list_target
%type <Gillian.Gil_syntax.Expr.t>  top_level_expr_target
%type <Jslogic.JSAsrt.t>       top_level_js_assertion_target
%type <(string * Utils.Containers.SS.t) option * Jslogic.JSAsrt.t> top_level_js_pre_target

%start js_pred_target
%start js_only_spec_target
%start js_logic_cmds_target
%start top_level_js_assertion_list_target
%start top_level_js_assertion_target
%start top_level_js_pre_target
%start top_level_expr_target

%%


constant_target:
  | MIN_FLOAT { Constant.Min_float }
  | MAX_FLOAT { Constant.Max_float }
  | MAX_SAFE_INTEGER { Constant.MaxSafeInteger }
  | EPSILON   { Constant.Epsilon }
  | RANDOM    { Constant.Random }
  | PI        { Constant.Pi }
  | UTCTIME   { Constant.UTCTime }
  | LOCALTIME { Constant.LocalTime }


type_target:
  | UNDEFTYPELIT { Type.UndefinedType }
  | NULLTYPELIT  { Type.NullType }
  | EMPTYTYPELIT { Type.EmptyType }
  | NONETYPELIT  { Type.NoneType }
  | BOOLTYPELIT  { Type.BooleanType }
  | NUMTYPELIT   { Type.NumberType }
  | STRTYPELIT   { Type.StringType }
  | OBJTYPELIT   { Type.ObjectType }
  | LISTTYPELIT  { Type.ListType }
  | TYPETYPELIT  { Type.TypeType }
  | SETTYPELIT   { Type.SetType }

lit_target:
  | UNDEFINED                 { Literal.Undefined }
  | NULL                      { Literal.Null }
  | EMPTY                     { Literal.Empty }
  | constant_target           { Literal.Constant $1 }
  | TRUE                      { Literal.Bool true }
  | FALSE                     { Literal.Bool false }
  | FLOAT                     { Literal.Num $1 }
  | NAN                       { Literal.Num nan }
  | INFINITY                  { Literal.Num infinity }
  | STRING                    { Literal.String $1 }
  | LOC                       { Literal.Loc $1 }
  | type_target               { Literal.Type $1 }
  | LSTNIL                    { Literal.LList [] }
  | LSTOPEN LSTCLOSE          { Literal.LList [] }
  | LNONE                     { Literal.Nono }


unop_target:
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

binop_target:
  | EQUAL              { BinOp.Equal }
  | LESSTHAN           { BinOp.FLessThan }
  | LESSTHANEQUAL      { BinOp.FLessThanEqual }
  | LESSTHANSTRING     { BinOp.SLessThan }
  | PLUS               { BinOp.FPlus }
  | MINUS              { BinOp.FMinus }
  | TIMES              { BinOp.FTimes }
  | DIV                { BinOp.FDiv }
  | MOD                { BinOp.FMod }
  | AND                { BinOp.BAnd }
  | OR                 { BinOp.BOr }
  | BITWISEAND         { BinOp.BitwiseAnd }
  | BITWISEOR          { BinOp.BitwiseOr}
  | BITWISEXOR         { BinOp.BitwiseXor }
  | LEFTSHIFT          { BinOp.LeftShift }
  | SIGNEDRIGHTSHIFT   { BinOp.SignedRightShift }
  | UNSIGNEDRIGHTSHIFT { BinOp.UnsignedRightShift }
  | M_ATAN2            { BinOp.M_atan2 }
  | M_POW              { BinOp.M_pow }
  | STRCAT             { BinOp.StrCat }
  | SETDIFF            { BinOp.SetDiff }
  | SETMEM             { BinOp.BSetMem }
  | SETSUB             { BinOp.BSetSub }

nop_target:
  | SETUNION { NOp.SetUnion }
  | SETINTER { NOp.SetInter }
  | LSTCAT   { NOp.LstCat   }


expr_target:
  | lit=lit_target { Expr.Lit lit }
  | v = LVAR {
    let v_imported = Str.replace_first normalised_lvar_r "_lvar_n" v in
    Expr.LVar v_imported
  }
  | ALOC { Expr.ALoc $1 }
  | v = VAR { Expr.PVar v }
  | e1=expr_target; bop=binop_target; e2=expr_target { Expr.BinOp (e1, bop, e2) } %prec binop_prec
  | e1=expr_target; LSTCONS; e2=expr_target { Expr.NOp (LstCat, [ EList [ e1 ]; e2 ]) }
  | e1=expr_target; GREATERTHAN;  e2=expr_target { Expr.BinOp (e2, FLessThan, e1) }
  | e1=expr_target; GREATERTHANEQUAL; e2=expr_target { Expr.BinOp (e2, FLessThanEqual, e1) }
  | uop=unop_target; e=expr_target { Expr.UnOp (uop, e) } %prec unop_prec
  | MINUS; e=expr_target { Expr.UnOp (FUnaryMinus, e) } %prec unop_prec
  | LSTOPEN; exprlist = separated_nonempty_list(COMMA, expr_target); LSTCLOSE { Expr.EList exprlist }
  | SETOPEN; exprlist = separated_list(COMMA, expr_target); SETCLOSE
     { Expr.ESet (Expr.Set.elements (Expr.Set.of_list exprlist)) }
  | LSTNTH; LBRACE; e1=expr_target; COMMA; e2=expr_target; RBRACE
     { Expr.BinOp (e1, LstNth, e2) }
  | LSTSUB; LBRACE; e1=expr_target; COMMA; e2=expr_target; COMMA; e3 = expr_target; RBRACE
    { Expr.LstSub (e1, e2, e3) }
  | nop=nop_target; LBRACE; les=separated_list(COMMA, expr_target); RBRACE
     {
        let les =
          match (nop : NOp.t) with
          | SetInter
          | SetUnion -> Expr.Set.elements (Expr.Set.of_list les)
          | LstCat -> les
        in
        Expr.NOp (nop, les)
     }
  | STRNTH; LBRACE; e1=expr_target; COMMA; e2=expr_target; RBRACE
     { Expr.BinOp (e1, StrNth, e2) }
  | LBRACE; e=expr_target; RBRACE { e }
  | UNDERSCORE { Expr.LVar (Javert_utils.Js_generators.fresh_lvar ()) }


lvar_type_target:
  | lvar = LVAR; COLON; the_type = type_target
    { (lvar, Some the_type) }
  | lvar = LVAR;
    { (lvar, None) }


pure_assertion_target:
  | left_ass=pure_assertion_target; LAND; right_ass=pure_assertion_target
    { Formula.And (left_ass, right_ass) }
  | left_ass=pure_assertion_target; LOR; right_ass=pure_assertion_target
    { Formula.Or (left_ass, right_ass) }
  | LNOT; ass=pure_assertion_target { Formula.Not (ass) }
  | LTRUE { Formula.True }
  | LFALSE { Formula.False }
  | left_expr=expr_target; LEQUAL; right_expr=expr_target
    { Formula.Eq (left_expr, right_expr) }
  | left_expr=expr_target; LLESSTHAN; right_expr=expr_target
    { Formula.FLess (left_expr, right_expr) }
  | left_expr=expr_target; LLESSTHANEQUAL; right_expr=expr_target
    { Formula.FLessEq (left_expr, right_expr) }
  | left_expr=expr_target; LLESSTHANSTRING; right_expr=expr_target
    { Formula.StrLess (left_expr, right_expr) }
  | left_expr=expr_target; LSETMEM; right_expr=expr_target
    { Formula.SetMem (left_expr, right_expr) }
  | left_expr=expr_target; LSETSUB; right_expr=expr_target
    { Formula.SetSub (left_expr, right_expr) }
  | LFORALL; vars = separated_nonempty_list(COMMA, lvar_type_target); DOT; ass = pure_assertion_target
    { Formula.ForAll (vars, ass) }
  | delimited(LBRACE, pure_assertion_target, RBRACE)
    { $1 }

program_variable_target:
  | v = VAR { Expr.PVar v }

logic_variable_target:
  v = LVAR
  {
    let v_imported = Str.replace_first normalised_lvar_r "_lvar_n" v in
    (* Prefixed with _n_ to avoid clashes *)
    Expr.LVar v_imported }

type_env_pair_target:
  | lvar = logic_variable_target; COLON; the_type=type_target
    { (lvar, the_type) }
  | pvar = program_variable_target; COLON; the_type=type_target
    { (pvar, the_type) }

assertion_target:
  | left_ass=assertion_target; TIMES; right_ass=assertion_target
    { Asrt.Star (left_ass, right_ass) } %prec separating_conjunction
  | LBRACE; obj_expr=expr_target; COMMA; prop_expr=expr_target; RBRACE; LARROW; val_expr=expr_target
    { Asrt.PointsTo (obj_expr, prop_expr, val_expr) }
  | LMETADATA; LBRACE; eo = expr_target; COMMA; em = expr_target; RBRACE
    { Asrt.MetaData (eo, em) }
  | LEMP; { Asrt.Emp }
  | name = VAR; LBRACE; params = separated_list(COMMA, expr_target); RBRACE
    { (* validate_pred_assertion (name, params); *)
      Asrt.Pred (name, params) }
  | LTYPES; LBRACE; type_pairs = separated_list(COMMA, type_env_pair_target); RBRACE
    { Asrt.Types type_pairs }
  | EMPTYFIELDS; LBRACE; le=expr_target; COLON; domain=expr_target; RBRACE
    { Asrt.EmptyFields (le, domain) }
  | LBRACE; ass=assertion_target; RBRACE
    { ass }
  | f = pure_assertion_target
    { Asrt.Pure f }

/* COMMANDS */

binders_target:
  | LBRACKET; BIND; COLON; xs = separated_list(COMMA, LVAR); RBRACKET
    { xs }

macro_head_target:
 | name = VAR; LBRACE; params = separated_list(COMMA, expr_target); RBRACE
   { (name, params) }

var_and_le_target:
  | LBRACE; lvar = LVAR; DEFEQ; le = expr_target; RBRACE;
    { (lvar, le) }

var_and_var_target:
  | LBRACE; lvar1 = LVAR; DEFEQ; lvar2 = LVAR; RBRACE;
    { (lvar1, lvar2) }
;

logic_bindings_target:
  | LBRACKET; id = VAR; WITH; var_les = separated_list(AND, var_and_le_target); RBRACKET
    { (id, var_les) }

existentials_target:
  | LBRACKET; EXISTENTIALS; COLON; xs = separated_list(COMMA, LVAR); RBRACKET
    { xs }

(* [bind: (#x := le1) and ... ] *)
unfold_info_target:
  | LBRACKET; BIND; COLON; unfold_info = separated_list(AND, var_and_var_target); RBRACKET
    { unfold_info }
;

logic_cmd_target:
  | FOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; fold_info = option(logic_bindings_target)
    { LCmd.SL (Fold (name, les, fold_info)) }
  | UNFOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; unfold_info = option(unfold_info_target)
    { LCmd.SL (Unfold (name, les, unfold_info, false)) }
  | RECUNFOLD; name = VAR; LBRACE; les=separated_list(COMMA, expr_target); RBRACE; unfold_info = option(unfold_info_target)
    { LCmd.SL (Unfold (name, les, unfold_info, true)) }
  | UNFOLDALL; name = VAR
    { LCmd.SL (GUnfold name) }
  | INVARIANT; LBRACE; a = assertion_target; RBRACE; binders = option(binders_target)
    { LCmd.SL (Invariant (a, Option.value ~default:[ ] binders)) }
  | SEPASSERT; LBRACE; a = assertion_target; RBRACE; binders = option(binders_target)
    { LCmd.SL (SepAssert (a, Option.value ~default:[ ] binders)) }
  | APPLY; lemma_name = VAR; LBRACE; params = separated_list(COMMA, expr_target); RBRACE; binders = option(binders_target)
    { let binders = Option.value ~default:[] binders in
      LCmd.SL (ApplyLem (lemma_name, params, binders)) }
  | LIF; LBRACE; le=expr_target; RBRACE; LTHEN; CLBRACKET;
      then_lcmds = separated_list(SCOLON, logic_cmd_target);
      CRBRACKET; LELSE; CLBRACKET;
      else_lcmds = separated_list(SCOLON, logic_cmd_target);
       CRBRACKET;
    { LCmd.If (le, then_lcmds, else_lcmds)}
  | LIF; LBRACE; le=expr_target; RBRACE; LTHEN; CLBRACKET;
      then_lcmds = separated_list(SCOLON, logic_cmd_target);
      CRBRACKET;
    { LCmd.If (le, then_lcmds, [])}
  | macro = macro_head_target;
    { let (name, params) = macro in LCmd.Macro (name, params) }
  | ASSUME; LBRACE; a = pure_assertion_target; RBRACE
    { LCmd.Assume a }
  | ASSUME_TYPE; LBRACE; e=expr_target; COMMA; t=type_target; RBRACE
    { LCmd.AssumeType (e, t) }
  | v = VAR; DEFEQ; FRESH_SVAR; LBRACE; RBRACE
    { LCmd.FreshSVar v}
  | BRANCH; LBRACE; fo = pure_assertion_target; RBRACE
     { LCmd.Branch fo }

phi_target:
  v = VAR; COLON; args = separated_list(COMMA, expr_target)
    { (v, args) }

call_with_target:
  WITH; i=VAR { i }

lvar_le_pair_target:
  LBRACE; lv = LVAR; COLON; e=expr_target; RBRACE { (lv, e )}

use_subst_target:
  | USESUBST; LBRACKET; lab=VAR; MINUS; subst = separated_list(COMMA, lvar_le_pair_target); RBRACKET
      { (lab, subst) }
  | USESUBST; LBRACKET; lab=VAR RBRACKET
      { (lab, []) }

new_target:
  | expr_target; COMMA; expr_target
    { Some $1, Some $3}
  | expr_target
    { Some $1, None}

cmd_target:
  | SKIP
    { LabCmd.LBasic (Skip) }
  | v=VAR; DEFEQ; e=expr_target
    { LabCmd.LBasic (Assignment (v, e)) }
  | VAR; DEFEQ; NEW; LBRACE; option(new_target); RBRACE
    {
      let loc, metadata = (match $5 with
      | Some (Some arg_a, Some arg_b) -> Some arg_a, Some arg_b
      | Some (Some arg_a, None) -> None, Some arg_a
      | Some (None, Some _) -> raise (Failure "Parser: Impossible")
      | _ -> None, None
      ) in
        LabCmd.LBasic (New ($1, loc, metadata)) }
  | v=VAR; DEFEQ; LBRACKET; e1=expr_target; COMMA; e2=expr_target; RBRACKET
    { LabCmd.LBasic (Lookup (v, e1, e2)) }
  | LBRACKET; e1=expr_target; COMMA; e2=expr_target; RBRACKET; DEFEQ; e3=expr_target
    { LabCmd.LBasic (Mutation (e1, e2, e3)) }
  | DELETE; LBRACE; e1=expr_target; COMMA; e2=expr_target; RBRACE
    { LabCmd.LBasic (Delete (e1, e2)) }
  | DELETEOBJ; LBRACE; e1=expr_target; RBRACE
    { LabCmd.LBasic (DeleteObj (e1)) }
  | v=VAR; DEFEQ; HASFIELD; LBRACE; e1=expr_target; COMMA; e2=expr_target; RBRACE
    { LabCmd.LBasic (HasField (v, e1, e2)) }
  | v = VAR; DEFEQ; GETFIELDS; LBRACE; e=expr_target; RBRACE
    { LabCmd.LBasic (GetFields (v, e)) }
  | v = VAR; DEFEQ; METADATA; LBRACE; e=expr_target; RBRACE
    { LabCmd.LBasic (MetaData (v, e)) }
  | GOTO; i=VAR
    { LabCmd.LGoto i }
  | GOTO LBRACKET; e=expr_target; RBRACKET; i=VAR; j=VAR
    { LabCmd.LGuardedGoto (e, i, j) }
  | v=VAR; DEFEQ; e=expr_target;
    LBRACE; es=separated_list(COMMA, expr_target); RBRACE; oi = option(call_with_target); subst = option(use_subst_target)
    { LabCmd.LCall (v, e, es, oi, subst) }
  | v=VAR; DEFEQ; EXTERN; pname=VAR;
    LBRACE; es=separated_list(COMMA, expr_target); RBRACE; oi = option(call_with_target)
    { LabCmd.LECall (v, PVar pname, es, oi) }
  | v=VAR; DEFEQ; APPLY;
    LBRACE; es=expr_target; RBRACE; oi = option(call_with_target)
    { LabCmd.LApply (v, es, oi) }
  | v = VAR; DEFEQ; ARGUMENTS
    { (LabCmd.LArguments v) }
  | PHI; LBRACE; phi_args =separated_list(SCOLON, phi_target); RBRACE
    { match phi_args with
      | [] -> raise (Failure "EMPTY PHI")
      | _  -> LabCmd.LPhiAssignment phi_args }
  | RETURN { LabCmd.LReturnNormal }
  | THROW  { LabCmd.LReturnError  }
  | lcmd = logic_cmd_target
    { LabCmd.LLogic lcmd }

cmd_with_annot:
  | cmd = cmd_target
    {
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
      let annot : Annot.t = Annot.make ~origin_loc () in
      annot, cmd
    }

cmd_with_label:
  | cmd = cmd_with_annot
    { let annot, cmd = cmd in annot, None, cmd }
  | lab = VAR; COLON; cmd = cmd_with_annot
    { let annot, cmd = cmd in annot, Some lab, cmd }

cmd_list_target:
  cmd_list = separated_nonempty_list(SCOLON, cmd_with_label)
    { cmd_list }

/* SPECIFICATIONS */

spec_line:
  OASSERT; assertion = assertion_target; CASSERT { assertion }

mult_spec_line:
  OASSERT; assertions = separated_list(SCOLON, assertion_target); CASSERT { assertions }

lab_spec_target:
  | LESSTHAN; sspec_name = VAR; COLON; lvars = separated_list (COMMA, LVAR); GREATERTHAN
    { (sspec_name, SS.of_list lvars) }
  | LESSTHAN; sspec_name = VAR; GREATERTHAN
    { (sspec_name, SS.empty) }

pre_post_target:
  | lab_spec = option(lab_spec_target); pre = spec_line; posts = mult_spec_line; NORMAL
    { Spec.{ pre; posts; flag = Normal; to_verify = true; label = lab_spec } }
  | lab_spec = option(lab_spec_target); pre = spec_line; posts = mult_spec_line; ERROR
  { Spec.{ pre; posts; flag = Error; to_verify = true; label = lab_spec} }

spec_head_target:
  spec_name = VAR; LBRACE; spec_params = separated_list(COMMA, VAR); RBRACE
  { (spec_name, spec_params) }

spec_target:
  incomplete = option(INCOMPLETE); SPEC; spec_head = spec_head_target;
  proc_specs = separated_nonempty_list(SCOLON, pre_post_target)
  { let (name, params) = spec_head in
    let incomplete = (incomplete <> None) in
    let is_normalised = !Config.previously_normalised in
    Spec.{
      name = name;
      params = params;
      sspecs = proc_specs;
      normalised = is_normalised;
      incomplete;
      to_verify = true }
  }


/* PROCEDURES */

proc_head_target:
  PROC; proc_name = VAR; LBRACE; param_list = separated_list(COMMA, VAR); RBRACE
  { (proc_name, param_list) }

proc_target:
  spec = option(spec_target); proc_head = proc_head_target; CLBRACKET; cmd_list = cmd_list_target; CRBRACKET; SCOLON
    {
      let name, params = proc_head in
      let proc : EProc.t = {
        name; body = Array.of_list cmd_list; params; spec
      } in
      proc
    }

/* LEMMAS */

jsil_lemma_proof_target:
  OLCMD; proof = separated_list(SCOLON, logic_cmd_target); CLCMD;
  { proof }

jsil_lemma_variant_target:
  VARIANT LBRACE; variant = expr_target; RBRACE
  { variant }

jsil_lemma_head_target:
  lemma_name = VAR; LBRACE; lemma_params = separated_list(COMMA, VAR); RBRACE
  {
    (lemma_name, lemma_params)
  }

jsil_lemma_target:
  LEMMA; lemma_head = jsil_lemma_head_target; variant = option(jsil_lemma_variant_target);
    pre = spec_line; posts = mult_spec_line; existentials=option(existentials_target); proof = option(jsil_lemma_proof_target);
  { let (name, params) = lemma_head in
    let existentials = Option.value ~default:[] existentials in
    Lemma.{ name; params; pre; posts; variant; proof; existentials }
  }

/* PREDICATES */

assertion_id_target:
  | LBRACKET; v=VAR; RBRACKET
      { (v, []) }
  | LBRACKET; v=VAR; COLON; lvars=separated_nonempty_list(COMMA, LVAR); RBRACKET
      { (v, lvars) }

named_assertion_target:
  id = option(assertion_id_target); a = assertion_target
  { (id, a) }

pred_param_target:
  (* Program variable with in-parameter status and optional type *)
  | in_param = option(PLUS); v = VAR; t = option(preceded(COLON, type_target))
    { let in_param = Option.fold ~some:(fun _ -> true) ~none:false in_param in
      (v, t), in_param }

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

pred_defs_target:
  COLON; defs = separated_nonempty_list(COMMA, named_assertion_target)
  { defs }

pred_facts_target:
  FACTS; COLON; facts = separated_nonempty_list(AND, pure_assertion_target); SCOLON
  { facts }

pred_target:
  a = option(ABSTRACT); n = option(NOUNFOLD); p = option(PURE); PRED; pred_head = pred_head_target;
  definitions = option(pred_defs_target); SCOLON;
  facts=option(pred_facts_target);
  {
    let abstract = Option.is_some a in
    let nounfold = abstract || Option.is_some n in
    let pure = Option.is_some p in
    let (name, num_params, params, ins) = pred_head in
    let definitions = Option.value ~default:[] definitions in
    let () = if (abstract <> (definitions = [])) then
      raise (Failure (Format.asprintf "JSIL: Malformed predicate %s: either abstract with definition or non-abstract without definition." name))
    in
    let normalised = !Config.previously_normalised in
    let facts = Option.value ~default:[] facts in
    Pred.{ name; num_params; params; ins; definitions; facts; pure; abstract; nounfold; normalised } }

/* MACROS */

macro_head_def_target:
 | name = VAR; LBRACE; params = separated_list(COMMA, VAR); RBRACE
   { (name, params) }

macro_target:
  MACRO; head = macro_head_def_target; COLON; lcmds = separated_list(SCOLON, logic_cmd_target)
  { let (name, params) = head in
    Macro.{ name = name; params = params; definition = lcmds } }

/* ONLY SPECS */

only_spec_target:
(* axiomatic spec xpto (x, y) pre: assertion, post: assertion, flag: NORMAL|ERROR *)
  AXIOMATIC; incomplete = option(INCOMPLETE); SPEC; head = spec_head_target;
  sspecs = separated_nonempty_list(SCOLON, pre_post_target);
  { let (name, params) = head in
    let normalised = !Config.previously_normalised in
    let incomplete = (incomplete <> None) in
    Spec.{ name; params; sspecs; normalised; incomplete; to_verify = false } }


/* BI ABDUCTION SPECIFICATIONS */

bi_spec_target:
(* bispec xpto (x, y) : [[ assertion ]] *)
  BISPEC; spec_head = spec_head_target; COLON; asrt = spec_line
  {
    let (name, params) = spec_head in
    let is_normalised = !Config.previously_normalised in
    let bi_spec : BiSpec.t =
      {
        name       = name;
        params     = params;
        pre        = asrt;
        normalised = is_normalised
      } in
    bi_spec
  }

/* PROGRAMS */

declaration_target:
  | prog = declaration_target; lemma = jsil_lemma_target
    { EProg.add_lemma prog lemma }

  | lemma = jsil_lemma_target
    {
      let prog = EProg.full_init () in EProg.add_lemma prog lemma }

  | prog = declaration_target; pred = pred_target
    { EProg.add_pred prog pred }

  | pred = pred_target
    { let prog = EProg.full_init () in EProg.add_pred prog pred }

  | prog = declaration_target; proc = proc_target
    { EProg.add_proc prog proc }

  | proc = proc_target
    { let prog = EProg.full_init () in EProg.add_proc prog proc }

  | prog = declaration_target; macro = macro_target
    { EProg.add_macro prog macro }

  | macro = macro_target
    { let prog = EProg.full_init () in EProg.add_macro prog macro }

  | prog = declaration_target; ospec = only_spec_target
    { EProg.add_ospec prog ospec  }

  | ospec = only_spec_target
    { let prog = EProg.full_init () in EProg.add_ospec prog ospec }

  | prog = declaration_target; bispec = bi_spec_target
    { EProg.add_bispec prog bispec }

  | bispec = bi_spec_target
    { let prog = EProg.full_init () in EProg.add_bispec prog bispec }


import_target:
  IMPORT; imports = separated_nonempty_list(COMMA, FILENAME); SCOLON { imports }


jsil_main_target:
  | imports = option(import_target); prog = declaration_target; EOF
      { let imports = Option.value ~default:[] imports in
        EProg.update_imports prog imports }



(******** JAVASCRIPT LOGIC ********)


(* Expressions *)

js_program_variable_target:
  | v = VAR
    { (* let _ = validate_pvar v in *) v }

just_logic_variable_target:
  v = LVAR
  { (* validate_lvar v; *) v }

js_lvar_type_target:
  | lvar = just_logic_variable_target; COLON; the_type = type_target
    { (lvar, the_type) }

js_lexpr_target:
(* Logic literal *)
  | lit = lit_target
    { JSExpr.Lit lit }
(* program variable *)
  | pvar = js_program_variable_target
    { JSExpr.PVar pvar }
(* Logic variable *)
  | lvar = LVAR
    { JSExpr.LVar lvar }
(* e binop e *)
  | e1=js_lexpr_target; bop=binop_target; e2=js_lexpr_target
    { JSExpr.BinOp (e1, bop, e2) } %prec binop_prec
(* List cons *)
  | e1=js_lexpr_target; LSTCONS; e2=js_lexpr_target { JSExpr.NOp (LstCat, [ EList [ e1 ]; e2 ]) }
(* unop e *)
  | uop=unop_target; e=js_lexpr_target
    { JSExpr.UnOp (uop, e) } %prec unop_prec
(* nop (le1, ..., len) *)
  | nop=nop_target; LBRACE; les=separated_list(COMMA, js_lexpr_target); RBRACE
    {
      let les = match (nop : NOp.t) with
        | SetInter
        | SetUnion -> JSExpr.SJSExpr.elements (JSExpr.SJSExpr.of_list les)
        | LstCat -> les
      in
      JSExpr.NOp (nop, les)
    }
(* - e *)
(* Unary negation has the same precedence as logical not, not as binary negation. *)
  | MINUS; e=js_lexpr_target
    { JSExpr.UnOp (FUnaryMinus, e) } %prec unop_prec
(* {{ e, ..., e }} *)
  | LSTOPEN; exprlist = separated_nonempty_list(COMMA, js_lexpr_target); LSTCLOSE
    { JSExpr.EList exprlist }
(* -{- e, ..., e -}- *)
  | SETOPEN; exprlist = separated_list(COMMA, js_lexpr_target); SETCLOSE
    { JSExpr.ESet (JSExpr.SJSExpr.elements (JSExpr.SJSExpr.of_list exprlist)) }
(* l-nth(e1, e2) *)
  | LSTNTH; LBRACE; e1=js_lexpr_target; COMMA; e2=js_lexpr_target; RBRACE
    { JSExpr.BinOp (e1, LstNth, e2) }
(* s-nth(e1, e2) *)
  | STRNTH; LBRACE; e1=js_lexpr_target; COMMA; e2=js_lexpr_target; RBRACE
    { JSExpr.BinOp (e1, StrNth, e2) }
(* l-sub(e1, e2, e3) *)
| LSTSUB; LBRACE; e1=js_lexpr_target; COMMA; e2=js_lexpr_target; COMMA; e3 = js_lexpr_target; RBRACE
    { JSExpr.LstSub (e1, e2, e3) }
(* this *)
  | THIS { JSExpr.This }
(* (e) *)
  | LBRACE; e=js_lexpr_target; RBRACE
    { e }
(* _ *)
  | UNDERSCORE
    { JSExpr.LVar (Javert_utils.Js_generators.fresh_lvar ()) }
(* $$scope *)
  | SCOPELEXPR { JSExpr.Scope }


(* Assertions *)


var_js_le_pair_target:
  v=VAR; COLON; le=js_lexpr_target { (v, le) }


js_type_env_pair_target:
  | v = VAR; COLON; the_type=type_target
    { (v, the_type) }
  | v = LVAR; COLON; the_type=type_target
    { (v, the_type) }

js_pure_assertion_target:
(* P /\ Q *)
  | left_ass=js_pure_assertion_target; LAND; right_ass=js_pure_assertion_target
    { JSAsrt.And (left_ass, right_ass) }
(* P \/ Q *)
  | left_ass=js_pure_assertion_target; LOR; right_ass=js_pure_assertion_target
    { JSAsrt.Or (left_ass, right_ass) }
(* ! Q *)
  | LNOT; ass=js_pure_assertion_target
    { JSAsrt.Not (ass) }
(* true *)
  | LTRUE
    { JSAsrt.True }
(* false *)
  | LFALSE
    { JSAsrt.False }
(* E == E *)
  | left_expr=js_lexpr_target; LEQUAL; right_expr=js_lexpr_target
    { JSAsrt.Eq (left_expr, right_expr) }
(* E <# E *)
  | left_expr=js_lexpr_target; LLESSTHAN; right_expr=js_lexpr_target
    { JSAsrt.Less (left_expr, right_expr) }
(* E <=# E *)
  | left_expr=js_lexpr_target; LLESSTHANEQUAL; right_expr=js_lexpr_target
    { JSAsrt.LessEq (left_expr, right_expr) }
(* E s<# E *)
  | left_expr=js_lexpr_target; LLESSTHANSTRING; right_expr=js_lexpr_target
    { JSAsrt.StrLess (left_expr, right_expr) }
(* E --e-- E *)
  | left_expr=js_lexpr_target; LSETMEM; right_expr=js_lexpr_target
    { JSAsrt.SetMem (left_expr, right_expr) }
(* E --s-- E *)
  | left_expr=js_lexpr_target; LSETSUB; right_expr=js_lexpr_target
    { JSAsrt.SetSub (left_expr, right_expr) }
(* forall X, Y, Z . P *)
  | LFORALL; vars = separated_nonempty_list(COMMA, js_lvar_type_target); DOT; ass = js_pure_assertion_target
    { JSAsrt.ForAll (vars, ass) }
(* (P) *)
  | delimited(LBRACE, js_pure_assertion_target, RBRACE)
    { $1 }

js_assertion_target:
(* Pure *)
  | f = js_pure_assertion_target
    { JSAsrt.Pure f }
(* P * Q *)
(* The precedence of the separating conjunction is not the same as the arithmetic product *)
  | left_ass=js_assertion_target; TIMES; right_ass=js_assertion_target
    { JSAsrt.Star (left_ass, right_ass) } %prec separating_conjunction
(* (E, E) -> E *)
  | LBRACE; obj_expr=js_lexpr_target; COMMA; prop_expr=js_lexpr_target; RBRACE; LARROW; val_expr=js_lexpr_target
    { JSAsrt.PointsTo (obj_expr, prop_expr, val_expr) }
(* emp *)
  | LEMP;
    { JSAsrt.Emp }
(* schain(fid: le) *)
  | SCHAIN; LBRACE; fid=VAR; COLON; le=js_lexpr_target; RBRACE
    { JSAsrt.SChain (fid, le) }
(* x(e1, ..., en) *)
  | name = VAR; LBRACE; params = separated_list(COMMA, js_lexpr_target); RBRACE
    {
      (* validate_pred_assertion (name, params); *)
      JSAsrt.Pred (name, params)
    }
(* types (type_pairs) *)
  | LTYPES; LBRACE; type_pairs = separated_list(COMMA, js_type_env_pair_target); RBRACE
    { JSAsrt.Types type_pairs }
(* scope(x: le) *)
  | SCOPE; LBRACE; v=VAR; COLON; le=js_lexpr_target; RBRACE
    { JSAsrt.Scope (v, le) }
(* closure(x_0: le_0, ..., x_n: le_n; fid_0: le_0', ..., fid_n: le_n') *)
  | CLOSURE; LBRACE; var_les=separated_list(COMMA, var_js_le_pair_target); SCOLON; fid_scs=separated_list(COMMA, var_js_le_pair_target); RBRACE
    { JSAsrt.Closure (var_les, fid_scs)  }
(* sc_scope(pid, x: le1, le2) *)
  | SCSCOPE; LBRACE; pid=VAR; COMMA; x=VAR; COLON; le1=js_lexpr_target; COMMA; le2=js_lexpr_target; RBRACE
    { JSAsrt.VarSChain (pid, x, le1, le2) }
(* o_chains(pid1: le1, pid2: le2) *)
  | OCHAINS; LBRACE; pid1=VAR; COLON; le1=js_lexpr_target; COMMA; pid2=VAR; COLON; le2=js_lexpr_target; RBRACE
    { JSAsrt.OSChains (pid1, le1, pid2, le2) }
(* empty_fields (le : le_domain) *)
  | EMPTYFIELDS; LBRACE; le=js_lexpr_target; COLON; domain=js_lexpr_target; RBRACE
    { JSAsrt.EmptyFields (le, domain) }
(* Metadata (eo, em) *)
  | LMETADATA; LBRACE; eo = js_lexpr_target; COMMA; em = js_lexpr_target; RBRACE
    { (* validate_pred_assertion (name, params); *)
      JSAsrt.MetaData (eo, em)
    }
(* (P) *)
  | delimited(LBRACE, js_assertion_target, RBRACE)
    { $1 }


(* Predicates *)

js_named_assertion_target:
  id = option(assertion_id_target); a = js_assertion_target
  { (id, a) }

js_pred_defs_target:
  COLON; defs = separated_nonempty_list(COMMA, js_named_assertion_target)
  { defs }

js_pred_facts_target:
  FACTS; COLON; facts = separated_nonempty_list(AND, pure_assertion_target); SCOLON
  { facts }

js_pred_target:
(* pred name (arg1, ..., argn) : [def1_id: x1, ...] def1, ..., [def1_id: x1, ...] defn ; *)
  PRED; abstract=option(ABSTRACT); nounfold = option(NOUNFOLD); pure = option(PURE); pred_head = pred_head_target;
  definitions = option(js_pred_defs_target); SCOLON;
  facts=option(js_pred_facts_target); EOF
    { (* Add the predicate to the collection *)
      let (name, num_params, params, ins) = pred_head in
      let abstract = Option.is_some abstract in
      let nounfold = abstract || Option.is_some nounfold in
      let pure = Option.is_some pure in
      let definitions = Option.value ~default:[] definitions in
      let facts = Option.value ~default:[] facts in
      let () = if (abstract <> (definitions = [])) then
        raise (Failure (Format.asprintf "JS: Malformed predicate %s: either abstract with definition or non-abstract without definition." name))
      in
      Jslogic.JSPred.{ name; num_params; params; ins; definitions; facts; abstract; pure; nounfold }
    }


(* Specifications *)

outcome_target:
  | NORMAL { Flag.Normal }
  | ERROR  { Flag.Error  }

js_pre_post_target:
(* [lab: #x1, ..., #xn] [[ a1 ]] [[ a1'; ...; an' ]] flag *)
  lab_spec = option(lab_spec_target);
  OASSERT; pre  = js_assertion_target; CASSERT;
  OASSERT; post = separated_list(SCOLON, js_assertion_target); CASSERT;
  flag = outcome_target;
  { let sspec : JSSpec.st = { pre; post; flag; label = lab_spec } in sspec }

js_only_spec_target:
(* js_only_spec xpto (x, y) pre: assertion, post: assertion, flag: NORMAL|ERROR *)
  JSOS; spec_head = spec_head_target;
  sspecs = separated_nonempty_list(SCOLON, js_pre_post_target); EOF
  {
    let (name, params) = spec_head in
    Jslogic.JSSpec.{ name; params; sspecs }
  }


js_lvar_le_pair_target:
  LBRACE; lv = LVAR; COLON; le=js_lexpr_target; RBRACE { (lv, le )}

js_var_and_le_target:
  | LBRACE; lvar = LVAR; DEFEQ; le = js_lexpr_target; RBRACE;
    { (lvar, le) }

js_macro_head_target:
 | name = VAR; LBRACE; params = separated_list(COMMA, js_lexpr_target); RBRACE
   { (name, params) }

js_logic_bindings_target:
  | LBRACKET; id = VAR; WITH; var_les = separated_list(AND, js_var_and_le_target); RBRACKET
    { (id, var_les) }

(* [bind: (#x := le1) and ... ] *)
js_unfold_info_target:
  | LBRACKET; BIND; COLON; unfold_info = separated_list(AND, var_and_var_target); RBRACKET
    { unfold_info }

js_logic_cmd_target:
(* fold x(e1, ..., en) *)
  | FOLD; assertion = js_assertion_target; fold_info = option(js_logic_bindings_target)
    {
      JSLCmd.Fold (assertion, fold_info)
    }


(* unfold x(e1, ..., en) [ def1 with x1 := le1, ..., xn := len ] *)
  | UNFOLD; assertion = js_assertion_target; unfold_info = option(js_unfold_info_target)
    { JSLCmd.Unfold (assertion, unfold_info) }

(* unfold_all x *)
  | UNFOLDALL; name = VAR;
    { JSLCmd.GUnfold name }

  | BRANCH; LBRACE; pf = js_pure_assertion_target; RBRACE
    { JSLCmd.Branch pf }

(* flash x(e1, ..., en) *)
  | FLASH; assertion = js_assertion_target;
    { JSLCmd.Flash (assertion) }

(* if(le) { lcmds } else { lcmds } *)
  | LIF; LBRACE; le=js_lexpr_target; RBRACE; LTHEN; CLBRACKET;
      then_lcmds = separated_list(SCOLON, js_logic_cmd_target);
      CRBRACKET; LELSE; CLBRACKET;
      else_lcmds = separated_list(SCOLON, js_logic_cmd_target);
       CRBRACKET;
    { JSLCmd.If (le, then_lcmds, else_lcmds) }

(* if(e) { lcmd* } *)
  | LIF; LBRACE; le=js_lexpr_target; RBRACE; LTHEN; CLBRACKET;
      then_lcmds = separated_list(SCOLON, js_logic_cmd_target);
      CRBRACKET;
    { JSLCmd.If (le, then_lcmds, []) }

  | macro = js_macro_head_target;
    { let (name, params) = macro in JSLCmd.Macro (name, params) }

(* assert a *)
  | ASSERT; a = js_assertion_target; binders = option(binders_target);
    { JSLCmd.Assert (a, Option.value ~default:[ ] binders) }

(* assume a *)
  | ASSUME; a = js_pure_assertion_target;
    { JSLCmd.Assume a }

(* invariant a *)
  | INVARIANT; a = js_assertion_target; binders = option(binders_target);
    { JSLCmd.Invariant (a, Option.value ~default:[ ] binders)  }

(* apply lemma_name(args) *)
   | APPLY; lemma_name = VAR; LBRACE; params = separated_list(COMMA, js_lexpr_target); RBRACE
     {
      JSLCmd.ApplyLemma (lemma_name, params)
    }

(* use_subst [ spec_lab : #x: bla, #y: ble] *)
  | USESUBST; LBRACKET; spec_lab=VAR; COLON; subst_lst = separated_nonempty_list(COMMA, js_lvar_le_pair_target); RBRACKET
     {
        JSLCmd.UseSubst(spec_lab, subst_lst)
     }

(* (lcmd) *)
  | LBRACE; lcmds=js_logic_cmd_target; RBRACE
    { lcmds }


js_logic_cmds_target:
  | separated_list(SCOLON, js_logic_cmd_target); EOF
    { $1 }


top_level_js_assertion_list_target:
  al = separated_list(SCOLON, js_assertion_target); EOF { al }

top_level_js_assertion_target:
  a = js_assertion_target; EOF { a }


js_sspec_existentials_target: COLON; existentials=separated_list(COMMA, LVAR)
  { Containers.SS.of_list existentials }

js_sspec_lab_target:
  LBRACKET; lab=VAR; existentials = option(js_sspec_existentials_target); RBRACKET
    {
      match existentials with
        | None -> (lab, Containers.SS.empty)
        | Some existentials -> (lab, existentials)
    }

top_level_js_pre_target:
  lab = option(js_sspec_lab_target); a=js_assertion_target; EOF { (lab, a) }

top_level_expr_target:
  e = expr_target; EOF { e }