%{
  open CLogic
%}

(* punctuation *)
%token COLON
%token DOT
%token SCOLON
%token COMMA
%token LBRACE
%token RBRACE
%token LCBRACE
%token RCBRACE
%token LBRACK
%token RBRACK
%token LDBRACK
%token RDBRACK
%token SETOP
%token SETCL
%token BIGOR
%token EOF

%token UNKOWN (* Ignored if not in an annotation *)
%token ANNOT_OPEN
%token ANNOT_CLOSE

%token <float> NUMBER
%token <string> IDENTIFIER
%token <string> LVAR
%token <string> LOC

(* Type things *)
%token INTT
%token FLOATT
%token LONGT
%token SINGLET
%token PTRT
%token STRUCT

(* Gil Type things *)
%token GNUMT
%token GSETT

(* Key words *)
%token IF
%token ELSE
%token PREDICATE
%token SPECIFICATION
%token REQUIRES
%token ENSURES
%token FOLD
%token UNFOLD
%token ASSERT
%token EXISTS
%token NULL
%token NIL
%token EMP
%token TRUE
%token FALSE
%token CTRUE
%token CFALSE
%token FORALL

(* BinOps *)
%token MALLOCPOINTSTO
%token POINTSTO
%token IMPLIES
%token STAR
%token LEQ
%token LLT
%token LLEQ
%token PLUS
%token LSTCONS
%token LSTCAT
%token ENOT
%token LNOT
%token EQ
%token SETMEM
%token SETSUB
%token SETDIFF

(* UnOps *)
%token LEN

(* NOps *)
%token SETUNION

(* Precedence *)
%nonassoc DOT

%left IMPLIES
%left STAR
%nonassoc LNOT

%nonassoc EQ
%nonassoc SETSUB
%left SETDIFF
%nonassoc LSTCONS
%left LSTCAT
%left PLUS

%nonassoc binop_prec
%nonassoc unop_prec

%start <CLogic.CProg.t> prog
%start <CLogic.CLCmd.t> logic_command_entry
%%

prog:
  | ANNOT_OPEN; pr = annot; ANNOT_CLOSE;
    rest = prog
    { CProg.merge pr rest }
  | any_C_token ; pr = prog { pr }
  | EOF { CProg.empty }

logic_command_entry:
  l = logic_command; EOF { l }

logic_command:
  | ASSERT; ex = exists_opt; a = assertion;
    { CLCmd.Assert (a, ex) }
  | FOLD; pname = IDENTIFIER; LBRACE; el = separated_list(COMMA, expression); RBRACE;
    { CLCmd.Fold (pname, el) }
  | UNFOLD; pname = IDENTIFIER; LBRACE; el = separated_list(COMMA, expression); RBRACE;
    { CLCmd.Unfold (pname, el) }
  | IF; LBRACE; e = expression; RBRACE; LCBRACE; cl = separated_nonempty_list(SCOLON, logic_command); RCBRACE
    { CLCmd.If (e, cl, []) }
  | IF; LBRACE; e = expression; RBRACE; LCBRACE; cl1 = separated_nonempty_list(SCOLON, logic_command); RCBRACE
    ELSE; LCBRACE; cl2 = separated_nonempty_list(SCOLON, logic_command); RCBRACE
    { CLCmd.If (e, cl1, cl2) }

exists_opt:
  | res = option(exists) { Option.value ~default:[] res }

exists:
  | LDBRACK; EXISTS; se = separated_list(COMMA, LVAR); RDBRACK;
    { se }

annot:
  | { CProg.empty }
  | pred = predicate; pr = annot {
    CProg.add_pred pred pr
  }
  | spec = specification; pr = annot {
    CProg.add_spec spec pr
  }

specification:
  | SPECIFICATION; fname = IDENTIFIER; LBRACE;
    params = separated_list(COMMA, IDENTIFIER);
    RBRACE; LCBRACE;
    sspecs = separated_nonempty_list(BIGOR, single_spec)
    RCBRACE {
      CSpec.{
        fname;
        params;
        sspecs;
      }
    }

single_spec:
  | spec_annot = option(spec_annot); REQUIRES; COLON; pre = assertion;
    ENSURES; COLON; posts = separated_nonempty_list(SCOLON, assertion);
    {  CSpec.{
        pre; posts; spec_annot
      }
    }


spec_annot:
  | LBRACK; lab = IDENTIFIER; exs = with_existentials; RBRACK; { { label = lab; existentials =  exs } }


predicate:
  | PREDICATE; pname = IDENTIFIER; LBRACE;
    params_ins = separated_list(COMMA, pred_params_ins);
    RBRACE; LCBRACE;
    defs = separated_nonempty_list(SCOLON, pred_definition);
    RCBRACE;
    {
      let (params, ins) = List.split params_ins in
      (* ins looks like [true, false, true] *)
      let ins = List.mapi (fun i is_in -> if is_in then Some i else None) ins in
      (* ins looks like [Some 0, None, Some 2] *)
      let ins = List.filter Option.is_some ins in
      (* ins looks like [Some 0, Some 2] *)
      let ins = List.map Option.get ins in
      (* ins looks like [0, 2] *)
  		let pred_ins = if (List.length ins) > 0 then ins else (List.mapi (fun i _ -> i) params) in
      (* if ins is empty then everything is an in *)
      CPred.{
        name = pname;
        params = params;
        ins = pred_ins;
        definitions = defs;
      }
    }

pred_definition:
  | d = option(asrt_annot); a = assertion { (d, a) }

asrt_annot:
  | LDBRACK; lab = IDENTIFIER; exs = with_existentials; RDBRACK; { { label = lab; existentials =  exs } }

with_existentials:
  | exs = option (existentials) { Option.value ~default:[] exs }

existentials:
  | COLON; lvs = separated_nonempty_list(COMMA, lvar_with_gil_type); { lvs }


pred_params_ins:
  | inp = option(PLUS); x = IDENTIFIER; typ = option(with_gil_type)
    { let isin = Option.is_some inp in
      ((x, typ), isin) }



assertion:
  | LBRACE; la = assertion; RBRACE { la }
  | EMP { CAssert.Emp }
  | la1 = assertion; STAR; la2 = assertion { CAssert.Star (la1, la2) }
  | e = expression; POINTSTO; cs = constructor {
    CAssert.PointsTo ( e, cs )
  }
  | e = expression; MALLOCPOINTSTO; cs = constructor {
    CAssert.MallocPointsTo ( e, cs )
  }
  | f = formula { CAssert.Pure f }
  | pname = IDENTIFIER; LBRACE; el = separated_list(COMMA, expression); RBRACE
    { CAssert.Pred (pname, el) }

formula:
  | LBRACE; formula; RBRACE { $2 }
  | e1 = expression; LLT; e2 = expression
    { CFormula.Less (e1, e2) }
  | e1 = expression; LLEQ; e2 = expression
    { CFormula.LessEq (e1, e2) }
  | e1 = expression; LEQ; e2 = expression
    { CFormula.Eq (e1, e2) }
  | e1 = expression; SETMEM; e2 = expression
    { CFormula.SetMem (e1, e2) }
  | LNOT; f = formula
    { CFormula.Not f }
  | f1 = formula; IMPLIES; f2 = formula
    { CFormula.Implies (f1, f2) }
  | FORALL; lvts = separated_nonempty_list(COMMA, lvar_with_gil_type); DOT; f = formula
    { CFormula.ForAll (lvts, f) }
  | TRUE { CFormula.True }
  | FALSE { CFormula.False }

lvar_with_gil_type:
  | l = LVAR; g = option(with_gil_type) { (l, g) }

with_gil_type:
  | COLON; g = gil_type { g }

gil_type:
  | GNUMT { GilType.NumberType }
  | GSETT { GilType.SetType }

constructor:
  | STRUCT; id = IDENTIFIER; LCBRACE;
    el = separated_nonempty_list(SCOLON, expression); RCBRACE
    { CConstructor.ConsStruct (id, el) }
  /* | id = IDENTIFIER; LCBRACE;
    el = separated_nonempty_list(SCOLON, expression); RCBRACE
    { Constructor.ConsTyp (id, el) } */
  | e = expression
    { CConstructor.ConsExpr e }


expression:
  | LBRACE; e = expression; RBRACE { e }
  | s = sval { CExpr.SVal s }
  | e = simple_expr { CExpr.SExpr e }
  | LBRACK; el = separated_list(COMMA, expression); RBRACK
    { CExpr.EList el }
  | NIL { CExpr.EList [] }
  | e1 = expression; b = binop; e2 = expression
    { CExpr.BinOp (e1, b, e2) } %prec binop_prec
  | u = unop; e = expression
    { CExpr.UnOp (u, e) } %prec unop_prec
  | SETOP; el = separated_list(COMMA, expression); SETCL
    { CExpr.ESet el }
  | n = nop; LBRACE; el = separated_nonempty_list(COMMA, expression); RBRACE
    { CExpr.NOp (n, el) }

sval:
  | NULL
    { if Compcert.Archi.ptr64 then CSVal.Slong (Num 0.) else CSVal.Sint (Num 0.) }
  | CTRUE
    { CSVal.Sint (Num 1.) }
  | CFALSE
    { CSVal.Sint (Num 0.) }
  | INTT; LBRACE; se = simple_expr; RBRACE
    { CSVal.Sint se }
  | FLOATT; LBRACE; se = simple_expr; RBRACE
    { CSVal.Sfloat se }
  | SINGLET; LBRACE; se = simple_expr; RBRACE
    { CSVal.Ssingle se }
  | LONGT; LBRACE; se = simple_expr; RBRACE
    { CSVal.Slong se }
  | PTRT; LBRACE; sl = simple_expr; COMMA; so = simple_expr; RBRACE
    { CSVal.Sptr (sl, so) }


simple_expr:
  | pvar = IDENTIFIER { CSimplExpr.PVar pvar }
  | n = NUMBER { CSimplExpr.Num n }
  | lvar = LVAR { CSimplExpr.LVar lvar }
  | loc = LOC { CSimplExpr.Loc loc }

binop:
  | LSTCONS { CBinOp.LstCons }
  | LSTCAT  { CBinOp.LstCat }
  | PLUS    { CBinOp.Plus }
  | EQ      { CBinOp.Equal }
  | SETSUB  { CBinOp.SetSub }
  | SETDIFF { CBinOp.SetDiff }

unop:
  | LEN  { CUnOp.LstLen }
  | ENOT { CUnOp.Not }

nop:
  | SETUNION { CNOp.SetUnion }


(* This is a bit hacky, but I don't have a better solution *)
any_C_token:
  | LEN
  | FOLD
  | EXISTS
  | UNKOWN
  | IF
  | ELSE
  | ANNOT_CLOSE (* I can close a comment *)
  | ASSERT
  | NUMBER
  | IDENTIFIER
  | LVAR
  | LOC
  | INTT
  | DOT
  | FLOATT
  | LONGT
  | SINGLET
  | PTRT
  | GNUMT
  | GSETT
  | FORALL
  | IMPLIES
  | STRUCT
  | PREDICATE
  | SPECIFICATION
  | REQUIRES
  | ENSURES
  | NULL
  | NIL
  | ENOT
  | LNOT
  | EQ
  | EMP
  | TRUE
  | FALSE
  | CTRUE
  | CFALSE
  | SETMEM
  | SETUNION
  | SETOP
  | SETCL
  | POINTSTO
  | BIGOR
  | MALLOCPOINTSTO
  | STAR
  | LEQ
  | LLT
  | LLEQ
  | PLUS
  | LSTCONS
  | LSTCAT
  | COLON
  | SCOLON
  | COMMA
  | LBRACE
  | RBRACE
  | LCBRACE
  | RCBRACE
  | LBRACK
  | RBRACK { () }