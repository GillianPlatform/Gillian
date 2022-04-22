%{
  open CLogic

  let process_pred_params params_ins =
      let (params, ins) = List.split params_ins in
      (* ins looks like [true, false, true] *)
      let ins = List.mapi (fun i is_in -> if is_in then Some i else None) ins in
      (* ins looks like [Some 0, None, Some 2] *)
      let ins = List.filter Option.is_some ins in
      (* ins looks like [Some 0, Some 2] *)
      let ins = List.map Option.get ins in
      (* ins looks like [0, 2] *)
      (* if ins is empty then everything is an in *)
  		let ins = if (List.length ins) > 0 then ins else (List.mapi (fun i _ -> i) params) in
      (params, ins)

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

%token ANNOT_OPEN
%token ANNOT_CLOSE

%token <string> STRING
%token <Z.t> INTEGER
%token <string> IDENTIFIER
%token <string> LVAR
%token <string> LOC

(* Type things *)
%token INT16T
%token INTT
%token CHART
%token FLOATT
%token LONGT
%token SINGLET
%token PTRT
%token FUNPTRT
%token STRUCT

(* Gil Type things *)
%token GINTT
%token GSETT
%token GLISTT

(* Key words *)
%token VERIFY
%token IMPORT
%token NOUNFOLD
%token IF
%token ELSE
%token PREDICATE
%token ABSTRACT
%token PURE
%token LEMMA
%token HYPOTHESIS
%token CONCLUSIONS
%token PROOF
%token SPECIFICATION
%token AXIOMATIC
%token REQUIRES
%token ENSURES
%token FOLD
%token UNFOLD
%token REC_UNFOLD
%token UNFOLD_ALL
%token SYMB_EXEC
%token APPLY
%token ASSERT
%token INVARIANT
%token FOR_LOOP
%token BRANCH
%token BIND
%token NULL
%token NIL
%token EMP
%token TRUE
%token FALSE
%token CTRUE
%token CFALSE
%token FORALL
%token MALLOCED
%token ARRAY
%token MALLOCED_ARRAY
%token UNDEFS
%token ZEROS

(* BinOps *)
%token MALLOCPOINTSTO
%token GLOBALPOINTSTO
%token POINTSTO
%token IMPLIES
%token STAR
%token LOR
%token LAND
%token LEQ
%token LLT
%token LLEQ
%token AND
%token OR
%token LT
%token PLUS
%token MINUS
%token DIV
%token PTRPLUS
%token LSTCONS
%token LSTCAT
%token ENOT
%token LNOT
%token EQ
%token LSETMEM
%token SETSUB
%token SETMEM
%token SETDIFF

(* UnOps & lists *)
%token LEN
%token LSUB

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
  | INVARIANT; COLON; bindings = exists_opt; assertion = assertion;
    {CLCmd.Invariant {
      assertion = assertion;
      bindings
    }}
  | APPLY; lname = IDENTIFIER; LBRACE; el = separated_list(COMMA, expression); RBRACE;
    { CLCmd.Apply (lname, el) }
  | ASSERT; ex = exists_opt; a = assertion;
    { CLCmd.Assert (a, ex) }
  | FOLD; pname = IDENTIFIER; LBRACE; el = separated_list(COMMA, expression); RBRACE;
    { CLCmd.Fold (pname, el) }
  | REC_UNFOLD; pred = IDENTIFIER; LBRACE; params = separated_list(COMMA, expression); RBRACE; bindings = option(fold_bindings)
    { CLCmd.Unfold { pred; params; bindings; recursive=true } }
  | UNFOLD; pred = IDENTIFIER; LBRACE; params = separated_list(COMMA, expression); RBRACE; bindings = option(fold_bindings)
    { CLCmd.Unfold { pred; params; bindings; recursive=false } }
  | UNFOLD_ALL; pred = IDENTIFIER { CLCmd.Unfold_all pred }
  | SYMB_EXEC { CLCmd.SymbExec }
  | BRANCH; f = formula
    { CLCmd.Branch f }
  | IF; LBRACE; e = expression; RBRACE; LCBRACE; cl = separated_nonempty_list(SCOLON, logic_command); RCBRACE
    { CLCmd.If (e, cl, []) }
  | IF; LBRACE; e = expression; RBRACE; LCBRACE; cl1 = separated_nonempty_list(SCOLON, logic_command); RCBRACE
    ELSE; LCBRACE; cl2 = separated_nonempty_list(SCOLON, logic_command); RCBRACE
    { CLCmd.If (e, cl1, cl2) }

fold_bindings:
  | LDBRACK; BIND; se = separated_list(COMMA, single_fold_binding); RDBRACK
    { se }

single_fold_binding:
  | a = LVAR; COLON; b = LVAR { (a, b) }

exists_opt:
  | res = option(exists) { Option.value ~default:[] res }

lvar_or_pvar:
  | LVAR { $1 }
  | IDENTIFIER { $1 }

exists:
  | LDBRACK; BIND; se = separated_list(COMMA, lvar_or_pvar); RDBRACK;
    { se }

annot:
  | { CProg.empty }
  | imports = import_gil; pr = annot {
    CProg.add_imports imports pr
  }
  | apred = abstract_predicate; pr = annot {
    CProg.add_abs_pred apred pr
  }
  | pred = predicate; pr = annot {
    CProg.add_pred pred pr
  }
  | spec = specification; pr = annot {
    CProg.add_spec spec pr
  }
  | spec = onlyspec; pr = annot {
    CProg.add_only_spec spec pr
  }
  | spec = lemma; pr = annot {
    CProg.add_lemma spec pr
  }

import_gil:
  | verify = option(VERIFY); IMPORT; files = separated_list(COMMA, STRING); SCOLON
    {  List.map (fun x -> (x ^ ".gil", Option.is_some verify)) files }


lemma:
  | LEMMA; name = IDENTIFIER;
    LBRACE; params = separated_list(COMMA, IDENTIFIER); RBRACE;
    LCBRACE;
    HYPOTHESIS; COLON; hypothesis = assertion;
    CONCLUSIONS; COLON; conclusions = separated_nonempty_list(SCOLON, assertion);
    proof = option(lemma_proof);
    RCBRACE;
    {
      CLemma.{
        name;
        params;
        hypothesis;
        conclusions;
        proof
      }
    }

lemma_proof:
  | PROOF; COLON; proof = separated_nonempty_list(SCOLON, logic_command); { proof }

onlyspec:
  | AXIOMATIC; spec = specification {
    spec
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


abstract_predicate:
  | ABSTRACT; pure=option(PURE); PREDICATE; pname = IDENTIFIER; LBRACE;
    params_ins = separated_list(COMMA, pred_params_ins);
    RBRACE;
    {
      let (params, pred_ins) = process_pred_params params_ins in
      CAbsPred. {
        pure = Option.is_some(pure);
        name = pname;
        params;
        ins = pred_ins;
      }
    }

predicate:
  | pure=option(PURE); PREDICATE; no_unfold=option(NOUNFOLD); pname = IDENTIFIER; LBRACE;
    params_ins = separated_list(COMMA, pred_params_ins);
    RBRACE; LCBRACE;
    defs = separated_nonempty_list(SCOLON, pred_definition);
    RCBRACE;
    {
      let (params, pred_ins) = process_pred_params params_ins in
      CPred.{
        name = pname;
        params = params;
        ins = pred_ins;
        definitions = defs;
        no_unfold = Option.is_some(no_unfold);
        pure = Option.is_some(pure);
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

typ:
  | INT16T { Chunk.Mint16unsigned }
  | INTT  { Chunk.Mint32 }
  | LONGT { Chunk.Mint64 }
  | FLOATT { Chunk.Mfloat64 }
  | SINGLET { Chunk.Mfloat32 }
  | CHART { Chunk.Mint8unsigned }
  | PTRT { Chunk.ptr }

assertion:
  | LBRACE; la = assertion; RBRACE { la }
  | EMP { CAssert.Emp }
  | la1 = assertion; STAR; la2 = assertion { CAssert.Star (la1, la2) }
  | e = expression; POINTSTO; cs = constructor {
    CAssert.PointsTo { ptr=e; constr=cs; typ=Normal }
  }
  | e = expression; MALLOCPOINTSTO; cs = constructor {
    CAssert.PointsTo { ptr=e; constr=cs; typ=Malloced }
  }
  | v = IDENTIFIER; GLOBALPOINTSTO; cs = constructor {
    let ptr = CExpr.SExpr (String v) in
    CAssert.PointsTo { ptr; constr=cs; typ=Global }
  }
  | f = formula { CAssert.Pure f }
  | ZEROS; LBRACE; ptr = expression; COMMA; size = expression; RBRACE
    { CAssert.Zeros(ptr, size) }
  | ARRAY; LBRACE; ptr = expression; COMMA; chunk = typ; COMMA;  size = expression; COMMA; content = expression; RBRACE
    { CAssert.Array { ptr; size; chunk; content; malloced = false } }
  | MALLOCED_ARRAY; LBRACE; ptr = expression; COMMA; chunk = typ; COMMA; size = expression; COMMA; content = expression; RBRACE
    { CAssert.Array { ptr; size; chunk; content; malloced = true }}
  | UNDEFS; LBRACE; ptr = expression; COMMA; size = expression; RBRACE
    { CAssert.Undefs (ptr, size)}
  | MALLOCED; LBRACE; ptr = expression; COMMA; ofs = expression; RBRACE
    { CAssert.Malloced(ptr, ofs) }
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
  | e1 = expression; LSETMEM; e2 = expression
    { CFormula.SetMem (e1, e2) }
  | LNOT; f = formula
    { CFormula.Not f }
  | f1 = formula; LAND; f2 = formula
    { CFormula.And (f1, f2) }
  | f1 = formula; LOR; f2 = formula
    { CFormula.Or (f1, f2) }
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
  | GINTT { GilType.IntType }
  | GSETT { GilType.SetType }
  | GLISTT { GilType.ListType }

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
  | LSUB; LBRACE; lst = expression; COMMA; start = expression; COMMA; length = expression; RBRACE;
    { CExpr.LstSub (lst, start, length) }
  | n = nop; LBRACE; el = separated_nonempty_list(COMMA, expression); RBRACE
    { CExpr.NOp (n, el) }

sval:
  | NULL
    { if Compcert.Archi.ptr64 then CSVal.Slong (Int Z.zero) else CSVal.Sint (Int Z.zero) }
  | CTRUE
    { CSVal.Sint (Int Z.one) }
  | CFALSE
    { CSVal.Sint (Int Z.zero) }
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
  | FUNPTRT; LBRACE; s = IDENTIFIER; RBRACE
    { CSVal.Sfunptr s }


simple_expr:
  | pvar = IDENTIFIER { CSimplExpr.PVar pvar }
  | n = INTEGER { CSimplExpr.Int n }
  | lvar = LVAR { CSimplExpr.LVar lvar }
  | loc = LOC { CSimplExpr.Loc loc }
  | str = STRING { CSimplExpr.String str}

binop:
  | LSTCONS { CBinOp.LstCons }
  | LSTCAT  { CBinOp.LstCat }
  | PLUS    { CBinOp.Plus }
  | MINUS   { CBinOp.Minus }
  | PTRPLUS { CBinOp.PtrPlus }
  | STAR    { CBinOp.Times }
  | DIV     { CBinOp.Div }
  | EQ      { CBinOp.Equal }
  | SETSUB  { CBinOp.SetSub }
  | SETMEM  { CBinOp.SetMem }
  | SETDIFF { CBinOp.SetDiff }
  | LT      { CBinOp.LessThan }
  | AND     { CBinOp.And }
  | OR      { CBinOp.Or }

unop:
  | LEN  { CUnOp.LstLen }
  | ENOT { CUnOp.Not }

nop:
  | SETUNION { CNOp.SetUnion }


(* This is a bit hacky, but I don't have a better solution *)
any_C_token:
  | LEN
  | FOLD
  | BIND
  | IF
  | ELSE
  | ANNOT_CLOSE (* I can close a comment *)
  | ASSERT
  | INTEGER
  | IDENTIFIER
  | LVAR
  | LOC
  | INTT
  | DOT
  | FLOATT
  | LONGT
  | SINGLET
  | PTRT
  | CHART
  | FUNPTRT
  | GINTT
  | GSETT
  | FORALL
  | IMPLIES
  | STRUCT
  | PREDICATE
  | LEMMA
  | HYPOTHESIS
  | CONCLUSIONS
  | PROOF
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
  | LSETMEM
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
  | RBRACK
  | RDBRACK
  | LDBRACK
  | MALLOCED
  | ARRAY
  | ZEROS
  | PTRPLUS
  | MINUS
  | NOUNFOLD
  | UNDEFS
  | APPLY
  | IMPORT
  | VERIFY
  | PURE
  | STRING
  | INT16T
  | AXIOMATIC
  | ABSTRACT
  | UNFOLD_ALL
  | UNFOLD
  | BRANCH
  | MALLOCED_ARRAY
  | LSUB
  | INVARIANT
  | SYMB_EXEC
  | FOR_LOOP
  | REC_UNFOLD
  | DIV
  | LT
  | LOR
  | LAND
  | GLOBALPOINTSTO
  | GLISTT
  | SETSUB
  | SETDIFF
  | SETMEM
  | AND
  | OR
  { () }