exception CannotHappen

(* syntax *)

type comparison_op =
  | Equal
  | NotEqual
  | TripleEqual
  | NotTripleEqual
  | Lt
  | Le
  | Gt
  | Ge
  | In
  | InstanceOf

type arith_op =
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | Ursh
  | Lsh
  | Rsh
  | Bitand
  | Bitor
  | Bitxor

type bool_op = And | Or

type bin_op =
  | Comparison of comparison_op
  | Arith      of arith_op
  | Boolean    of bool_op

type unary_op =
  | Not
  | TypeOf
  | Positive
  | Negative
  | Pre_Decr
  | Post_Decr
  | Pre_Incr
  | Post_Incr
  | Bitnot
  | Void

type var = string

type annotation_type =
  | Import  (** Import a JSIL or GIL file *)
  | TopRequires  (** Precondition of global  *)
  | TopEnsures  (** Normal postcondition of global *)
  | TopEnsuresErr  (** Error postcondition of global *)
  | Requires  (** Precondition of function *)
  | Ensures  (** Normal postcondition of function *)
  | EnsuresErr  (** Error postcondition of function *)
  | Id  (** Function identifier *)
  | Codename  (** Codename *)
  | Pred  (** Predicate *)
  | OnlySpec  (** Specification without function body *)
  | Invariant  (** Invariant *)
  | Lemma  (** Lemma *)
  | Tactic
      (** General tactic: fold, unfold, recursive unfold, assert, flash, callspec, and many more to come... *)
  | BiAbduce  (** Bi-abduction indicator *)
  | Call  (** Function call with substitution *)
  | JSIL_only  (** Function called in JSIL only *)

type annotation = { annot_type : annotation_type; annot_formula : string }

type propname =
  | PropnameId     of string
  | PropnameString of string
  | PropnameNum    of float

type proptype = PropbodyVal | PropbodyGet | PropbodySet

type exp = {
  exp_loc : Loc.t;
  exp_stx : exp_syntax;
  exp_annot : annotation list;
}

and exp_syntax =
  | Num           of float  (** 17 *)
  | String        of string  (** "abc" *)
  | Label         of string * exp  (** label: exp *)
  | Null  (** null *)
  | Bool          of bool  (** true, false *)
  | Var           of var  (** x *)
  | If            of exp * exp * exp option  (** if (e){e}{e} *)
  | While         of exp * exp  (** while (e){e} *)
  | DoWhile       of exp * exp  (** do {e} while e *)
  | VarDec        of (var * exp option) list  (** var x *)
  | This  (** this *)
  | Delete        of exp  (** delete e *)
  | Comma         of exp * exp  (** e, e *)
  | Unary_op      of unary_op * exp  (** unary_op e *)
  | BinOp         of exp * bin_op * exp  (** e op e*)
  | Access        of exp * string  (** e.x *)
  | Call          of exp * exp list  (** e(e1,..,en) *)
  | Assign        of exp * exp  (** e = e *)
  | AssignOp      of exp * arith_op * exp  (** e op= e *)
  | FunctionExp   of bool * string option * var list * exp
      (** function (x1,..,x2){e} *)
  | Function      of bool * string option * var list * exp
      (** function x(x1,..,x2){e} *)
  | New           of exp * exp list  (** new e(e1,..,en) *)
  | Obj           of (propname * proptype * exp) list  (** {x_i : e_i} *)
  | Array         of exp option list  (** [e1,...,en] *)
  | CAccess       of exp * exp  (** e[e] *)
  | With          of exp * exp  (** with (e){e} *)
  | Skip
  | Throw         of exp  (** throw e *)
  | Return        of exp option  (** return e *)
  | RegExp        of string * string  (** / pattern / flags *)
  | For           of exp option * exp option * exp option * exp
      (** for (e1; e2; e3) {e4} *)
  | ForIn         of exp * exp * exp  (** for (exp in exp) {exp}*)
  | Break         of string option
  | Continue      of string option
  | Try           of exp * (string * exp) option * exp option
      (** try e catch e finally e *)
  | Switch        of exp * (switch_case * exp) list
  | Debugger
  | ConditionalOp of exp * exp * exp  (** (e ? e : e) *)
  | Block         of exp list  (** { es } *)
  | Script        of bool * exp list

(* top node *)
and switch_case = Case of exp | DefaultCase

let mk_exp s o annots = { exp_loc = o; exp_stx = s; exp_annot = annots }

(** Returns true if the given ast is a Script AND is in Strict mode *)
let script_and_strict = function
  | Script (true, _) -> true
  | _                -> false
