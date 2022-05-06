module GilType = Gillian.Gil_syntax.Type

let pp_option pp = Fmt.option ~none:(Fmt.any "None") pp
let pp_list ?(sep = Fmt.any ", ") = Fmt.list ~sep

type assert_annot = {
  label : string;
  (* Label of the spec *)
  existentials : (string * GilType.t option) list; (* Existentials of the spec *)
}

let pp_assert_annot ?(post = format_of_string "") fmt { label; existentials } =
  let pp_typ f t = Format.fprintf f " : %s" (GilType.str t) in
  let pp_lv_type f (lv, t) = Format.fprintf f "%s%a" lv (pp_option pp_typ) t in
  match existentials with
  | [] -> Format.fprintf fmt "[%s]%(%)" label post
  | _ ->
      Format.fprintf fmt "[%s: %a]%(%)" label (pp_list pp_lv_type) existentials
        post

module CBinOp = struct
  type t =
    | LstCons
    | LstCat
    | Plus
    | Minus
    | Times
    | Div
    | PtrPlus
    | Equal
    | SetSub
    | SetDiff
    | SetMem
    | LessThan
    | And
    | Or

  let str = function
    | LstCons -> "::"
    | LstCat -> "@"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | PtrPlus -> "p+"
    | Equal -> "="
    | SetSub -> "-s-"
    | SetDiff -> "-d-"
    | SetMem -> "-e-"
    | LessThan -> "<"
    | And -> "&"
    | Or -> "|"
end

module CUnOp = struct
  type t = LstLen | Not

  let str = function
    | LstLen -> "len"
    | Not -> "!"
end

module CNOp = struct
  type t = SetUnion

  let str = function
    | SetUnion -> "-u-"
end

module CSimplExpr = struct
  type t =
    | PVar of string
    | LVar of string
    | Loc of string
    | Int of Z.t
    | Bool of bool
    | String of string

  let pp fmt = function
    | PVar s | LVar s | Loc s -> Fmt.string fmt s
    | Int i -> Z.pp_print fmt i
    | Bool true -> Fmt.string fmt "true"
    | Bool false -> Fmt.string fmt "false"
    | String s -> Fmt.pf fmt "`%s`" s
end

module CSVal = struct
  type t =
    | Sint of CSimplExpr.t
    | Sfloat of CSimplExpr.t
    | Ssingle of CSimplExpr.t
    | Slong of CSimplExpr.t
    | Sptr of CSimplExpr.t * CSimplExpr.t
    | Sfunptr of string

  (* Symbol *)

  let pp fmt sval =
    let ppse = CSimplExpr.pp in
    match sval with
    | Sint se -> Format.fprintf fmt "int(%a)" ppse se
    | Slong se -> Format.fprintf fmt "long(%a)" ppse se
    | Sfloat se -> Format.fprintf fmt "float(%a)" ppse se
    | Ssingle se -> Format.fprintf fmt "single(%a)" ppse se
    | Sptr (se1, se2) -> Format.fprintf fmt "ptr(%a, %a)" ppse se1 ppse se2
    | Sfunptr s -> Format.fprintf fmt "funptr(%s)" s
end

module CExpr = struct
  type t =
    | SExpr of CSimplExpr.t
    | SVal of CSVal.t
    | EList of t list
    | LstSub of t * t * t
    | ESet of t list
    | BinOp of t * CBinOp.t * t
    | NOp of CNOp.t * t list
    | UnOp of CUnOp.t * t

  let rec pp fmt = function
    | SExpr se -> CSimplExpr.pp fmt se
    | SVal sv -> CSVal.pp fmt sv
    | BinOp (e1, b, e2) -> Fmt.pf fmt "(%a %s %a)" pp e1 (CBinOp.str b) pp e2
    | UnOp (u, e) -> Fmt.pf fmt "(%s %a)" (CUnOp.str u) pp e
    | EList el -> Fmt.pf fmt "[ %a ]" (pp_list pp) el
    | ESet es -> Fmt.pf fmt "-{ %a }-" (pp_list pp) es
    | NOp (n, el) -> Fmt.pf fmt "%s (%a)" (CNOp.str n) (pp_list pp) el
    | LstSub (lst, start, len) ->
        Fmt.pf fmt "lsub(%a, %a, %a)" pp lst pp start pp len
end

module CConstructor = struct
  type t = ConsExpr of CExpr.t | ConsStruct of string * CExpr.t list

  (* | ConsTyp of string * CExpr.t list *)
  (* It seems that the CSyntax already handles removing typedef aliasing *)

  let pp fmt = function
    | ConsExpr e -> CExpr.pp fmt e
    | ConsStruct (s, el) ->
        Format.fprintf fmt "@[<v 2>struct %s {@ %a@]@ }" s
          (pp_list ~sep:(Fmt.any ";@ ") CExpr.pp)
          el

  (* | ConsTyp (s, el) ->
        Format.fprintf fmt "@[<v 2>%s {@ %a@]@ }" s
          (pp_list ~sep:(format_of_string ";@ ") CExpr.pp)
          el *)
end

module CFormula = struct
  type t =
    | True
    | False
    | Eq of CExpr.t * CExpr.t
    | Less of CExpr.t * CExpr.t
    | LessEq of CExpr.t * CExpr.t
    | SetMem of CExpr.t * CExpr.t
    | And of t * t
    | Or of t * t
    | Not of t
    | Implies of t * t
    | ForAll of (string * GilType.t option) list * t

  let rec pp fmt f =
    let ppe = CExpr.pp in
    let pp_lvt fmt = function
      | s, None -> Format.pp_print_string fmt s
      | s, Some typ -> Format.fprintf fmt "%s : %s" s (GilType.str typ)
    in
    match f with
    | True -> Format.pp_print_string fmt "True"
    | False -> Format.pp_print_string fmt "False"
    | Eq (e1, e2) -> Format.fprintf fmt "(%a == %a)" ppe e1 ppe e2
    | Less (e1, e2) -> Format.fprintf fmt "(%a <# %a)" ppe e1 ppe e2
    | LessEq (e1, e2) -> Format.fprintf fmt "(%a <=# %a)" ppe e1 ppe e2
    | Not f -> Format.fprintf fmt "(not %a)" pp f
    | Or (f1, f2) -> Format.fprintf fmt "(%a || %a)" pp f1 pp f2
    | And (f1, f2) -> Format.fprintf fmt "(%a && %a)" pp f1 pp f2
    | Implies (f1, f2) -> Format.fprintf fmt "(%a => %a)" pp f1 pp f2
    | SetMem (e1, e2) -> Format.fprintf fmt "(%a --e-- %a)" ppe e1 ppe e2
    | ForAll (lvts, f) ->
        Format.fprintf fmt "(forall %a. %a)" (pp_list pp_lvt) lvts pp f
end

module CAssert = struct
  type points_to_type = Normal | Malloced | Global

  type t =
    | Malloced of (CExpr.t * CExpr.t)
    | Array of {
        ptr : CExpr.t;
        chunk : Chunk.t;
        size : CExpr.t;
        content : CExpr.t;
        malloced : bool;
      }
    | Undefs of (CExpr.t * CExpr.t)
    | Zeros of (CExpr.t * CExpr.t)
    | Star of t * t
    | Pure of CFormula.t
    | PointsTo of {
        ptr : CExpr.t;
        constr : CConstructor.t;
        typ : points_to_type;
      }
    | Pred of string * CExpr.t list
    | Emp

  let rec pp fmt a =
    match a with
    | Array { ptr; chunk; size; content; malloced } ->
        let pname = if malloced then "MARRAY" else "ARRAY" in
        Fmt.pf fmt "@[<h>%s(%a, %a, %a, %a)@]" pname CExpr.pp ptr Chunk.pp chunk
          CExpr.pp size CExpr.pp content
    | Malloced (e1, e2) ->
        Fmt.pf fmt "@[<h>MALLOCED(%a, %a)@]" CExpr.pp e1 CExpr.pp e2
    | Zeros (e1, e2) ->
        Fmt.pf fmt "@[<h>ZEROS(%a, %a)@]" CExpr.pp e1 CExpr.pp e2
    | Undefs (e1, e2) ->
        Fmt.pf fmt "@[<h>UNDEFS(%a, %a)@]" CExpr.pp e1 CExpr.pp e2
    | Star (a1, a2) -> Format.fprintf fmt "%a@ * %a" pp a1 pp a2
    | Pure f -> CFormula.pp fmt f
    | PointsTo { ptr; constr; typ } ->
        let string_of_typ = function
          | Normal -> ""
          | Malloced -> "m"
          | Global -> "s"
        in
        Format.fprintf fmt "(%a -%s> %a)" CExpr.pp ptr (string_of_typ typ)
          CConstructor.pp constr
    | Pred (s, el) -> Format.fprintf fmt "%s(%a)" s (pp_list CExpr.pp) el
    | Emp -> Format.fprintf fmt "emp"
end

module CLCmd = struct
  type t =
    | If of CExpr.t * t list * t list
        (** Conditional execution of logic command *)
    | Unfold of {
        pred : string;
        params : CExpr.t list;
        bindings : (string * string) list option;
        recursive : bool;
      }  (** Unfolding of a specific predicate *)
    | Unfold_all of string
        (** Recursively unfold all predicates with the given name (with a fuel). *)
    | Fold of string * CExpr.t list  (** Fold a predicate *)
    | Apply of string * CExpr.t list  (** Apply a lemma *)
    | Assert of CAssert.t * string list
        (** Assert for verification, takes an assertion and binders *)
    | Branch of CFormula.t
        (** The symbolic engine should branch on the given formula *)
    | Invariant of { assertion : CAssert.t; bindings : string list }
        (** Loop invariant *)
    | SymbExec
        (** Ignore the next function specification and symbolically execute instead *)

  let rec pp fmt lcmd =
    let pp_unfold_bindings ft b =
      Fmt.pf ft "[[bind %a]]"
        (Fmt.list ~sep:Fmt.comma
           (Fmt.pair ~sep:(Fmt.any ":@ ") Fmt.string Fmt.string))
        b
    in
    let pp_bindings ft b =
      match b with
      | [] -> ()
      | b -> Fmt.pf ft " [[bind %a]]" (Fmt.list ~sep:Fmt.comma Fmt.string) b
    in
    match lcmd with
    | Apply (s, el) -> Fmt.pf fmt "apply @[%s(%a)@]" s (pp_list CExpr.pp) el
    | Unfold { pred; params; bindings; recursive } ->
        Format.fprintf fmt "unfold%a @[%s(%a) [[bind %a]]@] "
          (fun fmt b -> if b then Fmt.string fmt "*" else ())
          recursive pred (pp_list CExpr.pp) params
          (Fmt.option pp_unfold_bindings)
          bindings
    | Unfold_all pred -> Fmt.pf fmt "unfold_all %s" pred
    | Fold (s, el) ->
        Format.fprintf fmt "fold @[%s(%a)@]" s (pp_list CExpr.pp) el
    | Assert (a, ex) ->
        Format.fprintf fmt "assert%a @[%a@]" pp_bindings ex CAssert.pp a
    | Invariant { assertion; bindings } ->
        Fmt.pf fmt "invariant %a %a" pp_bindings bindings CAssert.pp assertion
    | Branch f -> Format.fprintf fmt "branch %a" CFormula.pp f
    | If (e, cl1, cl2) -> (
        match cl2 with
        | [] ->
            Format.fprintf fmt "@[<v 2>if (%a) {@\n%a@]@\n}" CExpr.pp e
              (pp_list ~sep:(Fmt.any ";@\n") pp)
              cl1
        | _ ->
            Format.fprintf fmt
              "@[<v 2>if (%a) {@\n%a@]@\n@[<v 2>} else {@\n%a@]@\n}" CExpr.pp e
              (pp_list ~sep:(Fmt.any ";@\n") pp)
              cl1
              (pp_list ~sep:(Fmt.any ";@\n") pp)
              cl2)
    | SymbExec -> Format.fprintf fmt "symb_exec"
end

module CAbsPred = struct
  type t = {
    pure : bool;
    name : string;
    params : (string * GilType.t option) list;
    ins : int list;
  }

  let pp_params fmt (params, ins) =
    let pp_typ_opt f = function
      | None -> ()
      | Some t -> Format.fprintf f ": %s" (GilType.str t)
    in
    let plus f k = if List.mem k ins then Format.fprintf f "+" else () in
    let rec aux k = function
      | [] -> ()
      | [ (a, typ) ] -> Format.fprintf fmt "%a%s%a" plus k a pp_typ_opt typ
      | (a, typ) :: r ->
          Format.fprintf fmt "%a%s%a, " plus k a pp_typ_opt typ;
          aux (k + 1) r
    in
    aux 0 params

  let pp fmt pred =
    let pp_pure f = function
      | true -> Fmt.pf f "pure "
      | false -> ()
    in
    Fmt.pf fmt "abstract %apred %s(%a)" pp_pure pred.pure pred.name pp_params
      (pred.params, pred.ins)
end

module CPred = struct
  type t = {
    pure : bool;
    name : string;
    params : (string * GilType.t option) list;
    definitions : (assert_annot option * CAssert.t) list;
    ins : int list;
    no_unfold : bool;
  }

  let pp_params fmt (params, ins) =
    let pp_typ_opt f = function
      | None -> ()
      | Some t -> Format.fprintf f ": %s" (GilType.str t)
    in
    let plus f k = if List.mem k ins then Format.fprintf f "+" else () in
    let rec aux k = function
      | [] -> ()
      | [ (a, typ) ] -> Format.fprintf fmt "%a%s%a" plus k a pp_typ_opt typ
      | (a, typ) :: r ->
          Format.fprintf fmt "%a%s%a, " plus k a pp_typ_opt typ;
          aux (k + 1) r
    in
    aux 0 params

  let pp_def fmt (da, a) =
    Format.fprintf fmt "%a%a"
      (pp_option (pp_assert_annot ~post:(format_of_string " ")))
      da CAssert.pp a

  let pp fmt pred =
    let pp_pure f = function
      | true -> Fmt.pf f "pure "
      | false -> ()
    in
    Format.fprintf fmt "@[<v 2>%apred %s %s(%a) {@\n%a@]@\n}" pp_pure pred.pure
      pred.name
      (if pred.no_unfold then "nounfold" else "")
      pp_params (pred.params, pred.ins)
      (pp_list ~sep:(Fmt.any ";@\n") pp_def)
      pred.definitions
end

module CSpec = struct
  type st = {
    pre : CAssert.t;
    posts : CAssert.t list;
    spec_annot : assert_annot option;
  }

  type t = { fname : string; params : string list; sspecs : st list }

  let pp_sspec fmt sspec =
    Format.fprintf fmt "%arequires: @[%a@]@\nensures:  @[%a@]"
      (pp_option (pp_assert_annot ~post:(format_of_string "@\n")))
      sspec.spec_annot CAssert.pp sspec.pre
      (pp_list ~sep:(Fmt.any ";@\n") CAssert.pp)
      sspec.posts

  let pp fmt spec =
    Format.fprintf fmt "@[<v 2>spec %s(%a) {@\n%a@]@\n}" spec.fname
      (pp_list Format.pp_print_string)
      spec.params
      (pp_list ~sep:(Fmt.any "@\nOR@\n") pp_sspec)
      spec.sspecs
end

module CLemma = struct
  type t = {
    name : string;
    params : string list;
    hypothesis : CAssert.t;
    conclusions : CAssert.t list;
    proof : CLCmd.t list option;
  }

  let pp ft lemma =
    let open Fmt in
    let pp_proof ftt proof =
      Fmt.pf ftt "proof: @[<v 0>%a@]" (list ~sep:semi CLCmd.pp) proof
    in
    Fmt.pf ft
      "@[<v 2>lemma %s%a@ @[<h 2>{@ hypothesis: @[<h 0>%a@]@ conclusions: @[<h \
       0>%a@]@ proof: @[<v 0>%a@]@]}"
      lemma.name
      (parens (list ~sep:comma string))
      lemma.params CAssert.pp lemma.hypothesis (list ~sep:sp CAssert.pp)
      lemma.conclusions (option pp_proof) lemma.proof
end

module CProg = struct
  type t = {
    preds : CPred.t list;
    specs : CSpec.t list;
    lemmas : CLemma.t list;
    only_specs : CSpec.t list;
    imports : (string * bool) list;
    abs_preds : CAbsPred.t list;
  }

  let add_only_spec spec prog =
    { prog with only_specs = spec :: prog.only_specs }

  let add_imports imports prog = { prog with imports = imports @ prog.imports }
  let add_pred pred prog = { prog with preds = pred :: prog.preds }
  let add_abs_pred pred prog = { prog with abs_preds = pred :: prog.abs_preds }
  let add_spec spec prog = { prog with specs = spec :: prog.specs }
  let add_lemma lemma prog = { prog with lemmas = lemma :: prog.lemmas }

  let merge p1 p2 =
    {
      only_specs = p1.only_specs @ p2.only_specs;
      imports = p1.imports @ p2.imports;
      specs = p1.specs @ p2.specs;
      preds = p1.preds @ p2.preds;
      lemmas = p1.lemmas @ p2.lemmas;
      abs_preds = p1.abs_preds @ p2.abs_preds;
    }

  let empty =
    {
      preds = [];
      specs = [];
      lemmas = [];
      imports = [];
      only_specs = [];
      abs_preds = [];
    }

  let pp ft prog =
    let open Fmt in
    pf ft "[@<h>%a@ %a@ %a@]" (list ~sep:sp CPred.pp) prog.preds
      (list ~sep:sp CLemma.pp) prog.lemmas (list ~sep:sp CSpec.pp) prog.specs
end
