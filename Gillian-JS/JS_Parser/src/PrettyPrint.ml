open List
open GJS_syntax

let string_of_comparison_op x =
  match x with
  | Equal          -> "=="
  | NotEqual       -> "!="
  | TripleEqual    -> "==="
  | NotTripleEqual -> "!=="
  | Lt             -> "<"
  | Le             -> "<="
  | Gt             -> ">"
  | Ge             -> ">="
  | In             -> "in"
  | InstanceOf     -> "instanceof"

let string_of_bool_op x =
  match x with
  | And -> "&&"
  | Or  -> "||"

let string_of_arith_op x =
  match x with
  | Plus   -> "+"
  | Minus  -> "-"
  | Times  -> "*"
  | Div    -> "/"
  | Mod    -> "%"
  | Ursh   -> ">>>"
  | Lsh    -> "<<"
  | Rsh    -> ">>"
  | Bitand -> "&"
  | Bitor  -> "|"
  | Bitxor -> "^"

let string_of_bin_op x =
  match x with
  | Comparison op -> string_of_comparison_op op
  | Arith op      -> string_of_arith_op op
  | Boolean op    -> string_of_bool_op op

let string_of_unary_op op =
  match op with
  | Not                  -> "!"
  | TypeOf               -> "typeof"
  | Positive             -> "+"
  | Negative             -> "-"
  | Pre_Decr | Post_Decr -> "--"
  | Pre_Incr | Post_Incr -> "++"
  | Bitnot               -> "~"
  | Void                 -> "void"

let string_of_var x = x

let string_of_vars xs = String.concat "," xs

let string_of_annot_type atype =
  match atype with
  | Import        -> "@import"
  | TopRequires   -> "@toprequires"
  | TopEnsures    -> "@topensures"
  | TopEnsuresErr -> "@topensureserr"
  | Requires      -> "@pre"
  | Ensures       -> "@post"
  | EnsuresErr    -> "@posterr"
  | Id            -> "@id"
  | Codename      -> "@codename"
  | Pred          -> "@pred"
  | Invariant     -> "@invariant"
  | OnlySpec      -> "@onlyspec"
  | Tactic        -> "@tactic"
  | Lemma         -> "@lemma"
  | BiAbduce      -> "@biabduce"
  | Call          -> "@call"
  | JSIL_only     -> "@JSIL"

let string_of_annot annot =
  string_of_annot_type annot.annot_type ^ " " ^ annot.annot_formula

let string_of_annots annots =
  let annot_string = String.concat "\n" (map string_of_annot annots) in
  if annot_string <> "" then Printf.sprintf "/** %s */" annot_string else ""

let string_of_propname pn =
  match pn with
  | PropnameId id    -> id
  | PropnameString s -> Printf.sprintf "'%s'" s
  | PropnameNum n    -> string_of_float n

let rec string_of_exp with_annot exp =
  let annot_string =
    if with_annot then string_of_annots exp.exp_annot else ""
  in
  Printf.sprintf "%s%s"
    (if annot_string <> "" then annot_string ^ "\n" else "")
    (string_of_exp_syntax_1 exp.exp_stx with_annot)

and string_of_var_in_dec with_annot (x, v) =
  match v with
  | None   -> x
  | Some v -> Printf.sprintf "%s = (%s)" x (string_of_exp with_annot v)

and string_of_exp_syntax_1 expstx with_annot =
  let f = string_of_exp with_annot in
  let fop e =
    match e with
    | None   -> ""
    | Some e -> f e
  in
  let string_op s =
    match s with
    | None     -> ""
    | Some str -> str
  in
  match expstx with
  | Num n                      -> string_of_float n
  | String x                   -> Printf.sprintf "\"%s\"" x
  | Label (x, e)               -> Printf.sprintf "%s: %s" x (f e)
  | Null                       -> "null"
  | Bool b                     -> string_of_bool b
  | Var x                      -> string_of_var x
  | If (e1, e2, None)          -> Printf.sprintf "if (%s) {\n%s\n}" (f e1) (f e2)
  | If (e1, e2, Some e3)       ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (f e1) (f e2) (f e3)
  | While (e1, e2)             -> Printf.sprintf "while (%s) \n%s\n" (f e1)
                                    (f e2)
  | DoWhile (e1, e2)           -> Printf.sprintf "do \n%s\n while (%s) \n"
                                    (f e1) (f e2)
  | VarDec xs                  ->
      Printf.sprintf "var %s"
        (String.concat ", " (map (string_of_var_in_dec with_annot) xs))
  | This                       -> "this"
  | Delete e                   -> Printf.sprintf "delete %s" (f e)
  | Comma (e1, e2)             -> Printf.sprintf "%s , %s" (f e1) (f e2)
  | Unary_op (op, e)           -> (
      match op with
      | Post_Decr | Post_Incr ->
          Printf.sprintf "%s %s" (f e) (string_of_unary_op op)
      | _                     -> Printf.sprintf "%s %s" (string_of_unary_op op)
                                   (f e))
  | BinOp (e1, op, e2)         ->
      Printf.sprintf "(%s) %s (%s)" (f e1) (string_of_bin_op op) (f e2)
  | Access (e, x)              -> Printf.sprintf "(%s).%s" (f e) x
  | Call (e1, e2s)             ->
      Printf.sprintf "(%s)(%s)" (f e1) (String.concat "," (map f e2s))
  | Assign (e1, e2)            -> Printf.sprintf "%s = %s" (f e1) (f e2)
  | AssignOp (e1, op, e2)      ->
      Printf.sprintf "%s %s= %s" (f e1) (string_of_arith_op op) (f e2)
  | FunctionExp (_, n, xs, e)  ->
      Printf.sprintf "function %s(%s) \n%s\n" (string_op n) (string_of_vars xs)
        (f e)
  | Function (_, n, xs, e)     ->
      Printf.sprintf "function %s(%s) \n%s\n" (string_op n) (string_of_vars xs)
        (f e)
  | New (e1, e2s)              ->
      Printf.sprintf "new (%s)(%s)" (f e1) (String.concat "," (map f e2s))
  | Obj l                      ->
      Printf.sprintf "{%s}"
        (String.concat "; "
           (map
              (fun (x, p, e) ->
                match p with
                | PropbodyVal ->
                    Printf.sprintf "%s : %s" (string_of_propname x) (f e)
                | PropbodyGet ->
                    Printf.sprintf "get %s %s" (string_of_propname x) (f e)
                | PropbodySet ->
                    Printf.sprintf "set %s %s" (string_of_propname x) (f e))
              l))
  | Array es                   ->
      Printf.sprintf "[%s]"
        (String.concat ", "
           (map
              (fun e ->
                match e with
                | None   -> ""
                | Some e -> Printf.sprintf "%s" (f e))
              es))
  | CAccess (e1, e2)           -> Printf.sprintf "(%s)[%s]" (f e1) (f e2)
  | With (e1, e2)              -> Printf.sprintf "with (%s){\n%s\n}" (f e1)
                                    (f e2)
  | Skip                       -> "Skip"
  | Throw e                    -> Printf.sprintf "throw %s" (f e)
  | Return (Some e)            -> Printf.sprintf "return %s" (f e)
  | Return None                -> "return"
  | RegExp (s1, s2)            -> Printf.sprintf "/%s/%s" s1 s2
  | ForIn (e1, e2, e3)         ->
      Printf.sprintf "for (%s in %s) %s" (f e1) (f e2) (f e3)
  | For (e1, e2, e3, e4)       ->
      Printf.sprintf "for (%s ; %s ; %s) %s" (fop e1) (fop e2) (fop e3) (f e4)
  | Break None                 -> "break"
  | Break (Some l)             -> Printf.sprintf "break %s" l
  | Continue None              -> "continue"
  | Continue (Some l)          -> Printf.sprintf "continue %s" l
  | Try (e1, e2s, e3)          ->
      Printf.sprintf "try %s\n%s" (f e1)
        (string_of_catch_finally with_annot e2s e3)
  | Switch (e1, e2s)           ->
      Printf.sprintf "switch (%s) {\n%s\n}" (f e1)
        (String.concat "\n" (map (string_of_case with_annot) e2s))
  | Debugger                   -> "debugger"
  | ConditionalOp (e1, e2, e3) ->
      Printf.sprintf "((%s) ? (%s) : (%s))" (f e1) (f e2) (f e3)
  | Block es                   -> Printf.sprintf "{ %s }"
                                    (String.concat ";\n" (map f es))
  | Script (_, es)             -> Printf.sprintf "%s"
                                    (String.concat ";\n" (map f es))

and string_of_catch_finally with_annot catch finally =
  (match catch with
  | None        -> ""
  | Some (v, e) -> Printf.sprintf "catch (%s) %s" v (string_of_exp with_annot e))
  ^
  match finally with
  | None   -> ""
  | Some e -> Printf.sprintf " finally %s" (string_of_exp with_annot e)

and string_of_case with_annot (case, exp) =
  let label =
    match case with
    | Case e      -> Printf.sprintf "case %s:" (string_of_exp with_annot e)
    | DefaultCase -> "default:"
  in
  Printf.sprintf "%s\n %s \n" label (string_of_exp with_annot exp)

let string_of_exp_syntax expstx = string_of_exp_syntax_1 expstx false
