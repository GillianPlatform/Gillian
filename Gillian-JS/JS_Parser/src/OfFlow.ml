open Flow_parser.Flow_ast
open GJS_syntax
open Error
module Loc = Flow_parser.Loc

type loc = Loc.t
type pos = Loc.position

exception CannotHappen of string

(* IF YOU ARE GOING TO DIVE INTO THIS CODE, PLEASE READ THIS :

   To understand better the way this code works, the reader is invited to read the ESTree spec : https://github.com/estree/estree
   This spec describes the AST that is given by the `esprima` parser. This code uses `flow-parser`, which outputs a very similar AST, but in OCaml.
   We only care about the parts of that AST that were available in ES5, all the rest will raise a `NotEcmaScript5` exception.

   Moreover, we part of the parsing of JS Logic annotations here. In particular, the `flow-parser` separates the comments from the code, althoug it gives information
   about where said annotations are.
   Each transforming function takes the possible annotations that will be attached to the produced content. Then it detaches its own annotations from the annotations
   of its child(ren) by selecting only the annotations that are happening before the children.
*)

(*******  This part deals with extracting annotations *******)

let deal_with_whitespace (s : string) =
  let renl = Str.regexp "\n" in
  let s : string = Str.global_replace renl " " s in
  let retb = Str.regexp "[\t]+" in
  let s : string = Str.global_replace retb " " s in
  let resp = Str.regexp "[ ]+" in
  let s : string = Str.global_replace resp " " s in
  s

let remove_first = function
  | _ :: b :: r -> b :: r
  | _ ->
      raise
        (CannotHappen
           "Removing the first of a list that has less than 2 elements")

let make_annotation (atype, adesc) =
  let atype =
    match atype with
    | "requires" -> Some Requires
    | "ensures" -> Some Ensures
    | "ensureserr" -> Some EnsuresErr
    | "toprequires" -> Some TopRequires
    | "topensures" -> Some TopEnsures
    | "topensureserr" -> Some TopEnsuresErr
    | "pre" -> Some Requires
    | "post" -> Some Ensures
    | "posterr" -> Some EnsuresErr
    | "id" -> Some Id
    | "pred" -> Some Pred
    | "onlyspec" -> Some OnlySpec
    | "invariant" -> Some Invariant
    | "lemma" -> Some Lemma
    | "tactic" -> Some Tactic
    | "codename" -> Some Codename
    | "biabduce" -> Some BiAbduce
    | "call" -> Some Call
    | "JSIL" -> Some JSIL_only
    | "import" -> Some Import
    | _ -> None
  in
  Option.map (fun annot_type -> { annot_type; annot_formula = adesc }) atype

let get_annotations (comments : loc Comment.t list) :
    (loc * GJS_syntax.annotation list) list =
  (* Extracts strig from comment *)
  let mapkeep f l = List.map (fun (a, b) -> (a, f b)) l in
  let filterkeep f l = List.filter (fun (_, b) -> f b) l in
  let only_string comment = comment.Comment.text in
  let simp_comments = mapkeep only_string comments in
  (* Cleans comment strings *)
  let clean cs = deal_with_whitespace (String.trim cs) in
  let clean_comments = mapkeep clean simp_comments in
  let split cs = List.map clean (String.split_on_char '@' cs) in
  let splited_comments = mapkeep split clean_comments in
  (* splited_comments is a list of (loc * string list),
     if the string_list inside the tuple is of size < 2,
     it means that there was no '@' in the comment, therefore it is not
     an annotation, we remove that comment from our considerations *)
  let only_annot_comments =
    filterkeep (fun s -> List.length s >= 2) splited_comments
  in
  (* Since we splitted on the '@' char, and are only interested in what's after,
     we can always remove the first element of the list *)
  let only_annots = mapkeep remove_first only_annot_comments in
  let spaces =
    List.map
      (fun (_, z) ->
        (List.map (fun x -> try Some (String.index x ' ') with _ -> None)) z)
      only_annots
  in
  let annot_pairs =
    List.map2
      (fun (loc, cms) il ->
        ( loc,
          List.map2
            (fun c i ->
              match i with
              | None -> None
              | Some i ->
                  Some
                    ( String.trim (String.sub c 0 i),
                      String.trim (String.sub c i (String.length c - i)) ))
            cms il ))
      only_annots spaces
  in
  let rec filter_and_get = function
    | a :: r -> (
        match a with
        | None -> filter_and_get r
        | Some x -> x :: filter_and_get r)
    | [] -> []
  in
  let annot_pairs_clean = mapkeep filter_and_get annot_pairs in
  let annot_pairs = mapkeep (List.map make_annotation) annot_pairs_clean in
  mapkeep filter_and_get annot_pairs

(******* Just a small part to deal with directives *********)

let get_directives st_lst =
  let rec loop curr rest =
    match rest with
    | [] -> curr
    | (_, a) :: rp -> (
        match a with
        | Statement.Expression { directive = Some d; _ } -> loop (d :: curr) rp
        | _ -> curr (* There is not more directives *))
  in
  List.rev (loop [] st_lst)

(** Returns true if the block is preceeded by a "use strict" directive *)
let block_is_strict st_lst = List.mem "use strict" (get_directives st_lst)

(******* Now we deal with AST transformation *********)

let lower_pos posa posb = Loc.pos_cmp posa posb < 0
let lower_eq_pos posa posb = Loc.pos_cmp posa posb <= 0

type loc_compare = Before | After | Child | Parent

let compare_loc loca locb =
  let open Loc in
  if lower_pos loca._end locb.start then Before
  else if lower_pos locb._end loca.start then After
  else if lower_eq_pos loca.start locb.start && lower_eq_pos locb._end loca._end
  then Parent
  else if lower_eq_pos locb.start loca.start && lower_eq_pos loca._end locb._end
  then Child
  else raise (ParserError Overlapping_Syntax)

(* Loc utils *)
let before loca locb =
  match compare_loc loca locb with
  | Before -> true
  | _ -> false

let child loca locb =
  match compare_loc loca locb with
  | Child -> true
  | _ -> false

let after loca locb =
  match compare_loc loca locb with
  | After -> true
  | _ -> false

let get_first = function
  | [] -> raise (CannotHappen "Getting the first element of an empty list")
  | a :: _ -> a

let rec get_last = function
  | [] -> raise (CannotHappen "Getting the last element of an empty list")
  | [ a ] -> a
  | _ :: r -> get_last r

let rem_locs (annots : (loc * annotation list) list) : annotation list =
  List.flatten (List.map (fun (_, b) -> b) annots)

let leading (annots : (loc * annotation list) list) (loc : loc) =
  List.filter (fun (l, _) -> before l loc) annots

let leading_list
    (annots : (loc * annotation list) list)
    (l_with_loc : (loc * 'a) list) : (loc * annotation list) list =
  match l_with_loc with
  | [] -> annots
  | lll ->
      let fl, _ = get_first lll in
      leading annots fl

let partition_inner (loc : loc) (annots : (loc * annotation list) list) =
  List.partition (fun (l, _) -> child l loc) annots

let char_plus loc num =
  let open Loc in
  let _end = { loc._end with column = loc._end.column + num } in
  let start = _end in
  { loc with start; _end }

let char_after loc = char_plus loc 1

let rec with_start_loc start_loc ll =
  match ll with
  | [] -> []
  | (loc, a) :: r ->
      let nl = char_after loc in
      (start_loc, (loc, a)) :: with_start_loc nl r

(* Takes a list of annotations (with locs), check if its empty. If it is not it raises the appropriate UnusedAnnotations error *)
let check_unused_annots (loc : loc) annotation_l =
  let flattened = rem_locs annotation_l in
  let () =
    if List.length flattened > 0 then
      let message = List.map PrettyPrint.string_of_annot flattened in
      raise (ParserError (UnusedAnnotations (message, loc)))
  in
  ()

(* Add annotations to a JSParser expression. *)
let add_annot annots exp = { exp with exp_annot = exp.exp_annot @ annots }

(* Option utils *)
let option_map f o =
  match o with
  | None -> None
  | Some i -> Some (f i)

(* Flow AST utils *)
let get_str_id ((_, b) : (loc, loc) Identifier.t) = b.name

let get_str_pattern_restricted pat loc =
  let open Pattern in
  match pat with
  | Identifier.(Identifier { name = _, { name; _ }; _ }) -> name
  | Expression _ ->
      raise
        (ParserError
           (NotEcmaScript5 ("ES5: Unsupported pattern: Expression", loc)))
  | Object _ ->
      raise
        (ParserError (NotEcmaScript5 ("ES5: Unsupported pattern: Object", loc)))
  | Array _ ->
      raise
        (ParserError (NotEcmaScript5 ("ES5: Unsupported pattern: Array", loc)))

let function_param_filter params =
  (* We are in ES5, so the only pattern available when declaring a function is an identifier.
      also there is no `rest` *)
  let _, Function.Params.{ params; rest; _ } = params in
  let () =
    match rest with
    | Some (lr, _) ->
        raise (ParserError (NotEcmaScript5 ("Using rest params", lr)))
    | None -> ()
  in
  (* Now we are going to filter patterns that are not Identifier, if we find something else, we raise an error *)
  let rec f = function
    | [] -> []
    (* TODO: This is where the default parameter values will appear via the Assignment pattern *)
    | (_, param) :: r ->
        let Function.Param.{ argument = loc, pattern; default } = param in
        let () =
          match default with
          | Some (loc, _) ->
              raise
                (ParserError (NotEcmaScript5 ("Using default argument", loc)))
          | None -> ()
        in
        get_str_pattern_restricted pattern loc :: f r
  in
  f params

(* Actual transformations *)

(* let[@ocaml.deprecated] absolutely_do_not_ever_actually_use = mk_exp This 0 [] *)

let transform_unary_op loc =
  let open Expression in
  function
  | Unary.Minus -> Negative
  | Unary.Plus -> Positive
  | Unary.Not -> Not
  | Unary.BitNot -> Bitnot
  | Unary.Typeof -> TypeOf
  | Unary.Void -> Void
  | Unary.Delete ->
      raise
        (CannotHappen
           "Delete is a special case of operator that should have been caught \
            earlier")
  | Unary.Await ->
      raise
        (ParserError
           (NotEcmaScript5 ("The await keyword is not part of ES5", loc)))

let transform_binary_op loc =
  let open Expression in
  function
  | Binary.Equal -> Comparison Equal
  | Binary.NotEqual -> Comparison NotEqual
  | Binary.StrictEqual -> Comparison TripleEqual
  | Binary.StrictNotEqual -> Comparison NotTripleEqual
  | Binary.LessThan -> Comparison Lt
  | Binary.LessThanEqual -> Comparison Le
  | Binary.GreaterThan -> Comparison Gt
  | Binary.GreaterThanEqual -> Comparison Ge
  | Binary.LShift -> Arith Lsh
  | Binary.RShift -> Arith Rsh
  | Binary.RShift3 -> Arith Ursh
  | Binary.Plus -> Arith Plus
  | Binary.Minus -> Arith Minus
  | Binary.Mult -> Arith Times
  | Binary.Exp ->
      raise
        (ParserError
           (NotEcmaScript5
              ("Exponentiation operator (**) is not part of ES5", loc)))
  | Binary.Div -> Arith Div
  | Binary.Mod -> Arith Mod
  | Binary.BitOr -> Arith Bitor
  | Binary.Xor -> Arith Bitxor
  | Binary.BitAnd -> Arith Bitand
  | Binary.In -> Comparison In
  | Binary.Instanceof -> Comparison InstanceOf

let transform_assignment_op loc =
  let open Expression.Assignment in
  let not_es5 op_name =
    ParserError
      (NotEcmaScript5 ("The " ^ op_name ^ " operator is not part of ES5", loc))
  in
  function
  | PlusAssign -> Plus
  | MinusAssign -> Minus
  | MultAssign -> Times
  | ExpAssign -> raise (not_es5 "exponentiation")
  | DivAssign -> Div
  | ModAssign -> Mod
  | LShiftAssign -> Lsh
  | RShiftAssign -> Rsh
  | RShift3Assign -> Ursh
  | BitOrAssign -> Bitor
  | BitXorAssign -> Bitxor
  | BitAndAssign -> Bitand
  | AndAssign -> raise (not_es5 "&&=")
  | OrAssign -> raise (not_es5 "||=")
  | NullishAssign -> raise (not_es5 "??=")

let transform_logical_op loc =
  let open Expression in
  function
  | Logical.Or -> Boolean Or
  | Logical.And -> Boolean And
  | Logical.NullishCoalesce ->
      raise
        (ParserError
           (NotEcmaScript5
              ("The Nullish Coalescing operator is not part of ES5", loc)))

let transform_update_op prefix op =
  let open Expression.Update in
  match (prefix, op) with
  | true, Increment -> Pre_Incr
  | true, Decrement -> Pre_Decr
  | false, Increment -> Post_Incr
  | false, Decrement -> Post_Decr

let rec transform_properties ~parent_strict start_loc annotations properties =
  let open Expression.Object.Property in
  match properties with
  | [] ->
      let () = check_unused_annots start_loc annotations in
      []
  | (loc, Init { key; value; shorthand = _ }) :: r ->
      let inner_annots, rest_annots =
        partition_inner (Loc.btwn start_loc loc) annotations
      in
      let trans_key = transform_prop_key key in
      let trans_val = transform_expression ~parent_strict inner_annots value in
      (* PETAR: ALLOWING SHORTHAND!
         let () =
           if shorthand then
             Format.printf "Shorthand:\n\tKey: %s\n\tValue: %s"
               (PrettyPrint.string_of_propname trans_key)
               (PrettyPrint.string_of_exp_syntax trans_val.exp_stx);
           raise
             (ParserError
                (NotEcmaScript5 ("Shorthand properties are not part of ES5", loc))) *)
      (trans_key, PropbodyVal, trans_val)
      :: transform_properties ~parent_strict (char_after loc) rest_annots r
  | (loc, Get { key; value; _ }) :: r | (loc, Set { key; value; _ }) :: r ->
      let inner_annots, rest_annots =
        partition_inner (Loc.btwn start_loc loc) annotations
      in
      let trans_key = transform_prop_key key in
      let lf, func = value in
      let fun_annots, leading_annots = partition_inner lf inner_annots in
      let trans_fun =
        transform_function ~parent_strict ~expression:true start_loc
          (rem_locs leading_annots) fun_annots func
      in
      let typ =
        match properties with
        | (_, Get _) :: _ -> PropbodyGet
        | (_, Set _) :: _ -> PropbodySet
        | _ ->
            raise
              (CannotHappen "Impossible: accessor other than getter or setter")
      in
      (trans_key, typ, trans_fun)
      :: transform_properties ~parent_strict (char_after loc) rest_annots r
  | (loc, Method _) :: _ ->
      raise
        (ParserError
           (NotEcmaScript5 ("Methods are not allowed in ES5 for objects", loc)))

and transform_prop_key key =
  let open Expression.Object.Property in
  match key with
  | StringLiteral (_, { value = s; _ }) -> PropnameString s
  | NumberLiteral (_, { value = f; _ }) -> PropnameNum f
  | Identifier i -> PropnameId (get_str_id i)
  | BigIntLiteral (l, _) | PrivateName (l, _) | Computed (l, _) ->
      raise
        (ParserError
           (NotEcmaScript5
              ( "Only strings or string and int literals are authorised as \
                 property key in ES5",
                l )))

and transform_expression_sequence
    ~parent_strict
    start_pos
    leading_annots
    inner_annots
    expr_list =
  let rec aux stpos annots acc expl =
    match expl with
    | [] ->
        let () = check_unused_annots start_pos annots in
        acc
    | exp :: r ->
        let le, _ = exp in
        let e_annots, rest_annots =
          partition_inner (Loc.btwn stpos (char_after le)) annots
        in
        let trans_exp = transform_expression ~parent_strict e_annots exp in
        let trans_acc = mk_exp (Comma (acc, trans_exp)) start_pos [] in
        aux (char_after le) rest_annots trans_acc r
  in
  match expr_list with
  | fst :: snd :: rest ->
      let lfst, _ = fst in
      let fst_annots, rest_annots =
        partition_inner (Loc.btwn start_pos (char_after lfst)) inner_annots
      in
      let trans_fst = transform_expression ~parent_strict fst_annots fst in
      let lsnd, _ = snd in
      let snd_annots, rest_annots =
        List.partition
          (fun (l, _) -> child l (Loc.btwn (char_after lfst) lsnd))
          rest_annots
      in
      let trans_snd = transform_expression ~parent_strict snd_annots snd in
      let acc = mk_exp (Comma (trans_fst, trans_snd)) start_pos [] in
      add_annot leading_annots (aux (char_after lsnd) rest_annots acc rest)
  | _ -> raise (CannotHappen "Expression sequence with less than 2 elements")

and transform_expression
    ~parent_strict
    (annotations : (loc * annotation list) list)
    (expression : (loc, loc) Expression.t) : exp =
  let loc, expr = expression in
  (* We supposedly passed with the expression :
     - the annotations of its children
     - its own leading annotations.
     Therefore, we can simply split them here. *)
  let leading_annots, inner_annots =
    List.partition (fun (l, _) -> before l loc) annotations
  in
  let leading_annots = rem_locs leading_annots in
  let first_pos = Loc.first_char loc in
  match expr with
  | Expression.(Unary Unary.{ operator; argument; _ }) -> (
      let trans_arg =
        transform_expression ~parent_strict inner_annots argument
      in
      match operator with
      | Expression.Unary.Delete -> mk_exp (Delete trans_arg) loc leading_annots
      | o ->
          let trans_op = transform_unary_op loc o in
          mk_exp (Unary_op (trans_op, trans_arg)) loc leading_annots)
  | Expression.(Binary Binary.{ left; right; operator; _ }) ->
      let leftloc, _ = left in
      let left_annots, right_annots =
        partition_inner (Loc.btwn first_pos (char_after leftloc)) inner_annots
      in
      let trans_left = transform_expression ~parent_strict left_annots left in
      let trans_right =
        transform_expression ~parent_strict right_annots right
      in
      let trans_op = transform_binary_op loc operator in
      mk_exp (BinOp (trans_left, trans_op, trans_right)) loc leading_annots
  | Expression.(Assignment Assignment.{ left; right; operator; _ }) -> (
      let leftloc, _ = left in
      let left_annots, right_annots =
        partition_inner (Loc.btwn first_pos (char_after leftloc)) inner_annots
      in
      let trans_left =
        let locpat, pat = left in
        match pat with
        | Pattern.(Identifier Identifier.{ name = _, { name; _ }; _ }) ->
            mk_exp (Var name) locpat (rem_locs left_annots)
        | Pattern.Expression ex ->
            transform_expression ~parent_strict left_annots ex
        | _ ->
            raise
              (ParserError
                 (NotEcmaScript5
                    ( "Pattern matching in assignment is not authorized in ES5",
                      locpat )))
      in
      let trans_right =
        transform_expression ~parent_strict right_annots right
      in
      match operator with
      | None -> mk_exp (Assign (trans_left, trans_right)) loc leading_annots
      | Some o ->
          let trans_op = transform_assignment_op loc o in
          mk_exp
            (AssignOp (trans_left, trans_op, trans_right))
            loc leading_annots)
  | Expression.(Logical Logical.{ left; right; operator; _ }) ->
      let leftloc, _ = left in
      let left_annots, right_annots =
        partition_inner (Loc.btwn first_pos leftloc) inner_annots
      in
      let trans_left = transform_expression ~parent_strict left_annots left in
      let trans_right =
        transform_expression ~parent_strict right_annots right
      in
      let trans_op = transform_logical_op loc operator in
      mk_exp (BinOp (trans_left, trans_op, trans_right)) loc leading_annots
  | Expression.(Update Update.{ operator; argument; prefix; _ }) ->
      let trans_arg =
        transform_expression ~parent_strict inner_annots argument
      in
      let trans_op = transform_update_op prefix operator in
      mk_exp (Unary_op (trans_op, trans_arg)) loc leading_annots
  | Expression.(Member Member.{ _object; property; _ }) -> (
      let lobject, _ = _object in
      let object_annots, mem_annots =
        partition_inner (Loc.btwn first_pos lobject) inner_annots
      in
      let trans_obj =
        transform_expression ~parent_strict object_annots _object
      in
      let open Expression.Member in
      match property with
      | PropertyIdentifier i ->
          let strname = get_str_id i in
          mk_exp (Access (trans_obj, strname)) loc leading_annots
      | PropertyExpression e ->
          let trans_prop = transform_expression ~parent_strict mem_annots e in
          mk_exp (CAccess (trans_obj, trans_prop)) loc leading_annots
      | PropertyPrivateName (l, _) ->
          raise
            (ParserError
               (NotEcmaScript5 ("Private properties are not part of ES5", l))))
  | Expression.This _ ->
      let () = check_unused_annots loc inner_annots in
      mk_exp This loc leading_annots
  | Expression.(Object Object.{ properties; _ }) ->
      let open Expression.Object in
      (* First we filter object properties *)
      let props =
        List.map
          (function
            | Property p -> p
            | SpreadProperty (l, _) ->
                raise
                  (ParserError
                     (NotEcmaScript5 ("Use of spread property illegal in ES5", l))))
          properties
      in
      let trans_props =
        transform_properties ~parent_strict first_pos inner_annots props
      in
      mk_exp (Obj trans_props) loc leading_annots
  | Expression.(Sequence Sequence.{ expressions; _ }) ->
      transform_expression_sequence ~parent_strict first_pos leading_annots
        inner_annots expressions
  | Expression.(New New.{ callee; arguments; _ }) ->
      let arguments =
        Option.fold ~none:[]
          ~some:(fun (_, al) -> al.Expression.ArgList.arguments)
          arguments
      in
      let calleeloc, _ = callee in
      let callee_annots, arg_annots =
        partition_inner (Loc.btwn first_pos calleeloc) inner_annots
      in
      let trans_callee =
        transform_expression ~parent_strict callee_annots callee
      in
      let exprs_args =
        List.map
          Expression.(
            function
            | Expression e -> e
            | Spread _ ->
                raise
                  (ParserError
                     (NotEcmaScript5 ("Use of spread illegal in ES5", loc))))
          arguments
      in
      let trans_args =
        transform_expr_list ~parent_strict (char_after calleeloc) arg_annots
          exprs_args
      in
      mk_exp (New (trans_callee, trans_args)) loc leading_annots
  | Expression.(Conditional Conditional.{ test; consequent; alternate; _ }) ->
      let trans_cond =
        transform_expr_list ~parent_strict first_pos inner_annots
          [ test; consequent; alternate ]
      in
      let trans_test, trans_cons, trans_alt =
        match trans_cond with
        | [ t; c; a ] -> (t, c, a)
        | _ -> raise (CannotHappen "Inconsistent size of array after mapping")
      in
      mk_exp
        (ConditionalOp (trans_test, trans_cons, trans_alt))
        loc leading_annots
  | Expression.(Array Array.{ elements; _ }) ->
      let expr_opt_els =
        List.map
          Expression.Array.(
            function
            | Expression e -> Some e
            | Spread _ ->
                raise
                  (ParserError
                     (NotEcmaScript5 ("Use of spread illegal in ES5", loc)))
            | Hole _ -> None)
          elements
      in
      let rec trans_els_opt start_pos annots expr_opt_l =
        match expr_opt_l with
        | [] ->
            let () = check_unused_annots start_pos annots in
            []
        | exp_opt :: r -> (
            match exp_opt with
            | Some exp ->
                let le, _ = exp in
                let this_annots, rest_annots =
                  partition_inner (Loc.btwn start_pos le) annots
                in
                Some (transform_expression ~parent_strict this_annots exp)
                :: trans_els_opt (char_after le) rest_annots r
            | None -> None :: trans_els_opt start_pos annots r)
      in
      let trans_els = trans_els_opt first_pos inner_annots expr_opt_els in
      mk_exp (Array trans_els) loc leading_annots
  | Expression.(Identifier (_, { name; _ })) ->
      mk_exp (Var name) loc (rem_locs annotations)
  | Expression.(StringLiteral { value = s; _ }) ->
      mk_exp (String s) loc leading_annots
  | Expression.(BooleanLiteral { value = b; _ }) ->
      mk_exp (Bool b) loc leading_annots
  | Expression.(NullLiteral _) -> mk_exp Null loc leading_annots
  | Expression.(NumberLiteral { value = f; _ }) ->
      mk_exp (Num f) loc leading_annots
  | Expression.(BigIntLiteral _) ->
      raise (ParserError (NotEcmaScript5 ("BigInt not part of ES5", loc)))
  | Expression.(RegExpLiteral { pattern; flags; _ }) ->
      mk_exp (RegExp (pattern, flags)) loc leading_annots
  | Expression.(Call Call.{ callee; arguments = _, { arguments; _ }; _ }) ->
      let calleeloc, _ = callee in
      let callee_annots, arg_annots =
        partition_inner (Loc.btwn first_pos calleeloc) inner_annots
      in
      let trans_callee =
        transform_expression ~parent_strict callee_annots callee
      in
      let exprs_args =
        List.map
          Expression.(
            function
            | Expression e -> e
            | Spread _ ->
                raise
                  (ParserError
                     (NotEcmaScript5 ("Use of spread illegal in ES5", loc))))
          arguments
      in
      let trans_args =
        transform_expr_list ~parent_strict (char_after calleeloc) arg_annots
          exprs_args
      in
      mk_exp (Call (trans_callee, trans_args)) loc leading_annots
  | Expression.Function fn | Expression.ArrowFunction fn ->
      transform_function ~parent_strict ~expression:true first_pos
        leading_annots inner_annots fn
  | e ->
      let str = Expression.show_t' (fun _ _ -> ()) (fun _ _ -> ()) e in
      print_string str;
      raise (ParserError (Unhandled_Expression loc))

and transform_expr_list ~parent_strict start_pos annots exprl =
  match exprl with
  | [] ->
      let () = check_unused_annots start_pos annots in
      []
  | exp :: r ->
      let le, _ = exp in
      let this_annots, rest_annots =
        partition_inner (Loc.btwn start_pos le) annots
      in
      transform_expression ~parent_strict this_annots exp
      :: transform_expr_list ~parent_strict (char_after le) rest_annots r

and create_assignment lpat pattern exp =
  let open Pattern in
  let open Flow_parser.Flow_ast.Pattern.Object in
  match pattern with
  | Identifier.(Identifier { name = _, { name; _ }; _ }) -> [ (name, exp) ]
  | Expression _ ->
      raise
        (ParserError
           (NotEcmaScript5 ("ES5: Unsupported pattern: Expression", lpat)))
  | Object { properties; _ } -> (
      match exp with
      | None ->
          raise
            (ParserError
               (NotEcmaScript5
                  ("ES5: Object pattern assignment without rhs", lpat)))
      | Some exp ->
          List.map
            (fun property ->
              match property with
              | Property (_, { key; _ }) ->
                  let propname : string =
                    match key with
                    | Identifier (_, { name; _ }) -> name
                    | _ ->
                        raise
                          (ParserError
                             (NotEcmaScript5
                                ( "ES5: Unsupported identifier in object \
                                   pattern: literal/computed",
                                  lpat )))
                  in
                  let propvalue =
                    { exp with exp_stx = Access (exp, propname) }
                  in
                  (propname, Some propvalue)
              | RestElement _ ->
                  raise
                    (ParserError
                       (NotEcmaScript5
                          ( "ES5: RestElement not supported in object pattern \
                             assignment",
                            lpat ))))
            properties)
  | Array { elements; _ } -> (
      let open Flow_parser.Flow_ast.Pattern.Array in
      match exp with
      | None ->
          raise
            (ParserError
               (NotEcmaScript5
                  ("ES5: Array pattern assignment without rhs", lpat)))
      | Some exp ->
          let aux_var =
            List.fold_right
              (fun element ac ->
                if ac <> None then ac
                else
                  match element with
                  | Hole _ -> ac
                  | Element
                      ( _,
                        {
                          argument = _, Identifier { name = _, { name; _ }; _ };
                          _;
                        } ) -> Some name
                  | _ ->
                      raise
                        (ParserError
                           (NotEcmaScript5
                              ( "ES5: Only identifiers supported in array \
                                 pattern assignment",
                                lpat ))))
              elements None
          in
          let aux_var = Option.get aux_var in
          let aux_exp = { exp with exp_annot = [] } in
          let aux_var_exp = { aux_exp with exp_stx = Var aux_var } in
          (aux_var, Some exp)
          :: List.concat
               (List.mapi
                  (fun i oelement ->
                    match oelement with
                    | Hole _ -> []
                    | Element
                        ( _,
                          {
                            argument =
                              _, Identifier { name = _, { name; _ }; _ };
                            _;
                          } ) ->
                        let propname : string = name in
                        let index =
                          { aux_exp with exp_stx = Num (float_of_int i) }
                        in
                        let propvalue =
                          {
                            aux_exp with
                            exp_stx = CAccess (aux_var_exp, index);
                          }
                        in
                        [ (propname, Some propvalue) ]
                    | Element _ ->
                        raise
                          (ParserError
                             (NotEcmaScript5
                                ( "ES5: Only identifiers supported in array \
                                   pattern assignment",
                                  lpat )))
                    | RestElement _ ->
                        raise
                          (ParserError
                             (NotEcmaScript5
                                ( "ES5: RestElement not supported in object \
                                   pattern assignment",
                                  lpat ))))
                  elements))

and transform_variable_decl
    ~parent_strict
    declaration
    total_loc
    leading_annots
    inner_annots =
  (* We are in ES5, the only patterns available when declaring variable is an identifier.
     Also, the kind has to be var, since let and cons were introduced in ES2015. *)
  let open Statement.VariableDeclaration in
  let start = Loc.first_char total_loc in
  let { kind; declarations; _ } = declaration in
  let () =
    match kind with
    | Var -> ()
    | _ ->
        raise
          (ParserError
             (NotEcmaScript5
                ("Using Let or Const is not authorized in ES5 !", start)))
  in
  let rec f stloc remaining_annots = function
    | [] ->
        let () = check_unused_annots total_loc remaining_annots in
        []
    | (_, Declarator.{ id = lpat, pattern; init }) :: r ->
        (* TODO: This is where the are object and array assignment patterns are enabled *)
        let exp, rest_annot, last_char =
          match init with
          | None -> (None, remaining_annots, char_after lpat)
          | Some (le, e) ->
              let this_annot, rest_annot =
                partition_inner (Loc.btwn stloc le) remaining_annots
              in
              ( Some (transform_expression ~parent_strict this_annot (le, e)),
                rest_annot,
                char_after le )
        in
        create_assignment lpat pattern exp @ f last_char rest_annot r
  in
  match declarations with
  | [] ->
      raise (CannotHappen "Empty list of declarators in variable declaration !")
  | children ->
      mk_exp (VarDec (f start inner_annots children)) total_loc leading_annots

and transform_function
    ~parent_strict
    ~(expression : bool)
    (* If true, gives a FunctionExp, other a Function (statement) *)
      (start_pos : Loc.t)
    (leading_annots : annotation list)
    (inner_annots : (loc * annotation list) list)
    (fn : (loc, loc) Function.t) : exp =
  let Function.{ id; params; body; _ } = fn in
  let id =
    match id with
    | None -> None
    | Some (_, { name; _ }) -> Some name
  in
  let param_strs = function_param_filter params in
  let body_transf, strictness =
    match body with
    | Function.BodyBlock (_, fbody) ->
        let Statement.Block.{ body; _ } = fbody in
        let strictness = parent_strict || block_is_strict body in
        let children =
          trans_stmt_list ~parent_strict:strictness start_pos body inner_annots
        in
        (mk_exp (Block children) start_pos [], strictness)
        (* Annotations are attached to the children *)
    | Function.BodyExpression expr ->
        (transform_expression ~parent_strict inner_annots expr, false)
  in
  if expression then
    mk_exp
      (FunctionExp (strictness, id, param_strs, body_transf))
      start_pos leading_annots
  else
    mk_exp
      (Function (strictness, id, param_strs, body_transf))
      start_pos leading_annots

and transform_statement
    ~parent_strict
    (annotations : (loc * annotation list) list)
    (statement : (loc, loc) Statement.t) : exp =
  let loc, stmt = statement in
  (* We supposedly passed with the statements :
     - the annotations of its children
     - its own leading annotations.
     Therefore, we can simply split them here. *)
  let leading_annots, inner_annots =
    List.partition (fun (l, _) -> before l loc) annotations
  in
  let leading_annots = rem_locs leading_annots in
  let first_pos = Loc.first_char loc in
  match stmt with
  | Statement.Block { body; _ } ->
      let children =
        trans_stmt_list ~parent_strict first_pos body inner_annots
      in
      mk_exp (Block children) loc leading_annots
  | Statement.FunctionDeclaration fn ->
      transform_function ~parent_strict ~expression:false first_pos
        leading_annots inner_annots fn
  | Statement.VariableDeclaration vd ->
      transform_variable_decl ~parent_strict vd loc leading_annots inner_annots
  | Statement.Expression e ->
      let Statement.Expression.{ expression; _ } = e in
      transform_expression ~parent_strict annotations expression
      (* all the annotations are given to the child directly *)
  | Statement.(
      If { test = ltest, testexp; consequent = lcons, consstmt; alternate; _ })
    ->
      let test_annots, other_annots =
        partition_inner (Loc.btwn loc ltest) inner_annots
      in
      (* annots between the `if` and the end of the test` *)
      let cons_annots, other_annots =
        partition_inner (Loc.btwn (char_after ltest) lcons) other_annots
      in
      (* annots between the test and the end of the consequent *)
      let trans_test =
        transform_expression ~parent_strict test_annots (ltest, testexp)
      in
      let trans_cons =
        transform_statement ~parent_strict cons_annots (lcons, consstmt)
      in
      let trans_alt, rest_annots =
        match alternate with
        | None -> (None, other_annots)
        | Some (lalt, { body = _, altstmt; comments = _ }) ->
            let alt_annots, rest_annots =
              partition_inner (Loc.btwn (char_after lcons) lalt) other_annots
            in
            let trans_alt =
              Some
                (transform_statement ~parent_strict alt_annots (lalt, altstmt))
            in
            (trans_alt, rest_annots)
      in
      let () = check_unused_annots loc rest_annots in
      mk_exp (If (trans_test, trans_cons, trans_alt)) loc leading_annots
  | Statement.(Labeled Labeled.{ label; body; comments = _ }) ->
      let locbody, _ = body in
      let loclab, Identifier.{ name = lab; comments = _ } = label in
      let child_annot, rest_annot =
        partition_inner (Loc.btwn (char_after loclab) locbody) inner_annots
      in
      let () = check_unused_annots loc rest_annot in
      let trans_child = transform_statement ~parent_strict child_annot body in
      mk_exp (Label (lab, trans_child)) loc leading_annots
  | Statement.(Break Break.{ label; comments = _ }) ->
      let () = check_unused_annots loc inner_annots in
      (* We do not consider annotations between the 'break' and the label if there is one *)
      let lab = option_map get_str_id label in
      mk_exp (Break lab) loc leading_annots
  | Statement.(Continue Continue.{ label; comments = _ }) ->
      let () = check_unused_annots loc inner_annots in
      (* We do not consider annotations between the 'break' and the label if there is one *)
      let lab = option_map get_str_id label in
      mk_exp (Continue lab) loc leading_annots
  | Statement.(With With.{ _object; body; comments = _ }) ->
      let lobj, _ = _object in
      let obj_annot, body_annot =
        partition_inner (Loc.btwn loc lobj) inner_annots
      in
      let trans_obj = transform_expression ~parent_strict obj_annot _object in
      let trans_body = transform_statement ~parent_strict body_annot body in
      (* Every remaining annotation goes to the body *)
      mk_exp (With (trans_obj, trans_body)) loc leading_annots
  | Statement.(Switch { discriminant; cases; exhaustive_out = _; comments = _ })
    ->
      let ldisc, _ = discriminant in
      let expr_annots, other_annots = partition_inner ldisc inner_annots in
      let trans_discr =
        transform_expression ~parent_strict expr_annots discriminant
      in
      let trans_cases =
        List.map (transform_case ~parent_strict other_annots) cases
      in
      (* This might lose some annotations between the end of a case and the beginning of the next one *)
      mk_exp (Switch (trans_discr, trans_cases)) loc leading_annots
  | Statement.(Throw Throw.{ argument; comments = _ }) ->
      let trans_arg =
        transform_expression ~parent_strict inner_annots argument
      in
      mk_exp (Throw trans_arg) loc leading_annots
  | Statement.(Try Try.{ block; handler; finalizer; comments = _ }) ->
      let lblock, Statement.Block.{ body; comments = _ } = block in
      let block_annots, other_annots = partition_inner lblock inner_annots in
      let block_start = Loc.first_char lblock in
      let trans_inside_block =
        trans_stmt_list ~parent_strict block_start body block_annots
      in
      let trans_block = mk_exp (Block trans_inside_block) lblock [] in
      let trans_handler, other_annots =
        match handler with
        | None -> (None, other_annots)
        | Some
            Statement.Try.CatchClause.(
              lhandler, { param; body = lbody, bblock; comments = _ }) ->
            let str_param =
              match param with
              | None ->
                  raise
                    (ParserError
                       (NotEcmaScript5
                          ("In ES5, catch must have a parameter!", lhandler)))
              (* TODO: Which patterns are expected here? *)
              | Some (lpat, pat) -> get_str_pattern_restricted pat lpat
            in
            let body_start = Loc.first_char lbody in
            let body_annots, other_annots =
              partition_inner lbody other_annots
            in
            let Statement.Block.{ body; comments = _ } = bblock in
            let trans_inside_body =
              trans_stmt_list ~parent_strict body_start body body_annots
            in
            let trans_body = mk_exp (Block trans_inside_body) lbody [] in
            let h = Some (str_param, trans_body) in
            (h, other_annots)
      in
      let trans_final, other_annots =
        match finalizer with
        | Some (lfin, Statement.Block.{ body; comments = _ }) ->
            let fin_annots, other_annots = partition_inner lfin other_annots in
            let block_start = Loc.first_char lfin in
            let trans_inside_final =
              trans_stmt_list ~parent_strict block_start body fin_annots
            in
            let trans_final = mk_exp (Block trans_inside_final) lfin [] in
            let h = Some trans_final in
            (h, other_annots)
        | None -> (None, other_annots)
      in
      let () = check_unused_annots loc other_annots in
      mk_exp (Try (trans_block, trans_handler, trans_final)) loc leading_annots
  | Statement.(While While.{ test; body; comments = _ }) ->
      let ltest, _ = test in
      let expr_annots, body_annots =
        partition_inner (Loc.btwn first_pos ltest) inner_annots
      in
      let trans_exp = transform_expression ~parent_strict expr_annots test in
      let trans_body = transform_statement ~parent_strict body_annots body in
      mk_exp (While (trans_exp, trans_body)) loc leading_annots
  | Statement.(DoWhile DoWhile.{ test; body; comments = _ }) ->
      let ltest, _ = test in
      let expr_annots, body_annots =
        partition_inner (Loc.btwn first_pos ltest) inner_annots
      in
      let trans_test = transform_expression ~parent_strict expr_annots test in
      let trans_body = transform_statement ~parent_strict body_annots body in
      mk_exp (DoWhile (trans_body, trans_test)) loc leading_annots
  | Statement.(ForIn ForIn.{ left; right; body; each = _; comments = _ }) ->
      let open Statement.ForIn in
      let trans_left, other_annots, endleft =
        match left with
        | LeftPattern (locleft, leftpat) ->
            let left_annots, other_annots =
              partition_inner (Loc.btwn first_pos locleft) inner_annots
            in
            (* TODO: Which patterns are expected here? *)
            let strpat = get_str_pattern_restricted leftpat locleft in
            let exp = mk_exp (Var strpat) locleft (rem_locs left_annots) in
            (exp, other_annots, char_after locleft)
        | LeftDeclaration (locleft, vdecl) ->
            let left_annots, other_annots =
              partition_inner (Loc.btwn first_pos locleft) inner_annots
            in
            let t =
              transform_variable_decl ~parent_strict vdecl locleft []
                left_annots
            in
            (t, other_annots, char_after locleft)
      in
      let trans_right, other_annots, endright =
        let locright, _ = right in
        let right_annots, other_annots =
          partition_inner (Loc.btwn endleft locright) other_annots
        in
        let exp = transform_expression ~parent_strict right_annots right in
        (exp, other_annots, char_after locright)
      in
      let trans_body, rest_annots =
        let locbody, _ = body in
        let body_annots, rest_annots =
          partition_inner (Loc.btwn endright locbody) other_annots
        in
        let exp = transform_statement ~parent_strict body_annots body in
        (exp, rest_annots)
      in
      let () = check_unused_annots loc rest_annots in
      mk_exp (ForIn (trans_left, trans_right, trans_body)) loc leading_annots
  | Statement.(For For.{ init; test; update; body; comments = _ }) ->
      let open Statement.For in
      let trans_init, other_annots, endinit =
        match init with
        | None -> (None, inner_annots, first_pos)
        | Some (InitDeclaration (ldec, vd)) ->
            let init_annots, other_annots =
              partition_inner (Loc.btwn first_pos ldec) inner_annots
            in
            let t =
              transform_variable_decl ~parent_strict vd ldec [] init_annots
            in
            (Some t, other_annots, char_after ldec)
        | Some (InitExpression (le, e)) ->
            let init_annots, other_annots =
              partition_inner (Loc.btwn first_pos le) inner_annots
            in
            let t = transform_expression ~parent_strict init_annots (le, e) in
            (Some t, other_annots, char_after le)
      in
      let trans_test, other_annots, endtest =
        match test with
        | None -> (None, other_annots, endinit)
        | Some (le, e) ->
            let test_annots, other_annots =
              partition_inner (Loc.btwn endinit le) other_annots
            in
            let t = transform_expression ~parent_strict test_annots (le, e) in
            (Some t, other_annots, char_after le)
      in
      let trans_update, other_annots, endupdate =
        match update with
        | None -> (None, other_annots, endtest)
        | Some (le, e) ->
            let update_annots, other_annots =
              partition_inner (Loc.btwn endinit le) other_annots
            in
            let t = transform_expression ~parent_strict update_annots (le, e) in
            (Some t, other_annots, char_after le)
      in
      let trans_body, rest_annots =
        let lbody, _ = body in
        let body_annots, other_annots =
          partition_inner (Loc.btwn endupdate lbody) other_annots
        in
        let t = transform_statement ~parent_strict body_annots body in
        (t, other_annots)
      in
      let () = check_unused_annots loc rest_annots in
      mk_exp
        (For (trans_init, trans_test, trans_update, trans_body))
        loc leading_annots
  | Statement.(Return Return.{ argument; return_out = _; comments = _ }) ->
      let trans_arg =
        option_map (transform_expression ~parent_strict inner_annots) argument
      in
      mk_exp (Return trans_arg) loc leading_annots
  | Statement.Empty { comments = _ } ->
      let () = check_unused_annots loc inner_annots in
      mk_exp Skip loc leading_annots
  | Statement.Debugger { comments = _ } ->
      let () = check_unused_annots loc inner_annots in
      mk_exp Debugger loc leading_annots
  | _ -> raise (ParserError (Unhandled_Statement loc))

and transform_case ~parent_strict annots case =
  (* I don't know how correct the following is. *)
  let open Statement.Switch.Case in
  let lcase, { test; consequent; comments = _ } = case in
  let inner_annots = List.filter (fun (l, _) -> child l lcase) annots in
  let test_end, trans_test =
    match test with
    | None ->
        (* `default` is of length 7 *)
        let test_end = char_plus lcase 7 in
        let trans_test = DefaultCase in
        (test_end, trans_test)
    | Some (le, e) ->
        let test_end = char_after le in
        let trans_test =
          Case (transform_expression ~parent_strict [] (le, e))
        in
        (test_end, trans_test)
  in
  let trans_cons =
    trans_stmt_list ~parent_strict test_end consequent inner_annots
  in
  (trans_test, mk_exp (Block trans_cons) test_end [])

and trans_stmt_list ~parent_strict start_loc raw_stmts annots =
  let stmts_with_start_loc = with_start_loc start_loc raw_stmts in
  let trans_stmt (st_l, (lloc, s)) =
    let children_annotations =
      List.filter (fun (l, _) -> child l (Loc.btwn st_l lloc)) annots
    in
    transform_statement ~parent_strict children_annotations (lloc, s)
  in
  List.map trans_stmt stmts_with_start_loc

let transform_program
    ~parse_annotations
    ~force_strict
    (prog : (loc, loc) Program.t) =
  let start_loc = Loc.none in
  (* As of @esy-ocaml/flow-parser v0.76, this is position (0, 0) *)
  let ( loc,
        Program.
          {
            statements = raw_stmts;
            all_comments = cmts;
            interpreter = _;
            comments = _;
          } ) =
    prog
  in
  let strictness = force_strict || block_is_strict raw_stmts in
  let annots = if parse_annotations then get_annotations cmts else [] in
  let stmts =
    trans_stmt_list ~parent_strict:strictness start_loc raw_stmts annots
  in
  (* A script never has annotations *)
  mk_exp (Script (strictness, stmts)) loc []
