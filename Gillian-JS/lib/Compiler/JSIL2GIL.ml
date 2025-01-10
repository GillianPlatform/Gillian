open Jsil_syntax
open Javert_utils
module Gil = Gillian.Gil_syntax
module GAsrt = Gil.Asrt
module GSLCmd = Gil.SLCmd
module GLCmd = Gil.LCmd
module GSpec = Gil.Spec
module GLemma = Gil.Lemma
module GPred = Gil.Pred
module GMacro = Gil.Macro
module GProc = Gil.Proc
module GProg = Gil.Prog
module GBiSpec = Gil.BiSpec
module GCmd = Gil.Cmd
module Expr = Gil.Expr
module Annot = Gil.Annot

(**
 *  Fresh identifiers
 *)
let fresh_sth (name : string) : (unit -> string) * (unit -> unit) =
  let counter = ref 0 in
  let r () = counter := 0 in
  let f () =
    let v = name ^ string_of_int !counter in
    counter := !counter + 1;
    v
  in
  (f, r)

let fresh_then, reset_then = fresh_sth "glab_then_"
let fresh_else, reset_else = fresh_sth "glab_else_"
let fresh_var, reset_var = fresh_sth "gvar_aux_"

let resource_error args =
  if Utils.Exec_mode.is_biabduction_exec !Config.current_exec_mode then
    GCmd.Logic (GLCmd.Assume False)
  else Fail (JSILNames.resourceError, args)

let reset_generators () =
  reset_then ();
  reset_else ();
  reset_var ()

let rec jsil2gil_expr (e : Expr.t) : Expr.t =
  let f = jsil2gil_expr in
  match e with
  | UnOp (op, e') -> (
      let e = Expr.UnOp (op, f e') in
      match op with
      | Gil.UnOp.LstLen -> Expr.int_to_num e
      | _ -> e)
  | BinOp (e1, op, e2) ->
      let e1 = f e1 in
      let e2 =
        match op with
        | Gil.BinOp.LstNth -> Expr.num_to_int (f e2)
        | _ -> f e2
      in
      BinOp (e1, op, e2)
  | LstSub (lst, start, len) ->
      let lst = f lst in
      let start = Expr.num_to_int (f start) in
      let len = Expr.num_to_int (f len) in
      LstSub (lst, start, len)
  | NOp (op, es) ->
      let es = es |> List.map f in
      NOp (op, es)
  | EList es ->
      let es = es |> List.map f in
      EList es
  | ESet es ->
      let es = es |> List.map f in
      ESet es
  | _ -> e

let rec jsil2gil_formula (f : Gil.Formula.t) : Gil.Formula.t =
  let ff = jsil2gil_formula in
  let fe = jsil2gil_expr in
  match f with
  | True | False -> f
  | Not f -> Not (ff f)
  | And (f1, f2) -> And (ff f1, ff f2)
  | Or (f1, f2) -> Or (ff f1, ff f2)
  | Impl (f1, f2) -> Impl (ff f1, ff f2)
  | Eq (e1, e2) -> Eq (fe e1, fe e2)
  | FLess (e1, e2) -> FLess (fe e1, fe e2)
  | FLessEq (e1, e2) -> FLessEq (fe e1, fe e2)
  | ILess (e1, e2) -> ILess (fe e1, fe e2)
  | ILessEq (e1, e2) -> ILessEq (fe e1, fe e2)
  | StrLess (e1, e2) -> StrLess (fe e1, fe e2)
  | SetMem (e1, e2) -> SetMem (fe e1, fe e2)
  | SetSub (e1, e2) -> SetSub (fe e1, fe e2)
  | ForAll (qts, f) -> ForAll (qts, ff f)
  | IsInt e -> IsInt (fe e)

let rec jsil2gil_asrt (a : Asrt.t) : GAsrt.t =
  let f = jsil2gil_asrt in
  let fe = jsil2gil_expr in
  match a with
  | Emp -> [ Emp ]
  | Star (a1, a2) -> f a1 @ f a2
  | PointsTo (e1, e2, e3) ->
      [ Asrt_utils.points_to ~loc:(fe e1) ~field:(fe e2) ~value:(fe e3) ]
  | MetaData (e1, e2) -> [ Asrt_utils.metadata ~loc:(fe e1) ~metadata:(fe e2) ]
  | EmptyFields (e1, e2) ->
      [ Asrt_utils.empty_fields ~loc:(fe e1) ~domain:(fe e2) ]
  | Pred (pn, es) -> [ Pred (pn, List.map fe es) ]
  | Pure f -> [ Pure (jsil2gil_formula f) ]
  | Types vts -> [ Types (List.map (fun (v, t) -> (fe v, t)) vts) ]

let jsil2gil_slcmd (slcmd : SLCmd.t) : GSLCmd.t =
  match slcmd with
  | Fold (pn, es, info) -> Fold (pn, List.map jsil2gil_expr es, info)
  | Unfold (pn, es, info, b) -> Unfold (pn, List.map jsil2gil_expr es, info, b)
  | GUnfold pn -> GUnfold pn
  | ApplyLem (x, es, xs) -> ApplyLem (x, List.map jsil2gil_expr es, xs)
  | SepAssert (a, xs) -> SepAssert (jsil2gil_asrt a, xs)
  | Invariant (a, xs) -> Invariant (jsil2gil_asrt a, xs)

let rec jsil2gil_lcmd (lcmd : LCmd.t) : GLCmd.t =
  let f = jsil2gil_lcmd in
  let fs = List.map f in
  let fe = jsil2gil_expr in
  let ff = jsil2gil_formula in
  match lcmd with
  | If (e, lcmds1, lcmds2) -> If (fe e, fs lcmds1, fs lcmds2)
  | Branch f -> Branch (ff f)
  | Macro (x, es) -> Macro (x, List.map fe es)
  | Assert f -> Assert (ff f)
  | Assume f -> Assume (ff f)
  | AssumeType (x, t) -> AssumeType (fe x, t)
  | FreshSVar x -> FreshSVar x
  | SL slcmd -> SL (jsil2gil_slcmd slcmd)

let jsil2gil_sspec (sspec : Spec.st) : GSpec.st =
  let ss_label =
    Option.map (fun (l, vl) -> (l, Containers.SS.elements vl)) sspec.label
  in
  {
    ss_pre = jsil2gil_asrt sspec.pre;
    ss_posts = List.map jsil2gil_asrt sspec.posts;
    (* FIXME: bring in variant *)
    ss_variant = None;
    ss_flag = sspec.flag;
    ss_to_verify = sspec.to_verify;
    ss_label;
  }

let jsil2gil_spec (spec : Spec.t) : GSpec.t =
  {
    spec_name = spec.name;
    spec_params = spec.params;
    spec_sspecs = List.map jsil2gil_sspec spec.sspecs;
    spec_normalised = spec.normalised;
    spec_incomplete = spec.incomplete;
    spec_to_verify = spec.to_verify;
  }

(* TODO: Bring in OX *)
let jsil2gil_lemma (lemma : Lemma.t) : GLemma.t =
  {
    lemma_name = lemma.name;
    lemma_source_path = None;
    lemma_internal = false;
    (* TODO (Alexis): Set depending on module of lemma *)
    lemma_params = lemma.params;
    lemma_specs =
      [
        {
          lemma_hyp = jsil2gil_asrt lemma.pre;
          lemma_concs = List.map jsil2gil_asrt lemma.posts;
          lemma_spec_variant = Option.map jsil2gil_expr lemma.variant;
        };
      ];
    lemma_proof = Option.map (List.map jsil2gil_lcmd) lemma.proof;
    lemma_variant = Option.map jsil2gil_expr lemma.variant;
    lemma_existentials = lemma.existentials;
  }

let jsil2gil_pred (pred : Pred.t) : GPred.t =
  {
    pred_name = pred.name;
    pred_source_path = None;
    pred_internal = false;
    (* TODO (Alexis): Set depending on module of pred *)
    pred_num_params = pred.num_params;
    pred_params = pred.params;
    pred_ins = pred.ins;
    pred_definitions =
      List.map (fun (info, asrt) -> (info, jsil2gil_asrt asrt)) pred.definitions;
    pred_facts = List.map jsil2gil_formula pred.facts;
    pred_guard = None;
    (* TODO: Support for predicates with tokens *)
    pred_pure = pred.pure;
    pred_abstract = pred.abstract;
    pred_nounfold = pred.nounfold;
    pred_normalised = pred.normalised;
  }

let jsil2gil_macro (macro : Macro.t) : GMacro.t =
  {
    macro_name = macro.name;
    macro_params = macro.params;
    macro_definition = List.map jsil2gil_lcmd macro.definition;
  }

let jsil2gil_bispec (bispec : BiSpec.t) : GBiSpec.t =
  {
    bispec_name = bispec.name;
    bispec_params = bispec.params;
    bispec_pres = [ jsil2gil_asrt bispec.pre ];
    bispec_normalised = bispec.normalised;
  }

let jsil2core (lab : string option) (cmd : LabCmd.t) :
    (string option * string GCmd.t) list =
  let fe = jsil2gil_expr in
  match cmd with
  | LBasic Skip -> [ (lab, GCmd.Skip) ]
  | LBasic (Assignment (x, e)) -> [ (lab, GCmd.Assignment (x, fe e)) ]
  (*
      C(x := new(l1, l2) :-
        lab: aux1 := [new](l1, l2);
        aux2 := l-nth(aux1, 0)

    *)
  | LBasic (New (x, e1, e2)) ->
      let e1 = fe @@ Option.value ~default:(Expr.Lit Empty) e1 in
      let e2 = fe @@ Option.value ~default:(Expr.Lit Null) e2 in
      let aux1 = fresh_var () in
      let e' = Expr.BinOp (Expr.PVar aux1, LstNth, Expr.zero_i) in
      let cmd1 : string GCmd.t =
        GCmd.LAction (aux1, JSILNames.alloc, [ e1; e2 ])
      in
      let cmd2 : string GCmd.t = Assignment (x, e') in
      [ (lab, cmd1); (None, cmd2) ]
  (*
       C(x := [e1, e2]) :-
          lab:  aux1 := e1;
                aux2 := e2;
                aux3 := [GetCell](aux1, aux2);
                goto [l-nth(aux3, 2) = Nono] then else;
          then: fail [ResourceError](aux1, aux2);
          else: x    := l-nth(aux3, 2)
    *)
  | LBasic (Lookup (x, e1, e2)) ->
      let aux1 = fresh_var () in
      let aux2 = fresh_var () in
      let aux3 = fresh_var () in
      let then_lab = fresh_then () in
      let else_lab = fresh_else () in
      let lnth_expr : Expr.t = BinOp (Expr.PVar aux3, LstNth, Expr.int 2) in
      let cmd1 : string GCmd.t = Assignment (aux1, fe e1) in
      let cmd2 : string GCmd.t = Assignment (aux2, fe e2) in
      let cmd3 : string GCmd.t =
        LAction (aux3, JSILNames.getCell, [ Expr.PVar aux1; Expr.PVar aux2 ])
      in
      let cmd4 : string GCmd.t =
        GuardedGoto (BinOp (lnth_expr, Equal, Lit Nono), then_lab, else_lab)
      in
      let cmd5 : string GCmd.t =
        resource_error [ Expr.PVar aux1; Expr.PVar aux2 ]
      in
      let cmd6 : string GCmd.t = Assignment (x, lnth_expr) in
      [
        (lab, cmd1);
        (None, cmd2);
        (None, cmd3);
        (None, cmd4);
        (Some then_lab, cmd5);
        (Some else_lab, cmd6);
      ]
  (*
       C([e1, e2] := e3) :-
          lab:  aux1 := e1;
                aux2 := e2;
                aux3 := e3;
                aux4 := [GetCell](aux1, aux2);
                aux5 := [SetCell](l-nth(aux4, 0), l-nth(aux4, 1), aux3)
    *)
  | LBasic (Mutation (e1, e2, e3)) ->
      let aux1 = fresh_var () in
      let aux2 = fresh_var () in
      let aux3 = fresh_var () in
      let aux4 = fresh_var () in
      let aux5 = fresh_var () in
      let e1' = Expr.BinOp (Expr.PVar aux4, LstNth, Expr.zero_i) in
      let e2' = Expr.BinOp (Expr.PVar aux4, LstNth, Expr.one_i) in
      let cmd1 : string GCmd.t = Assignment (aux1, fe e1) in
      let cmd2 : string GCmd.t = Assignment (aux2, fe e2) in
      let cmd3 : string GCmd.t = Assignment (aux3, fe e3) in
      let cmd4 : string GCmd.t =
        LAction (aux4, JSILNames.getCell, [ Expr.PVar aux1; Expr.PVar aux2 ])
      in
      let cmd5 : string GCmd.t =
        LAction (aux5, JSILNames.setCell, [ e1'; e2'; Expr.PVar aux3 ])
      in
      [ (lab, cmd1); (None, cmd2); (None, cmd3); (None, cmd4); (None, cmd5) ]
  (*
       C(delete(e1, e2)) :-
          lab:  aux1 := e1;
                aux2 := e2;
                aux3 := [GetCell](aux1, aux2);
                goto [l-nth(aux3, 2) = nono] then else;
          then: fail [ResourceError, cell](aux1, aux2);
          else: aux4 := [DeleteCell](l-nth(aux3, 0), l-nth(aux3, 1))
    *)
  | LBasic (Delete (e1, e2)) ->
      let aux1 = fresh_var () in
      let aux2 = fresh_var () in
      let aux3 = fresh_var () in
      let aux4 = fresh_var () in
      let then_lab = fresh_then () in
      let else_lab = fresh_else () in
      let e1' = Expr.BinOp (Expr.PVar aux3, LstNth, Expr.zero_i) in
      let e2' = Expr.BinOp (Expr.PVar aux3, LstNth, Expr.one_i) in
      let e3' = Expr.BinOp (Expr.PVar aux3, LstNth, Expr.int 2) in
      let cmd1 : string GCmd.t = Assignment (aux1, fe e1) in
      let cmd2 : string GCmd.t = Assignment (aux2, fe e2) in
      let cmd3 : string GCmd.t =
        LAction (aux3, JSILNames.getCell, [ Expr.PVar aux1; Expr.PVar aux2 ])
      in
      let cmd4 : string GCmd.t =
        GuardedGoto (BinOp (e3', Equal, Lit Nono), then_lab, else_lab)
      in
      let cmd5 : string GCmd.t =
        resource_error [ Expr.PVar aux1; Expr.PVar aux2 ]
      in
      let cmd6 : string GCmd.t =
        LAction (aux4, JSILNames.setCell, [ e1'; e2'; Expr.Lit Nono ])
      in
      [
        (lab, cmd1);
        (None, cmd2);
        (None, cmd3);
        (None, cmd4);
        (Some then_lab, cmd5);
        (Some else_lab, cmd6);
      ]
  (*
      C(deleteObj (e)) :-
        lab: aux1 := e;
             aux2 := [GetAllProps](aux1);
             goto [ l-nth(aux2, 1) = empty ] then else;
       then: fail [ResourceError](aux1);
       else: aux3 := [DeleteObject](l-nth(aux2, 0))
    *)
  | LBasic (DeleteObj e) ->
      let aux1 = fresh_var () in
      let aux2 = fresh_var () in
      let aux3 = fresh_var () in
      let e1 = Expr.BinOp (Expr.PVar aux2, LstNth, Expr.zero_i) in
      let e2 = Expr.BinOp (Expr.PVar aux2, LstNth, Expr.one_i) in
      let then_lab = fresh_then () in
      let else_lab = fresh_else () in
      let cmd1 : string GCmd.t = Assignment (aux1, fe e) in
      let cmd2 : string GCmd.t =
        LAction (aux2, JSILNames.getAllProps, [ Expr.PVar aux1 ])
      in
      let cmd3 : string GCmd.t =
        GuardedGoto (BinOp (e2, Equal, Lit Empty), then_lab, else_lab)
      in
      let cmd4 : string GCmd.t = resource_error [ e1 ] in
      let cmd5 : string GCmd.t = LAction (aux3, JSILNames.delObj, [ e1 ]) in
      [
        (lab, cmd1);
        (None, cmd2);
        (None, cmd3);
        (Some then_lab, cmd4);
        (Some else_lab, cmd5);
      ]
  (*
       C(x := hasField(e1, e2)) :-
          lab:  aux1 := e1;
                aux2 := e2;
                aux3 := [GetCell](aux1, aux2);
                x    := not (l-nth(aux3, 2) = none)
    *)
  | LBasic (HasField (x, e1, e2)) ->
      let aux1 = fresh_var () in
      let aux2 = fresh_var () in
      let aux3 = fresh_var () in
      let e =
        Expr.UnOp
          (UNot, BinOp (BinOp (PVar aux3, LstNth, Expr.int 2), Equal, Lit Nono))
      in
      let cmd1 : string GCmd.t = Assignment (aux1, fe e1) in
      let cmd2 : string GCmd.t = Assignment (aux2, fe e2) in
      let cmd3 : string GCmd.t =
        LAction (aux3, JSILNames.getCell, [ Expr.PVar aux1; Expr.PVar aux2 ])
      in
      let cmd4 : string GCmd.t = Assignment (x, e) in
      [ (lab, cmd1); (None, cmd2); (None, cmd3); (None, cmd4) ]
  (*
       C(x := getfields(e)) :-
         lab:  aux1 := e
               aux2 := [GetAllProps](aux1)
               goto [aux2 = empty] then else;
         then: fail [ResourceError, cell](aux1);
         else: x := l-nth(aux2, 1)
    *)
  | LBasic (GetFields (x, e)) ->
      let aux1 = fresh_var () in
      let aux2 = fresh_var () in
      let then_lab = fresh_then () in
      let else_lab = fresh_else () in
      let cmd1 : string GCmd.t = Assignment (aux1, fe e) in
      let cmd2 : string GCmd.t =
        LAction (aux2, JSILNames.getAllProps, [ Expr.PVar aux1 ])
      in
      let cmd3 : string GCmd.t =
        GuardedGoto
          (BinOp (Expr.PVar aux2, Equal, Lit Empty), then_lab, else_lab)
      in
      let cmd4 : string GCmd.t = resource_error [ Expr.PVar aux1 ] in
      let cmd5 : string GCmd.t =
        Assignment (x, BinOp (PVar aux2, LstNth, Expr.one_i))
      in
      [
        (lab, cmd1);
        (None, cmd2);
        (None, cmd3);
        (Some then_lab, cmd4);
        (Some else_lab, cmd5);
      ]
  (*
       C(x := MetaData(e)) :-
         lab: aux1 := e
              aux2 := [GetMetadata](aux1)
              goto [l-nth(aux2, 1) = none] then else;
        then: fail [ResourceError, cell](aux1);
        else: x := l-nth(aux2, 1)

    *)
  | LBasic (MetaData (x, e)) ->
      let aux1 = fresh_var () in
      let aux2 = fresh_var () in
      let then_lab = fresh_then () in
      let else_lab = fresh_else () in
      let e' : Expr.t = BinOp (Expr.PVar aux2, LstNth, Expr.one_i) in
      let cmd1 : string GCmd.t = Assignment (aux1, fe e) in
      let cmd2 : string GCmd.t =
        LAction (aux2, JSILNames.getMetadata, [ Expr.PVar aux1 ])
      in
      let cmd3 : string GCmd.t =
        GuardedGoto (BinOp (e', Equal, Lit Nono), then_lab, else_lab)
      in
      let cmd4 : string GCmd.t = resource_error [ Expr.PVar aux1 ] in
      let cmd5 : string GCmd.t =
        Assignment (x, BinOp (Expr.PVar aux2, LstNth, Expr.one_i))
      in
      [
        (lab, cmd1);
        (None, cmd2);
        (None, cmd3);
        (Some then_lab, cmd4);
        (Some else_lab, cmd5);
      ]
  | LLogic lcmd -> [ (lab, GCmd.Logic (jsil2gil_lcmd lcmd)) ]
  | LGoto j -> [ (lab, GCmd.Goto j) ]
  | LGuardedGoto (e, j, k) -> [ (lab, GCmd.GuardedGoto (fe e, j, k)) ]
  | LCall (x, e, es, j, subst) ->
      [ (lab, GCmd.Call (x, fe e, List.map fe es, j, subst)) ]
  | LECall (x, e, es, j) -> [ (lab, GCmd.ECall (x, fe e, List.map fe es, j)) ]
  | LApply (x, e, j) -> [ (lab, GCmd.Apply (x, fe e, j)) ]
  | LArguments x -> [ (lab, GCmd.Arguments x) ]
  | LPhiAssignment es ->
      let es = List.map (fun (x, e) -> (x, List.map fe e)) es in
      [ (lab, GCmd.PhiAssignment es) ]
  | LReturnNormal -> [ (lab, GCmd.ReturnNormal) ]
  | LReturnError -> [ (lab, GCmd.ReturnError) ]

let jsil2core_proc (proc : EProc.t) : ('a, string) GProc.t =
  let body = Array.to_list proc.body in
  let body' =
    List.concat
      (List.map
         (fun (annot, lab, cmd) ->
           let cmds = jsil2core lab cmd in
           List.map (fun (lab, cmd) -> (annot, lab, cmd)) cmds)
         body)
  in
  {
    proc_name = proc.name;
    proc_source_path = None;
    proc_internal = false;
    (* TODO (Alexis): Set depending on module of proc *)
    proc_body = Array.of_list body';
    proc_params = proc.params;
    proc_spec = Option.map jsil2gil_spec proc.spec;
    proc_aliases = [];
    proc_calls = [];
    (* TODO *)
  }

let translate_tbl (tbl : (string, 'a) Hashtbl.t) (f : 'a -> 'b) :
    (string, 'b) Hashtbl.t =
  let size = (Hashtbl.stats tbl).max_bucket_length in
  let tbl' : (string, 'b) Hashtbl.t = Hashtbl.create size in
  Hashtbl.iter (fun k v -> Hashtbl.add tbl' k (f v)) tbl;
  tbl'

let jsil2core_prog (prog : EProg.t) : ('a, string) GProg.t =
  let new_procs = Hashtbl.create Config.big_tbl_size in

  Hashtbl.iter
    (fun _ proc ->
      let proc' = jsil2core_proc proc in
      Hashtbl.add new_procs proc'.proc_name proc')
    prog.procs;

  let result : (Annot.Basic.t, string) GProg.t =
    GProg.make ~imports:prog.imports
      ~preds:(translate_tbl prog.preds jsil2gil_pred)
      ~lemmas:(translate_tbl prog.lemmas jsil2gil_lemma)
      ~only_specs:(translate_tbl prog.only_specs jsil2gil_spec)
      ~procs:new_procs
      ~macros:(translate_tbl prog.macros jsil2gil_macro)
      ~bi_specs:(translate_tbl prog.bi_specs jsil2gil_bispec)
      ~proc_names:prog.proc_names ~predecessors:(Hashtbl.create 1) ()
  in
  result
