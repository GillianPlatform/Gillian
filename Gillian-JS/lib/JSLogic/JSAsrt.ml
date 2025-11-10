open Gillian.Gil_syntax
open Jsil_syntax
open JSLogicCommon

type pt =
  | And of pt * pt
  | Or of pt * pt
  | Not of pt
  | True
  | False
  | Eq of JSExpr.t * JSExpr.t
  | Less of JSExpr.t * JSExpr.t
  | LessEq of JSExpr.t * JSExpr.t
  | StrLess of JSExpr.t * JSExpr.t
  | ForAll of (string * Type.t) list * pt
  | SetMem of JSExpr.t * JSExpr.t
  | SetSub of JSExpr.t * JSExpr.t
  | IsInt of JSExpr.t

type t =
  | Pure of pt
  | Star of t * t
  | PointsTo of JSExpr.t * JSExpr.t * JSExpr.t
  | MetaData of JSExpr.t * JSExpr.t
  | Emp
  | Pred of string * JSExpr.t list
  | Types of (string * Type.t) list
  | Scope of string * JSExpr.t
  | VarSChain of string * string * JSExpr.t * JSExpr.t
  | OSChains of string * JSExpr.t * string * JSExpr.t
  | Closure of (string * JSExpr.t) list * (string * JSExpr.t) list
  | SChain of string * JSExpr.t
  | EmptyFields of JSExpr.t * JSExpr.t

let star (asrts : t list) : t =
  List.fold_left
    (fun ac a ->
      if not (a = Emp) then if ac = Emp then a else Star (a, ac) else ac)
    Emp asrts

let rec js2jsil_pure (scope_le : Expr.t option) (a : pt) : Expr.t =
  let f = js2jsil_pure scope_le in
  let fe = JSExpr.js2jsil scope_le in

  (* What about metadata here? Or extensibility *)
  match a with
  | And (a1, a2) -> Expr.BinOp (f a1, And, f a2)
  | Or (a1, a2) -> BinOp (f a1, Or, f a2)
  | Not a -> UnOp (Not, f a)
  | True -> Expr.true_
  | False -> Expr.false_
  | Eq (le1, le2) -> BinOp (fe le1, Equal, fe le2)
  | Less (le1, le2) -> BinOp (fe le1, FLessThan, fe le2)
  | LessEq (le1, le2) -> BinOp (fe le1, FLessThanEqual, fe le2)
  | StrLess (le1, le2) -> BinOp (fe le1, StrLess, fe le2)
  | ForAll (s, a) ->
      let new_binders = List.map (fun (x, t) -> (x, Some t)) s in
      ForAll (new_binders, f a)
  | SetMem (le1, le2) -> BinOp (fe le1, SetMem, fe le2)
  | SetSub (le1, le2) -> BinOp (fe le1, SetSub, fe le2)
  | IsInt e -> UnOp (IsInt, fe e)

let rec js2jsil
    (cur_fid : string option)
    (cc_tbl : cc_tbl_type)
    (vis_tbl : vis_tbl_type)
    (fun_tbl : pre_fun_tbl_type)
    (scope_le : Expr.t option)
    (a : t) : Asrt.t =
  let f = js2jsil cur_fid cc_tbl vis_tbl fun_tbl scope_le in
  let fp = js2jsil_pure scope_le in
  let fe = JSExpr.js2jsil scope_le in

  let compile_pred s les =
    let a' = Asrt.Pred (s, List.map fe les) in
    if s = funobj_pred_name then
      match les with
      | [ _; Lit (String fid); le_sc; _; _ ] ->
          Asrt.Star (a', f (SChain (fid, le_sc)))
      | _ -> a'
    else a'
  in

  (* What about metadata here? Or extensibility *)
  match a with
  | Pure f -> Asrt.Pure (fp f)
  | Star (a1, a2) -> Asrt.Star (f a1, f a2)
  | PointsTo (le1, le2, le3) -> Asrt.PointsTo (fe le1, fe le2, fe le3)
  | MetaData (le1, le2) -> Asrt.MetaData (fe le1, fe le2)
  | Emp -> Asrt.Emp
  | Types vts ->
      Asrt.Types (List.map (fun (v, t) -> (Expr.from_var_name v, t)) vts)
  | EmptyFields (e, domain) -> Asrt.EmptyFields (fe e, fe domain)
  | Pred (name, [ loc; Lit (String fid); sch; args_len; fproto ])
    when name = "JSFunctionObject" || name = "JSFunctionObjectStrong" ->
      let len = List.length (get_vis_list vis_tbl fid) in
      let a_len =
        Asrt.Pure
          (BinOp
             (Lit (Num (float_of_int (len - 1))), Equal, UnOp (LstLen, fe sch)))
      in
      let a_lg =
        Asrt.Pure
          (BinOp
             ( Lit (Loc locGlobName),
               Equal,
               Expr.BinOp (fe sch, LstNth, Expr.Lit (Num (float_of_int 0))) ))
      in
      let a_pred =
        compile_pred name [ loc; Lit (String fid); sch; args_len; fproto ]
      in
      Asrt.star [ a_lg; a_len; a_pred ]
  | Pred (s, les) -> compile_pred s les
  (* le_x'  = Te(le_x)
        le_sc' = Te(le_sc)
     ----------------------------------------------
     Tr(scope(x: le_x, le_sc, fid)) ::=
       ((l-nth(le_sc', i), "x") -> le_x')               if Phi(fid, x) != 0
       ((lg, "x") -> {{"d", le_x', true, true, false}}) if Phi(fid, x) = 0 or bot *)
  | VarSChain (fid, x, le_x, le_sc) ->
      let i = psi cc_tbl vis_tbl fid x in
      (* let a_len = Asrt.Pure (Eq (Lit (Num (float_of_int len)), UnOp (LstLen, fe le_sc))) in *)
      let a' =
        match i with
        | None | Some 0 ->
            let desc =
              Expr.EList
                [
                  Expr.Lit (String "d");
                  fe le_x;
                  Expr.Lit (Bool true);
                  Expr.Lit (Bool true);
                  Expr.Lit (Bool false);
                ]
            in
            Asrt.PointsTo (Expr.Lit (Loc locGlobName), Expr.Lit (String x), desc)
        | Some i ->
            let le_x = fe le_x in
            let le_er =
              Expr.BinOp (fe le_sc, LstNth, Lit (Num (float_of_int i)))
            in
            let not_lg =
              Asrt.Pure
                (UnOp (Not, BinOp (le_er, Equal, Lit (Loc locGlobName))))
            in
            let not_none =
              Asrt.Pure (UnOp (Not, BinOp (le_x, Equal, Lit Nono)))
            in
            Asrt.star
              [ not_lg; not_none; Asrt.PointsTo (le_er, Lit (String x), le_x) ]
      in
      (* add_extra_scope_chain_info fid le_sc a'*)
      a'
  (* f_sc' = Te (f_sc)
       g_sc' = Te (g_sc)
       i = Phi^o(fid, gid)
     --------------------------------------------------
     Tr(OChains(fid: f_sc, gid: g_sc)) ::= IteratedStar_{0 <= j <= i} l-nth(f_sc', j) = l-nth(g_sc', j) *)
  | OSChains (fid1, le_sc1, fid2, le_sc2) ->
      let i = o_psi vis_tbl fid1 fid2 in
      let is = Array.to_list (Array.init i (fun i -> i)) in
      let le_sc1' = fe le_sc1 in
      let le_sc2' = fe le_sc2 in
      let f j =
        Asrt.Pure
          (BinOp
             ( BinOp (le_sc1', LstNth, Lit (Num (float_of_int j))),
               Equal,
               BinOp (le_sc2', LstNth, Lit (Num (float_of_int j))) ))
      in
      Asrt.star (List.map f is)
  (*  Tr(scope(x: le_x)) ::= Tr(scope(x: le_x, sc, fid)) *)
  | Scope (x, le) ->
      let fid =
        try Option.get cur_fid
        with _ -> raise (Failure "DEATH: js2jsil_assertion")
      in
      f (VarSChain (fid, x, le, Scope))
  (* Tr (closure(x1: le1, ..., xn: len; fid1: le_sc1, ..., fidm: le_scm)) ::=
     Tr ((IteratedStar_{1 <= j <= n} scope(xj: lej, le_sc1, fid1)) *
          (IteratedStar_{1 < j <= m} OChains(fid1: le_sc1, fidj: le_scj)) *)
  | Closure (var_les, fid_sc_les) ->
      let (fid_1, le_sc_1), rest_fid_sc_les =
        match fid_sc_les with
        | (fid_1, le_sc_1) :: rest_fid_sc_les ->
            ((fid_1, le_sc_1), rest_fid_sc_les)
        | _ -> raise (Failure "Empty scope chains in closure assertion")
      in

      let asrt_vars =
        List.map (fun (x, le_x) -> VarSChain (fid_1, x, le_x, le_sc_1)) var_les
      in
      let asrt_scs =
        List.map
          (fun (fid_j, le_sc_j) -> OSChains (fid_j, le_sc_j, fid_1, le_sc_1))
          rest_fid_sc_les
      in

      let fsclens =
        List.map
          (fun (x, y) -> (List.length (get_vis_list vis_tbl x), x, y))
          fid_sc_les
      in
      let fsclens =
        List.sort (fun (x1, _, _) (x2, _, _) -> Stdlib.compare x1 x2) fsclens
      in

      Format.printf "Sorting:\n\t%s"
        (String.concat "\n\t"
           (List.map (fun (x, y, _) -> Printf.sprintf "%s : %d" y x) fsclens));
      assert (
        let x0, _, _ = List.hd fsclens in
        let x1, _, _ = List.hd (List.tl fsclens) in
        x0 < x1);

      let fsclens =
        List.mapi
          (fun i (x, y, z) ->
            let x = if i <> 0 then x - 1 else x in
            (x, y, z))
          fsclens
      in

      Format.printf "Adjusted:\n\t%s"
        (String.concat "\n\t"
           (List.map (fun (x, y, _) -> Printf.sprintf "%s : %d" y x) fsclens));

      let asrt_lens : t list =
        List.map
          (fun (x, _, z) ->
            Pure (Eq (UnOp (LstLen, z), Lit (Num (float_of_int x)))))
          fsclens
      in

      f (star (asrt_vars @ asrt_scs @ asrt_lens))
  (*
    let len_fid1 = List.length (get_vis_list vis_tbl fid1) in
    let len_fid2 = List.length (get_vis_list vis_tbl fid2) in

    Format.printf "fid1: %s; len: %d\nfid2: %s; len: %d" fid1 len_fid1 fid2 len_fid2;

    let len_fid1 = LEq (UnOp(LstLen, le_sc1'), Lit (Num (float_of_int len_fid1))) in
    let len_fid2 = LEq (UnOp(LstLen, le_sc2'), Lit (Num (float_of_int len_fid2))) in  *)
  (*
    le_fid = "fid"
    vis_tbl(fid) = {{ fid_1, ..., fid_n }}
    le_sc' = Te(le_sc)
    ------------------------------------------------------
    Tr(schain(le_fid, le_sc)) ::= le_sc' == {{ fid_1, ..., fid_{n-1} }}
  *)
  | SChain (fid, le) ->
      let vis_list = get_vis_list vis_tbl fid in
      let scope_chain_list =
        vislist_2_les vis_list (List.length vis_list - 1)
      in
      Asrt.Pure (BinOp (fe le, Equal, EList scope_chain_list))

let errors_assertion () =
  Asrt.Star
    ( Pred (type_error_pred_name, [ PVar var_te ]),
      Pred (syntax_error_pred_name, [ PVar var_se ]) )

let js2jsil_tactic
    (cc_tbl : cc_tbl_type)
    (vis_tbl : vis_tbl_type)
    (fun_tbl : pre_fun_tbl_type)
    (fid : string)
    (scope_var : string)
    (a : t) : Asrt.t =
  let vis_list = get_vis_list vis_tbl fid in
  let scope_chain_list = vislist_2_les vis_list (List.length vis_list) in
  let a' =
    js2jsil (Some fid) cc_tbl vis_tbl fun_tbl (Some (Expr.PVar scope_var)) a
  in

  (*  x__scope == {{ #x1, ..., #xn }} *)
  let a'' =
    Asrt.Pure (BinOp (Expr.PVar scope_var, Equal, Expr.EList scope_chain_list))
  in

  (*  x__this == #this                *)
  let a_this =
    Asrt.Pure (BinOp (Expr.PVar var_this, Equal, Expr.LVar this_logic_var_name))
  in

  Asrt.star [ a'; a''; a_this ]
