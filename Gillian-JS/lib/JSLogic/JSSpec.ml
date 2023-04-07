open JSLogicCommon
open Gillian.Gil_syntax
open Jsil_syntax

type st = {
  pre : JSAsrt.t;
  post : JSAsrt.t list;
  flag : Flag.t;
  label : (string * SS.t) option;
}

type t = { name : string; params : string list; sspecs : st list }

let js2jsil_st
    (pre : JSAsrt.t)
    (post : JSAsrt.t list)
    (cc_tbl : cc_tbl_type)
    (vis_tbl : vis_tbl_type)
    (fun_tbl : pre_fun_tbl_type)
    (fid : string)
    (params : string list) : Asrt.t * Asrt.t list =
  let vis_list = get_vis_list vis_tbl fid in

  let scope_chain_list = vislist_2_les vis_list (List.length vis_list - 1) in
  let final_scope_chain_list =
    Expr.EList
      (scope_chain_list @ [ LVar (Javert_utils.Js_generators.fresh_lvar ()) ])
  in

  let pre' : Asrt.t =
    JSAsrt.js2jsil (Some fid) cc_tbl vis_tbl fun_tbl
      (Some (Expr.PVar var_scope)) pre
  in
  let post' : Asrt.t list =
    List.map
      (fun post ->
        JSAsrt.js2jsil (Some fid) cc_tbl vis_tbl fun_tbl
          (Some final_scope_chain_list) post)
      post
  in

  (* x \in params -> (! (x == empty)) *)
  let params_not_empty : Asrt.t list =
    List.map
      (fun x -> Asrt.Pure (Not (Eq (Expr.PVar x, Expr.Lit Empty))))
      params
  in
  let params_not_none : Asrt.t list =
    List.map (fun x -> Asrt.Pure (Not (Eq (Expr.PVar x, Expr.Lit Nono)))) params
  in

  let params_and_lists : Asrt.t list =
    List.map
      (fun x ->
        let fml : Formula.t =
          Eq (Expr.UnOp (TypeOf, Expr.PVar x), Expr.Lit (Type ListType))
        in
        let fml : Formula.t = if x = var_scope then fml else Not fml in
        Asrt.Pure fml)
      params
  in

  (*  x__this == #this                *)
  let a_this =
    Asrt.Pure (Eq (Expr.PVar var_this, Expr.LVar this_logic_var_name))
  in
  (*  x__scope == {{ #x1, ..., #xn }} *)
  let a_scope =
    Asrt.Pure (Eq (Expr.PVar var_scope, Expr.EList scope_chain_list))
  in

  (* let er_sc_list  = (match scope_chain_list with | [] -> [] | _ -> List.tl scope_chain_list) in
     let scope_mds   = List.map (fun _ -> Expr.LVar (Javert_utils.Js_generators.fresh_lvar ())) er_sc_list in
     let a_scope_mds = List.map2 (fun scl md -> Asrt.MetaData (scl, md)) er_sc_list scope_mds in
     let a_mds_ers   = List.map (fun er -> Asrt.PointsTo (er, Expr.Lit (String "@er"), Expr.Lit (Bool true))) scope_mds in *)

  (*
     let a_scope_post = Asrt.LEq (Expr.PVar JS2JSIL_Helpers.var_scope_final, Expr.EList (scope_chain_list @ [ Expr.PVar JS2JSIL_Helpers.var_er ])) in
   *)
  if fid = main_fid then (pre', post')
  else
    ( Asrt.star
        ([ pre'; a_scope ] (* a_mds_ers @ a_scope_mds @ *) @ [ a_this ]
        @ params_not_empty @ params_not_none @ params_and_lists),
      post' )
