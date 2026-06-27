open JSLogicCommon
open Jsil_syntax

type t =
  | Fold of JSAsrt.t * (string * (string * JSExpr.t) list) option  (** Fold *)
  | Unfold of JSAsrt.t * (string * string) list option  (** Single Unfold *)
  | GUnfold of string  (** Global unfold *)
  | Flash of JSAsrt.t  (** Unfold/fold *)
  | If of JSExpr.t * t list * t list  (** If-then-else *)
  | Branch of JSAsrt.pt (* Branching *)
  | ApplyLemma of string * JSExpr.t list  (** Lemma *)
  | Macro of string * JSExpr.t list  (** Macro *)
  | Assert of (JSAsrt.t * string list)  (** Assert *)
  | Assume of JSAsrt.pt  (** Assume *)
  | Invariant of (JSAsrt.t * string list)  (** Invariant *)
  | UseSubst of string * (string * JSExpr.t) list

let rec js2jsil
    (cc_tbl : cc_tbl_type)
    (vis_tbl : vis_tbl_type)
    (fun_tbl : pre_fun_tbl_type)
    (fid : string)
    (scope_var : string)
    (logic_cmd : t) : LCmd.t list =
  let f = js2jsil cc_tbl vis_tbl fun_tbl fid scope_var in
  let fe = JSExpr.js2jsil None in

  let translate_folding_info folding_info =
    match folding_info with
    | None -> None
    | Some (def_id, var_le_list) ->
        Some (def_id, List.map (fun (x, le) -> (x, fe le)) var_le_list)
  in

  match logic_cmd with
  | Fold (Pred (s, les), fold_info) ->
      [ LCmd.SL (Fold (s, List.map fe les, translate_folding_info fold_info)) ]
  | Flash (Pred (s, les)) ->
      let p_name, les' = (s, List.map fe les) in
      [
        LCmd.SL (Unfold (p_name, les', None, false));
        LCmd.SL (Fold (p_name, les', None));
      ]
  | Unfold (Pred (s, les), unfold_info) ->
      [ LCmd.SL (Unfold (s, List.map fe les, unfold_info, false)) ]
  | GUnfold name -> [ LCmd.SL (GUnfold name) ]
  | Assert (assertion, binders) ->
      let a' =
        JSAsrt.js2jsil_tactic cc_tbl vis_tbl fun_tbl fid scope_var assertion
      in
      [ LCmd.SL (SepAssert (a', binders)) ]
  | Assume pf -> [ LCmd.Assume (JSAsrt.js2jsil_pure None pf) ]
  | Macro (s, les) -> [ LCmd.Macro (s, List.map fe les) ]
  | If (le, lcthen, lcelse) ->
      [
        LCmd.If
          ( fe le,
            List.concat (List.map f lcthen),
            List.concat (List.map f lcelse) );
      ]
  | ApplyLemma (lname, lparams) ->
      [ LCmd.SL (ApplyLem (lname, List.map fe lparams, [])) ]
  | UseSubst _ -> raise (Failure "DEATH. USESUBST CANNOT BE TRANSLATED!!!")
  | Branch pf -> [ Branch (JSAsrt.js2jsil_pure None pf) ]
  | _ -> raise (Failure "Unsupported JS logic command")

let str_of_folding_info (unfold_info : (string * string) list option) : string =
  match unfold_info with
  | None -> ""
  | Some unfold_info_list ->
      let unfold_info_list =
        List.map (fun (v1, v2) -> "(" ^ v1 ^ " := " ^ v2 ^ ")") unfold_info_list
      in
      let unfold_info_list_str = String.concat " and " unfold_info_list in
      " [bind: " ^ unfold_info_list_str ^ " ]"
