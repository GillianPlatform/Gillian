module SSubst = Gillian.Symbolic.Subst
module Expr = Gillian.Gil_syntax.Expr
module Id = Gillian.Gil_syntax.Id
module LVar = Gillian.Gil_syntax.LVar

(***************************************************************)
(** Separation Logic Commmands **)

(***************************************************************)

type folding_info = string * (LVar.t * Expr.t) list
type unfold_info = (LVar.t * LVar.t) list

(** {b JSIL Separation Logic commands}. *)
type t =
  | Fold of string * Expr.t list * folding_info option  (** Fold *)
  | Unfold of string * Expr.t list * unfold_info option * bool  (** Unfold *)
  | GUnfold of string  (** Global Unfold *)
  | ApplyLem of string * Expr.t list * Id.any_var Id.t list  (** Apply lemma *)
  | SepAssert of Asrt.t * Id.any_var Id.t list  (** Assert *)
  | Invariant of Asrt.t * Id.any_var Id.t list  (** Invariant *)

let pp_folding_info =
  let pp_ui f (v, le) = Fmt.pf f "(%a := %a)" Id.pp v Expr.pp le in
  let pp_non_opt f (id, uil) =
    Fmt.pf f " [ %s with %a ]" id (Fmt.list ~sep:(Fmt.any " and ") pp_ui) uil
  in
  Fmt.option pp_non_opt

let pp_unfold_info =
  let pp_ui f (v1, v2) = Fmt.pf f "(%a := %a)" Id.pp v1 Id.pp v2 in
  let pp_non_opt f uil =
    Fmt.pf f " [bind: %a]" (Fmt.list ~sep:(Fmt.any " and ") pp_ui) uil
  in
  Fmt.option pp_non_opt

let pp fmt (lcmd : t) : unit =
  let pp_args = Fmt.list ~sep:Fmt.comma Expr.pp in
  let pp_binders f b =
    match b with
    | [] -> ()
    | _ -> Fmt.pf f "[bind: %a]" (Fmt.list ~sep:Fmt.comma Id.pp) b
  in
  match lcmd with
  | Fold (name, les, fold_info) ->
      Fmt.pf fmt "fold %s(%a)%a" name pp_args les pp_folding_info fold_info
  | Unfold (name, les, unfold_info, b) ->
      let keyword = if b then "unfold*" else "unfold" in
      Fmt.pf fmt "%s %s(%a) %a" keyword name pp_args les pp_unfold_info
        unfold_info
  | GUnfold name -> Fmt.pf fmt "unfold_all %s" name
  | ApplyLem (lem_name, lparams, binders) ->
      Fmt.pf fmt "apply %s(%a) %a" lem_name pp_args lparams pp_binders binders
  | SepAssert (a, binders) ->
      Fmt.pf fmt "sep_assert (%a) %a" Asrt.pp a pp_binders binders
  | Invariant (a, binders) ->
      Fmt.pf fmt "invariant %a %a" (Fmt.parens Asrt.pp) a pp_binders binders
