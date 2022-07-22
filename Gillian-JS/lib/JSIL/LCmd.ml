module SSubst = Gillian.Symbolic.Subst
module Expr = Gillian.Gil_syntax.Expr
module Formula = Gillian.Gil_syntax.Formula
module Type = Gillian.Gil_syntax.Type

(***************************************************************)
(** Logic Commmands                                           **)

(***************************************************************)

(** {b JSIL logic commands}. *)
type t =
  | If of Expr.t * t list * t list  (** If-then-else     *)
  | Branch of Formula.t  (** branching on a FO formual *)
  | Macro of string * Expr.t list  (** Macro            *)
  | Assert of Formula.t  (** Assert           *)
  | Assume of Formula.t  (** Assume           *)
  | AssumeType of Expr.t * Type.t  (** Assume Type      *)
  | FreshSVar of string
  | SL of SLCmd.t

let rec pp fmt lcmd =
  let pp_list = Fmt.list ~sep:Fmt.semi pp in
  let pp_params = Fmt.list ~sep:Fmt.comma Expr.pp in
  match lcmd with
  | If (le, then_lcmds, else_lcmds) ->
      if List.length else_lcmds > 0 then
        Fmt.pf fmt
          "if (%a) @[<hov 2>then {@\n%a@]@\n@[<hov 2>} else {\n%a@]@\n}" Expr.pp
          le pp_list then_lcmds pp_list else_lcmds
      else
        Fmt.pf fmt "if (%a) @[<hov 2>then {@\n%a@]@\n}" Expr.pp le pp_list
          then_lcmds
  | Branch fo -> Fmt.pf fmt "branch (%a)" Formula.pp fo
  | Macro (name, lparams) -> Fmt.pf fmt "%s(%a)" name pp_params lparams
  | Assert a -> Fmt.pf fmt "assert (%a)" Formula.pp a
  | Assume a -> Fmt.pf fmt "assume (%a)" Formula.pp a
  | FreshSVar x -> Fmt.pf fmt "%s := fresh_svar()" x
  | SL sl_cmd -> SLCmd.pp fmt sl_cmd
  | AssumeType (e, t) ->
      Fmt.pf fmt "assume_type (%a, %s)" Expr.pp e (Type.str t)
