module Expr = Gillian.Gil_syntax.Expr
module Id = Gillian.Gil_syntax.Id
module Var = Gillian.Gil_syntax.Var
module LVar = Gillian.Gil_syntax.LVar

(**************************************************************)
(**************************************************************)
(** JSIL Commmands **)

(**************************************************************)
(**************************************************************)

type t =
  | Basic of BCmd.t  (** JSIL basic commands *)
  | Logic of LCmd.t  (** JSIL Logic commands *)
  | Goto of int  (** Unconditional goto *)
  | GuardedGoto of Expr.t * int * int  (** Conditional goto *)
  | Call of
      Var.t
      * Expr.t
      * Expr.t list
      * int option
      * (string * (LVar.t * Expr.t) list) option  (** Procedure call *)
  | ECall of Var.t * Expr.t * Expr.t list * int option
      (** External Procedure call *)
  | Apply of Var.t * Expr.t * int option
      (** Application-style procedure call *)
  | Arguments of Var.t  (** Arguments of the current function *)
  | PhiAssignment of (Var.t * Expr.t list) list  (** PHI assignment *)
  | ReturnNormal  (** Normal return *)
  | ReturnError  (** Error return *)

let pp_logic_bindings fmt lbinds =
  let lab, subst_lst = lbinds in
  if List.length subst_lst > 0 then
    Fmt.pf fmt "[%s - %a]" lab
      (Fmt.list ~sep:Fmt.comma (fun f (x, le) ->
           Fmt.pf f "%a: %a" Id.pp x Expr.pp le))
      subst_lst
  else Fmt.pf fmt "[%s]" lab

let pp fmt cmd =
  let pp_params = Fmt.list ~sep:Fmt.comma Expr.pp in
  let pp_error f er = Fmt.pf f " with %i" er in
  match cmd with
  | Basic bcmd -> BCmd.pp fmt bcmd
  | Goto j -> Fmt.pf fmt "goto %i" j
  | GuardedGoto (e, j, k) -> Fmt.pf fmt "goto [%a] %i %i" Expr.pp e j k
  | Call (var, name, args, error, subst) ->
      let pp_subst f lbs = Fmt.pf f " use_subst %a" pp_logic_bindings lbs in
      Fmt.pf fmt "%a := %a(%a)%a%a" Id.pp var Expr.pp name pp_params args
        (Fmt.option pp_error) error (Fmt.option pp_subst) subst
  | ECall (var, name, args, error) ->
      Fmt.pf fmt "%a := extern %a(%a)%a" Id.pp var Expr.pp name pp_params args
        (Fmt.option pp_error) error
  | Apply (var, arg, error) ->
      Fmt.pf fmt "%a := apply(%a)%a" Id.pp var Expr.pp arg (Fmt.option pp_error)
        error
  | Arguments var -> Fmt.pf fmt "%a := args" Id.pp var
  | PhiAssignment lva ->
      let vars, var_args = List.split lva in
      Fmt.pf fmt "PHI(%a: %a)"
        Fmt.(list ~sep:comma Id.pp)
        vars
        Fmt.(list ~sep:semi pp_params)
        var_args
  | ReturnNormal -> Fmt.string fmt "return"
  | ReturnError -> Fmt.string fmt "throw"
  | Logic lcmd -> LCmd.pp fmt lcmd
