module Expr = Gillian.Gil_syntax.Expr

(***************************************************************)
(***************************************************************)
(** JSIL Labelled Commmands                                   **)

(***************************************************************)
(***************************************************************)

type t =
  | LBasic of BCmd.t
  | LLogic of LCmd.t
  | LGoto of string
  | LGuardedGoto of Expr.t * string * string
  | LCall of
      string
      * Expr.t
      * Expr.t list
      * string option
      * (string * (string * Expr.t) list) option
  | LECall of string * Expr.t * Expr.t list * string option
  | LApply of string * Expr.t * string option
  | LArguments of string
  | LPhiAssignment of (string * Expr.t list) list
  | LReturnNormal
  | LReturnError

let pp_logic_bindings fmt lbinds =
  let lab, subst_lst = lbinds in
  if List.length subst_lst > 0 then
    Fmt.pf fmt "[%s - %a]" lab
      (Fmt.list ~sep:Fmt.comma (fun f (x, le) -> Fmt.pf f "%s: %a" x Expr.pp le))
      subst_lst
  else Fmt.pf fmt "[%s]" lab

let pp fmt lcmd =
  let pp_params = Fmt.list ~sep:Fmt.comma Expr.pp in
  let pp_error f er = Fmt.pf f " with %s" er in
  match lcmd with
  | LBasic bcmd -> BCmd.pp fmt bcmd
  | LGoto j -> Fmt.pf fmt "goto %s" j
  | LGuardedGoto (e, j, k) -> Fmt.pf fmt "goto [%a] %s %s" Expr.pp e j k
  | LCall (var, name, args, error, subst) ->
      let pp_subst f lbs = Fmt.pf f " use_subst %a" pp_logic_bindings lbs in
      Fmt.pf fmt "%s := %a(%a)%a%a" var Expr.pp name pp_params args
        (Fmt.option pp_error) error (Fmt.option pp_subst) subst
  | LECall (var, name, args, error) ->
      Fmt.pf fmt "%s := extern %a(%a)%a" var Expr.pp name pp_params args
        (Fmt.option pp_error) error
  | LApply (var, arg, error) ->
      Fmt.pf fmt "%s := apply(%a)%a" var Expr.pp arg (Fmt.option pp_error) error
  | LArguments var -> Fmt.pf fmt "%s := args" var
  | LPhiAssignment lva ->
      let vars, var_args = List.split lva in
      Fmt.pf fmt "PHI(%a: %a)"
        Fmt.(list ~sep:comma string)
        vars
        Fmt.(list ~sep:semi pp_params)
        var_args
  | LReturnNormal -> Fmt.string fmt "return"
  | LReturnError -> Fmt.string fmt "throw"
  | LLogic lcmd -> LCmd.pp fmt lcmd
