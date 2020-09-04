module Expr = Gillian.Gil_syntax.Expr

(**************************************************************)
(**************************************************************)
(** JSIL Commmands                                           **)

(**************************************************************)
(**************************************************************)

type t =
  | Basic         of BCmd.t  (** JSIL basic commands *)
  | Logic         of LCmd.t  (** JSIL Logic commands *)
  | Goto          of int  (** Unconditional goto *)
  | GuardedGoto   of Expr.t * int * int  (** Conditional goto *)
  | Call          of
      string
      * Expr.t
      * Expr.t list
      * int option
      * (string * (string * Expr.t) list) option  (** Procedure call *)
  | ECall         of string * Expr.t * Expr.t list * int option
      (** External Procedure call *)
  | Apply         of string * Expr.t * int option
      (** Application-style procedure call *)
  | Arguments     of string  (** Arguments of the current function *)
  | PhiAssignment of (string * Expr.t list) list  (** PHI assignment *)
  | ReturnNormal  (** Normal return *)
  | ReturnError  (** Error return *)

let pp_logic_bindings fmt lbinds =
  let lab, subst_lst = lbinds in
  if List.length subst_lst > 0 then
    Fmt.pf fmt "[%s - %a]" lab
      (Fmt.list ~sep:Fmt.comma (fun f (x, le) -> Fmt.pf f "%s: %a" x Expr.pp le))
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
      Fmt.pf fmt "%s := %a(%a)%a%a" var Expr.pp name pp_params args
        (Fmt.option pp_error) error (Fmt.option pp_subst) subst
  | ECall (var, name, args, error) ->
      Fmt.pf fmt "%s := extern %a(%a)%a" var Expr.pp name pp_params args
        (Fmt.option pp_error) error
  | Apply (var, arg, error) ->
      Fmt.pf fmt "%s := apply(%a)%a" var Expr.pp arg (Fmt.option pp_error) error
  | Arguments var -> Fmt.pf fmt "%s := args" var
  | PhiAssignment lva ->
      let vars, var_args = List.split lva in
      Fmt.pf fmt "PHI(%a: %a)"
        Fmt.(list ~sep:comma string)
        vars
        Fmt.(list ~sep:semi pp_params)
        var_args
  | ReturnNormal -> Fmt.string fmt "return"
  | ReturnError -> Fmt.string fmt "throw"
  | Logic lcmd -> LCmd.pp fmt lcmd

let vars (cmd : t) : Containers.SS.t =
  let open Containers in
  let ve = Expr.vars in
  let vb = BCmd.vars in
  match cmd with
  | Basic bcmd -> vb bcmd
  | Goto _ -> SS.empty
  | GuardedGoto (e, _, _) -> ve e
  | Call (x, e, le, _, _) | ECall (x, e, le, _) ->
      SS.add x
        (SS.union (ve e) (List.fold_left SS.union SS.empty (List.map ve le)))
  | Apply (x, e, _) -> SS.add x (ve e)
  | Arguments x -> SS.singleton x
  | PhiAssignment lele ->
      List.fold_left SS.union SS.empty
        (List.map
           (fun (_, le) -> List.fold_left SS.union SS.empty (List.map ve le))
           lele)
  | ReturnNormal -> SS.empty
  | ReturnError -> SS.empty
  | Logic lcmd -> SS.empty

let successors (cmd : t) (i : int) : int list =
  match cmd with
  | Basic _ | PhiAssignment _ | Logic _ | Arguments _ -> [ i + 1 ]
  | Goto j -> [ j ]
  | GuardedGoto (e, j, k) -> [ j; k ]
  | Call (_, _, _, j, _) | ECall (_, _, _, j) | Apply (_, _, j) -> (
      match j with
      | None   -> [ i + 1 ]
      | Some j -> [ i + 1; j ] )
  | ReturnNormal | ReturnError -> []
