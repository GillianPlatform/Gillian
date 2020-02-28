(**************************************************************)
(**************************************************************)
(** GIL Commmands                                           **)

(**************************************************************)
(**************************************************************)

type logic_bindings_t = string * (string * Expr.t) list

type 'label t = 'label TypeDef__.cmd =
  | Skip  (** Skip                *)
  | Assignment    of string * Expr.t  (** Assignment          *)
  | LAction       of string * string * Expr.t list  (** Local Actions       *)
  | Logic         of LCmd.t  (** GIL Logic commands *)
  | Goto          of 'label  (** Unconditional goto  *)
  | GuardedGoto   of Expr.t * 'label * 'label  (** Conditional goto    *)
  | Call          of
      string * Expr.t * Expr.t list * 'label option * logic_bindings_t option
      (** Procedure call *)
  | ECall         of string * Expr.t * Expr.t list * 'label option
      (** External Procedure call           *)
  | Apply         of string * Expr.t * 'label option
      (** Application-style procedure call  *)
  | Arguments     of string  (** Arguments of the current function *)
  | PhiAssignment of (string * Expr.t list) list  (** PHI assignment      *)
  | ReturnNormal  (** Normal return       *)
  | ReturnError  (** Error return        *)
  | Fail          of string * Expr.t list  (** Failure             *)

let successors (cmd : int t) (i : int) : int list =
  match cmd with
  | Goto j -> [ j ]
  | GuardedGoto (_, j, k) -> [ j; k ]
  | Call (_, _, _, j, _) | ECall (_, _, _, j) | Apply (_, _, j) -> (
      match j with
      | None   -> [ i + 1 ]
      | Some j -> [ i + 1; j ] )
  | ReturnNormal | ReturnError | Fail _ -> []
  | Skip | Assignment _ | LAction _ | Logic _ | Arguments _ | PhiAssignment _ ->
      [ i + 1 ]

let pp_logic_bindings =
  let aux fmt lbinds =
    let lab, subst_lst = lbinds in
    if List.length subst_lst > 0 then
      Fmt.pf fmt "%s - %a" lab
        (Fmt.list ~sep:Fmt.comma (fun f (x, le) ->
             Fmt.pf f "%s: %a" x Expr.pp le))
        subst_lst
    else Fmt.pf fmt "%s" lab
  in
  Fmt.brackets aux

(** GIL All Statements *)
let pp ~(pp_label : 'a Fmt.t) fmt (cmd : 'a t) =
  let pp_params = Fmt.list ~sep:Fmt.comma Expr.pp in
  let pp_error f er = Fmt.pf f " with %a" pp_label er in
  match cmd with
  | Skip -> Fmt.string fmt "skip"
  | Assignment (x, e) -> Fmt.pf fmt "%s := @[%a@]" x Expr.pp e
  | LAction (x, name, es) -> Fmt.pf fmt "%s := [%s](@[%a@])" x name pp_params es
  | Goto j -> Fmt.pf fmt "goto %a" pp_label j
  | GuardedGoto (e, j, k) ->
      Fmt.pf fmt "@[<h>goto [%a] %a %a@]" Expr.pp e pp_label j pp_label k
  | Call (var, name, args, error, subst) ->
      let pp_subst f lbs = Fmt.pf f " use_subst %a" pp_logic_bindings lbs in
      Fmt.pf fmt "%s := %a(@[%a@])%a%a" var Expr.pp name pp_params args
        (Fmt.option pp_error) error (Fmt.option pp_subst) subst
  | ECall (var, name, args, error) ->
      Fmt.pf fmt "%s := extern %a(@[%a@])%a" var Expr.pp name pp_params args
        (Fmt.option pp_error) error
  | Apply (var, arg, error) ->
      Fmt.pf fmt "%s := apply(@[%a@])%a" var Expr.pp arg (Fmt.option pp_error)
        error
  | Arguments var -> Fmt.pf fmt "%s := args" var
  | PhiAssignment lva ->
      Fmt.pf fmt "PHI(@[%a@])"
        Fmt.(
          list ~sep:semi
            (hbox (pair ~sep:(any ": ") string (list ~sep:comma Expr.pp))))
        lva
  | ReturnNormal -> Fmt.string fmt "return"
  | ReturnError -> Fmt.string fmt "throw"
  | Logic lcmd -> LCmd.pp fmt lcmd
  | Fail (err_type, es) -> Fmt.pf fmt "fail [%s](@[%a@])" err_type pp_params es

let pp_labeled = pp ~pp_label:Fmt.string

let pp_indexed = pp ~pp_label:Fmt.int
