(**************************************************************)
(**************************************************************)
(** GIL Commmands **)

(**************************************************************)
(**************************************************************)

type logic_bindings_t = string * (Id.LVar.t * Expr.t) list [@@deriving yojson]

type function_call = TypeDef__.function_call = {
  var_name : Id.Var.t;
  fun_name : Expr.t;
  args : Expr.t list;
  bindings : logic_bindings_t option;
}
[@@deriving yojson]

type 'label t = 'label TypeDef__.cmd =
  | Skip  (** Skip *)
  | Assignment of Id.Var.t * Expr.t  (** Assignment *)
  | LAction of Id.Var.t * string * Expr.t list  (** Local Actions *)
  | Logic of LCmd.t  (** GIL Logic commands *)
  | Goto of 'label  (** Unconditional goto *)
  | GuardedGoto of Expr.t * 'label * 'label  (** Conditional goto *)
  | Call of function_call * 'label option  (** Procedure call *)
  | Par of function_call list  (** Parallel calls *)
  | ECall of Id.Var.t * Expr.t * Expr.t list * 'label option
      (** External Procedure call *)
  | Apply of Id.Var.t * Expr.t * 'label option
      (** Application-style procedure call *)
  | Arguments of Id.Var.t  (** Arguments of the current function *)
  | PhiAssignment of (Id.Var.t * Expr.t list) list  (** PHI assignment *)
  | ReturnNormal  (** Normal return *)
  | ReturnError  (** Error return *)
  | Fail of string * Expr.t list  (** Failure *)
[@@deriving yojson]

let equal = TypeDef__.equal_cmd

let pvars (cmd : 'label t) : Id.Var.Set.t =
  let module S = Id.Var.Set in
  let fold = List.fold_left S.union S.empty in
  let pvars_es es = fold (List.map Expr.pvars es) in
  let pvars_fcall { var_name = x; fun_name = e; args = es; _ } =
    S.union (S.add x (Expr.pvars e)) (pvars_es es)
  in
  match cmd with
  | Skip -> S.empty
  | Assignment (x, e) -> S.add x (Expr.pvars e)
  | LAction (x, _, es) -> S.add x (pvars_es es)
  | Logic lcmd -> LCmd.pvars lcmd
  | Goto _ -> S.empty
  | GuardedGoto (e, _, _) -> Expr.pvars e
  | Call (fc, _) -> pvars_fcall fc
  | Par fcs -> fold (List.map pvars_fcall fcs)
  | ECall (x, e, es, _) -> S.union (S.add x (Expr.pvars e)) (pvars_es es)
  | Apply (x, e, _) -> S.add x (Expr.pvars e)
  | Arguments x -> S.singleton x
  | PhiAssignment phis -> fold (List.map (fun (_, es) -> pvars_es es) phis)
  | ReturnNormal | ReturnError -> S.singleton Id.Var.return_variable
  | Fail (_, es) -> pvars_es es

let lvars (cmd : 'label t) : Id.LVar.Set.t =
  let module S = Id.LVar.Set in
  let fold = List.fold_left S.union S.empty in
  let lvars_es es = fold (List.map Expr.lvars es) in
  let lvars_fcall { fun_name = e; args = es; _ } =
    S.union (Expr.lvars e) (lvars_es es)
  in
  match cmd with
  | Skip -> S.empty
  | Assignment (_, e) -> Expr.lvars e
  | LAction (_, _, es) -> lvars_es es
  | Logic lcmd -> LCmd.lvars lcmd
  | Goto _ -> S.empty
  | GuardedGoto (e, _, _) -> Expr.lvars e
  | Call (fc, _) -> lvars_fcall fc
  | Par fcs -> fold (List.map lvars_fcall fcs)
  | ECall (_, e, es, _) -> S.union (Expr.lvars e) (lvars_es es)
  | Apply (_, e, _) -> Expr.lvars e
  | Arguments _ -> S.empty
  | PhiAssignment phis -> fold (List.map (fun (_, es) -> lvars_es es) phis)
  | ReturnNormal | ReturnError -> S.empty
  | Fail (_, es) -> lvars_es es

let locs (cmd : 'label t) : Id.Any_loc.Set.t =
  let module S = Id.Any_loc.Set in
  let fold = List.fold_left S.union S.empty in
  let locs_es es = fold (List.map Expr.locs es) in
  let locs_fcall { fun_name = e; args = es; _ } =
    S.union (Expr.locs e) (locs_es es)
  in
  match cmd with
  | Skip -> S.empty
  | Assignment (_, e) -> Expr.locs e
  | LAction (_, _, es) -> locs_es es
  | Logic lcmd -> LCmd.locs lcmd
  | Goto _ -> S.empty
  | GuardedGoto (e, _, _) -> Expr.locs e
  | Call (fc, _) -> locs_fcall fc
  | Par fcs -> fold (List.map locs_fcall fcs)
  | ECall (_, e, es, _) -> S.union (Expr.locs e) (locs_es es)
  | Apply (_, e, _) -> Expr.locs e
  | Arguments _ -> S.empty
  | PhiAssignment phis -> fold (List.map (fun (_, es) -> locs_es es) phis)
  | ReturnNormal | ReturnError -> S.empty
  | Fail (_, es) -> locs_es es

let successors (cmd : int t) (i : int) : int list =
  match cmd with
  | Goto j -> [ j ]
  | GuardedGoto (_, j, k) -> [ j; k ]
  | Call (_, error_label)
  | ECall (_, _, _, error_label)
  | Apply (_, _, error_label) -> (i + 1) :: Option.to_list error_label
  | Par _ -> [ i + 1 ]
  | ReturnNormal | ReturnError | Fail _ -> []
  | Skip | Assignment _ | LAction _ | Logic _ | Arguments _ | PhiAssignment _ ->
      [ i + 1 ]

let pp_logic_bindings =
  let aux fmt lbinds =
    let lab, subst_lst = lbinds in
    if List.length subst_lst > 0 then
      Fmt.pf fmt "%s - %a" lab
        (Fmt.list ~sep:Fmt.comma (fun f (x, le) ->
             Fmt.pf f "%a: %a" Id.pp x Expr.pp le))
        subst_lst
    else Fmt.pf fmt "%s" lab
  in
  Fmt.brackets aux

(** GIL All Statements *)
let pp ~(pp_label : 'a Fmt.t) fmt (cmd : 'a t) =
  let pp_params = Fmt.list ~sep:Fmt.comma Expr.pp in
  let pp_error f er = Fmt.pf f " with %a" pp_label er in
  let pp_subst f lbs = Fmt.pf f " use_subst %a" pp_logic_bindings lbs in
  let pp_fcall error_label f { var_name; fun_name; args; bindings } =
    Fmt.pf f "%a := %a(@[%a@])%a%a" Id.pp var_name Expr.pp fun_name pp_params
      args (Fmt.option pp_error) error_label (Fmt.option pp_subst) bindings
  in
  match cmd with
  | Skip -> Fmt.string fmt "skip"
  | Assignment (x, e) -> Fmt.pf fmt "%a := @[%a@]" Id.pp x Expr.pp e
  | LAction (x, name, es) ->
      Fmt.pf fmt "%a := [%s](@[%a@])" Id.pp x name pp_params es
  | Goto j -> Fmt.pf fmt "goto %a" pp_label j
  | GuardedGoto (e, j, k) ->
      Fmt.pf fmt "@[<h>goto [%a] %a %a@]" Expr.pp e pp_label j pp_label k
  | Call (call, err) -> pp_fcall err fmt call
  | Par calls ->
      Fmt.pf fmt "@[<h>par [ @[%a@] ]@]"
        Fmt.(list ~sep:semi (pp_fcall None))
        calls
  | ECall (var, name, args, error) ->
      Fmt.pf fmt "%a := extern %a(@[%a@])%a" Id.pp var Expr.pp name pp_params
        args (Fmt.option pp_error) error
  | Apply (var, arg, error) ->
      Fmt.pf fmt "%a := apply(@[%a@])%a" Id.pp var Expr.pp arg
        (Fmt.option pp_error) error
  | Arguments var -> Fmt.pf fmt "%a := args" Id.pp var
  | PhiAssignment lva ->
      Fmt.pf fmt "PHI(@[%a@])"
        Fmt.(
          list ~sep:semi
            (hbox (pair ~sep:(any ": ") Id.pp (list ~sep:comma Expr.pp))))
        lva
  | ReturnNormal -> Fmt.string fmt "return"
  | ReturnError -> Fmt.string fmt "throw"
  | Logic lcmd -> LCmd.pp fmt lcmd
  | Fail (err_type, es) -> Fmt.pf fmt "fail [%s](@[%a@])" err_type pp_params es

let pp_labeled = pp ~pp_label:Fmt.string
let pp_indexed = pp ~pp_label:Fmt.int

let call ?bindings ?error var_name fun_name args =
  Call ({ var_name; fun_name; args; bindings }, error)
