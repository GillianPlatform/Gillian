(**************************************************************)
(**************************************************************)
(** GIL Commmands                                           **)

(**************************************************************)
(**************************************************************)

module SS = Containers.SS

type logic_bindings_t = string * (string * Expr.t) list [@@deriving yojson]

type 'label function_call = 'label TypeDef__.function_call = {
  var_name : string;
  fct_name : Expr.t;
  args : Expr.t list;
  err_lab : 'label option;
  bindings : logic_bindings_t option;
}
[@@deriving yojson]

type 'label t = 'label TypeDef__.cmd =
  | Skip  (** Skip                *)
  | Assignment of string * Expr.t  (** Assignment          *)
  | LAction of string * string * Expr.t list  (** Local Actions       *)
  | Logic of LCmd.t  (** GIL Logic commands *)
  | Goto of 'label  (** Unconditional goto  *)
  | GuardedGoto of Expr.t * 'label * 'label  (** Conditional goto    *)
  | Call of 'label function_call  (** Procedure call *)
  | ECall of string * Expr.t * Expr.t list * 'label option
      (** External Procedure call           *)
  | Par of 'label function_call list  (** Parallel composition *)
  | Apply of string * Expr.t * 'label option
      (** Application-style procedure call  *)
  | Arguments of string  (** Arguments of the current function *)
  | PhiAssignment of (string * Expr.t list) list  (** PHI assignment      *)
  | ReturnNormal  (** Normal return       *)
  | ReturnError  (** Error return        *)
  | Fail of string * Expr.t list  (** Failure             *)
[@@deriving yojson]

let fold = List.fold_left SS.union SS.empty

let pvars (cmd : 'label t) : SS.t =
  let pvars_es es = fold (List.map Expr.pvars es) in
  let pvars_fcall { var_name = x; fct_name = e; args = es; _ } =
    SS.union (SS.add x (Expr.pvars e)) (pvars_es es)
  in
  match cmd with
  | Skip -> SS.empty
  | Assignment (x, e) -> SS.add x (Expr.pvars e)
  | LAction (x, _, es) -> SS.add x (pvars_es es)
  | Logic lcmd -> LCmd.pvars lcmd
  | Goto _ -> SS.empty
  | GuardedGoto (e, _, _) -> Expr.pvars e
  | Par fcalls ->
      List.fold_left
        (fun acc f -> pvars_fcall f |> SS.union acc)
        SS.empty fcalls
  | Call fcall -> pvars_fcall fcall
  | ECall (x, e, es, _) -> SS.union (SS.add x (Expr.pvars e)) (pvars_es es)
  | Apply (x, e, _) -> SS.add x (Expr.pvars e)
  | Arguments x -> SS.singleton x
  | PhiAssignment phis -> fold (List.map (fun (_, es) -> pvars_es es) phis)
  | ReturnNormal | ReturnError -> SS.singleton "ret"
  | Fail (_, es) -> pvars_es es

let lvars (cmd : 'label t) : SS.t =
  let lvars_es es = fold (List.map Expr.lvars es) in
  let lvars_fcall { fct_name = e; args = es; _ } =
    SS.union (Expr.lvars e) (lvars_es es)
  in
  match cmd with
  | Skip -> SS.empty
  | Assignment (_, e) -> Expr.lvars e
  | LAction (_, _, es) -> lvars_es es
  | Logic lcmd -> LCmd.lvars lcmd
  | Goto _ -> SS.empty
  | GuardedGoto (e, _, _) -> Expr.lvars e
  | Par fcalls ->
      List.fold_left
        (fun acc f -> lvars_fcall f |> SS.union acc)
        SS.empty fcalls
  | Call fcall -> lvars_fcall fcall
  | ECall (_, e, es, _) -> SS.union (Expr.lvars e) (lvars_es es)
  | Apply (_, e, _) -> Expr.lvars e
  | Arguments _ -> SS.empty
  | PhiAssignment phis -> fold (List.map (fun (_, es) -> lvars_es es) phis)
  | ReturnNormal | ReturnError -> SS.empty
  | Fail (_, es) -> lvars_es es

let locs (cmd : 'label t) : SS.t =
  let locs_es es = fold (List.map Expr.locs es) in
  let locs_fcall { fct_name = e; args = es; _ } =
    SS.union (Expr.lvars e) (locs_es es)
  in
  match cmd with
  | Skip -> SS.empty
  | Assignment (_, e) -> Expr.locs e
  | LAction (_, _, es) -> locs_es es
  | Logic lcmd -> LCmd.lvars lcmd
  | Goto _ -> SS.empty
  | GuardedGoto (e, _, _) -> Expr.locs e
  | Par fcalls ->
      List.fold_left (fun acc f -> locs_fcall f |> SS.union acc) SS.empty fcalls
  | Call fcall -> locs_fcall fcall
  | ECall (_, e, es, _) -> SS.union (Expr.lvars e) (locs_es es)
  | Apply (_, e, _) -> Expr.locs e
  | Arguments _ -> SS.empty
  | PhiAssignment phis -> fold (List.map (fun (_, es) -> locs_es es) phis)
  | ReturnNormal | ReturnError -> SS.empty
  | Fail (_, es) -> locs_es es

let successors (cmd : int t) (i : int) : int list =
  match cmd with
  | Goto j -> [ j ]
  | GuardedGoto (_, j, k) -> [ j; k ]
  | Par [] -> failwith "Encountered a par constructor with no function calls!"
  | Par fcalls ->
      if List.exists (fun f -> Option.is_some f.err_lab) fcalls then
        failwith
          "Invalid GIL program: parallel calls may not have error labels!";
      [ i + 1 ]
      (* We want to resume execution one label past the last function call *)
  | Call { err_lab; _ } | ECall (_, _, _, err_lab) | Apply (_, _, err_lab) ->
      (i + 1) :: Option.to_list err_lab
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
  let pp_error fmt er = Fmt.pf fmt " with %a" pp_label er in
  let pp_fcall fmt { var_name; fct_name; args; err_lab; bindings } =
    let pp_subst fmt lbs = Fmt.pf fmt " use_subst %a" pp_logic_bindings lbs in
    Fmt.pf fmt "%s := %a(@[%a@])%a%a" var_name Expr.pp fct_name pp_params args
      (Fmt.option pp_error) err_lab (Fmt.option pp_subst) bindings
  in
  match cmd with
  | Skip -> Fmt.string fmt "skip"
  | Assignment (x, e) -> Fmt.pf fmt "%s := @[%a@]" x Expr.pp e
  | LAction (x, name, es) -> Fmt.pf fmt "%s := [%s](@[%a@])" x name pp_params es
  | Goto j -> Fmt.pf fmt "goto %a" pp_label j
  | GuardedGoto (e, j, k) ->
      Fmt.pf fmt "@[<h>goto [%a] %a %a@]" Expr.pp e pp_label j pp_label k
  | Par fs -> Fmt.pf fmt "@[<h>par [%a]@]" Fmt.(list ~sep:semi pp_fcall) fs
  | Call fcall -> pp_fcall fmt fcall
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

(* Legacy interface to simplify migration *)
let call (var_name, fct_name, args, err_lab, bindings) =
  Call { var_name; fct_name; args; err_lab; bindings }
