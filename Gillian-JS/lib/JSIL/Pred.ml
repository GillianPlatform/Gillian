module SSubst = Gillian.Symbolic.Subst
module L = Logging
module Type = Gillian.Gil_syntax.Type
module Expr = Gillian.Gil_syntax.Expr

(** {b JSIL logic predicate}. *)
type t = {
  name : string;  (** Name of the predicate  *)
  num_params : int;  (** Number of parameters   *)
  params : (Gil_syntax.Var.t * Type.t option) list;
      (** Actual parameters      *)
  ins : int list;  (** Ins                    *)
  definitions : ((string * string list) option * Asrt.t) list;
      (** Predicate definitions  *)
  facts : Expr.t list;  (** Facts about the predicate *)
  pure : bool;  (** Is the predicate pure  *)
  abstract : bool;  (** Is the predicate abstract  *)
  nounfold : bool;  (** Should the predicate be unfolded? *)
  normalised : bool;  (** If the predicate has been previously normalised *)
}

(** Creates/populates a Hashtbl from the predicate list pred_defs *)
let init (preds : t list) : (string, t) Hashtbl.t =
  let pred_def_tbl = Hashtbl.create Config.small_tbl_size in
  List.iter
    (fun pred_def -> Hashtbl.add pred_def_tbl pred_def.name pred_def)
    preds;
  pred_def_tbl

let pp fmt pred =
  let { name; params; ins; definitions; _ } = pred in
  let exist_ins = List.length pred.ins <> List.length pred.params in
  let params_with_info =
    if exist_ins then
      List.mapi
        (fun i (v, t) ->
          ((if List.mem i ins then "+" else "") ^ Gil_syntax.Var.str v, t))
        params
    else List.map (fun (v, t) -> (Gil_syntax.Var.str v, t)) params
  in
  let pp_param fmt (v, t) =
    match t with
    | None -> Fmt.pf fmt "%s" v
    | Some typ -> Fmt.pf fmt "%s : %s" v (Type.str typ)
  in
  let pp_id_ex fmt id_ex =
    match id_ex with
    | None -> ()
    | Some (id, exs) ->
        if List.length exs > 0 then
          Fmt.pf fmt "[%s: %a]" id Fmt.(list ~sep:(any ", ") string) exs
        else Fmt.pf fmt "[%s]" id
  in
  let pp_abstract fmt = function
    | true -> Fmt.pf fmt "abstract "
    | false -> ()
  in
  let pp_pure fmt = function
    | true -> Fmt.pf fmt "pure "
    | false -> ()
  in
  let pp_nounfold fmt = function
    | true -> Fmt.pf fmt "nounfold "
    | false -> ()
  in
  let pp_def fmt (id_ex, asrt) =
    Fmt.pf fmt "%a%a" pp_id_ex id_ex Asrt.pp asrt
  in
  let pp_facts fmt = function
    | [] -> ()
    | facts ->
        Fmt.pf fmt "@\nfacts: %a;" Fmt.(list ~sep:(any " and ") Expr.pp) facts
  in
  Fmt.pf fmt "@[<v 2>%a%a%apred %s(%a):@\n%a;%a@]" pp_abstract pred.abstract
    pp_pure pred.pure pp_nounfold pred.nounfold name
    Fmt.(list ~sep:(any ", ") pp_param)
    params_with_info
    Fmt.(list ~sep:(any ",@\n") pp_def)
    definitions pp_facts pred.facts

let empty_pred_tbl () = Hashtbl.create Config.small_tbl_size
