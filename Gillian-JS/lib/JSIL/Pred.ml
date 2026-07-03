module SSubst = Gillian.Symbolic.Subst
module L = Logging
module Type = Gillian.Gil_syntax.Type
module Expr = Gillian.Gil_syntax.Expr

(** {b JSIL logic predicate}. *)
type t = {
  name : string;  (** Name of the predicate *)
  num_params : int;  (** Number of parameters *)
  params : (string * Type.t option) list;  (** Actual parameters *)
  ins_number : int;
      (** Number of in-parameters (the first [ins_number] ones) *)
  definitions : ((string * string list) option * Asrt.t) list;
      (** Predicate definitions *)
  facts : Expr.t list;  (** Facts about the predicate *)
  pure : bool;  (** Is the predicate pure *)
  abstract : bool;  (** Is the predicate abstract *)
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
  let { name; params; ins_number; definitions; _ } = pred in
  let pp_param fmt (v, t) =
    match t with
    | None -> Fmt.pf fmt "%s" v
    | Some typ -> Fmt.pf fmt "%s : %s" v (Type.str typ)
  in
  (* Prints params as [(in1, ..., ink; out1, ..., outm)] *)
  let pp_params fmt params =
    let ins = List.filteri (fun i _ -> i < ins_number) params in
    let outs = List.filteri (fun i _ -> i >= ins_number) params in
    Fmt.pf fmt "%a;%a"
      Fmt.(list ~sep:(any ", ") pp_param)
      ins
      Fmt.(list ~sep:(any ", ") pp_param)
      outs
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
    pp_pure pred.pure pp_nounfold pred.nounfold name pp_params params
    Fmt.(list ~sep:(any ",@\n") pp_def)
    definitions pp_facts pred.facts

let empty_pred_tbl () = Hashtbl.create Config.small_tbl_size
