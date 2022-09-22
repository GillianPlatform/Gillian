open VisitorUtils

type t = {
  lemma_name : string;
  lemma_params : string list;
  lemma_proof : WLCmd.t list option;
  lemma_variant : WLExpr.t option;
  lemma_hypothesis : WLAssert.t;
  lemma_conclusion : WLAssert.t;
  lemma_id : int;
  lemma_loc : CodeLoc.t;
}
[@@deriving yojson]

let get_id l = l.lemma_id
let get_loc l = l.lemma_loc

let get_by_id id lemma =
  let lcmd_list_visitor = list_visitor_builder WLCmd.get_by_id id in
  let lexpr_getter = WLExpr.get_by_id id in
  let aux_proof = Option.fold ~some:lcmd_list_visitor ~none:`None in
  let aux_variant = Option.fold ~some:lexpr_getter ~none:`None in
  let assert_getter = WLAssert.get_by_id id in
  let self_or_none = if get_id lemma = id then `WLemma lemma else `None in
  self_or_none
  |>> (assert_getter, lemma.lemma_hypothesis)
  |>> (assert_getter, lemma.lemma_conclusion)
  |>> (aux_proof, lemma.lemma_proof)
  |>> (aux_variant, lemma.lemma_variant)

(* TODO: write pretty_print function *)

(* let str = Format.asprintf "%a" pp *)
