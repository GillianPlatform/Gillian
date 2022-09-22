open VisitorUtils

type t = {
  pred_name : string;
  pred_params : (string * WType.t option) list;
  pred_definitions : WLAssert.t list;
  pred_ins : int list;
  pred_nounfold : bool;
  pred_loc : CodeLoc.t;
  pred_id : int;
}
[@@deriving yojson]

let get_id p = p.pred_id
let get_loc p = p.pred_loc
let get_name p = p.pred_name
let get_ins p = p.pred_ins

let get_by_id id pred =
  let lassert_list_visitor = list_visitor_builder WLAssert.get_by_id id in
  let self_or_none = if get_id pred = id then `WPred pred else `None in
  self_or_none |>> (lassert_list_visitor, pred.pred_definitions)

(* TODO: write pretty_print function *)

(* let str = Format.asprintf "%a" pp *)
