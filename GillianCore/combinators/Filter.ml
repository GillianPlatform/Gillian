type filter_mode = Hide | ShowOnly

module type FilterVals = sig
  val mode : filter_mode
  val action_filters : string list
  val preds_filters : string list
end

module Make (Filter : FilterVals) (S : MyMonadicSMemory.S) :
  MyMonadicSMemory.S with type t = S.t = struct
  include S

  let filter =
    match Filter.mode with
    | Hide -> not
    | ShowOnly -> Fun.id

  let keep_action s = filter (List.mem s Filter.action_filters)
  let keep_pred s = filter (List.mem s Filter.preds_filters)

  let list_actions () =
    List.filter
      (fun (a, _, _) -> keep_action @@ S.action_to_str a)
      (S.list_actions ())

  let action_from_str s = if keep_action s then S.action_from_str s else None

  let action_to_str a =
    if keep_action (S.action_to_str a) then S.action_to_str a
    else failwith "Cannot convert hidden action to string"

  let list_preds () =
    List.filter
      (fun (p, _, _) -> keep_pred @@ S.pred_to_str p)
      (S.list_preds ())

  let pred_from_str s = if keep_pred s then S.pred_from_str s else None

  let pred_to_str p =
    if keep_pred (S.pred_to_str p) then S.pred_to_str p
    else failwith "Cannot convert hidden predicate to string"
end
