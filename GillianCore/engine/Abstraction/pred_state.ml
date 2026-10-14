type 'a t = {
  state : 'a;
  preds : Preds.t;
  wands : Wands.t;
  pred_defs : MP.preds_tbl_t;
}

let pp pp_state fmt (astate : 'a t) : unit =
  let { state; preds; wands; _ } = astate in
  Fmt.pf fmt "%a@\n@[<v 2>PREDICATES:@\n%a@]@\n@[<v 2>WANDS:@\n%a@]@\n" pp_state
    state Preds.pp preds Wands.pp wands

let of_yojson state_of_yojson (yojson : Yojson.Safe.t) : ('a t, string) result =
  (* TODO: Deserialize other components of pstate *)
  let open Syntaxes.Result in
  let rec aux = function
    | Some state, Some preds, Some wands, [] ->
        Ok { state; preds; pred_defs = MP.init_pred_defs (); wands }
    | None, preds, wands, ("state", state_yojson) :: rest ->
        let* state = state_of_yojson state_yojson in
        aux (Some state, preds, wands, rest)
    | state, None, wands, ("preds", preds_yojson) :: rest ->
        let* preds = Preds.of_yojson preds_yojson in
        aux (state, Some preds, wands, rest)
    | state, preds, None, ("wands", wands_yojson) :: rest ->
        let* wands = Wands.of_yojson wands_yojson in
        aux (state, preds, Some wands, rest)
    | _ -> Error "Cannot parse yojson into PState"
  in
  match yojson with
  | `Assoc sections -> aux (None, None, None, sections)
  | _ -> Error "Cannot parse yojson into PState"

let to_yojson state_to_yojson pstate =
  (* TODO: Serialize other components of pstate *)
  let { state; preds; wands; _ } = pstate in
  `Assoc
    [
      ("state", state_to_yojson state);
      ("preds", Preds.to_yojson preds);
      ("wands", Wands.to_yojson wands);
    ]

let clear_resource clear_state_resource (astate : 'a t) : 'a t =
  let { state; preds; wands = _; pred_defs } = astate in
  let state = clear_state_resource state in
  let preds_list = Preds.to_list preds in
  List.iter
    (fun (name, vs) ->
      let pred_def = Hashtbl.find pred_defs name in
      if not pred_def.pred.pred_pure then
        let _ =
          Preds.pop preds (fun (name', vs') ->
              name' = name && List.for_all2 Expr.equal vs' vs)
        in
        ())
    preds_list;
  { state; preds; wands = Wands.init []; pred_defs }

let copy_with_state (astate : 'a t) (state : 'a) : 'a t =
  {
    state;
    preds = Preds.copy astate.preds;
    wands = Wands.copy astate.wands;
    pred_defs = astate.pred_defs;
  }
