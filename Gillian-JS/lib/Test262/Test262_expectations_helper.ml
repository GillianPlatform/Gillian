module CState = Test262_outcome.State

let get_metadata cstate cval =
  let act_ret =
    CState.execute_action JSILNames.getMetadata cstate
      [ CState.eval_expr cstate (Lit cval) ]
  in
  try
    match act_ret with
    | [ Ok (_, [ _; e ]) ] -> Some e
    | _ -> None
  with _ -> None

let get_cell cstate loc prop =
  let act_ret =
    CState.execute_action JSILNames.getCell cstate
      [ CState.eval_expr cstate (Lit loc); CState.eval_expr cstate (Lit prop) ]
  in
  try
    match act_ret with
    | [ Ok (_, [ _; _; cell ]) ] -> Some cell
    | _ -> None
  with _ -> None

let is_test262_error ret_val ret_state =
  let open Gil_syntax.Literal in
  let proto_name = String "@proto" in
  let constructor_name = String "constructor" in
  let get_constructor_loc = function
    | LList [ String "d"; Loc fobj; _; _; _ ] -> Some (Loc fobj)
    | _ -> None
  in
  let construct_name = String "@construct" in
  let ( let* ) = Option.bind in
  let final_proto =
    let* md = get_metadata ret_state ret_val in
    let* proto = get_cell ret_state md proto_name in
    let* constructor = get_cell ret_state proto constructor_name in
    let* cloc = get_constructor_loc constructor in
    let* md = get_metadata ret_state cloc in
    get_cell ret_state md construct_name
  in
  match final_proto with
  | Some (String constructor) when String.sub constructor 0 12 = "Test262Error"
    -> true
  | _ -> false

let error_has_proto str ret_val ret_state =
  let ( let* ) = Option.bind in
  let open Gil_syntax.Literal in
  let proto_name = String "@proto" in
  let final_proto =
    let* md = get_metadata ret_state ret_val in
    get_cell ret_state md proto_name
  in
  match final_proto with
  | Some (Loc s) when String.equal s str -> true
  | _ -> false

let is_syntax_error = error_has_proto "$lserr_proto"
let is_ref_error = error_has_proto "$lrferr"

let parsing_failure_is_jsparser
    ({ additional_data; _ } : Utils.Gillian_result.Error.compilation_error) =
  match additional_data with
  | Some (`Assoc a) -> (
      match List.assoc_opt "is_parser_error" a with
      | Some (`Bool true) -> true
      | _ -> false)
  | _ -> false
