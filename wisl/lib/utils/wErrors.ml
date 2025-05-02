let consistency_errors = ref false

type severity = SevError | SevWarning | SevInformation | SevHint

let int_of_severity = function
  | SevError -> 1
  | SevWarning -> 2
  | SevInformation -> 3
  | SevHint -> 4

type related_info_t = { rel_range : CodeLoc.t; rel_msg : string }

type t = {
  range : CodeLoc.t;
  message : string;
  code : string;
  severity : severity;
  related_information : related_info_t list;
  function_name : string;
}

type res_t = (unit, t) result

type error_code_t =
  | UndefinedProp
  | ExtensibilityError
  | UndefinedVar
  | SyntaxError
  | MissingResource
  | UnconsistentStmtBloc
  | FunctionNotVerified
  | UndefinedFunction
  | UndefinedLemma
  | MissingInvariant

let str_error_code = function
  | UndefinedProp -> "UndefinedProp"
  | ExtensibilityError -> "ExtensibilityError"
  | UndefinedVar -> "UndefinedVar"
  | SyntaxError -> "SyntaxError"
  | MissingResource -> "MissingResource"
  | UnconsistentStmtBloc -> "UnconsistentStmtBloc"
  | FunctionNotVerified -> "FunctionNotVerified"
  | UndefinedFunction -> "UndefinedFunction"
  | UndefinedLemma -> "UndefinedLemma"
  | MissingInvariant -> "MissingInvariant"

let get_errors results =
  let rec get_errors' errs = function
    | [] -> errs
    | Ok _ :: r -> get_errors' errs r
    | Error e :: r -> get_errors' (e :: errs) r
  in
  get_errors' [] results

let build_consistency_error message range function_name =
  let code = str_error_code UnconsistentStmtBloc in
  let severity = SevError in
  let related_information = [] in
  { message; range; code; severity; related_information; function_name }

let build_warning_not_called range function_name =
  let code = str_error_code FunctionNotVerified in
  let message =
    "This function is never verified because it has no specification and is \
     never called from a function that is verified"
  in
  let severity = SevWarning in
  let related_information = [] in
  { code; message; severity; related_information; range; function_name }

let build_warning_invariant range =
  let code = str_error_code MissingInvariant in
  let message =
    "This while loop will certainly need an invariant in order to be verified \
     correctly"
  in
  let severity = SevWarning in
  let related_information = [] in
  { code; message; severity; related_information; range; function_name = "" }

let build_err_string error_code id loc message =
  Format.sprintf "%s;%i;%s;%s"
    (str_error_code error_code)
    id (CodeLoc.str loc) message

let json_related { rel_range; rel_msg } =
  `Assoc
    [
      ("message", `String rel_msg); ("location", CodeLoc.json_with_uri rel_range);
    ]

let json_related_list l = `List (List.map json_related l)

let json { range; message; code; severity; related_information; _ } =
  `Assoc
    [
      ("range", CodeLoc.json range);
      ("message", `String message);
      ("code", `String code);
      ("source", `String "wisl-verify");
      ("severity", `Int (int_of_severity severity));
      ("relatedInformation", json_related_list related_information);
    ]
