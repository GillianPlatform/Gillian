val consistency_errors : bool ref

type severity = SevError | SevWarning | SevInformation | SevHint

type related_info_t = { rel_range : CodeLoc.t; rel_msg : string }

type t = {
  range : CodeLoc.t;
  message : string;
  code : string;
  severity : severity;
  related_information : related_info_t list;
  function_name : string;
}

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

val str_error_code : error_code_t -> string

type res_t = (unit, t) result

val build_warning_not_called : CodeLoc.t -> string -> t

val get_errors : res_t list -> t list

val json : t -> Yojson.Safe.t

val build_err_string : error_code_t -> int -> CodeLoc.t -> string -> string

val build_consistency_error : string -> CodeLoc.t -> string -> t

val build_warning_invariant : CodeLoc.t -> t
