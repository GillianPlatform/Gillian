(** Throw this exception if some unexpected goto
    was received for compilation *)
exception UnexpectedGoto of string

(** Throw this exception if there is a bug in the compiler code *)
exception Code_error of string

(** Throw this exception if the user made a mistake when invoking Kanillian *)
exception User_error of string

let () =
  Printexc.register_printer (function
    | UnexpectedGoto msg ->
        Some ("Did not expect to receive this goto for compilation:\n\n" ^ msg)
    | Code_error msg ->
        Some ("There is a bug in the Kanillian compiler:\n\n" ^ msg)
    | User_error msg -> Some ("User error - " ^ msg)
    | _ -> None)

let unexpected s = raise (UnexpectedGoto s)
let code_error s = raise (Code_error s)
let user_error s = raise (User_error s)
let assert_ e m = if not e then code_error m

let rethrow_gerror f =
  try f () with
  | Gerror.Code_error (_, msg) -> code_error msg
  | Gerror.Unexpected_irep (_, msg) -> unexpected msg
