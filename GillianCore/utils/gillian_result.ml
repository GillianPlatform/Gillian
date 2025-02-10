module Error = struct
  type compilation_error = {
    msg : string;
    loc : Location.t option;
    additional_data : Yojson.Safe.t option;
  }

  type internal_error = {
    msg : string;
    backtrace : string option;
    additional_data : Yojson.Safe.t option;
  }

  type t =
    | CompilationError of compilation_error
    | OperationError of string
        (** Handled failure unrelated to analysis, e.g. unable to read input file *)
    | InternalError of internal_error  (** Something went very wrong! *)

  let pp fmt = function
    | CompilationError { msg; _ } ->
        Fmt.pf fmt "Error during compilation.\n%s" msg
    | OperationError o -> Fmt.pf fmt "%s" o
    | InternalError { msg; _ } -> Fmt.pf fmt "Internal error!\n%s" msg

  let show = Fmt.to_to_string pp

  let to_error_code = function
    | CompilationError _ -> 1
    | OperationError _ -> 2
    | InternalError _ -> 3
end

open Error

module Exc = struct
  exception
    Gillian_internal_error of {
      msg : string;
      additional_data : Yojson.Safe.t option;
    }

  exception Gillian_error of Error.t

  let internal_error ?(additional_data : (string * Yojson.Safe.t) list = []) msg
      : exn =
    let additional_data =
      match additional_data with
      | [] -> None
      | _ -> Some (`Assoc additional_data)
    in
    Gillian_internal_error { msg; additional_data }
end

type 'a t = ('a, Error.t) result

let compilation_error ?additional_data ?loc msg =
  Error (CompilationError { msg; loc; additional_data })

let operation_error msg = Error (OperationError msg)

let internal_error ?additional_data ?backtrace msg =
  let backtrace =
    match backtrace with
    | Some b -> b
    | None -> Some Printexc.(get_callstack 10 |> raw_backtrace_to_string)
  in
  Error (InternalError { msg; backtrace; additional_data })

let to_error_code = function
  | Ok _ -> 0
  | Error e -> Error.to_error_code e

let try_ f =
  try f () with
  | Exc.Gillian_internal_error { msg; additional_data } ->
      let backtrace = Some (Printexc.get_backtrace ()) in
      internal_error ?additional_data ~backtrace msg
  | Exc.Gillian_error e -> Error e
  | Failure msg ->
      let backtrace = Some (Printexc.get_backtrace ()) in
      internal_error ~backtrace msg
  | e ->
      let backtrace = Some (Printexc.get_backtrace ()) in
      let msg = "Internal error!\n" ^ Printexc.to_string e in
      internal_error ~backtrace msg
