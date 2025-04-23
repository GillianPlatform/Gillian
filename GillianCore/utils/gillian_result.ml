module Error = struct
  type analysis_failure = { msg : string; loc : Location.t option }

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
    | AnalysisFailures of analysis_failure list
    | CompilationError of compilation_error
    | OperationError of string
        (** Handled failure unrelated to analysis, e.g. unable to read input file *)
    | InternalError of internal_error  (** Something went very wrong! *)

  let pp fmt = function
    | AnalysisFailures es ->
        let msgs =
          es
          |> List.mapi @@ fun i ({ msg; loc } : analysis_failure) ->
             Fmt.str "%d. %s%a" (i + 1) msg Location.pp_full loc
        in
        Fmt.pf fmt "Analysis failures!\n%a\n"
          (Fmt.list ~sep:(Fmt.any "\n") Fmt.string)
          msgs
    | CompilationError { msg; loc; _ } ->
        Fmt.pf fmt "Error during compilation, at%a.\n%s" Location.pp_full loc
          msg
    | OperationError o -> Fmt.pf fmt "%s" o
    | InternalError { msg; _ } -> Fmt.pf fmt "Internal error!\n%s" msg

  let show = Fmt.to_to_string pp

  let to_error_code = function
    | AnalysisFailures _ -> 1
    | CompilationError _ -> 2
    | OperationError _ -> 124
    | InternalError _ -> 125
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

  let verification_failure ?loc msg =
    Gillian_error (AnalysisFailures [ { msg; loc } ])
end

type 'a t = ('a, Error.t) result

let analysis_failures errs = Error (AnalysisFailures errs)
let analysis_failure ?loc msg = Error (AnalysisFailures [ { msg; loc } ])

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

(** "Merges" two results by selecting the error with highest precedence.
  If both results are AnalysisFailures, they are properly merged.
  If both results are Ok, takes the result of merge_ok, which takes the {b first} value by default *)
let merge ?(merge_ok : 'a -> 'b -> 'c = fun x _ -> x) e1 e2 =
  match (e1, e2) with
  | Error (InternalError e), _ | _, Error (InternalError e) ->
      Error (InternalError e)
  | Error (OperationError e), _ | _, Error (OperationError e) ->
      Error (OperationError e)
  | Error (CompilationError e), _ | _, Error (CompilationError e) ->
      Error (CompilationError e)
  | Error (AnalysisFailures e1), Error (AnalysisFailures e2) ->
      Error (AnalysisFailures (e1 @ e2))
  | Error (AnalysisFailures e), _ | _, Error (AnalysisFailures e) ->
      Error (AnalysisFailures e)
  | Ok a, Ok b -> Ok (merge_ok a b)

(* Execution should continue if ok or analysis failure (to get the rest of the results).
   Other errors should halt immediately. *)
let should_continue = function
  | Ok _ | Error (AnalysisFailures _) -> true
  | Error (InternalError _ | OperationError _ | CompilationError _) -> false
