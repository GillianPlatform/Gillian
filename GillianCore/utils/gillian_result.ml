module Error = struct
  type analysis_failure = {
    msg : string;  (** Brief, e.g. for a diagnostic *)
    details : string option; [@default None]
        (** The full message, if longer than [msg] *)
    loc : Location.t option;
    is_preprocessing : bool;
    in_target : string option;
  }
  [@@deriving yojson]

  type compilation_error = {
    msg : string;
    loc : Location.t option;
    additional_data : Yojson.Safe.t option;
  }
  [@@deriving yojson]

  type internal_error = {
    msg : string;
    backtrace : string option;
    additional_data : Yojson.Safe.t option;
  }
  [@@deriving yojson]

  let current_target = ref None

  type t =
    | AnalysisFailures of analysis_failure list
    | CompilationError of compilation_error
    | OperationError of string
        (** Handled failure unrelated to analysis, e.g. unable to read input
            file *)
    | InternalError of internal_error  (** Something went very wrong! *)
  [@@deriving yojson]

  let make_analysis_failure
      ?(is_preprocessing = false)
      ?in_target
      ?details
      ?loc
      msg =
    let in_target = Option_utils.coalesce in_target !current_target in
    { msg; details; loc; is_preprocessing; in_target }

  let make_analysis_failures ?is_preprocessing ?in_target ?loc msg =
    AnalysisFailures
      [ make_analysis_failure ?is_preprocessing ?in_target ?loc msg ]

  let pp fmt e =
    match e with
    | AnalysisFailures es ->
        let msgs =
          es
          |> List.mapi @@ fun i ({ msg; details; loc; _ } : analysis_failure) ->
             let lines =
               String.split_on_char '\n' (Option.value ~default:msg details)
             in
             (* A multi-line message gets its location on a line of its own *)
             let lines =
               match
                 (lines, String.trim (Fmt.str "%a" Location.pp_full loc))
               with
               | _, "" -> lines
               | [ line ], loc -> [ line ^ " " ^ loc ]
               | lines, loc -> lines @ [ loc ]
             in
             (* The box keeps a multi-line message's lines past the number *)
             Fmt.str "%d. @[<v>%a@]" (i + 1) Fmt.(list ~sep:cut string) lines
        in
        Fmt.pf fmt "Analysis failures!\n%a\n"
          (Fmt.list ~sep:(Fmt.any "\n") Fmt.string)
          msgs
    | CompilationError { msg; loc = None; _ } ->
        Fmt.pf fmt "Error during compilation.\n%s" msg
    | CompilationError { msg; loc; additional_data } ->
        let () =
          Fmt.pf fmt "Error during compilation, at%a.\n%s" Location.pp_full loc
            msg
        in
        let () =
          match additional_data with
          | Some j -> Fmt.pf fmt "\n%a" (Yojson.Safe.pretty_print ?std:None) j
          | None -> ()
        in
        ()
    | OperationError o -> Fmt.pf fmt "%s" o
    | InternalError { msg; backtrace; additional_data } ->
        let () = Fmt.pf fmt "Internal error!\n%s" msg in
        let () =
          match additional_data with
          | Some j -> Fmt.pf fmt "\n%a" (Yojson.Safe.pretty_print ?std:None) j
          | None -> ()
        in
        let () =
          match backtrace with
          | Some b -> Fmt.pf fmt "\n%s" b
          | None -> ()
        in
        ()

  let show = Fmt.to_to_string pp

  let to_error_code = function
    | AnalysisFailures _ -> 1
    | CompilationError _ -> 2
    | OperationError _ -> 124
    | InternalError _ -> 125

  let additional_data = function
    | CompilationError { additional_data; _ }
    | InternalError { additional_data; _ } -> additional_data
    | AnalysisFailures _ | OperationError _ -> None
end

open Error

let with_target target f =
  let old_target = !current_target in
  current_target := Some target;
  let finally () = current_target := old_target in
  Fun.protect ~finally f

module Exc = struct
  exception
    Gillian_internal_error of {
      msg : string;
      additional_data : Yojson.Safe.t option;
    }

  exception Gillian_error of Error.t

  let () =
    Printexc.register_printer @@ function
    | Gillian_error e -> Some (Error.show e)
    | _ -> None

  let internal_error ?(additional_data : (string * Yojson.Safe.t) list = []) msg
      : exn =
    let additional_data =
      match additional_data with
      | [] -> None
      | _ -> Some (`Assoc additional_data)
    in
    Gillian_internal_error { msg; additional_data }

  let analysis_failure ?(is_preprocessing = false) ?in_target ?loc msg =
    Gillian_error (make_analysis_failures ~is_preprocessing ?in_target ?loc msg)

  let compilation_error ?(additional_data : Yojson.Safe.t option) ?loc msg =
    Gillian_error (CompilationError { msg; loc; additional_data })
end

type 'a t = ('a, Error.t) result

let analysis_failure ?is_preprocessing ?in_target ?loc msg =
  Error (make_analysis_failures ?is_preprocessing ?in_target ?loc msg)

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

(** "Merges" two results by selecting the error with highest precedence. If both
    results are AnalysisFailures, they are properly merged. If both results are
    Ok, takes the result of merge_ok, which takes the {b first} value by default
*)
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
  | Error (InternalError _ | OperationError _ | CompilationError _) -> false
  | Error (AnalysisFailures fs)
    when List.exists (fun f -> f.is_preprocessing) fs -> false
  | Ok _ | Error (AnalysisFailures _) -> true
