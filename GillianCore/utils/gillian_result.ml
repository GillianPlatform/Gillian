open Prelude

type failure = Unknown_failure | Internal_error | Verification_failure
[@@deriving yojson]

type t = (unit, failure) result

exception Exn of failure

let to_yojson = result_to_yojson [%to_yojson: unit] failure_to_yojson
let of_yojson = result_of_yojson [%of_yojson: unit] failure_of_yojson
let ok = Ok ()
let unknown_failure = Error Unknown_failure
let internal_error = Error Internal_error
let verification_failure = Error Verification_failure

let merge a b =
  match (a, b) with
  | Error Unknown_failure, _ | _, Error Unknown_failure -> unknown_failure
  | Error Internal_error, _ | _, Error Internal_error -> internal_error
  | Error Verification_failure, _ | _, Error Verification_failure ->
      verification_failure
  | Ok (), Ok () -> ok

let to_exit_code = function
  | Ok _ -> 0
  | Error Unknown_failure -> 1
  | Error Internal_error -> 2
  | Error Verification_failure -> 3
