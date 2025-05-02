open Utils
open Containers
open Syntaxes.Result

type t = string * WVal.t * CodeLoc.t

let apply_manual_fold = function
  | WVal.Bool value ->
      let prev = !Config.manual_proof in
      Config.manual_proof := value;
      Ok (fun () -> Config.manual_proof := prev)
  | _ -> Error "Config statement 'manual_fold' expects a boolean value"

let apply name value =
  match name with
  | "manual_fold" -> apply_manual_fold value
  | _ -> Fmt.error "Unknown config statement '%s'" name

let apply_all configs =
  let rec aux dupes acc = function
    | [] -> Ok acc
    | ((name, value, loc) : t) :: rest -> (
        match
          if SS.mem name dupes then
            Fmt.error "Duplicate config statement '%s'" name
          else apply name value
        with
        | Ok reset ->
            let dupes = SS.add name dupes in
            let acc = reset :: acc in
            aux dupes acc rest
        | Error e ->
            let loc = CodeLoc.to_location loc in
            Gillian_result.compilation_error ~loc e)
  in
  let+ resetters = aux SS.empty [] configs in
  let resetter () = List.iter (fun f -> f ()) resetters in
  Config.reset_config_f := Some resetter
