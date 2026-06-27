open Utils
open Prelude
open Containers
open Syntaxes.Result

type t = string * WVal.t * CodeLoc.t

type config_option = {
  apply : WVal.t -> (unit, string) result;
  finalise : unit -> ((unit -> unit) option, string) result;
}

let manual_fold () =
  let v = ref (None : bool option) in
  let apply = function
    | WVal.Bool value ->
        if Option.is_some !v then Error "manual_fold has alredy been set"
        else
          let () = v := Some value in
          Ok ()
    | _ -> Error "Config statement 'manual_fold' expects a boolean value"
  in
  let finalise () =
    match !v with
    | None -> Ok None
    | Some v ->
        let f = set_with_reset Config.manual_proof v in
        Ok (Some f)
  in
  ("manual_fold", { apply; finalise })

let string_list_of_wval = function
  | WVal.Str s -> Some [ s ]
  | WVal.VList ss -> (
      ss
      |> List_utils.flaky_map @@ function
         | WVal.Str s -> Some s
         | _ -> None)
  | _ -> None

let ignores () =
  let v = ref SS.empty in
  let apply wval =
    match string_list_of_wval wval with
    | Some s ->
        let () = v := SS.(union !v (of_list s)) in
        Ok ()
    | None ->
        Error "Config statement 'ignore' expects a string or a list of strings"
  in
  let finalise () =
    let f = set_with_reset Config.Verification.things_to_exclude !v in
    Ok (Some f)
  in
  ("ignore", { apply; finalise })

let init_config_options () = [ manual_fold (); ignores () ]

let apply opts name value =
  match List.assoc_opt name opts with
  | Some opt -> opt.apply value
  | None -> Fmt.error "Unknown config statement '%s'" name

let rec map_res f = function
  | [] -> Ok []
  | x :: xs -> (
      match f x with
      | Ok x ->
          let* xs = map_res f xs in
          Ok (x :: xs)
      | Error (msg, loc) ->
          let loc = Option.map CodeLoc.to_location loc in
          Gillian_result.compilation_error ?loc msg)

let iter_res f xs = map_res f xs |> Result.map ignore

let apply_all configs =
  let opts = init_config_options () in
  let* () =
    configs
    |> iter_res @@ fun (name, value, loc) ->
       match apply opts name value with
       | Ok () -> Ok ()
       | Error msg -> Error (msg, Some loc)
  in
  let* resetters =
    opts
    |> map_res @@ fun (_, opt) ->
       opt.finalise () |> Result.map_error (fun m -> (m, None))
  in
  let resetters = List.filter_map Fun.id resetters in
  let resetter () = List.iter (fun f -> f ()) resetters in
  let () = Config.reset_config_f := Some resetter in
  Ok ()
