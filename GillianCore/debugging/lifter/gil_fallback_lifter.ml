open Lifter
open Syntaxes.Option

module type Gil_lifterWithState = sig
  module Lifter : Lifter.S

  val get_state : unit -> Lifter.t
end

module Make
    (SMemory : SMemory.S)
    (PC : ParserAndCompiler.S)
    (TLLifter : functor
      (Gil : Gil_lifterWithState)
      (V : Verifier.S with type annot = PC.Annot.t)
      ->
      S
        with type memory = SMemory.t
         and type tl_ast = PC.tl_ast
         and type memory_error = SMemory.err_t
         and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
         and type annot = PC.Annot.t)
    (Verifier : Verifier.S with type annot = PC.Annot.t) :
  S
    with type memory = SMemory.t
     and type tl_ast = PC.tl_ast
     and type memory_error = SMemory.err_t
     and type cmd_report = Verifier.SAInterpreter.Logging.ConfigReport.t
     and type annot = PC.Annot.t = struct
  let gil_state = ref None

  module Gil_lifter = Gil_lifter.Make (PC) (Verifier) (SMemory)

  module Gil_lifterWithState = struct
    module Lifter = Gil_lifter

    let get_state () = !gil_state |> Option.get
  end

  module TLLifter = TLLifter (Gil_lifterWithState) (Verifier)

  type t = {
    gil : Gil_lifter.t; [@to_yojson Gil_lifter.dump]
    tl : TLLifter.t option; [@to_yojson opt_to_yojson TLLifter.dump]
  }
  [@@deriving to_yojson]

  type memory = SMemory.t
  type tl_ast = PC.tl_ast
  type memory_error = SMemory.err_t
  type cmd_report = Verifier.SAInterpreter.Logging.ConfigReport.t
  type annot = PC.Annot.t

  let init proc_name tl_ast exec_data =
    let gil, gil_result = Gil_lifter.init proc_name tl_ast exec_data in
    gil_state := Some gil;
    let ret =
      match TLLifter.init_opt proc_name tl_ast exec_data with
      | None -> ({ gil; tl = None }, gil_result)
      | Some (tl, tl_result) -> ({ gil; tl = Some tl }, tl_result)
    in
    ret

  let init_opt proc_name tl_ast exec_data =
    Some (init proc_name tl_ast exec_data)

  let dump = to_yojson

  let handle_cmd prev_id branch_case exec_data { gil; tl } =
    match gil |> Gil_lifter.handle_cmd prev_id branch_case exec_data with
    | Stop -> (
        match tl with
        | None -> Stop
        | Some tl -> tl |> TLLifter.handle_cmd prev_id branch_case exec_data)
    | r -> r

  let get_gil_map state = state.gil |> Gil_lifter.get_gil_map

  let get_lifted_map_opt state =
    let+ tl = state.tl in
    tl |> TLLifter.get_lifted_map

  let get_lifted_map state =
    match get_lifted_map_opt state with
    | None -> failwith "Can't get lifted map!"
    | Some map -> map

  let get_unifys_at_id id { gil; tl } =
    match tl with
    | Some tl -> tl |> TLLifter.get_unifys_at_id id
    | None -> gil |> Gil_lifter.get_unifys_at_id id

  let get_root_id { gil; tl } =
    match tl with
    | Some tl -> tl |> TLLifter.get_root_id
    | None -> gil |> Gil_lifter.get_root_id

  let path_of_id id { gil; tl } =
    match tl with
    | Some tl -> tl |> TLLifter.path_of_id id
    | None -> gil |> Gil_lifter.path_of_id id

  let existing_next_steps id { gil; tl } =
    match tl with
    | Some tl -> tl |> TLLifter.existing_next_steps id
    | None -> gil |> Gil_lifter.existing_next_steps id

  let next_step_specific id case { gil; tl } =
    match tl with
    | Some tl -> tl |> TLLifter.next_step_specific id case
    | None -> gil |> Gil_lifter.next_step_specific id case

  let previous_step id { gil; tl } =
    match tl with
    | Some tl -> tl |> TLLifter.previous_step id
    | None -> gil |> Gil_lifter.previous_step id

  let select_next_path case id { gil; tl } =
    match tl with
    | Some tl -> tl |> TLLifter.select_next_path case id
    | None -> gil |> Gil_lifter.select_next_path case id

  let find_unfinished_path ?at_id { gil; tl } =
    match tl with
    | Some tl -> tl |> TLLifter.find_unfinished_path ?at_id
    | None -> gil |> Gil_lifter.find_unfinished_path ?at_id

  let memory_error_to_exception_info = TLLifter.memory_error_to_exception_info
  let add_variables = TLLifter.add_variables
end
