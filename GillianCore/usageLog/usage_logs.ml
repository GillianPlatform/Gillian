open Utils
open Syntaxes.Option

module Diff = struct
  let run_cmd ?(code_1_ok = false) cmd =
    let result =
      try
        let ((in_, _, err) as p) = Unix.open_process_full cmd [||] in
        let output = In_channel.input_all in_ in
        let err = In_channel.input_all err in
        match Unix.close_process_full p with
        | Unix.WEXITED 0 -> Ok output
        | Unix.WEXITED 1 when code_1_ok -> Ok output
        | Unix.WEXITED x -> Error (Fmt.str "exited with code %d" x, err)
        | Unix.WSIGNALED x -> Error (Fmt.str "killed by signal %d" x, err)
        | Unix.WSTOPPED x -> Error (Fmt.str "stopped by signal %d" x, err)
      with e -> Error (Printexc.to_string e, Printexc.get_backtrace ())
    in
    match result with
    | Ok output -> Some output
    | Error (a, b) ->
        let msg = Fmt.(str "Usage_logs: Command '%s' failed: %s\n%s" cmd a b) in
        Logging.print_to_all msg;
        None

  let get_git_root dir =
    let cmd = Fmt.str "git -C %s rev-parse --show-toplevel" dir in
    run_cmd cmd

  let get_relative_path file =
    let+ root = get_git_root (Filename.dirname file) in
    String.sub file (String.length root)
      (String.length file - String.length root)

  let original_files : (string, string) Hashtbl.t = Hashtbl.create 0

  let write_tmp prefix content =
    let tmp_file = Filename.temp_file ("gillian_" ^ prefix ^ "_") ".tmp" in
    let oc = open_out tmp_file in
    let () = output_string oc content in
    let () = close_out oc in
    tmp_file

  let get_original_file file =
    let/ () = Hashtbl.find_opt original_files file in
    let dir = Filename.dirname file in
    (* let* () = if is_git_ok dir then Some () else None in *)
    let* rel_file = get_relative_path file in
    let* content =
      let cmd =
        Fmt.str "git -C %s show %s:%s" dir !Config.usage_logs_git_ref rel_file
      in
      run_cmd cmd
    in
    let original_file = write_tmp "original" content in
    let () = Hashtbl.replace original_files file original_file in
    Some original_file

  let get ?content original_path =
    let* old = get_original_file original_path in
    let old_file = Filename.quote old in
    let new_ =
      match content with
      | Some content -> write_tmp "diff" content
      | None -> original_path
    in
    let new_file = Filename.quote new_ in
    let cmd = Fmt.str "diff --color=never %s %s" old_file new_file in
    (* diff gives code 1 when there are differences *)
    run_cmd ~code_1_ok:true cmd
end

let write json =
  if !Config.usage_logs then (
    let oc =
      open_out_gen
        [ Open_creat; Open_text; Open_append ]
        0o666 !Config.usage_logs_file
    in
    Yojson.Safe.to_channel ~suf:"\n" oc json;
    flush oc;
    close_out oc)

type small_loc = {
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}
[@@deriving yojson]

let shrink_loc =
  let open Location in
  fun { loc_start; loc_end; _ } ->
    let { pos_line = start_line; pos_column = start_col } = loc_start in
    let { pos_line = end_line; pos_column = end_col } = loc_end in
    { start_line; start_col; end_line; end_col }

module Event = struct
  module Debug = struct
    type dap_step =
      | Step_over
      | Step_in
      | Step_out
      | Step_back
      | Continue
      | Continue_back
    [@@deriving yojson]

    type interaction =
      | Dap_step of { kind : dap_step; breakpoint : bool }
      | Step_specific of { has_case : bool }
      | Jump of { is_match : bool }
    [@@deriving yojson]

    type session_start = {
      filename : string;
      proc : string;
      diff : string option;
    }
    [@@deriving yojson]

    type content =
      | Session_start of session_start
      | Session_stop
      | Interaction of interaction
    [@@deriving yojson]

    type t = { session : string; content : content [@main] }
    [@@deriving yojson, make]
  end

  module Lsp = struct
    type analysis_failure = {
      msg : string; [@main]
      loc : small_loc option;
      is_preprocessing : bool;
      in_target : string option;
    }
    [@@deriving yojson, make]

    type misc_failure = { msg : string; loc : small_loc option }
    [@@deriving yojson]

    type result =
      | Success
      | Analysis_failures of analysis_failure list
      | Compilation_error of misc_failure
      | Other_error of string
    [@@deriving yojson]

    type t = { filename : string; result : result; diff : string option }
    [@@deriving yojson, make]

    let of_gillian_result = (* let open Gillian_result.Error in *)
      function
      | Ok _ -> Success
      | Error (Gillian_result.Error.AnalysisFailures fs) ->
          let fs =
            fs
            |> List.map
               @@ fun Gillian_result.Error.
                        { msg; loc; is_preprocessing; in_target } ->
               let loc = Option.map shrink_loc loc in
               make_analysis_failure ?loc ~is_preprocessing ?in_target msg
          in
          Analysis_failures fs
      | Error (CompilationError { msg; loc; _ }) ->
          let loc = Option.map shrink_loc loc in
          Compilation_error { msg; loc }
      | Error (OperationError msg | InternalError { msg; _ }) -> Other_error msg
  end

  type content = Debug of Debug.t | Lsp of Lsp.t [@@deriving yojson]
  type t = { time : float; content : content } [@@deriving yojson]

  let log event =
    if !Config.usage_logs then
      let json = to_yojson event in
      write json

  let make ?time content =
    let time =
      match time with
      | Some t -> t
      | None -> Unix.gettimeofday ()
    in
    { time; content }
end

open Event

module Debug = struct
  let log ?time debug_event =
    let event = make ?time (Debug debug_event) in
    log event

  let session_id = ref "Unknown session"

  let start ~filename ~proc =
    let time = Unix.gettimeofday () in
    let session = string_of_float time in
    let () = session_id := session in
    let diff = Diff.get filename in
    log (Debug.make ~session (Session_start { filename; proc; diff }))

  let stop () =
    let session = !session_id in
    log (Debug.make ~session Session_stop)

  let log_interaction interaction =
    let session = !session_id in
    let debug_event = Debug.make ~session (Interaction interaction) in
    log debug_event
end

module Lsp = struct
  open Lsp

  let is_similar_result = function
    | Success, Success -> true
    | Compilation_error _, Compilation_error _ -> true
    | Other_error _, Other_error _ -> true
    | Analysis_failures fs1, Analysis_failures fs2 ->
        List.length fs1 = List.length fs2
        && List.for_all2
             (fun (a : analysis_failure) (b : analysis_failure) ->
               a.msg = b.msg)
             fs1 fs2
    | _ -> false

  let log filename ~content ?old_result result =
    let result = of_gillian_result result in
    match old_result with
    | Some old_result
      when is_similar_result (of_gillian_result old_result, result) -> ()
    | _ ->
        let diff = Diff.get ~content filename in
        let lsp_event = make ~filename ~result ?diff () in
        let event = Event.make (Lsp lsp_event) in
        log event
end
