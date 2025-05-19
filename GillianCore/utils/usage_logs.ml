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

    type session_start = { filename : string; proc : string }
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

    type t = { filename : string; result : result } [@@deriving yojson, make]
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
    log (Debug.make ~session (Session_start { filename; proc }))

  let stop () =
    let session = !session_id in
    log (Debug.make ~session Session_stop)

  let log_interaction interaction =
    let session = !session_id in
    let debug_event = Debug.make ~session (Interaction interaction) in
    log debug_event
end

module Lsp = struct
  let log filename result =
    let lsp_event = Lsp.make ~filename ~result in
    let event = make (Lsp lsp_event) in
    log event
end
