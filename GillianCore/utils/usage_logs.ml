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

module Event = struct
  module Debug = struct
    type interaction =
      | StepOver
      | StepIn
      | StepOut
      | StepBack
      | Continue
      | ContinueBack
      | StepSpecific
      | Jump
      | HitBreakpoint
    [@@deriving yojson]

    type content = Session_start | Session_stop | Interaction of interaction
    [@@deriving yojson]

    type t = { session : string; content : content [@main] }
    [@@deriving yojson, make]
  end

  module Lsp = struct
    type t = { filename : string; result : Gillian_result.Error.t option }
    [@@deriving yojson, make]
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

  let with_session f =
    let time = Unix.gettimeofday () in
    let session = string_of_float time in
    let () = session_id := session in
    let () = log (Debug.make ~session Session_start) in
    let finally () = log (Debug.make ~session Session_stop) in
    Fun.protect ~finally f

  let log_interaction interaction =
    let session = !session_id in
    let debug_event = Debug.make ~session (Interaction interaction) in
    log debug_event
end

module Lsp = struct
  let log filename result =
    let result =
      match result with
      | Ok _ -> None
      | Error e -> Some e
    in
    let lsp_event = Lsp.make ~filename ?result () in
    let event = make (Lsp lsp_event) in
    log event
end
