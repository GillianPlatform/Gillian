open DebugProtocolEx
open Debugger.DebuggerTypes
module DL = Debugger_log

module Make (Debugger : Debugger.S) = struct
  let send_stopped_events stop_reason rpc =
    match stop_reason with
    | Step | ReachedStart | ReachedEnd ->
        DL.log (fun m -> m "Stopped: Step/ReachedStart/ReachedEnd");
        (* Send step stopped event after reaching the end to allow for stepping
           backwards *)
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Step ~thread_id:(Some 0)
              ())
    | Breakpoint ->
        DL.log (fun m -> m "Stopped: Breakpoint");
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Breakpoint
              ~thread_id:(Some 0) ())
    | ExecutionError ->
        DL.log (fun m -> m "Stopped: ExecutionError");
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Exception
              ~thread_id:(Some 0) ())

  let run dbg rpc =
    let promise, _ = Lwt.task () in
    Lwt.pause ();%lwt
    Debug_rpc.set_command_handler rpc
      (module Continue_command)
      (fun _ ->
        DL.log (fun m -> m "Continue request received");
        let stop_reason = Debugger.run dbg in
        send_stopped_events stop_reason rpc;%lwt
        Lwt.return (Continue_command.Result.make ()));
    Debug_rpc.set_command_handler rpc
      (module Next_command)
      (fun _ ->
        DL.log (fun m -> m "Next request received");
        try%lwt
          let stop_reason = Debugger.step dbg in
          send_stopped_events stop_reason rpc
        with
        | DL.FailureJson (e, json) ->
            DL.log (fun m -> m ~json "[Error] %s" e);
            failwith e
        | Failure e ->
            DL.log (fun m -> m "[Error] %s" e);
            failwith e);
    Debug_rpc.set_command_handler rpc
      (module Reverse_continue_command)
      (fun _ ->
        DL.log (fun m -> m "Reverse continue request received");
        let stop_reason = Debugger.run ~reverse:true dbg in
        send_stopped_events stop_reason rpc);
    Debug_rpc.set_command_handler rpc
      (module Step_back_command)
      (fun _ ->
        DL.log (fun m -> m "Step back request received");
        let stop_reason = Debugger.step_in ~reverse:true dbg in
        send_stopped_events stop_reason rpc);
    Debug_rpc.set_command_handler rpc
      (module Step_in_command)
      (fun _ ->
        DL.log (fun m -> m "Step in request received");
        let stop_reason = Debugger.step_in dbg in
        send_stopped_events stop_reason rpc);
    Debug_rpc.set_command_handler rpc
      (module Step_out_command)
      (fun _ ->
        DL.log (fun m -> m "Step out request received");
        let stop_reason = Debugger.step_out dbg in
        send_stopped_events stop_reason rpc);
    Lwt.join [ promise ]
end
