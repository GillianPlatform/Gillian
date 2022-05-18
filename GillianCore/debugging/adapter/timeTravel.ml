open DebugProtocolEx
open Debugger.DebuggerTypes

module Make (Debugger : Debugger.S) = struct
  let send_stopped_events stop_reason rpc resolver dbg =
    match stop_reason with
    | ReachedEnd ->
        "ReachedEnd: exiting" |> Log.to_rpc;
        Debug_rpc.send_event rpc
          (module Exited_event)
          Exited_event.Payload.(make ~exit_code:0);%lwt
        Debug_rpc.send_event rpc
          (module Terminated_event)
          Terminated_event.Payload.(make ());%lwt
        Debugger.terminate dbg;
        Lwt.wakeup_later_exn resolver Exit;
        Lwt.return_unit
    | Step | ReachedStart ->
        "Stopped: Step/ReachedStart" |> Log.to_rpc;
        (* Send step stopped event after reaching the end to allow for stepping
           backwards *)
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Step ~thread_id:(Some 0)
              ())
    | Breakpoint ->
        "Stopped: Breakpoint" |> Log.to_rpc;
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Breakpoint
              ~thread_id:(Some 0) ())
    | ExecutionError ->
        "Stopped: ExecutionError" |> Log.to_rpc;
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Exception
              ~thread_id:(Some 0) ())

  let run dbg rpc =
    let promise, resolver = Lwt.task () in
    Lwt.pause ();%lwt
    Debug_rpc.set_command_handler rpc
      (module Continue_command)
      (fun _ ->
        "Continue request received" |> Log.to_rpc;
        let stop_reason = Debugger.run dbg in
        send_stopped_events stop_reason rpc resolver dbg;%lwt
        Lwt.return (Continue_command.Result.make ()));
    Debug_rpc.set_command_handler rpc
      (module Next_command)
      (fun _ ->
        "Next request received" |> Log.to_rpc;
        let stop_reason = Debugger.step dbg in
        send_stopped_events stop_reason rpc resolver dbg);
    Debug_rpc.set_command_handler rpc
      (module Reverse_continue_command)
      (fun _ ->
        "Reverse continue request received" |> Log.to_rpc;
        let stop_reason = Debugger.run ~reverse:true dbg in
        send_stopped_events stop_reason rpc resolver dbg);
    Debug_rpc.set_command_handler rpc
      (module Step_back_command)
      (fun _ ->
        "Step back request received" |> Log.to_rpc;
        let stop_reason = Debugger.step_in ~reverse:true dbg in
        send_stopped_events stop_reason rpc resolver dbg);
    Debug_rpc.set_command_handler rpc
      (module Step_in_command)
      (fun _ ->
        "Step in request received" |> Log.to_rpc;
        let stop_reason = Debugger.step_in dbg in
        send_stopped_events stop_reason rpc resolver dbg);
    Debug_rpc.set_command_handler rpc
      (module Step_out_command)
      (fun _ ->
        "Step out request received" |> Log.to_rpc;
        let stop_reason = Debugger.step_out dbg in
        send_stopped_events stop_reason rpc resolver dbg);
    Lwt.join [ promise ]
end
