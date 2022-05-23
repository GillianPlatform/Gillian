open DebugProtocolEx
open Debugger.DebuggerTypes

module Make (Debugger : Debugger.S) = struct
  let run launch_args dbg rpc =
    let promise, resolver = Lwt.task () in
    Lwt.pause ();%lwt
    let send_initialize_event () =
      Debug_rpc.send_event rpc (module Initialized_event) ()
    in
    Debug_rpc.set_command_handler rpc
      (module Configuration_done_command)
      (fun _ ->
        "Configuration done " |> Log.to_rpc;
        let open Launch_command.Arguments in
        if not launch_args.stop_on_entry then (
          "Do not stop on entry" |> Log.to_rpc;
          let stop_reason = Debugger.run ~launch:true dbg in
          match stop_reason with
          | Step ->
              "Debugger stopped because of step after running. This should not \
               happen" |> Log.to_rpc;
              Lwt.return_unit
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
          | ReachedStart ->
              (* Send step stopped event to allow for stepping backwards *)
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(
                  make ~reason:Stopped_event.Payload.Reason.Step
                    ~thread_id:(Some 0) ())
          | Breakpoint ->
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(
                  make ~reason:Stopped_event.Payload.Reason.Breakpoint
                    ~thread_id:(Some 0) ())
          | ExecutionError ->
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(
                  make ~reason:Stopped_event.Payload.Reason.Exception
                    ~thread_id:(Some 0) ()))
        else (
          "Stop on entry" |> Log.to_rpc;
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(
              make ~reason:Stopped_event.Payload.Reason.Entry
                ~thread_id:(Some 0) ())));
    Debug_rpc.set_command_handler rpc
      (module Disconnect_command)
      (fun _ ->
        "Disconnect request received: interrupting the debugger is not \
         supported" |> Log.to_rpc;
        Debugger.terminate dbg;
        Debug_rpc.remove_command_handler rpc (module Disconnect_command);
        Lwt.wakeup_later_exn resolver Exit;
        Lwt.return_unit);
    Lwt.join [ send_initialize_event (); promise ]
end
