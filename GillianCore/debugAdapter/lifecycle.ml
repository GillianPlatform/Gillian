open DebugProtocolEx

module Make (Debugger : Debugger.S) = struct
  let run ~launch_args ~dbg rpc =
    let promise, resolver = Lwt.task () in
    Lwt.pause ();%lwt
    let send_initialize_event () =
      Debug_rpc.send_event rpc (module Initialized_event) ()
    in
    Debug_rpc.set_command_handler rpc
      (module Configuration_done_command)
      (fun _ ->
        Log.info "Configuration done ";
        let open Launch_command.Arguments in
        if not launch_args.stop_on_entry then (
          Log.info "Do not stop on entry";
          let stop_reason = Debugger.run dbg in
          match stop_reason with
          | Debugger.Step       ->
              let () =
                Log.info
                  "Debugger stopped because of step after running. This should \
                   not happen"
              in
              Lwt.return_unit
          | Debugger.ReachedEnd ->
              (* Send step stopped event to allow for stepping backwards *)
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(
                  make ~reason:Stopped_event.Payload.Reason.Step
                    ~thread_id:(Some 0) ()))
        else (
          Log.info "Stop on entry";
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(
              make ~reason:Stopped_event.Payload.Reason.Entry
                ~thread_id:(Some 0) ())));
    Debug_rpc.set_command_handler rpc
      (module Disconnect_command)
      (fun _ ->
        Log.info
          "Disconnect request received: interrupting the debugger is not \
           supported";
        Debugger.terminate dbg;
        Debug_rpc.remove_command_handler rpc (module Disconnect_command);
        Lwt.wakeup_later_exn resolver Exit;
        Lwt.return_unit);
    Lwt.join [ send_initialize_event (); promise ]
end
