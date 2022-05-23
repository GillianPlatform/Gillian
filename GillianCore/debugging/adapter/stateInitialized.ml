open DebugProtocolEx
module DL = Debugger_log

module Make (Debugger : Debugger.S) = struct
  let run rpc =
    let promise, resolver = Lwt.task () in
    let prevent_reenter () =
      Debug_rpc.remove_command_handler rpc (module Launch_command);
      Debug_rpc.remove_command_handler rpc (module Attach_command)
    in
    Debug_rpc.set_command_handler rpc
      (module Launch_command)
      (fun (launch_args : DebugProtocolEx.Launch_command.Arguments.t) ->
        DL.log (fun () -> ("Launch request received", []));
        prevent_reenter ();
        let () =
          match
            Debugger.launch launch_args.program launch_args.procedure_name
          with
          | Ok dbg -> Lwt.wakeup_later resolver (launch_args, dbg)
          | Error err ->
              DL.log (fun () -> (err, []));
              Lwt.wakeup_later_exn resolver Exit
        in
        Lwt.return_unit);
    Debug_rpc.set_command_handler rpc
      (module Attach_command)
      (fun _ ->
        DL.log (fun () -> ("Attach request received", []));
        prevent_reenter ();
        Lwt.fail_with "Attach request is unsupported");
    Debug_rpc.set_command_handler rpc
      (module Disconnect_command)
      (fun _ ->
        DL.log (fun () -> ("Disconnect request received", []));
        Debug_rpc.remove_command_handler rpc (module Disconnect_command);
        Lwt.wakeup_later_exn resolver Exit;
        Lwt.return_unit);
    promise
end
