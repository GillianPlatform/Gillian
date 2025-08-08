open Protocol

(**/**)

module DL = Debugger_log

(**/**)

module Make (Debugger : Debugger.S) = struct
  let run rpc =
    let promise, resolver = Lwt.task () in
    let prevent_reenter () =
      Debug_rpc.remove_command_handler rpc (module Launch_command);
      Debug_rpc.remove_command_handler rpc (module Attach_command)
    in
    DL.set_rpc_command_handler rpc ~name:"Launch" ~catchall:false
      (module Launch_command)
      (fun (launch_args : Launch_command.Arguments.t) ->
        prevent_reenter ();
        let r =
          try Debugger.launch launch_args.program launch_args.procedure_name
          with e -> Gillian_result.internal_error (Printexc.to_string e)
        in
        let%lwt () =
          match r with
          | Ok dbg ->
              Lwt.wakeup_later resolver (launch_args, dbg);
              Lwt.return_unit
          | Error e -> raise (Gillian_result.Exc.Gillian_error e)
        in
        Lwt.return_unit);
    DL.set_rpc_command_handler rpc ~name:"Attach"
      (module Attach_command)
      (fun _ ->
        prevent_reenter ();
        Lwt.fail_with "Attach request is unsupported");
    DL.set_rpc_command_handler rpc ~name:"Disconnect"
      (module Disconnect_command)
      (fun _ ->
        Debug_rpc.remove_command_handler rpc (module Disconnect_command);
        Lwt.wakeup_later_exn resolver Exit;
        Lwt.return_unit);
    promise
end
