open DebugProtocolEx

module type S = sig
  val start : Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t
end

module Make (Debugger : Debugger.S) = struct
  module Lifecycle = Lifecycle.Make (Debugger)
  module TimeTravel = TimeTravel.Make (Debugger)

  let initialize rpc =
    let promise, resolver = Lwt.task () in
    let prevent_reenter () =
      Debug_rpc.remove_command_handler rpc (module Initialize_command)
    in
    Debug_rpc.set_command_handler rpc
      (module Initialize_command)
      (fun arg ->
        prevent_reenter ();
        Log.info "Initialize request received";
        let caps =
          Capabilities.(
            make ~supports_configuration_done_request:(Some true) ())
        in
        Lwt.wakeup_later resolver (arg, caps);
        Lwt.return caps);
    promise

  let launch rpc =
    let promise, resolver = Lwt.task () in
    let prevent_reenter () =
      Debug_rpc.remove_command_handler rpc (module Launch_command);
      Debug_rpc.remove_command_handler rpc (module Attach_command)
    in
    Debug_rpc.set_command_handler rpc
      (module Launch_command)
      (fun (launch_args : DebugProtocolEx.Launch_command.Arguments.t) ->
        Log.info "Launch request received";
        prevent_reenter ();
        let () =
          match
            Debugger.launch launch_args.Launch_command.Arguments.program
          with
          | Ok dbg    -> Lwt.wakeup_later resolver (launch_args, dbg)
          | Error err ->
              let () = Log.info err in
              Lwt.wakeup_later_exn resolver Exit
        in
        Lwt.return_unit);
    Debug_rpc.set_command_handler rpc
      (module Attach_command)
      (fun _ ->
        Log.info "Attach request received";
        prevent_reenter ();
        Lwt.fail_with "Attach request is unsupported");
    Debug_rpc.set_command_handler rpc
      (module Disconnect_command)
      (fun _ ->
        Log.info "Disconnect request received";
        Debug_rpc.remove_command_handler rpc (module Disconnect_command);
        Lwt.wakeup_later_exn resolver Exit;
        Lwt.return_unit);
    promise

  let setup_commands ~launch_args ~dbg rpc =
    Lwt.join [ Lifecycle.run ~launch_args ~dbg rpc; TimeTravel.run ~dbg rpc ]

  let start in_ out =
    Log.reset ();
    let rpc = Debug_rpc.create ~in_ ~out () in
    let cancel = ref (fun () -> ()) in
    Lwt.async (fun () ->
        (try%lwt
           Log.info "Initializing Debug Adapter...";
           let%lwt _, _ = initialize rpc in
           Log.info "Initialized Debug Adapter";
           let%lwt launch_args, dbg = launch rpc in
           setup_commands ~launch_args ~dbg rpc;%lwt
           fst (Lwt.task ())
         with Exit -> Lwt.return_unit);%lwt
        !cancel ();
        Lwt.return_unit);
    let loop = Debug_rpc.start rpc in
    (cancel := fun () -> Lwt.cancel loop);
    (try%lwt loop with Lwt.Canceled -> Lwt.return_unit);%lwt
    Log.info "Loop end";
    Lwt.return ()
end
