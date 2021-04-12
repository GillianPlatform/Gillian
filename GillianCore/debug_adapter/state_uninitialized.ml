open Debug_protocol_ex

let run rpc =
  let promise, resolver = Lwt.task () in
  let prevent_reenter () =
    Debug_rpc.remove_command_handler rpc (module Initialize_command)
  in
  Debug_rpc.set_command_handler
    rpc
    (module Initialize_command)
    (fun arg ->
      prevent_reenter ();
      Log.info "Initialize request received";
      let caps =
        Capabilities.(
          make
            ~supports_configuration_done_request:(Some true)
            ~supports_breakpoint_locations_request:(Some true)
            ~supports_step_back:(Some true)
            ())
      in
      Lwt.wakeup_later resolver (arg, caps);
      Lwt.return caps);
  promise
