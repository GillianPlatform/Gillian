open DebugProtocolEx
module DL = Debugger_log

module Make (Debugger : Debugger.S) = struct
  let run rpc =
    let promise, resolver = Lwt.task () in
    let prevent_reenter () =
      Debug_rpc.remove_command_handler rpc (module Initialize_command)
    in
    DL.set_rpc_command_handler rpc ~name:"Initialize"
      (module Initialize_command)
      (fun arg ->
        prevent_reenter ();
        let caps =
          Capabilities.(
            make ~supports_configuration_done_request:(Some true)
              ~supports_exception_info_request:(Some true)
              ~supports_step_back:(Some true) ())
        in
        Lwt.wakeup_later resolver (arg, caps);
        Lwt.return caps);
    promise
end
