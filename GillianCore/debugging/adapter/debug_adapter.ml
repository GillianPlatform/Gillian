include Debug_adapter_intf
module DL = Debugger_log

module Make : Make =
functor
  (Debugger : Debugger.S)
  ->
  struct
    module State_uninitialized = State_uninitialized.Make (Debugger)
    module State_initialized = State_initialized.Make (Debugger)
    module State_debug = State_debug.Make (Debugger)
    open Debug_protocol_ext.Custom_events (Debugger)

    let start in_ out =
      try%lwt
        Config.debug := true;
        let rpc = Debug_rpc.create ~in_ ~out () in
        DL.setup rpc;
        Printexc.record_backtrace true;
        let cancel = ref (fun () -> ()) in
        Lwt.async (fun () ->
            (try%lwt
               DL.log (fun m -> m "Initializing Debug Adapter...");
               let%lwt _, _ = State_uninitialized.run rpc in
               DL.log (fun m -> m "Initialized Debug Adapter");
               let%lwt launch_args, dbg = State_initialized.run rpc in
               let debug_state =
                 try Debugger.Inspect.get_debug_state dbg
                 with e ->
                   DL.log (fun m -> m "ERROR! %a" Fmt.exn e);
                   raise e
               in
               Debug_rpc.send_event rpc
                 (module Debug_state_update_event)
                 debug_state;%lwt
               State_debug.run launch_args dbg rpc;%lwt
               fst (Lwt.task ())
             with Exit -> Lwt.return_unit);%lwt
            !cancel ();
            Lwt.return_unit);
        let loop = Debug_rpc.start rpc in
        (cancel := fun () -> Lwt.cancel loop);
        (try%lwt loop with Lwt.Canceled -> Lwt.return_unit);%lwt
        Lwt.return ()
      with Failure e as f ->
        DL.to_file e;
        Lwt.pause ();%lwt
        raise f
  end
