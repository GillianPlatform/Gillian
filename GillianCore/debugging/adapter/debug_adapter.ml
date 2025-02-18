include Debug_adapter_intf
open Protocol
module DL = Debugger_log

module Make : Make =
functor
  (Debugger : Debugger.S)
  ->
  struct
    module State_uninitialized = State_uninitialized.Make (Debugger)
    module State_initialized = State_initialized.Make (Debugger)
    module State_debug = State_debug.Make (Debugger)

    let send_map_update dbg rpc =
      let map_update = Debugger.Inspect.get_map_update dbg in
      Debug_rpc.send_event rpc (module Map_update_event) map_update

    let send_stopped_events dbg rpc stop_reason =
      (match stop_reason with
      | Step | ReachedStart | ReachedEnd ->
          DL.log (fun m ->
              m
                ~json:[ ("reason", stop_reason_to_yojson stop_reason) ]
                "Stopped: Step/ReachedStart/ReachedEnd");
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
                ~thread_id:(Some 0) ()));%lwt
      let%lwt () = send_map_update dbg rpc in
      Lwt.return_unit

    let run_debugger rpc =
      try%lwt
        DL.log (fun m -> m "Initializing Debug Adapter...");
        let%lwt _, _ = State_uninitialized.run rpc in
        DL.log (fun m -> m "Initialized Debug Adapter");
        let%lwt launch_args, dbg = State_initialized.run rpc in
        let%lwt () = send_map_update dbg rpc in
        let cfg =
          {
            rpc;
            dbg;
            launch_args;
            send_stopped_events = send_stopped_events dbg rpc;
          }
        in
        State_debug.run cfg;%lwt
        fst (Lwt.task ())
      with Exit -> Lwt.return_unit

    let start in_ out =
      try%lwt
        Utils.Prelude.disable_stdout ();
        Config.debug := true;
        let rpc = Debug_rpc.create ~in_ ~out () in
        DL.setup rpc;
        Printexc.record_backtrace true;
        let cancel = ref (fun () -> ()) in
        Lwt.async (fun () ->
            let%lwt () = run_debugger rpc in
            !cancel ();
            Lwt.return_unit);
        let loop = Debug_rpc.start rpc in
        let () = cancel := fun () -> Lwt.cancel loop in
        let%lwt () = try%lwt loop with Lwt.Canceled -> Lwt.return_unit in
        Lwt.return ()
      with Failure e as f ->
        DL.to_file e;
        Lwt.pause ();%lwt
        raise f
  end
