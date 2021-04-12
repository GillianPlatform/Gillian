let start in_ out run_func =
  Log.reset ();
  let rpc = Debug_rpc.create ~in_ ~out () in
  let cancel = ref (fun () -> ()) in
  Lwt.async (fun () ->
      (try%lwt
         Log.info "Initializing Debug Adapter...";
         let%lwt _, _ = State_uninitialized.run rpc in
         Log.info "Initialized Debug Adapter";
         State_initialized.run rpc run_func;%lwt
         (* let%lwt launch_args, dbg = State_initialized.run rpc in *)
         (* State_debug.run ~launch_args ~dbg rpc;%lwt *)
         fst (Lwt.task ())
       with
      | Exit ->
        Lwt.return_unit);%lwt
      !cancel ();
      Lwt.return_unit);
  let loop = Debug_rpc.start rpc in
  (cancel := fun () -> Lwt.cancel loop);
  (try%lwt loop with Lwt.Canceled -> Lwt.return_unit);%lwt
  Log.info "Loop end";
  Lwt.return ()
