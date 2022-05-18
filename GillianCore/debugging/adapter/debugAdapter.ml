module type S = sig
  val start : Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t
end

module Make (Debugger : Debugger.S) = struct
  module StateUninitialized = StateUninitialized.Make (Debugger)
  module StateInitialized = StateInitialized.Make (Debugger)
  module StateDebug = StateDebug.Make (Debugger)

  let start in_ out =
    Log.reset ();
    let rpc = Debug_rpc.create ~in_ ~out () in
    let cancel = ref (fun () -> ()) in
    Lwt.async (fun () ->
        (try%lwt
           "Initializing Debug Adapter..." |> Log.to_rpc rpc;
           let%lwt _, _ = StateUninitialized.run rpc in
           "Initialized Debug Adapter" |> Log.to_rpc rpc;
           let%lwt launch_args, dbg = StateInitialized.run rpc in
           StateDebug.run launch_args dbg rpc;%lwt
           fst (Lwt.task ())
         with Exit -> Lwt.return_unit);%lwt
        !cancel ();
        Lwt.return_unit);
    let loop = Debug_rpc.start rpc in
    (cancel := fun () -> Lwt.cancel loop);
    (try%lwt loop with Lwt.Canceled -> Lwt.return_unit);%lwt
    "Loop end" |> Log.to_rpc rpc;
    Lwt.return ()
end
