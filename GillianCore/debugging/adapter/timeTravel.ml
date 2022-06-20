open DebugProtocolEx
open Debugger.DebuggerTypes
open DapCustom.Commands
open DapCustom.Events
module DL = Debugger_log

module Make (Debugger : Debugger.S) = struct
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
    Debug_rpc.send_event rpc
      (module Debug_state_update_event (Debugger))
      (Debugger.Inspect.get_debug_state dbg)

  let run dbg rpc =
    let send_stopped_events = send_stopped_events dbg rpc in
    let promise, _ = Lwt.task () in
    Lwt.pause ();%lwt
    DL.set_rpc_command_handler rpc ~name:"Continue"
      (module Continue_command)
      (fun _ ->
        let stop_reason = Debugger.run dbg in
        send_stopped_events stop_reason;%lwt
        Lwt.return (Continue_command.Result.make ()));
    DL.set_rpc_command_handler rpc ~name:"Next"
      (module Next_command)
      (fun _ ->
        let stop_reason = Debugger.step dbg in
        send_stopped_events stop_reason);
    DL.set_rpc_command_handler rpc ~name:"Reverse"
      (module Reverse_continue_command)
      (fun _ ->
        let stop_reason = Debugger.run ~reverse:true dbg in
        send_stopped_events stop_reason);
    DL.set_rpc_command_handler rpc ~name:"Step back"
      (module Step_back_command)
      (fun _ ->
        let stop_reason = Debugger.step ~reverse:true dbg in
        send_stopped_events stop_reason);
    DL.set_rpc_command_handler rpc ~name:"Step in"
      (module Step_in_command)
      (fun _ ->
        let stop_reason = Debugger.step_in dbg in
        send_stopped_events stop_reason);
    DL.set_rpc_command_handler rpc ~name:"Step out"
      (module Step_out_command)
      (fun _ ->
        let stop_reason = Debugger.step_out dbg in
        send_stopped_events stop_reason);
    DL.set_rpc_command_handler rpc ~name:"Jump"
      (module Jump_command)
      (fun { id } ->
        match dbg |> Debugger.jump_to_id id with
        | Error e ->
            Lwt.return
              (Jump_command.Result.make ~success:false ~err:(Some e) ())
        | Ok () ->
            send_stopped_events Step;%lwt
            Lwt.return (Jump_command.Result.make ~success:true ()));
    let module Step_specific_command = Step_specific_command (Debugger) in
    DL.set_rpc_command_handler rpc ~name:"Step specific"
      (module Step_specific_command)
      (fun { prev_id; branch_case } ->
        match dbg |> Debugger.step_specific branch_case prev_id with
        | Error e ->
            Lwt.return
              (Step_specific_command.Result.make ~success:false ~err:(Some e) ())
        | Ok stop_reason ->
            send_stopped_events stop_reason;%lwt
            Lwt.return (Step_specific_command.Result.make ~success:true ()));
    Lwt.join [ promise ]
end
