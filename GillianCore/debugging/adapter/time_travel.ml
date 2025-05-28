open Protocol

(**/**)

module DL = Debugger_log

(**/**)

module Make (Debugger : Debugger.S) = struct
  let run { dbg; rpc; send_stopped_events; _ } =
    let promise, _ = Lwt.task () in
    Lwt.pause ();%lwt
    DL.set_rpc_command_handler rpc ~name:"Continue"
      (module Continue_command)
      (fun _ ->
        let stop_reason = Debugger.continue dbg in
        send_stopped_events stop_reason;%lwt
        Lwt.return (Continue_command.Result.make ()));
    DL.set_rpc_command_handler rpc ~name:"Next"
      (module Next_command)
      (fun _ ->
        let stop_reason = Debugger.step_over dbg in
        send_stopped_events stop_reason);
    DL.set_rpc_command_handler rpc ~name:"Reverse continue"
      (module Reverse_continue_command)
      (fun _ ->
        let stop_reason = Debugger.continue_back dbg in
        send_stopped_events stop_reason);
    DL.set_rpc_command_handler rpc ~name:"Step back"
      (module Step_back_command)
      (fun _ ->
        let stop_reason = Debugger.step_back dbg in
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
      (fun { step_id } ->
        let id =
          match Logging.Report_id.of_string_opt step_id with
          | Some id -> id
          | None -> failwith "Invalid step id"
        in
        match dbg |> Debugger.jump id with
        | Error e -> raise (Gillian_result.Exc.Gillian_error e)
        | Ok () ->
            send_stopped_events Step;%lwt
            Lwt.return ());
    DL.set_rpc_command_handler rpc ~name:"Step specific"
      (module Step_specific_command)
      (fun { step_id; branch_case } ->
        let result =
          match proc_name_of_id step_id with
          | Some proc_name -> dbg |> Debugger.start_proc proc_name
          | None ->
              let prev_id =
                match Logging.Report_id.of_string_opt step_id with
                | Some id -> id
                | None -> raise (Failure "Invalid step id")
              in
              dbg |> Debugger.step_specific branch_case prev_id
        in
        match result with
        | Error e -> raise (Gillian_result.Exc.Gillian_error e)
        | Ok stop_reason ->
            send_stopped_events stop_reason;%lwt
            Lwt.return ());
    Lwt.join [ promise ]
end
