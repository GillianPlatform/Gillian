(* open Debug_protocol_ex

let run ~dbg rpc =
  let promise, _ = Lwt.task () in
  Debug_rpc.set_command_handler
    rpc
    (module Threads_command)
    (fun () ->
      let () = Log.info "Threads request received" in
      let main_thread = Thread.make ~id:0 ~name:"main" in
      Lwt.return (Threads_command.Result.make ~threads:[ main_thread ] ()));
  Debug_rpc.set_command_handler
    rpc
    (module Stack_trace_command)
    (fun _ ->
      let () = Log.info "Stack trace request received" in
      let (frames : Debugger.frame list) = Debugger.get_frames dbg in
      let stack_frames =
        frames
        |> Stdlib.List.map (fun (frame : Debugger.frame) ->
               let source_path =
                 Some (Source.make ~path:(Some frame.Debugger.source_path) ())
               in
               Stack_frame.make
                 ~id:frame.Debugger.index
                 ~name:frame.Debugger.name
                 ~source:source_path
                 ~line:frame.Debugger.line_num
                 ~column:frame.Debugger.col_num
                 ())
      in
      Lwt.return Stack_trace_command.Result.(make ~stack_frames ()));
  Debug_rpc.set_command_handler
    rpc
    (module Scopes_command)
    (fun _ ->
      let scopes = Debugger.get_scopes dbg in
      let scopes =
        scopes
        |> List.map (fun (scope : Debugger.scope) ->
               let name = scope.name in
               let variables_reference = scope.id in
               Scope.make ~name ~variables_reference ~expensive:false ())
      in
      Lwt.return (Scopes_command.Result.make ~scopes ()));
  Debug_rpc.set_command_handler
    rpc
    (module Variables_command)
    (fun args ->
      let variables = Debugger.get_variables args.variables_reference dbg in
      let variables =
        variables
        |> List.map (fun (var : Debugger.variable) ->
               let name = var.name in
               let value = var.value in
               let type_ = var.type_ in
               Variable.make ~name ~value ~type_ ~variables_reference:0 ())
      in
      Lwt.return (Variables_command.Result.make ~variables ()));
  Lwt.join [ promise ] *)
