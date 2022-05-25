open DebugProtocolEx
open Debugger.DebuggerTypes
module DL = Debugger_log

module Make (Debugger : Debugger.S) = struct
  let run dbg rpc =
    Debug_rpc.set_command_handler rpc
      (module Threads_command)
      (fun () ->
        DL.log (fun m -> m "Threads request received");
        let main_thread = Thread.make ~id:0 ~name:"main" in
        Lwt.return (Threads_command.Result.make ~threads:[ main_thread ] ()));
    Debug_rpc.set_command_handler rpc
      (module Stack_trace_command)
      (fun _ ->
        DL.log (fun m -> m "Stack trace request received");
        let (frames : frame list) = Debugger.get_frames dbg in
        let stack_frames =
          frames
          |> Stdlib.List.map (fun (frame : frame) ->
                 let source_path =
                   Some (Source.make ~path:(Some frame.source_path) ())
                 in
                 Stack_frame.make ~id:frame.index ~name:frame.name
                   ~source:source_path ~line:frame.start_line
                   ~column:frame.start_column ~end_line:(Some frame.end_line)
                   ~end_column:(Some frame.end_column) ())
        in
        Lwt.return Stack_trace_command.Result.(make ~stack_frames ()));
    Debug_rpc.set_command_handler rpc
      (module Scopes_command)
      (fun _ ->
        DL.log (fun m -> m "Scopes request received");
        let scopes = Debugger.get_scopes dbg in
        let scopes =
          scopes
          |> List.map (fun (scope : scope) ->
                 let name = scope.name in
                 let variables_reference = scope.id in
                 Scope.make ~name ~variables_reference ~expensive:false ())
        in
        Lwt.return (Scopes_command.Result.make ~scopes ()));
    Debug_rpc.set_command_handler rpc
      (module Variables_command)
      (fun args ->
        DL.log (fun m -> m "Variables request received");
        let variables = Debugger.get_variables args.variables_reference dbg in
        let variables =
          variables
          |> List.map (fun (var : variable) ->
                 let name = var.name in
                 let value = var.value in
                 let type_ = var.type_ in
                 let variables_reference = var.var_ref in
                 Variable.make ~name ~value ~type_ ~variables_reference ())
        in
        Lwt.return (Variables_command.Result.make ~variables ()));
    Debug_rpc.set_command_handler rpc
      (module Exception_info_command)
      (fun _ ->
        DL.log (fun m -> m "Exception info request received");
        let exception_info = Debugger.get_exception_info dbg in
        let exception_id = exception_info.id in
        let description = exception_info.description in
        let break_mode = Exception_break_mode.Always in
        Lwt.return
          (Exception_info_command.Result.make ~exception_id ~description
             ~break_mode ()));
    Lwt.return ()
end
