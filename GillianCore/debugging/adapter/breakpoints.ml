open DebugProtocolEx
module DL = Debugger_log

module Make (Debugger : Debugger.S) = struct
  let run dbg rpc =
    Lwt.pause ();%lwt
    DL.set_rpc_command_handler rpc ~name:"Set breakpoints"
      (module Set_breakpoints_command)
      (fun args ->
        let source =
          args.Set_breakpoints_command.Arguments.source.Source.path
        in
        let bp_list =
          args.Set_breakpoints_command.Arguments.breakpoints
          |> Option.value ~default:[]
          |> Stdlib.List.map (fun bp -> bp.Source_breakpoint.line)
        in
        let breakpoints =
          bp_list
          |> Stdlib.List.map (fun line ->
                 Breakpoint.make ~id:(Some line) ~verified:true
                   ~line:(Some line) ())
        in
        Debugger.set_breakpoints source bp_list dbg;
        Lwt.return Set_breakpoints_command.Result.(make ~breakpoints ()));
    Lwt.join []
end
