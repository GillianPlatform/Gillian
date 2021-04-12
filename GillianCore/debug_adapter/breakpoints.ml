(* open Debug_protocol_ex

let run ~dbg rpc =
  Lwt.pause ();%lwt
  Debug_rpc.set_command_handler
    rpc
    (module Set_breakpoints_command)
    (fun args ->
      let () = Log.info "Set breakpoints request received" in
      let source =
        Option.get args.Set_breakpoints_command.Arguments.source.Source.path
      in
      let bp_list =
        args.Set_breakpoints_command.Arguments.breakpoints
        |> Option.value ~default:[]
        |> Stdlib.List.map (fun bp -> bp.Source_breakpoint.line)
      in
      let bp_set = IntSet.of_list bp_list in
      let breakpoints =
        bp_list
        |> Stdlib.List.map (fun line ->
               Breakpoint.make
                 ~id:(Some line)
                 ~verified:true
                 ~line:(Some line)
                 ())
      in
      Debugger.set_breakpoints source bp_set dbg;
      Lwt.return Set_breakpoints_command.Result.(make ~breakpoints ()));
  Debug_rpc.set_command_handler
    rpc
    (module Breakpoint_locations_command)
    (fun args ->
      let () = Log.info "Breakpoint locations request received" in
      let breakpoints =
        [ Breakpoint_location.make
            ~line:args.Breakpoint_locations_command.Arguments.line
            ()
        ]
      in
      Lwt.return Breakpoint_locations_command.Result.(make ~breakpoints ()));
  Lwt.join [] *)
