module Make (Debugger : Debugger.S) = struct
  module Lifecycle = Lifecycle.Make (Debugger)
  module TimeTravel = Time_travel.Make (Debugger)
  module Inspect = Inspect.Make (Debugger)
  module Breakpoints = Breakpoints.Make (Debugger)

  let run launch_args dbg rpc =
    let dump_dbg () =
      Debugger.Inspect.(get_debug_state dbg |> debug_state_view_to_yojson)
    in
    Lwt.join
      [
        Lifecycle.run ~dump_dbg launch_args dbg rpc;
        Inspect.run ~dump_dbg dbg rpc;
        TimeTravel.run ~dump_dbg dbg rpc;
        Breakpoints.run ~dump_dbg dbg rpc;
      ]
end
