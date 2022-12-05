module Make (Debugger : Debugger.S) = struct
  module Lifecycle = Lifecycle.Make (Debugger)
  module TimeTravel = Time_travel.Make (Debugger)
  module Inspect = Inspect.Make (Debugger)
  module Breakpoints = Breakpoints.Make (Debugger)

  let run launch_args dbg rpc =
    Lwt.join
      [
        Lifecycle.run launch_args dbg rpc;
        Inspect.run dbg rpc;
        TimeTravel.run dbg rpc;
        Breakpoints.run dbg rpc;
      ]
end
