open Protocol

module Make (Debugger : Debugger.S) = struct
  module Lifecycle = Lifecycle.Make (Debugger)
  module TimeTravel = Time_travel.Make (Debugger)
  module Inspect = Inspect.Make (Debugger)
  module Breakpoints = Breakpoints.Make (Debugger)

  let run cfg =
    let () =
      Debugger_log.set_debug_state_dumper (fun () ->
          Debugger.Inspect.dump_state cfg.dbg)
    in
    Lwt.join
      [
        Lifecycle.run cfg;
        Inspect.run cfg;
        TimeTravel.run cfg;
        Breakpoints.run cfg;
      ]
end
