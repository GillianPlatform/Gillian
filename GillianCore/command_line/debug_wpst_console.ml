open Cmdliner

module Make (PC : ParserAndCompiler.S) (Debug_adapter : Debug_adapter.S) :
  Console.S = struct
  module Common_args = Common_args.Make (PC)

  let debug_wpst_info =
    let doc =
      "Starts Gillian in debugging mode for whole-program symbolic testing"
    in
    let man =
      [
        `S Manpage.s_description;
        `P
          "Starts Gillian in debugging mode for whole-program symbolic \
           testing, which communicates via the Debug Adapter Protocol";
      ]
    in
    Cmd.info "debugwpst" ~doc ~man

  let start_debug_adapter () =
    Config.current_exec_mode := Utils.Exec_mode.Symbolic;
    Lwt_main.run (Debug_adapter.start Lwt_io.stdin Lwt_io.stdout)

  let debug_wpst_t = Common_args.use Term.(const start_debug_adapter)
  let debug_wpst_cmd = Cmd.v debug_wpst_info debug_wpst_t
  let cmds = [ debug_wpst_cmd ]
end
