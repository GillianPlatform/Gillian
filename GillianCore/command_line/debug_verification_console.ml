open Cmdliner

module Make (PC : ParserAndCompiler.S) (Debug_adapter : Debug_adapter.S) :
  Console.S = struct
  module Common_args = Common_args.Make (PC)

  let manual =
    let doc = "Disable automatic folding and unfolding heuristics." in
    Arg.(value & flag & info [ "m"; "manual" ] ~doc)

  let debug_verify_info =
    let doc = "Starts Gillian in debugging mode for verification" in
    let man =
      [
        `S Manpage.s_description;
        `P
          "Starts Gillian in debugging mode for verification, which \
           communicates via the Debug Adapter Protocol";
      ]
    in
    Cmd.info "debugverify" ~doc ~man

  let start_debug_adapter manual () =
    Config.current_exec_mode := Utils.Exec_mode.Verification;
    Config.manual_proof := manual;
    Lwt_main.run (Debug_adapter.start Lwt_io.stdin Lwt_io.stdout)

  let debug_verify_t = Common_args.use Term.(const start_debug_adapter $ manual)
  let debug_verify_cmd = Cmd.v debug_verify_info debug_verify_t
  let cmds = [ debug_verify_cmd ]
end
