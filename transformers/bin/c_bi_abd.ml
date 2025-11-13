open Prebuilt.Lib.C_ALoc
module SMemory = MonadicSMemory

module Bi_cli =
  Transformers_abductor.Abductor.Cli (MyInitData) (SMemory) (ParserAndCompiler)
    (ExternalSemantics)

let act_cmd =
  match Bi_cli.cmds with
  | [ Normal cmd ] -> cmd
  | _ -> failwith "Unexpected command structure in bi-abduction CLI"

let () = exit (Cmdliner.Cmd.eval act_cmd)
