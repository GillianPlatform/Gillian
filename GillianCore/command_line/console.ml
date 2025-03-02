type cmd =
  | Normal of unit Cmdliner.Cmd.t
  | Debug of unit Cmdliner.Cmd.t
  | Lsp of unit Cmdliner.Cmd.t

module type S = sig
  val cmds : cmd list
end
