module type S = sig
  val cmds : int Cmdliner.Cmd.t list
end
