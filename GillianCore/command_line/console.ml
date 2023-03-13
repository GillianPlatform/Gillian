module type S = sig
  val cmds : unit Cmdliner.Cmd.t list
end
