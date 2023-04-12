include
  Gillian.Command_line.ParserAndCompiler.S
    with type init_data = Global_env.t
     and module Annot = Gillian.Gil_syntax.Annot.Basic

val init_compcert : unit -> unit
