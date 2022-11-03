include
  Gillian.CommandLine.ParserAndCompiler.S
    with type init_data = Global_env.t
     and module Annot = CAnnot

val init_compcert : unit -> unit
