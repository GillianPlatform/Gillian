include
  Gillian.Command_line.ParserAndCompiler.S
    with type init_data = unit
     and type tl_ast = Goto_lib.Program.t
     and module Annot = Kanillian_compiler.K_annot
