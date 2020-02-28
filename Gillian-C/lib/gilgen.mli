open Compcert
open CConstants
open Gillian.Gil_syntax

val trans_program :
  ?exec_mode:ExecMode.t ->
  ?gil_annot:Gil_logic_gen.gil_annots ->
  clight_prog:Clight.program ->
  Csharpminor.program ->
  (Annot.t, string) Prog.t

val trans_program_with_annots :
  ExecMode.t ->
  Clight.program ->
  Csharpminor.program ->
  CLogic.CProg.t ->
  (Annot.t, string) Prog.t
