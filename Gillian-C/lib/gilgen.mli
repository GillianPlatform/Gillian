open Compcert
open CConstants
open Gillian.Gil_syntax

type symbol

val is_def_sym : symbol -> bool

val sym_name : symbol -> string

val make_init_proc : string Cmd.t list -> (Annot.t, string) Proc.t

val trans_program :
  ?exec_mode:ExecMode.t ->
  ?gil_annot:Gil_logic_gen.gil_annots ->
  clight_prog:Clight.program ->
  Csharpminor.program ->
  (Annot.t, string) Prog.t * string Cmd.t list * symbol list

val trans_program_with_annots :
  ExecMode.t ->
  Clight.program ->
  Csharpminor.program ->
  CLogic.CProg.t ->
  (Annot.t, string) Prog.t * string Cmd.t list * symbol list
